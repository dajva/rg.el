;;; rg.el --- A search tool based on ripgrep. -*- lexical-binding: t; -*-

;; Copyright (C) 1985-1987, 1993-1999, 2001-2015 Free Software
;; Foundation, Inc.
;; Copyright (C) 2016-2017 David Landell <david.landell@sunnyhill.email>
;;
;; Author: David Landell <david.landell@sunnyhill.email>
;;         Roland McGrath <roland@gnu.org>
;; Version: 1.4.1
;; URL: https://github.com/dajva/rg.el
;; Package-Requires: ((cl-lib "0.5") (emacs "24") (s "1.10.0"))
;; Keywords: matching, tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This package is a frontend to ripgrep (rg) and works in a similar
;; way to Emacs built in `rgrep' command or external `ag' if you like.
;; It depends on and reuse parts of built in grep with adjustments to
;; ripgrep and is compatible with `wgrep'.

;; Install the package and and use the default key bindings:
;; (rg-enable-default-bindings)
;;
;; The default key binding prefix is C-c s but can be changed by
;; supplying a prefix of choice to the above function call:
;; (rg-enable-default-bindings "\M-s")

;; `rg' is the main entry point but there are functions for easy
;; searching:
;; `rg-project' - Search in a project.
;; `rg-dwim' - Hands free search.  Search thing at point in project in
;; files matching the type alias of the current buffer file name.

;; The search results buffer has bindings for modification of
;; the last search for quick reruns with refined parameters.
;; Possible refinements are: toggle case insensitive search, toggle
;; '--no-ignore' flag, change directory, change file pattern and change
;; search string.  See `rg-mode' for details.

;; This package by default use the setting of
;; `case-fold-search' variable to decide whether to do a case
;; sensitive search or not, similar to the '--smart-case' rg flag.
;; This can be changed by changing the `rg-ignore-case' variable.

;; ripgrep has built in type aliases that can be selected on
;; invocation of `rg'.  Customize `rg-custom-type-aliases' to add your
;; own aliases:
;; (setq rg-custom-type-aliases
;;   '(("foo" .    "*.foo *.bar")
;;     ("baz" .    "*.baz *.qux")))

;; The `rg-define-toggle' macro can be used to define a toggle-able
;; flag for the rg command line.  Such flags can then be toggled from
;; the results buffer to repeat the search with updated flags.

;; The two `rg-save-search' functions will allow for saving search
;; result buffers with or without custom naming.
;; `rg-list-searches' will display a list of all search buffers with
;; search info and allow jumping to results.

;; The default configuration of this package is compatible with `wgrep'.
;; If grouped mode and/or show columns is enabled you need to install
;; the wgrep-ag package from MELPA and configure it like this:
;; (add-hook 'rg-mode-hook 'wgrep-ag-setup)

;;; Code:

(require 'cl-lib)
(require 'edmacro)
(require 'grep)
(require 'ibuf-ext)
(require 'ibuffer)
(require 's)
(require 'vc-hooks)


;; Customizations/public vars
(defgroup rg nil
  "Settings for rg."
  :group 'tools
  :group 'external)

(defcustom rg-custom-type-aliases
  '(("gn" .    "*.gn *.gni")
    ("gyp" .    "*.gyp *.gypi"))
  "Alist of file type aliases that are added to the 'rg' built in aliases."
  :type '(alist :key-type string :value-type string)
  :group 'rg)

(defcustom rg-command-line-flags nil
  "List of command line flags for rg."
  :type '(repeat string)
  :group 'rg)

(defcustom rg-group-result nil
  "Group matches in the same file together.
If nil, the file name is repeated at the beginning of every match line."
  :type 'boolean
  :group 'rg)

(defcustom rg-show-columns nil
  "If t, show the columns of the matches in the output buffer."
  :type 'boolean
  :group 'rg)

(defcustom rg-ignore-case 'case-fold-search
  "Decides which mode of case insensitive search that is enabled.
CASE-FOLD-SEARCH means that the variable `case-fold-search' will
trigger smart-case functionality if non nil.
SMART means that case insensitive search will be triggered if the
search pattern contains only lower case.  If the pattern contains upper
case letters, case sensitive search will be performed.  This is similar
to the rg '--smart-case' flag.
FORCE will force case insensitive search regardless of the content of
the search pattern.
NIL means case sensitive search will be forced."
  :type '(choice (const :tag "Case Fold Search" case-fold-search)
                 (const :tag "Smart" smart)
                 (const :tag "Force" force)
                 (const :tag "Off" nil))
  :group 'rg)

(defcustom rg-keymap-prefix "\C-cs"
  "Prefix for global `rg' keymap."
  :type 'string
  :group 'rg)

(defcustom rg-show-header t
  "Show header in results buffer if non nil."
  :type 'boolean
  :group 'rg)

(defvar rg-filter-hook nil
  "Hook for new content in the rg buffer.
This hook is called every time the rg buffer has been updated with
new content and filtered through the `rg-filter' function.")

(defvar rg-finished-functions nil
  "Functions to call when a search is finished.
Each function is called with two arguments: the compilation buffer,and
a string describing how the process finished.")


;; Faces
(defgroup rg-face nil
  "Settings for rg faces."
  :group 'rg)

(defface rg-match-face
  `((t :inherit ,grep-match-face))
  "face for match highlight"
  :group 'rg-face)

(defface rg-error-face
  `((t :inherit ,grep-error-face))
  "face for error"
  :group 'rg-face)

(defface rg-context-face
  `((t :inherit ,grep-context-face))
  "face for context lines"
  :group 'rg-face)

(defface rg-info-face
  '((t :inherit compilation-info))
  "face for info"
  :group 'rg-face)

(defface rg-warning-face
  '((t :inherit compilation-warning))
  "face for warning"
  :group 'rg-face)

(defface rg-filename-face
  '((t :inherit rg-info-face))
  "face for filename"
  :group 'rg-face)

(defface rg-file-tag-face
  '((t :inherit font-lock-function-name-face))
  "face for file tag in grouped layout"
  :group 'rg-face)

(defface rg-toggle-on-face
  '((t :inherit rg-file-tag-face))
  "face for toggle 'on' text in header."
  :group 'rg-face)

(defface rg-toggle-off-face
  '((t :inherit rg-error-face))
  "face for toggle 'off' text in header."
  :group 'rg-face)

(defface rg-literal-face
  '((t :inherit rg-filename-face))
  "face for literal label in header."
  :group 'rg-face)

(defface rg-regexp-face
  '((t :inherit compilation-line-number))
  "face for regexp label in header."
  :group 'rg-face)


;; Internal vars
(defvar rg-builtin-type-aliases nil
  "Cache for 'rg --type-list'.")

(defvar rg-command "rg --color always --colors match:fg:red -n"
  "Command string for invoking rg.")

(defvar rg-last-search nil
  "Stores parameters of last search.  Becomes buffer local in `rg-mode' buffers.")

(defvar rg-hit-count 0
  "Stores number of hits in a search.")

(defvar rg-literal nil
  "If non nil do literal search instead of regexp search.")

(defvar rg-toggle-command-line-flags nil
  "List of command line flags defined by `rg-define-toggle' macro.")

(defvar rg-history nil "History for full rg commands.")
(defvar rg-files-history nil "History for files args.")
(defvar rg-pattern-history nil "History for search patterns.")

(defconst rg-special-type-aliases
  '(("all" . "all defined type aliases") ; rg --type all
    ("everything" . "*")) ; rg without '--type' arg
  "Internal type aliases for special purposes.
These are not produced by 'rg --type-list' but we need them anyway.")

(defconst rg-mode-font-lock-keywords
  '(;; Command output lines.
    (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or address\\)\\)$"
     1 'rg-error-face)
    ;; remove match from grep-regexp-alist before fontifying
    ("^rg[/a-zA-z]* started.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
    ("^rg[/a-zA-z]* finished \\(?:(\\([0-9]+ matches found\\))\\|with \\(no matches found\\)\\).*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 'rg-info-face nil t)
     (2 'rg-warning-face nil t))
    ("^rg[/a-zA-z]* \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 'rg-error-face)
     (2 'rg-error-face nil t))
    ;; "filename-linenumber-" or "linenumber-" format is used for
    ;; context lines in rg
    ("^\\(?:.+?-\\)?[0-9]+-.*\n" (0 'rg-context-face))))

(defconst rg-search-list-buffer-name "*Searches rg*")

(defvar rg-mode-map
  (let ((map (copy-keymap grep-mode-map)))
    (define-key map "c" 'rg-rerun-toggle-case)
    (define-key map "d" 'rg-rerun-change-dir)
    (define-key map "f" 'rg-rerun-change-files)
    (define-key map "g" 'rg-recompile)
    (define-key map "i" 'rg-rerun-toggle-ignore)
    (define-key map "l" 'rg-list-searches)
    (define-key map "r" 'rg-rerun-change-regexp)
    (define-key map "s" 'rg-save-search)
    (define-key map "S" 'rg-save-search-as-name)
    (define-key map "t" 'rg-rerun-change-literal)
    (define-key map "\C-n" 'rg-next-file)
    (define-key map "\C-p" 'rg-prev-file)
    map)
  "The keymap for `rg-mode'.")

(defvar rg-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'rg-dwim)
    (define-key map "k" 'rg-kill-saved-searches)
    (define-key map "l" 'rg-list-searches)
    (define-key map "p" 'rg-project)
    (define-key map "r" 'rg)
    (define-key map "s" 'rg-save-search)
    (define-key map "S" 'rg-save-search-as-name)
    (define-key map "t" 'rg-literal)
    map)
  "The global keymap for `rg'.")


;; Defuns
(defun rg-process-setup ()
  "Setup compilation variables and buffer for `rg'.
Set up `compilation-exit-message-function' and run `grep-setup-hook'."
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
         (if (eq status 'exit)
             ;; This relies on the fact that `compilation-start'
             ;; sets buffer-modified to nil before running the command,
             ;; so the buffer is still unmodified if there is no output.
             (cond ((and (zerop code) (buffer-modified-p))
                    `(,(format "finished (%d matches found)\n" rg-hit-count) . "matched"))
                   ((not (buffer-modified-p))
                    '("finished with no matches found\n" . "no match"))
                   (t
                    (cons msg code)))
           (cons msg code))))
  ;; Run this hook to intergrate with wgrep
  (run-hooks 'grep-setup-hook))

(defun rg-build-type-add-args ()
  "Build a list of --type-add: 'foo:*.foo' flags for each type in `rg-custom-type-aliases'."
  (mapcar
   (lambda (typedef)
     (let ((name (car typedef))
           (globs (cdr typedef)))
       (mapconcat
        (lambda (glob)
          (concat "--type-add "
                  (shell-quote-argument (concat name ":" glob))))
        (split-string globs) " ")))
   rg-custom-type-aliases))

(defun rg-build-template(&optional type custom)
  "Create command line template.  When TYPE is non nil, type flag template
will be added.  Optional CUSTOM is a file matching pattern that will be
added as a '--type-add' parameter to the rg command line."
  (let ((args (append
               (list (if rg-group-result
                         "--heading"
                       "--no-heading"))
               (rg-build-type-add-args)
               rg-command-line-flags
               rg-toggle-command-line-flags
               (list "-e" "<R>"))))
    (when rg-literal
      (setq args (cons "--fixed-strings" args)))
    (when rg-show-columns
      (setq args (cons "--column" args)))
    (when type
      (setq args (cons "--type <F>" args))
      (when custom
        (setq args (cons
                    (concat "--type-add "
                            (shell-quote-argument (concat "custom:" custom)))
                    args))))
    (mapconcat 'identity (cons rg-command (delete-dups args)) " ")))

(defun rg-list-builtin-type-aliases ()
  "Invokes rg --type-list and puts the result in an alist."
  (unless (executable-find "rg")
    (error "'rg' is not in path"))
  (mapcar
   (lambda (item)
     (let ((association (split-string item ":" t)))
       (cons (s-trim (car association))
             (s-trim
              (mapconcat 'identity (split-string (cadr association) "," t ) " ")))))
   (nbutlast (split-string
              (shell-command-to-string "rg --type-list") "\n") 1)))


(defun rg-get-type-aliases (&optional nospecial)
  "Return supported type aliases.
If NOSPECIAL is non nil the `rg-special-type-aliases' will not be
included."
  (unless rg-builtin-type-aliases
    (setq rg-builtin-type-aliases (rg-list-builtin-type-aliases)))
  (append rg-custom-type-aliases rg-builtin-type-aliases
          (unless nospecial rg-special-type-aliases)))

(defun rg-default-alias ()
  "Return the default alias by matching alias globs with the buffer file name."
  (let* ((bn (or (buffer-file-name)
                 (replace-regexp-in-string "<[0-9]+>\\'" "" (buffer-name))))
         (fn (and bn
                  (stringp bn)
                  (file-name-nondirectory bn))))
    (or
     (and fn
          (cl-find-if
           (lambda (alias)
             (string-match (mapconcat
                            'wildcard-to-regexp
                            (split-string (cdr alias) nil t)
                            "\\|")
                           fn))
           (rg-get-type-aliases t)))
     '("all" . "*"))))

(defun rg-read-input (&optional literal)
  "Prompt user for input and return a list of the results.
If LITERAL is non nil prompt for literal pattern."
  (let* ((pattern (rg-read-pattern nil literal))
         (files (rg-read-files pattern))
         (dir (read-directory-name "In directory: "
                                   nil default-directory t)))
    (list pattern files dir)))

(defun rg-read-files (pattern)
  "Read files argument for interactive rg.  PATTERN is the search string."
  (let ((default-alias (rg-default-alias)))
    (completing-read
     (concat "Search for \"" pattern
             "\" in files"
             (if default-alias
                 (concat
                  " (default: [" (car default-alias) "] = "
                  (cdr default-alias) ")"))
             ": ")
     (rg-get-type-aliases)
     nil nil nil 'rg-files-history
     (car default-alias))))

(defun rg-read-pattern (&optional default literal)
  "Read search pattern argument from user.
DEFAULT is the default pattern to use at the prompt.  If LITERAL is
  non nil prompt for literal string."
  (let ((default (or default (grep-tag-default)))
        (prompt (concat
                 (if (or literal rg-literal)
                     "Literal"
                   "Regexp")
                 " search for")))
    (with-no-warnings
      (if (and (= emacs-major-version 24)
               (< emacs-minor-version 3))
          (read-string
           (concat prompt
                   (if (and default (> (length default) 0))
                       (format " (default \"%s\"): " default) ": "))
           default 'rg-pattern-history)
        (read-regexp prompt default 'rg-pattern-history)))))

(defun rg-filter ()
  "Handle match highlighting escape sequences inserted by the rg process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Add File: in front of filename
        (when rg-group-result
          (while (re-search-forward "^\033\\[m\033\\[35m\\(.*?\\)\033\\[m$" end 1)
            (replace-match (concat (propertize "File:"
                                               'face nil 'font-lock-face 'rg-file-tag-face)
                                   " "
                                   (propertize (match-string 1)
                                               'face nil 'font-lock-face 'rg-filename-face))
                           t t))
          (goto-char beg))

        ;; Highlight rg matches and delete marking sequences.
        (while (re-search-forward "\033\\[m\033\\[31m\033\\[1m\\(.*?\\)\033\\[m" end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face 'rg-match-face)
                         t t)
          (setq rg-hit-count (+ rg-hit-count 1)))
        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
          (replace-match "" t t))))
    (run-hooks 'rg-filter-hook)))

;; The regexp and filter functions below were taken from ag.el
;; Kudos to the people from https://github.com/Wilfred/ag.el for these.
(defconst rg-file-line-column-pattern-nogroup
  "^\\(.+?\\):\\([1-9][0-9]*\\):\\([1-9][0-9]*\\):"
  "A regexp pattern that groups output into filename, line number and column number.")

(defconst rg-file-line-column-pattern-group
  "^\\([1-9][0-9]*\\):\\([1-9][0-9]*\\):"
  "A regexp pattern to match line number and column number with grouped output.")

(defconst rg-file-line-pattern-nogroup
  "^\\(.+?\\):\\([1-9][0-9]*\\):"
  "A regexp pattern that groups output into filename, line number.")

(defconst rg-file-line-pattern-group
  "^\\([1-9][0-9]*\\):"
  "A regexp pattern to match line number with grouped output.")

(defun rg-match-grouped-filename ()
  "Match filename backwards when a line/column match is found in grouped output mode."
  (save-match-data
    (save-excursion
      (when (re-search-backward "^File: \\(.*\\)$" (point-min) t)
        (list (match-string 1))))))

(define-compilation-mode rg-mode "rg"
  "Major mode for `rg' search results.
Commands:
\\<rg-mode-map>
\\[rg-rerun-change-dir]\t Repeat this search in another directory (`rg-rerun-change-dir').
\\[rg-rerun-change-files]\t Repeat this search with another file pattern (`rg-rerun-change-files').
\\[rg-rerun-change-regexp]\t Change the search regexp for the current search (`rg-rerun-change-regexp').
\\[rg-rerun-change-literal]\t Change the search literal for the current search (`rg-rerun-change-literal').
\\[rg-rerun-toggle-ignore]\t Repeat search with toggled '--no-ignore' flag (`rg-rerun-toggle-ignore').
\\[rg-rerun-toggle-case]\t Repeat search with toggled case insensitive setting (`rg-rerun-toggle-case').
\\[rg-save-search-as-name]\t Save search result, prompt for new name (`rg-save-search-as-name').
\\[rg-save-search]\t Save search result to some unique name (`rg-save-search').
\\[wgrep-change-to-wgrep-mode]\t Change mode to `wgrep'.

\\{rg-mode-map}"
  (setq grep-last-buffer (current-buffer))
  (set (make-local-variable 'tool-bar-map) grep-mode-tool-bar-map)
  (set (make-local-variable 'compilation-error-face)
       'rg-filename-face)
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(rg-group-with-column rg-nogroup-with-column rg-group-no-column  rg-nogroup-no-column))
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (list (cons 'rg-nogroup-no-column (list rg-file-line-pattern-nogroup 1 2))
             (cons 'rg-nogroup-with-column (list rg-file-line-column-pattern-nogroup 1 2 3))
             (cons 'rg-group-with-column (list rg-file-line-column-pattern-group 'rg-match-grouped-filename 1 2))
             (cons 'rg-group-no-column (list rg-file-line-pattern-group 'rg-match-grouped-filename 1))))

  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match.
  (set (make-local-variable 'compilation-directory-matcher) '("\\`a\\`"))
  (set (make-local-variable 'compilation-process-setup-function)
       'rg-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns) nil)
  (make-local-variable 'rg-last-search)
  (make-local-variable 'rg-hit-count)
  (make-local-variable 'rg-toggle-command-line-flags)
  (make-local-variable 'rg-literal)
  (rg-update-header-line)
  (add-hook 'compilation-filter-hook 'rg-filter nil t) )

(defun rg-project-root (file)
  "Find the project root of the given FILE."
  (or (when file
        (let ((backend (vc-backend file)))
          (or
           (when (and (require 'projectile nil t)
                      (fboundp 'projectile-project-root))
             (projectile-project-root))
           (when (and (require 'find-file-in-project nil t)
                      (fboundp 'ffip-project-root))
             (ffip-project-root))
           (when backend
             (vc-call-backend backend 'root file))
           (file-name-directory file))))
      default-directory))

(defun rg-list-toggle (elem list)
  "Remove ELEM from LIST if present or add it if not present.
Returns the new list."
  (if (member elem list)
      (delete elem list)
    (push elem list)))

(defun rg-push-uniq (elem list)
  "Add ELEM to LIST if not present.
Returns the new list."
  (if (member elem list)
      list
    (push elem list)))

(defun rg-delete-uniq (elem list)
  "Remove ELEM from LIST if present.
Returns the new list."
  (if (member elem list)
      (delete elem list)
    list))

(defun rg-build-command (pattern files)
  "Create the command for PATTERN and FILES."
  (concat (grep-expand-template
           (rg-build-template
            (not (equal files "everything"))
            (unless (assoc files (rg-get-type-aliases))
              (let ((glob files))
                (setq files "custom")
                glob)))
           pattern
           files)
          " ."))

(defun rg-header-render-label (name &optional nameface)
  "Return a fontified header label.
NAME is the label text NAMEFACE is a custom face that will be applied
to NAME."
  (concat (propertize "[" 'font-lock-face `(header-line bold))
          (propertize name 'font-lock-face `(,nameface header-line bold ))
          (propertize "]" 'font-lock-face `(header-line bold))
          ": "))

(defun rg-header-render-value (value)
  "Return a fontified header VALUE."
  (propertize value 'font-lock-face '(header-line)))

(defun rg-header-render-toggle (on)
  "Return a fontified toggle symbol.
If ON is render \"on\" string, otherwise render \"off\"."
  (let ((value (if on "on " "off"))
        (face (if on 'rg-toggle-on-face 'rg-toggle-off-face)))
    (propertize value 'font-lock-face `(bold ,face))))

;; TODO: Improve header structure to alloow for auto updates
(defun rg-update-header-line ()
  "Update the header line if `rg-show-header' is enabled."
  (when rg-show-header
    (let ((type (if rg-literal "literal" "regexp"))
          (typeface (if rg-literal 'rg-literal-face 'rg-regexp-face))
          (itemspace "  "))
      (setq header-line-format
            (if (null rg-last-search)
                (concat
                 (rg-header-render-label "command line")
                 (rg-header-render-value "no refinement"))
              (concat
               (rg-header-render-label type typeface)
               (rg-header-render-value (nth 0 rg-last-search)) itemspace
               (rg-header-render-label "files")
               (rg-header-render-value (nth 1 rg-last-search)) itemspace
               (rg-header-render-label "case")
               (rg-header-render-toggle (not (member "-i" rg-toggle-command-line-flags))) itemspace
               (rg-header-render-label "ign")
               (rg-header-render-toggle (not (member "--no-ignore" rg-toggle-command-line-flags)))))))))

(defun rg-run (pattern files dir &optional literal  confirm)
  "Execute rg command with supplied PATTERN, FILES and DIR.
If LITERAL is nil interpret PATTERN as regexp, otherwise as a literal.
CONFIRM allows the user to confirm and modify the command before
executing."
  (unless (executable-find "rg")
    (error "'rg' is not in path"))
  (unless (and (stringp pattern) (> (length pattern) 0))
    (signal 'user-error '("Empty string: No search done")))
  (unless (and (file-directory-p dir) (file-readable-p dir))
    (setq dir default-directory))
  (setq rg-literal literal)
  (rg-apply-case-flag pattern)
  (let ((command (rg-build-command pattern files))
        confirmed)
    (setq dir (file-name-as-directory (expand-file-name dir)))
    (if confirm
        (setq confirmed
              (read-from-minibuffer "Confirm: "
                                    command nil nil 'rg-history))
      (add-to-history 'rg-history command))
    ;; If user changed command we can't know the parts of the
    ;; search and needs to disable result buffer modifications.
    (cond ((and confirmed (not (string= confirmed command)))
           (setq-default rg-last-search nil)
           (setq command confirmed))
          (t
           (setq-default rg-last-search (list pattern files dir))))
    (let ((default-directory dir))
      ;; Setting process-setup-function makes exit-message-function work
      ;; even when async processes aren't supported.
      (compilation-start command 'rg-mode))
    (if (eq next-error-last-buffer (current-buffer))
        (setq default-directory dir))))

(defun rg-rerun ()
  "Run `rg-recompile' with `compilation-arguments' taken from `rg-last-search'."
  (let ((pattern (nth 0 rg-last-search))
        (files (nth 1 rg-last-search))
        (dir (nth 2 rg-last-search)))
    (setcar compilation-arguments
            (rg-build-command pattern files))
    ;; compilation-directory is used as search dir and
    ;; default-directory is used as the base for file paths.
    (setq compilation-directory dir)
    (setq default-directory compilation-directory)
    (rg-recompile)
    (rg-update-header-line)))

(defmacro rg-rerun-with-changes (varplist &rest body)
  "Rerun last search with changed parameters.
VARPLIST is a property list of the form (:parameter1 symbol1
[:parameter2 symbol2] ...) that specifies the parameters that will be
exposed in BODY.  The values of the parameters will be bound to
corresponding symbols.

BODY can modify the exposed parameters and these will be used together
with the non exposed unmodified parameters to rerun the the search.

Supported properties are :pattern, :files, :dir and :flags, where the
three first are bound to the corresponding parameters in `rg' from
`rg-last-search' and :flags is bound to
`rg-toggle-command-line-flags'.

Example:
\(rg-rerun-with-changes \(:pattern searchstring\)
  \(setq searchstring \"new string\"\)\)"
  (declare (debug ((&rest symbolp symbolp) body))
           (indent 1))
  (let ((pattern (or (plist-get varplist :pattern) (cl-gensym)))
        (files (or (plist-get varplist :files) (cl-gensym)))
        (dir (or (plist-get varplist :dir) (cl-gensym)))
        (flags (or (plist-get varplist :flags) (cl-gensym))))
    `(if rg-last-search
         (cl-destructuring-bind (,pattern ,files ,dir) rg-last-search
           (let ((,flags rg-toggle-command-line-flags))
             ,@body
             (setq rg-toggle-command-line-flags ,flags)
             (setq rg-last-search (list ,pattern ,files ,dir))
             (rg-rerun)))
       (message "Can't refine search since full command line search was used."))))

(defun rg-regexp-quote (regexp)
  "Return an 'rg' REGEXP string which match exactly STRING and nothing else."
  (replace-regexp-in-string "[][*.^\\|+?{}$()\]" "\\\\\\&" regexp))

(defun rg-apply-case-flag (pattern)
  "Make sure -i is added to the command if needed.
The value of the `rg-ignore-case' variable and the case of the
supplied PATTERN influences the result.  See `rg-ignore-case' for more
detailed info."
  (if (or (eq rg-ignore-case 'force)
          (and (or (eq rg-ignore-case 'smart)
                   (and (eq rg-ignore-case 'case-fold-search) case-fold-search))
               (isearch-no-upper-case-p pattern t)))
      (setq rg-toggle-command-line-flags
            (rg-push-uniq "-i" rg-toggle-command-line-flags))
    (setq rg-toggle-command-line-flags
          (rg-delete-uniq "-i" rg-toggle-command-line-flags))))

(defun rg-single-font-lock-match (face pos limit direction)
  "Return position of next match of 'font-lock-face property that equals FACE.
POS is the start position of the search and LIMIT is the limit of the
search.  If FACE is not found within LIMIT, LIMIT is returned.  If
DIRECTION is positive search forward in the buffer, otherwise search
backward."
  (let ((single-property-change-func
         (if (> direction 0)
             'next-single-property-change
           'previous-single-property-change)))
    (while
        (progn
          (setq pos (funcall single-property-change-func pos 'font-lock-face nil limit))
          (and (not (equal pos limit))
               (not (eq (get-text-property pos 'font-lock-face) face))))))
  pos)

(defun rg-navigate-file-group (steps)
  "Move point to the a matched result group in the compilation buffer.
STEPS decides how many groups to move past.  Negative value means
backwards and positive means forwards."
  (let ((pos (point))
        (steps-left (abs steps))
        (limit
         (if (< steps 0)
             (point-min)
           (point-max))))
    (while  (and (> steps-left 0) (not (equal pos limit)))
      (setq pos (rg-single-font-lock-match 'rg-file-tag-face pos limit steps))
      (setq steps-left (- steps-left 1)))
    (unless (equal pos limit)
      (goto-char pos))))

(defun rg-rename-target ()
  "Return the buffer that will be target for renaming."
  (let ((buffer (if (eq major-mode 'rg-mode)
                    (current-buffer)
                  (get-buffer "*rg*"))))
    (or buffer
        (error "Current buffer is not an rg-mode buffer and no buffer with name '*rg*'"))))

(defalias 'kill-rg 'kill-compilation)

;;;###autoload
(defmacro rg-define-toggle (flag &optional key default)
  "Define a command line flag that can be toggled from the rg result buffer.

This will create a function with prefix 'rg-custom-toggle-flag-'
concatenated with the FLAG name, stripped of any leading dashes.  Flag
must be a form that will be evaluated to a string at macro expansion
time.  For instance, if FLAG is '--invert-match' the function name
will be 'rg-custom-toggle-flag-invert-match.  If the flag contains a
value that will be excluded from the function name.

Optional KEY is a key binding that is added to `rg-mode-map'.  If the
optional DEFAULT parameter is non nil the flag will be enabled by default."
  (let* ((flagvalue (eval flag))
         (flagname (s-chop-prefixes '("-" "-") (car (s-split " " flagvalue t))))
         (funname (concat "rg-custom-toggle-flag-" flagname)))
    `(progn
       ,(if default
            `(setq rg-toggle-command-line-flags
                   (rg-push-uniq ,flagvalue rg-toggle-command-line-flags))
          `(setq rg-toggle-command-line-flags
                 (rg-delete-uniq ,flagvalue rg-toggle-command-line-flags)))
       ,(when key
          `(define-key rg-mode-map ,key (quote ,(intern funname))))
       (defun ,(intern funname) ()
         ,(format "Rerun last search with flag '%s' toggled." flagvalue)
         (interactive)
         (rg-rerun-with-changes (:flags flags)
           (setq flags (rg-list-toggle ,flagvalue flags)))))))

(defmacro rg-save-vars (varlist &rest body)
  "Save variables in VARLIST and restore them to original values after BODY has been run."
  (declare (indent 1))
  (let ((set-pairs
         (cl-loop for var in varlist
                  collect `(,(cl-gensym) ,var))))
    `(let ,set-pairs
       (unwind-protect
           (progn ,@body)
         ,@(cl-loop for pair in set-pairs
                    collect `(setq ,@(reverse pair)))))))

(defun rg-recompile ()
  "Run `recompile' while preserving some buffer local variables."
  (interactive)
  ;; Buffer locals will be reset in recompile so we need save them
  ;; here.
  (rg-save-vars (rg-literal rg-last-search rg-toggle-command-line-flags)
    (recompile)))

(defun rg-rerun-toggle-case ()
  "Rerun last search with toggled case sensitivity setting."
  (interactive)
  (rg-rerun-with-changes (:flags flags)
    (setq flags (rg-list-toggle "-i" flags))))

(defun rg-rerun-toggle-ignore ()
  "Rerun last search with toggled '--no-ignore' flag."
  (interactive)
  (rg-rerun-with-changes (:flags flags)
    (setq flags (rg-list-toggle "--no-ignore" flags))))

(defun rg-rerun-change-search-string ()
  "Rerun last search but prompt for new search pattern."
  (rg-rerun-with-changes (:pattern pattern)
    ;; Override read-from-minibuffer in order to insert the original
    ;; pattern in the input area.
    (let ((read-from-minibuffer-orig (symbol-function 'read-from-minibuffer)))
      (cl-letf (((symbol-function #'read-from-minibuffer)
                  (lambda (prompt &optional _ &rest args)
                    (apply read-from-minibuffer-orig prompt pattern args))))
        (setq pattern (rg-read-pattern pattern))))))

(defun rg-rerun-change-regexp ()
  "Rerun last search but prompt for new regexp."
  (interactive)
  (let ((rg-literal-orig rg-literal))
    (setq rg-literal nil)
    (condition-case nil
        (rg-rerun-change-search-string)
      ((error quit) (setq rg-literal rg-literal-orig)))))

(defun rg-rerun-change-literal ()
  "Rerun last search but prompt for new literal."
  (interactive)
  (let ((rg-literal-orig rg-literal))
    (setq rg-literal t)
    (condition-case nil
        (rg-rerun-change-search-string)
      ((error quit) (setq rg-literal rg-literal-orig)))))

(defun rg-rerun-change-files()
  "Rerun last search but prompt for new files."
  (interactive)
  (rg-rerun-with-changes (:files files)
    (setq files (completing-read
                 (concat "Repeat search in files (default: [" files "]): ")
                 (rg-get-type-aliases)
                 nil nil nil 'rg-files-history
                 files))))

(defun rg-rerun-change-dir()
  "Rerun last search but prompt for new dir."
  (interactive)
  (rg-rerun-with-changes (:dir dir)
    (setq dir (read-directory-name "In directory: "
                                   dir nil))))

(defun rg-next-file (n)
  "Move point to next file with a match.
Prefix arg N decides how many
files to navigate.  When `rg-group-result' is nil this is the same as
invoking `compilation-next-error', otherwise this will navigate to the
next file with grouped matches."
  (interactive "p")
  (if rg-group-result
      (rg-navigate-file-group n)
    (compilation-next-error n)))

(defun rg-prev-file (n)
  "Move point to previous file with a match.
Prefix arg N decides how many files to navigate.  When
`rg-group-result' is nil this is the same as invoking
`compilation-previous-error', otherwise this will navigate to the
previous file with grouped matches."
  (interactive "p")
  (if rg-group-result
      (rg-navigate-file-group (- n))
    (compilation-previous-error n)))

(defun rg-save-search-as-name (newname)
  "Save the search result in current *rg* result buffer.
The result buffer will be renamed to *rg NEWNAME*.  New searches will use the
standard *rg* buffer unless the search is done from a saved buffer in
which case the saved buffer will be reused."
  (interactive "sSave search as name: ")
  (let ((buffer (rg-rename-target)))
    (with-current-buffer buffer
      (rename-buffer (concat "*rg " newname "*")))))

(defun rg-save-search ()
  "Save the search result in current *rg* result buffer.
The result buffer will be renamed by the `rename-uniquify' function.
To choose a custom name, use `rg-save-search-as-name' instead.  New
searches will use the standard *rg* buffer unless the search is done
from a saved buffer in which case the saved buffer will be reused."
  (interactive)
  (let ((buffer (rg-rename-target)))
    (with-current-buffer buffer
      (rename-uniquely)
      ;; If the new buffer name became '*rg*', just rename again to make
      ;; sure the result is saved.
      (when (equal (buffer-name) "*rg*")
        (rename-uniquely)))))

(defun rg-kill-saved-searches ()
  "Kill all saved rg buffers.  The default *rg* buffer will be kept."
  (interactive)
  (when (y-or-n-p "Confirm kill all saved rg searches? ")
    (dolist (buf (buffer-list))
      (when (and (eq (with-current-buffer buf major-mode) 'rg-mode)
                 (not (equal (buffer-name buf) "*rg*")))
        (kill-buffer buf)))))

(defun rg-ibuffer-search-updated()
  (let ((list-buffer (get-buffer rg-search-list-buffer-name)))
    (when list-buffer
      (with-current-buffer list-buffer
        (ibuffer-update nil t)))))

(defun rg-ibuffer-buffer-killed ()
  "Function run when the search list buffer is killed."
  (remove-hook 'buffer-list-update-hook #'rg-ibuffer-search-updated)
  (remove-hook 'rg-filter-hook #'rg-ibuffer-search-updated))

(define-ibuffer-column rg-search-term
  (:name "Search" :props ('face 'rg-match-face))
  (ignore mark)
  (or (car rg-last-search) "N/A"))

(define-ibuffer-column rg-hit-count
  (:name "Hits")
  (ignore mark)
  (number-to-string rg-hit-count))

(define-ibuffer-column rg-search-dir
  (:name "Directory" :props ('face 'rg-filename-face))
  (ignore mark)
  (or (nth 2 rg-last-search) "N/A"))

(define-ibuffer-column rg-file-types
  (:name "Type")
  (ignore mark)
  (or (nth 1 rg-last-search) "N/A"))

;;;###autoload
(defun rg-list-searches ()
  "List all `rg-mode' buffers in `ibuffer'."
  (interactive)
  (let ((other-window (equal current-prefix-arg '(4))))
    (ibuffer other-window rg-search-list-buffer-name '((mode . rg-mode)) nil nil nil
             '((mark " "
                     (name 16 16 nil :elide) " "
                     (rg-search-term 18 18 nil :elide) " "
                     (rg-hit-count 7 7) " "
                     (rg-file-types 7 7) " "
                     (process 10 10)
                     (rg-search-dir 20 -1 nil :elide) " ")))
    (add-hook 'rg-filter-hook #'rg-ibuffer-search-updated)
    (add-hook 'buffer-list-update-hook #'rg-ibuffer-search-updated)
    (with-current-buffer rg-search-list-buffer-name
      (set (make-local-variable 'ibuffer-use-header-line) nil)
      (ibuffer-clear-filter-groups)
      (add-hook 'kill-buffer-hook #'rg-ibuffer-buffer-killed nil t))))

;;;###autoload
(defun rg-enable-default-bindings(&optional prefix)
  "Enable the global `rg' default key bindings under PREFIX key. If
prefix is not supplied `rg-keymap-prefix' is used."
  (interactive)
  (setq prefix (or prefix rg-keymap-prefix))
  (when prefix
    (global-set-key prefix rg-global-map)
    (message "Global key bindings for `rg' enabled with prefix: %s"
             (edmacro-format-keys prefix))))

(defun rg-run-in-project (regexp files)
  "Search for `REGEXP' in files of type `FILES' starting at the rg-project-root."
  (let ((root (rg-project-root buffer-file-name)))
    (if root
        (rg-run regexp files root)
      (signal 'user-error '("No project root found")))))

;;;###autoload
(defun rg-project (regexp files)
  "Run ripgrep in current project searching for REGEXP in FILES.
The project root will will be determined by either common project
packages like projectile and `find-file-in-project' or the source
version control system."
  (interactive
   (progn
     (let* ((regexp (rg-read-pattern))
            (files (rg-read-files regexp)))
       (list regexp files))))
  (rg-run-in-project regexp files))

;;;###autoload
(defun rg-dwim-regexp (regexp)
  "Run ripgrep in current project searching for REGEXP in files like the current file."
  (interactive
   (progn
     (let* ((regexp (rg-read-pattern)))
       (list regexp))))
  (let ((files (car (rg-default-alias))))
    (rg-run-in-project regexp files)))

;;;###autoload
(defun rg-dwim (&optional curdir)
  "Run ripgrep without user interaction figuring out the intention by magic(!).
The default magic searches for thing at point in files matching
current file under project root directory.

With \\[universal-argument] prefix (CURDIR), search is done in current dir
instead of project root."
  (interactive "P")
  (let* ((literal (grep-tag-default))
         (files (car (rg-default-alias)))
         (dir (or (when curdir default-directory)
                  (rg-project-root buffer-file-name))))
    (rg-run literal files dir 'literal)))

(defun rg-literal (pattern files dir &optional confirm)
  "Run ripgrep, searching for literal PATTERN in FILES in directory DIR.
With \\[universal-argument] prefix (CONFIRM), you can edit the
constructed shell command line before it is executed."
  (interactive
   (progn
     (append (rg-read-input 'literal)
             (list (equal current-prefix-arg '(4))))))
  (rg-run pattern files dir 'literal confirm))

;;;###autoload
(defun rg (regexp files dir &optional confirm)
  "Run ripgrep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `rg-custom-type-aliases' or
ripgrep builtin type aliases, e.g.  entering `elisp' is equivalent to `*.el'.

REGEXP is a regexp as defined by the ripgrep executable.

With \\[universal-argument] prefix (CONFIRM), you can edit the
constructed shell command line before it is executed.

Collect output in a buffer.  While ripgrep runs asynchronously, you
can use \\[next-error] (M-x `next-error'), or \\<grep-mode-map>\\[compile-goto-error] \
in the rg output buffer, to go to the lines where rg found matches."
  (interactive
   (progn
     (append (rg-read-input)
             (list (equal current-prefix-arg '(4))))))
  (rg-run regexp files dir nil confirm))

(provide 'rg)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; rg.el ends here
