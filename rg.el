;;; rg.el --- A ripgrep frontend, similar to built in grep.el. -*- lexical-binding: t; -*-

;; Copyright (C) 1985-1987, 1993-1999, 2001-2015 Free Software
;; Foundation, Inc.
;; Copyright (C) 2016-2017 David Landell <david.landell@sunnyhill.email>
;;
;; Author: David Landell <david.landell@sunnyhill.email>
;;         Roland McGrath <roland@gnu.org>
;; Version: 1.3.0
;; Homepage: https://github.com/dajva/rg.el
;; Package-Requires: ((cl-lib "0.5") (emacs "24") (s "1.10.0") (seq "2.19"))
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
;; way to Emacs built in `rgrep' command.  It depends on and reuse parts
;; of built in grep with adjustments to ripgrep and is compatible with
;; `wgrep'.

;; Install the package and bind the main entry point `rg':
;; (eval-after-load
;;   (global-set-key (kbd "M-s") 'rg))

;; There are also other entry points for easy searching:
;; `rg-project' - Search in a project.
;; `rg-dwim' - Handsfree search.  Search thing at point in project in
;; files matching the type alias of the current buffer file name.

;; The `rg' results buffer has bindings for modification of
;; the last search for quick reruns with refined parameters.
;; Possible refinements are: toggle case insensitive search, toggle
;; '--no-ignore' flag, change directory, change file pattern and change
;; search string.  See `rg-mode' for details.

;; This package (just as grep.el) use the setting of
;; `case-fold-search' variable to decide whether to do a case
;; sensitive search or not.  The behavior is similar to the ripgrep
;; '--smart-case' flag in that the search will be case insensitive if
;; `case-fold-search' is non nil and search pattern is all lowercase.
;; Otherwise it's case sensitive

;; ripgrep has built in type aliases that can be selected on
;; invocation of `rg'.  Customize `rg-custom-type-aliases' to add your
;; own aliases:
;; (setq rg-custom-type-aliases
;;   '(("foo" .    "*.foo *.bar")
;;     ("baz" .    "*.baz *.qux")))

;; The `rg-define-toggle' macro can be used to define a toggleable
;; flag for the rg command line.  Such flags can then be toggled from
;; the results buffer to repeat the search with updated flags.

;; The default configuration of this package is compatible with `wgrep'.
;; If grouped mode and/or show columns is enabled you need to install
;; the wgrep-ag package from MELPA and configure it like this:
;; (add-hook 'rg-mode-hook 'wgrep-ag-setup)

;;; Code:

(require 'cl-lib)
(require 'grep)
(require 's)
(require 'seq)
(require 'vc-hooks)

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

(defvar rg-builtin-type-aliases nil
  "Cache for 'rg --type-list'.")

(defvar rg-command "rg --color always --colors match:fg:red -n"
  "Command string for invoking rg.")

(defvar rg-last-search nil
  "Stores parameters of last search.  Becomes buffer local in rg-mode buffers.")

(defvar rg-toggle-command-line-flags nil
  "List of command line flags defined by `rg-define-toggle' macro.")

(defconst rg-special-type-aliases
  '(("all" . "all defined type aliases") ; rg --type all
    ("everything" . "*")) ; rg without '--type' arg
  "Type aliases that are not produced by 'rg --type-list' but are used
for special purposes.")

(defconst rg-mode-font-lock-keywords
  '(;; Command output lines.
    (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or address\\)\\)$"
     1 'rg-error-face)
    ;; remove match from grep-regexp-alist before fontifying
    ("^rg[/a-zA-z]* started.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
    ("^rg[/a-zA-z]* finished \\(?:(\\(matches found\\))\\|with \\(no matches found\\)\\).*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 'rg-info-face nil t)
     (2 'rg-warning-face nil t))
    ("^rg[/a-zA-z]* \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 'rg-error-face)
     (2 'rg-error-face nil t))
    ;; "filename-linenumber-" or "linenumber-" format is used for
    ;; context lines in rg
    ("^\\(?:.+?-\\)*?[0-9]+-.*\n" (0 'rg-context-face))))

(defvar rg-mode-map
  (let ((map (copy-keymap grep-mode-map)))
    (define-key map "c" 'rg-rerun-toggle-case)
    (define-key map "g" 'rg-recompile)
    (define-key map "i" 'rg-rerun-toggle-ignore)
    (define-key map "r" 'rg-rerun-change-regexp)
    (define-key map "f" 'rg-rerun-change-files)
    (define-key map "d" 'rg-rerun-change-dir)
    (define-key map "s" 'rg-save-search-as-name)
    (define-key map "S" 'rg-save-search)
    (define-key map "\C-n" 'rg-next-file)
    (define-key map "\C-p" 'rg-prev-file)
    map))

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
               (list "<R>"))))
    (when rg-show-columns
      (setq args (cons "--column" args)))
    (when type
      (setq args (cons "--type <F>" args))
      (when custom
        (setq args (cons
                    (concat "--type-add "
                            (shell-quote-argument (concat "custom:" custom)))
                    args))))
    (mapconcat 'identity (cons rg-command args) " ")))

(defun rg-list-builtin-type-aliases ()
"Invokes rg --type-list and puts the result in an alist."
  (mapcar
   (lambda (item)
     (let ((association (split-string item ":" t)))
       (cons (s-trim (car association))
             (s-trim
              (mapconcat 'identity (split-string (cadr association) "," t ) " ")))))
   (nbutlast (split-string
              (shell-command-to-string "rg --type-list") "\n") 1)))


(defun rg-get-type-aliases (&optional nospecial)
"Return supported type aliases.  If NOSPECIAL is non nil the
`rg-special-type-aliases' will not be included."
  (unless rg-builtin-type-aliases
    (setq rg-builtin-type-aliases (rg-list-builtin-type-aliases)))
  (append rg-builtin-type-aliases rg-custom-type-aliases
          (unless nospecial rg-special-type-aliases)))

(defun rg-default-alias ()
"Return the default alias by matching alias globs with the buffer
file name."
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

(defun rg-read-files (regexp)
"Read files argument for interactive rg.  REGEXP is the search string."
  (let ((default-alias (rg-default-alias)))
    (completing-read
     (concat "Search for \"" regexp
             "\" in files"
             (if default-alias
                 (concat
                  " (default: [" (car default-alias) "] = "
                  (cdr default-alias) ")"))
             ": ")
     (rg-get-type-aliases)
     nil nil nil 'grep-files-history
     (car default-alias))))

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
                         t t))
        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
          (replace-match "" t t))))))

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
\\[rg-rerun-change-regexp]\t Change the search string for the current search (`rg-rerun-change-regexp').
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
       'grep-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns) nil)
  (make-local-variable 'rg-last-search)
  (make-local-variable 'rg-toggle-command-line-flags)
  (add-hook 'compilation-filter-hook 'rg-filter nil t) )

(defun rg-project-root (file)
"Find the project root of the given FILE."
  (when file
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
       (file-name-directory file)))))

(defun rg-toggle-command-flag (flag flaglist)
"Remove FLAG from FLAGLIST line if present or add it if not present."
  (let (removed)
    (setq flaglist
          (seq-remove (lambda (enabled-flag)
                        (when (equal enabled-flag flag)
                          (setq removed t)))
                      flaglist))
    (unless removed
      (setq flaglist (cons flag flaglist))))
  flaglist)

(defun rg-build-command (regexp files)
"Create the command for REGEXP and FILES."
  (concat (grep-expand-template
           (rg-build-template
            (not (equal files "everything"))
            (unless (assoc files (rg-get-type-aliases))
              (let ((pattern files))
                (setq files "custom")
                pattern)))
           regexp
           files)
          " ."))

(defun rg-rerun ()
"Run `rg-recompile' with `compilation-arguments' taken from `rg-last-search'."
  (let ((regexp (nth 0 rg-last-search))
        (files (nth 1 rg-last-search))
        (dir (nth 2 rg-last-search)))
    (setcar compilation-arguments
            (rg-build-command regexp files))
    ;; compilation-directory is used as search dir and
    ;; default-directory is used as the base for file paths.
    (setq compilation-directory dir)
    (setq default-directory compilation-directory)
    (rg-recompile)))

(defmacro rg-rerun-with-changes (varplist &rest body)
"Rerun last search with changed parameters.  VARPLIST is a property
list of the form (:parameter1 symbol1 [:parameter2 symbol2] ...) that
specifies the parameters that will be exposed in BODY.  The values of
the parameters will be bound to corresponding symbols.

BODY can modify the exposed parameters and these will be used together
with the non exposed unmodified parameters to rerun the the search.

Supported properties are :regexp, :files, :dir and :flags, where the
three first are bound to the corresponding parameters in `rg' from
`rg-last-search' and :flags is bound to
`rg-toggle-command-line-flags'.

Example:
\(rg-rerun-with-changes \(:regexp searchstring\)
  \(setq searchstring \"new string\"\)\)"
  (declare (debug ((&rest symbolp symbolp) body))
           (indent 1))
  (let ((regexp (or (plist-get varplist :regexp) (cl-gensym)))
        (files (or (plist-get varplist :files) (cl-gensym)))
        (dir (or (plist-get varplist :dir) (cl-gensym)))
        (flags (or (plist-get varplist :flags) (cl-gensym))))
    `(cl-destructuring-bind (,regexp ,files ,dir) rg-last-search
       (let ((,flags rg-toggle-command-line-flags))
         ,@body
         (setq rg-toggle-command-line-flags ,flags)
         (setq rg-last-search (list ,regexp ,files ,dir))
         (rg-rerun)))))

(defun rg-read-regexp (prompt default history)
"Read regexp argument from user.  PROMPT is the read prompt, DEFAULT is the
default regexp and HISTORY is search history list."
  (with-no-warnings
    (if (and (= emacs-major-version 24)
             (< emacs-minor-version 3))
        (read-string
         (concat prompt
                 (if (and default (> (length default) 0))
                     (format " (default \"%s\"): " default) ": "))
         default history)
      (read-regexp prompt default history))))

(defun rg-set-case-sensitivity (regexp)
"Make sure -i is added to the command if needed.  If REGEXP contain
uppercase letters, case sensitive search is forced."
  (if (and case-fold-search
           (isearch-no-upper-case-p regexp t))
      (when (not (member "-i" rg-toggle-command-line-flags))
        (push "-i" rg-toggle-command-line-flags))
    (when (member "-i" rg-toggle-command-line-flags)
      (setq rg-toggle-command-line-flags (delete "-i" rg-toggle-command-line-flags)))))

(defun rg-single-font-lock-match (face pos limit direction)
"Return position of next match of 'font-lock-face property that
equals FACE.  POS is the start position of the search and LIMIT is
the limit of the search.  If FACE is not found within LIMIT, LIMIT is
returned. If DIRECTION is positive search forward in the buffer,
otherwise search backward."
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
"Move point to the a matched result group in the compilation
buffer.  STEPS decides how many groups to move past.  Negative value
means backwards and positive means forwards."
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

(defalias 'kill-rg 'kill-compilation)

;;;###autoload
(defmacro rg-define-toggle (flag &optional key default)
"Define a command line flag that can be toggled from the rg result
buffer.

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
            `(unless (member ,flagvalue rg-toggle-command-line-flags)
               (push ,flagvalue rg-toggle-command-line-flags))
          `(when (member ,flagvalue rg-toggle-command-line-flags)
             (setq rg-toggle-command-line-flags
                   (delete ,flagvalue rg-toggle-command-line-flags))))
       ,(when key
          `(define-key rg-mode-map ,key (quote ,(intern funname))))
       (defun ,(intern funname) ()
         ,(format "Rerun last search with flag '%s' toggled." flagvalue)
         (interactive)
         (rg-rerun-with-changes (:flags flags)
           (setq flags (rg-toggle-command-flag ,flagvalue flags)))))))

(defmacro rg-save-vars (varlist &rest body)
"Save variables in VARLIST and restore them to original values after
BODY has been run."
  (declare (indent 1))
  (let ((set-pairs
         (cl-loop for var in varlist
               collect `(,(cl-gensym) ,var))))
    `(let ,set-pairs
       ,@body
       ,@(cl-loop for pair in set-pairs
               collect `(setq ,@(reverse pair))))))

(defun rg-recompile ()
"Run `recompile' while preserving buffer some local variables."
  (interactive)
  ;; Buffer locals will be reset in recompile so we need save them
  ;; here.
  (rg-save-vars (rg-last-search rg-toggle-command-line-flags)
    (recompile)))

(defun rg-rerun-toggle-case ()
"Rerun last search with toggled case sensitivity setting."
  (interactive)
  (rg-rerun-with-changes (:flags flags)
    (setq flags (rg-toggle-command-flag "-i" flags))))

(defun rg-rerun-toggle-ignore ()
"Rerun last search with toggled '--no-ignore' flag."
  (interactive)
  (rg-rerun-with-changes (:flags flags)
    (setq flags (rg-toggle-command-flag "--no-ignore" flags))))

(defun rg-rerun-change-regexp()
"Rerun last search but prompt for new regexp."
  (interactive)
  (rg-rerun-with-changes (:regexp regexp)
    (let ((read-from-minibuffer-orig (symbol-function 'read-from-minibuffer)))
      ;; Override read-from-minibuffer in order to insert the original
      ;; regexp in the input area.
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (prompt &optional _ &rest args)
                   (apply read-from-minibuffer-orig prompt regexp args))))
        (setq regexp (rg-read-regexp "Search for" regexp 'grep-regexp-history))))))

(defun rg-rerun-change-files()
"Rerun last search but prompt for new files."
  (interactive)
  (rg-rerun-with-changes (:files files)
    (setq files (completing-read
                 (concat "Repeat search in files (default: [" files "]): ")
                 (rg-get-type-aliases)
                 nil nil nil 'grep-files-history
                 files))))

(defun rg-rerun-change-dir()
"Rerun last search but prompt for new dir."
  (interactive)
  (rg-rerun-with-changes (:dir dir)
    (setq dir (read-directory-name "In directory: "
                                   dir nil))))

(defun rg-next-file (n)
"Move point to next file with a match.  Prefix arg N decides how many
files to navigate.  When `rg-group-result' is nil this is the same as
invoking `compilation-next-error', otherwise this will navigate to the
next file with grouped matches."
  (interactive "p")
  (if rg-group-result
      (rg-navigate-file-group n)
    (compilation-next-error n)))

(defun rg-prev-file (n)
"Move point to previous file with a match.  Prefix arg N decides how many
files to navigate.  When `rg-group-result' is nil this is the same as
invoking `compilation-previous-error', otherwise this will navigate to the
previous file with grouped matches."
  (interactive "p")
  (if rg-group-result
      (rg-navigate-file-group (- n))
    (compilation-previous-error n)))

(defun rg-save-search-as-name (newname)
"Save the search result in current *rg* result buffer.  The result
buffer will be renamed to *rg NEWNAME*.  New searches will use the
standard *rg* buffer unless the search is done from a saved buffer in
which case the saved buffer will be reused."
  (interactive "sSave search as name: ")
  (unless (eq major-mode 'rg-mode)
    (error "Not an rg-mode buffer"))
  (rename-buffer (concat "*rg " newname "*")))

(defun rg-save-search ()
"Save the search result in current *rg* result buffer.  The result
buffer will be renamed by the `rename-uniquify' function.  To choose a
custom name, use `rg-save-search-as-name' instead.  New searches will
use the standard *rg* buffer unless the search is done from a saved
buffer in which case the saved buffer will be reused."
  (interactive)
  (unless (eq major-mode 'rg-mode)
    (error "Not an rg-mode buffer"))
  (rename-uniquely)
  ;; If the new buffer name became '*rg*', just rename again to make
  ;; sure the result is saved.
  (when (equal (buffer-name) "*rg*")
    (rename-uniquely)))

;;;###autoload
(defun rg-project ()
"Run ripgrep in current project.  The project root will will be
determined by either common project packages like projectile and
find-file-in-project or the source version control system."
  (interactive)
  (let* ((regexp (grep-read-regexp))
         (files (rg-read-files regexp)))
    (rg regexp files (rg-project-root buffer-file-name))))

;;;###autoload
(defun rg-dwim ()
"Run rg without user interaction figuring out the user's intention by
magic(!).  The default magic searches for thing at point in files matching
current file under project root directory.

With \\[universal-argument] prefix, search is done in current dir
instead of project root."
  (interactive)
  (let* ((curdir (equal current-prefix-arg '(4)))
         (regexp (grep-tag-default))
        (files (car (rg-default-alias)))
        (dir (or (when curdir default-directory)
                 (rg-project-root buffer-file-name))))
    (rg regexp files dir)))

;;;###autoload
(defun rg (regexp &optional files dir confirm)
"Run ripgrep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `rg-custom-type-aliases' or
ripgrep builtin type aliases, e.g.  entering `elisp' is equivalent to `*.el'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `rg-command'.

Collect output in a buffer.  While ripgrep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command use the setting of `case-fold-search' variable to decide
whether to do a case sensitive search or not.  If the search regexp
contains uppercase characters the setting is overridden and case
sensitive search is performed.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (unless (executable-find "rg")
       (error "'rg' is not in path."))
     (grep-compute-defaults)
     (cond
      ((and rg-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " rg-command
                                   nil nil 'grep-history)))
      (t (let* ((regexp (grep-read-regexp))
                (files (rg-read-files regexp))
                (dir (read-directory-name "In directory: "
                                          nil default-directory t))
                (confirm (equal current-prefix-arg '(4))))
           (list regexp files dir confirm))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (unless (and dir (file-directory-p dir) (file-readable-p dir))
      (setq dir default-directory))
    (let ((command regexp))
      (if (null files)
          (if (string= command rg-command)
              (setq command nil))
        (setq dir (file-name-as-directory (expand-file-name dir)))
        (rg-set-case-sensitivity regexp)
        (setq command (rg-build-command regexp files))
        (when command
          (if confirm
              (setq command
                    (read-from-minibuffer "Confirm: "
                                          command nil nil 'grep-history))
            (add-to-history 'grep-history command))))
      (when command
        (setq-default rg-last-search (list regexp files dir))
        (let ((default-directory dir))
          ;; Setting process-setup-function makes exit-message-function work
          ;; even when async processes aren't supported.
          (compilation-start command 'rg-mode))
        (if (eq next-error-last-buffer (current-buffer))
            (setq default-directory dir))))))

(provide 'rg)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; rg.el ends here
