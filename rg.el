;;; rg.el --- Use ripgrep (grep and ag replacement) like rgrep.

;; Copyright (C) 1985-1987, 1993-1999, 2001-2015 Free Software
;; Foundation, Inc.
;; Copyright (C) 2016-2017 David Landell <david.landell@sunnyhill.email>
;;
;; Author: David Landell <david.landell@sunnyhill.email>
;;         Roland McGrath <roland@gnu.org>
;; Version: 1.1.0
;; Homepage: https://github.com/davja/rg.el
;; Package-Requires: ((cl-lib "0.5") (s "1.10.0"))
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
;; way to Emacs built in grep package.  It depends on and reuses parts
;; of built in grep and is mostly adjustments to ripgrep's behavior
;; and output.

;; `rg' is the main entry point and works very much like builtin `rgrep'.

;;; Code:

(require 'cl-lib)
(require 'grep)
(require 's)

(defgroup rg nil
  "Settings for rg."
  :group 'tools
  :group 'external)

(defcustom rg-custom-type-aliases
  '(("gn" .    "*.gn *.gni")
    ("gyp" .    "*.gyp *.gypi"))
  "Alist of file type aliases that are added to the 'rg' builtin aliases."
  :type 'alist)

(defvar rg-builtin-type-aliases nil
  "Cache for 'rg --type-list'.")

(defvar rg-command "rg --no-heading --color always --colors match:fg:red"
  "Command string for invoking rg.")

(defvar rg-last-search nil
  "Stores parameters of last search.  Becomes buffer local in rg-mode buffers.")

(defconst rg-special-type-aliases
  '(("all" . "all defined type aliases") ; rg --type all
    ("everything" . "*")) ; rg wihtout '--type' arg
  "Type aliases that is not output by 'rg --type-list' but is used for specialpurposes.")

(defconst rg-mode-font-lock-keywords
  '(;; Command output lines.
    (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or address\\)\\)$"
     1 grep-error-face)
    ;; remove match from grep-regexp-alist before fontifying
    ("^rg[/a-zA-z]* started.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
    ("^rg[/a-zA-z]* finished \\(?:(\\(matches found\\))\\|with \\(no matches found\\)\\).*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 compilation-info-face nil t)
     (2 compilation-warning-face nil t))
    ("^rg[/a-zA-z]* \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 grep-error-face)
     (2 grep-error-face nil t))
    ;; "filename-linenumber-" format is used for context lines in GNU
    ;; grep and rg,
    ;; "filename=linenumber=" for lines with function names in "git grep -p".
    ("^.+?[-=][0-9]+[-=].*\n" (0 grep-context-face))))

(defvar rg-mode-map
  (let ((map grep-mode-map))
    (define-key map "c" 'rg-rerun-toggle-case)
    (define-key map "i" 'rg-rerun-toggle-ignore)
    (define-key map "r" 'rg-rerun-change-regexp)
    (define-key map "f" 'rg-rerun-change-files)
    (define-key map "d" 'rg-rerun-change-dir)
    map))

(defun rg-build-type-add-args ()
"Build a string of --type-add: 'foo:*.foo' flags for each type in `rg-custom-type-aliases'."
  (mapconcat
   (lambda (typedef)
     (let ((name (car typedef))
           (globs (cdr typedef)))
       (mapconcat
        (lambda (glob)
          (concat "--type-add '" name ":" glob "'"))
        (split-string globs) " ")))
   rg-custom-type-aliases " "))

(defun rg-build-template(&optional type custom)
"Create command line template. Wehn TYPE is non nil type flag template
will be added. CUSTOM is a custom file matching pattern that will be
added as a '--type-add' on the rg command line."
  (concat
   rg-command
   " "
   (rg-build-type-add-args) " "
   (when type
     (concat
      (when custom
        (concat "--type-add 'custom:" custom "' "))
      "--type <F> "))
   "<C> <R>"))

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
"Return supported type aliases.  If NOSPECIAL is non nil the `rg-special-type-aliases' will not be included."
  (unless rg-builtin-type-aliases
    (setq rg-builtin-type-aliases (rg-list-builtin-type-aliases)))
  (append rg-builtin-type-aliases rg-custom-type-aliases
          (unless nospecial rg-special-type-aliases)))

(defun rg-read-files (regexp)
"Read files arg for interactive rg.  REGEXP is the search string."
  (let* ((bn (or (buffer-file-name)
                 (replace-regexp-in-string "<[0-9]+>\\'" "" (buffer-name))))
         (fn (and bn
                  (stringp bn)
                  (file-name-nondirectory bn)))
         (default-alias
           (and fn
                (cl-find-if
                 (lambda (alias)
                   (string-match (mapconcat
                                  'wildcard-to-regexp
                                  (split-string (cdr alias) nil t)
                                  "\\|")
                                 fn))
                 (rg-get-type-aliases t))))
         (files (completing-read
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
    files))

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
        ;; Highlight rg matches and delete marking sequences.
        (while (re-search-forward "\033\\[38;5;9m\\(.*?\\)\033\\[[0-9]*m" end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face grep-match-face)
                         t t))
        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
          (replace-match "" t t))))))

(define-compilation-mode rg-mode "rg"
"Sets `grep-last-buffer' and `compilation-window-height'."
  (setq grep-last-buffer (current-buffer))
  (set (make-local-variable 'tool-bar-map) grep-mode-tool-bar-map)
  (set (make-local-variable 'compilation-error-face)
       grep-hit-face)
  (set (make-local-variable 'compilation-error-regexp-alist)
       grep-regexp-alist)
  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match.
  (set (make-local-variable 'compilation-directory-matcher) '("\\`a\\`"))
  (set (make-local-variable 'compilation-process-setup-function)
       'grep-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns)
       grep-error-screen-columns)
  (make-local-variable 'rg-last-search)
  (add-hook 'compilation-filter-hook 'rg-filter nil t))

(defun rg-expand-template (template regexp &optional files dir excl)
"Patch rg TEMPLATE string replacing <C>, <D>, <F>, <R>, and <X>."
  (when (string-match "<C>" template)
    (setq template
          (replace-match
           (if (and case-fold-search
                    (isearch-no-upper-case-p regexp t))
               "-i"
             "")
           t t template)))
  (grep-expand-template template regexp files dir excl))

(defalias 'kill-rg 'kill-compilation)

(defun rg-toggle-command-flag (flag)
"Remove FLAG from last search command line if present or add it if
not present."
  (let ((command (car compilation-arguments)))
    (setcar compilation-arguments
            (if (s-contains-p (concat " " flag " ") command)
                (s-replace (concat flag " ") "" command)
              (s-replace "rg" (concat "rg " flag) command)))))

(defmacro rg-rerun-with-changes (spec &rest body)
"Rerun last search with parameters VAR-REGEXP, VAR-FILES and VAR-DIR. Modify the
parameters in BODY.

\(fn (VAR-REGEXP VAR-FILES VAR-DIR) BODY...)"
  (declare (debug (symbolp symbolp symbolp body))
           (indent 1))
  (let ((regexp-sym (nth 0 spec))
        (files-sym (nth 1 spec))
        (dir-sym (nth 2 spec)))
    `(cl-destructuring-bind (,regexp-sym ,files-sym ,dir-sym) rg-last-search
       ,@body
       (rg ,regexp-sym ,files-sym ,dir-sym))))

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

;;;###autoload
(defun rg-rerun-toggle-case ()
"Rerun last search with toggled case sensitivity setting."
  (interactive)
  (rg-toggle-command-flag "-i")
  (recompile))

;;;###autoload
(defun rg-rerun-toggle-ignore ()
"Rerun last search with toggled '--no-ignore' flag."
  (interactive)
  (rg-toggle-command-flag "--no-ignore")
  (recompile))

;;;###autoload
(defun rg-rerun-change-regexp()
"Rerun last search but prompt for new regexp."
  (interactive)
  (rg-rerun-with-changes (regexp files dir)
    (let ((read-from-minibuffer-orig (symbol-function 'read-from-minibuffer)))
      ;; Override read-from-minibuffer in order to insert the original
      ;; regexp in the input area.
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (prompt &optional initial-contents &rest args)
                   (apply read-from-minibuffer-orig prompt regexp args))))
        (setq regexp (rg-read-regexp "Search for" regexp 'grep-regexp-history))))))

;;;###autoload
(defun rg-rerun-change-files()
"Rerun last search but prompt for new files."
  (interactive)
  (rg-rerun-with-changes (regexp files dir)
    (setq files (completing-read
                 (concat "Repeat search in files (default: [" files "]): ")
                 (rg-get-type-aliases)
                 nil nil nil 'grep-files-history
                 files))))

;;;###autoload
(defun rg-rerun-change-dir()
"Rerun last search but prompt for new dir."
  (interactive)
  (rg-rerun-with-changes (regexp files dir)
    (setq dir (read-directory-name "In directory: "
                                   dir nil))))

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
        (setq command (rg-expand-template
                       (rg-build-template
                        (not (equal files "everything"))
                        (unless (assoc files (rg-get-type-aliases))
                          (let ((pattern files))
                            (setq files "custom")
                            pattern)))
                       regexp
                       files))
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
