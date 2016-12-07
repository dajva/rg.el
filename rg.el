;;; rg.el - Use ripgrep (grep and ag replacement) from within Emacs.

;; Copyright (C) 2016 David Landell <david.landell@sunnyhill.email>
;;
;; Authors: David Landell <david.landell@sunnyhill.email>, Roland McGrath <roland@gnu.org>
;; Homepage: https://github.com/davja/rg.el

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

;; Commentary:

;; This package depends on and reuses parts of emacs builtin grep
;; package and is mostly adjustments to ripgrep's behavior and output.

(require 'grep)

(defvar rg-builtin-type-flags nil)
(defvar rg-template nil)
(defvar rg-command "rg --no-heading --color always --colors match:fg:red ")

(defcustom rg-custom-type-flags
  '(("gn" .    "*.gn*")
    ("gyp" .    "*.gyp*"))
  "Alist of aliases for the FILES argument to `rg' and `rg'."
  :type 'alist
  :group 'rg)

(defun rg-build-type-add-args ()
  "Builds a string of --type-add: 'foo:*.foo' flags for each type in
  `rg-custom-type-flags'."
  (mapconcat
   (lambda (typedef)
     (let ((name (car typedef))
           (globs (cdr typedef)))
       (mapconcat
        (lambda (glob)
          (concat "--type-add '" name ":" glob "'"))
        (split-string globs) " ")))
   rg-custom-type-flags " "))

(defun rg-compute-defaults ()
  "Compute defaults for rg invokation."
  (unless rg-template
    (setq rg-template
          (concat
           "rg --no-heading --color always --colors match:fg:red "
           (rg-build-type-add-args)
            " "
            "--type <F> <C> <R>"))))

(defun rg-list-builtin-types ()
  "Invokes rg --type-list and puts the result in an alist."
  (let ((builtins
         (mapcar
          (lambda (item)
            (let ((association (split-string item ":" t " ")))
              (cons (car association)
                    (mapconcat 'identity (split-string (cadr association) "," t " ") " "))))
          (nbutlast (split-string
                     (shell-command-to-string "rg --type-list") "\n") 1))))
    (push '("all" . ".*") builtins)))

(defun rg-get-type-flags ()
  "Returns supported type flags."
  (unless rg-builtin-type-flags
    (setq rg-builtin-type-flags (rg-list-builtin-types)))
  (append rg-builtin-type-flags rg-custom-type-flags))

(defun rg-read-files (regexp)
  "Read files arg for interactive rg."
  (let* ((bn (or (buffer-file-name)
         (replace-regexp-in-string "<[0-9]+>\\'" "" (buffer-name))))
     (fn (and bn
          (stringp bn)
          (file-name-nondirectory bn)))
     (rg-files-aliases (rg-get-type-flags))
     (default-alias
       (and fn
        (let ((aliases (remove (assoc "all" rg-files-aliases)
                       rg-files-aliases))
              alias)
          (while aliases
            (setq alias (car aliases)
              aliases (cdr aliases))
            (if (string-match (mapconcat
                       'wildcard-to-regexp
                       (split-string (cdr alias) nil t)
                       "\\|")
                      fn)
            (setq aliases nil)
              (setq alias nil)))
          alias)))
     (files (completing-read
         (concat "Search for \"" regexp
             "\" in files"
             (if default-alias
                 (concat
                  " (default: [" (car default-alias) "] = "
                  (cdr default-alias) ")"))
             ": ")
         rg-files-aliases
         nil t nil 'grep-files-history
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

(setq rg-mode-font-lock-keywords
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
  (add-hook 'compilation-filter-hook 'rg-filter nil t))

;;;###autoload
(defun rg (regexp &optional files dir confirm)
  "Run ripgrep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `rg-custom-type-flags' or
ripgrep builtin types, e.g.  entering `elisp' is equivalent to `*.el'.

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
       (error "rg is not in path."))
     (rg-compute-defaults)
     (grep-compute-defaults)
     (cond
      ((and rg-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " rg-command
                                   nil nil 'grep-history)))
      ((not rg-template)
       (error "rg.el: No `rg-template' available"))
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
        (setq command (grep-expand-template
                       rg-template
                       regexp
                       files))
        (when command
          (if confirm
              (setq command
                    (read-from-minibuffer "Confirm: "
                                          command nil nil 'grep-history))
            (add-to-history 'grep-history command))))
      (when command
        (let ((default-directory dir))
          ;; Setting process-setup-function makes exit-message-function work
          ;; even when async processes aren't supported.
          (compilation-start command 'rg-mode))
        (if (eq next-error-last-buffer (current-buffer))
            (setq default-directory dir))))))

(provide 'rg)
