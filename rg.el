;;; rg.el --- A search tool based on ripgrep. -*- lexical-binding: t; -*-

;; Copyright (C) 1985-1987, 1993-1999, 2001-2015 Free Software
;; Foundation, Inc.
;; Copyright (C) 2016-2018 David Landell <david.landell@sunnyhill.email>
;;
;; Author: David Landell <david.landell@sunnyhill.email>
;;         Roland McGrath <roland@gnu.org>
;; Version: 1.8.0
;; URL: https://github.com/dajva/rg.el
;; Package-Requires: ((cl-lib "0.5") (emacs "25.1") (s "1.10.0") (transient "0.1.0") (wgrep "2.1.10"))
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

;; A search package based on the ripgrep command line tool.
;; It allows you to interactively create searches, doing automatic
;; searches based on the editing context, refining and modifying
;; search results and much more.  It is also highly configurable to be
;; able to fit different users' needs.

;; If you are used to built-in Emacs rgrep command, transitioning to
;; rg should be simple.  rg provides a lot of extra features
;; but the basics are similar.
;;
;; The big benefit of using ripgrep instead of grep as a backend is
;; speed.  Especially when searching large source code repositories
;; where ripgrep really shines.

;; See info node `(rgel)Top' for documentation or online at https://rgel.readthedocs.io.


;;; Code:

(require 'cl-lib)
(require 'edmacro)
(require 'grep)
(require 'rg-ibuffer)
(require 'rg-menu)
(require 'rg-result)
(require 'rg-info-hack)
(require 's)
(require 'vc)


;; Customizations/public vars
(defgroup rg nil
  "Settings for rg."
  :group 'tools
  :group 'external)

(defcustom rg-custom-type-aliases
  '(("gyp" .    "*.gyp *.gypi"))
  "A list of file type aliases that are added to the 'rg' built in aliases.
Each list element may be a (string . string) cons containing the name of the
type alias and the file patterns, or a lambda returning a similar cons cell.
A lambda should return nil if it currently has no type aliases to contribute."
  :type '(repeat (choice (cons string string) function))
  :group 'rg)

(defcustom rg-executable (executable-find "rg")
  "'rg' executable."
  :type 'string
  :group 'rg)

(defcustom rg-command-line-flags nil
  "List of command line flags for rg.
Alternatively a function returning a list of flags."
  :type '(choice function (repeat string))
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

(defcustom rg-keymap-prefix (kbd "C-c s")
  "Prefix for global `rg' keymap."
  :type 'key-sequence
  :group 'rg)

(defcustom rg-default-alias-fallback "all"
  "The default file alias to use when no alias can be determined.
This must be a string that can be match against the types returned
from `rg-get-type-aliases'."
  :type 'string
  :group 'rg)

(defcustom rg-buffer-name "rg"
  "Search results buffer name.
Can be string or function."
  :type '(choice string function)
  :group 'rg)

(defcustom rg-ignore-ripgreprc t
  "Ingore the ripgrep config file.
Disabling this setting can break functionality of this package."
  :type 'boolean
  :group 'rg)

(defvar rg-command-line-flags-function 'identity
  "Function to modify command line flags of a search.
The argument of the function is an optional list of search specific
command line flags and the function shall return a list of command
line flags to use.")


;; Internal vars and structs
(defvar rg-builtin-type-aliases nil
  "Cache for 'rg --type-list'.")

(defvar rg-toggle-command-line-flags nil
  "List of command line flags defined by `rg-define-toggle' macro.")

(defvar rg-history nil "History for full rg commands.")
(defvar rg-files-history nil "History for files args.")
(defvar rg-pattern-history nil "History for search patterns.")

(defvar rg-required-command-line-flags
  '("--color always"
    "--colors match:fg:red"
    "--colors path:fg:magenta"
    "--colors line:fg:green"
    "--colors column:none"
    "-n"
    "--column"))

(defconst rg-internal-type-aliases
  '(("all" . "all defined type aliases") ; rg --type all
    ("everything" . "*")) ; rg without '--type' arg
  "Internal type aliases for special purposes.
These are not produced by 'rg --type-list' but we need them anyway.")

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
(defun rg-executable ()
  "Return the 'rg' executable.
Raises an error if it can not be found."
  (unless rg-executable
    (error "No 'rg' executable found"))
  (shell-quote-argument rg-executable))

(defun rg--buffer-name ()
  "Wrapper for variable `rg-buffer-name'.  Return string or call function."
  (if (functionp rg-buffer-name)
      (funcall rg-buffer-name)
    rg-buffer-name))

(defun rg-buffer-name (&optional name-of-mode)
  "Return search results buffer name.
NAME-OF-MODE is needed to pass this function to `compilation-start'."
  (ignore name-of-mode)
  (if rg-recompile
      (buffer-name)
    (format "*%s*" (rg--buffer-name))))

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
   (rg-get-custom-type-aliases)))

(defun rg-is-custom-file-pattern (files)
  "Return non nil if FILES is a custom file pattern."
  (not (assoc files (rg-get-type-aliases))))

(defun rg-build-command (pattern files literal flags)
  "Create the command line for PATTERN and FILES.
LITERAL determines if search will be literal or regexp based and FLAGS
are command line flags to use for the search."
  (let ((command-line
         (append
          rg-required-command-line-flags
          (rg-build-type-add-args)
          (if (functionp rg-command-line-flags)
              (funcall rg-command-line-flags)
            rg-command-line-flags)
          flags

          (list
           (if rg-group-result "--heading" "--no-heading"))
          (when rg-ignore-ripgreprc
            (list "--no-config"))
          (when (rg-is-custom-file-pattern files)
            (list (concat "--type-add " (shell-quote-argument (concat "custom:" files)))))
          (when literal
            (list "--fixed-strings"))
          (when (not (equal files "everything"))
            (list "--type <F>"))
          (list "-e <R>")
          (when (eq system-type 'windows-nt)
            (list ".")))))

    (grep-expand-template
     (mapconcat 'identity (cons (rg-executable) (delete-dups command-line)) " ")
     pattern
     (if (rg-is-custom-file-pattern files) "custom" files))))

(defun rg-invoke-rg-type-list ()
  "Invokes rg --type-list and return the result."
  (shell-command-to-string (concat (rg-executable) " --type-list")))

(defun rg-list-builtin-type-aliases ()
  "Invokes rg --type-list and puts the result in an alist."
  (let ((type-list (delete "" (split-string (rg-invoke-rg-type-list) "\n"))))
    (mapcar
     (lambda (type-alias)
       (setq type-alias (split-string type-alias ":" t))
       (cons (s-trim (car type-alias))
             (s-trim
              (mapconcat 'identity
                         (split-string (cadr type-alias) "," t )
                         " "))))
     type-list)))


(defun rg-get-custom-type-aliases ()
  "Get alist of custom type aliases.
Any lambda elements will be evaluated, and nil results will be
filtered out."
  (delq nil (mapcar
             (lambda (ct) (if (functionp ct) (funcall ct) ct))
             rg-custom-type-aliases)))

(defun rg-get-type-aliases (&optional skip-internal)
  "Return supported type aliases.
If SKIP-INTERNAL is non nil the `rg-internal-type-aliases' will be
excluded."
  (unless rg-builtin-type-aliases
    (setq rg-builtin-type-aliases (rg-list-builtin-type-aliases)))
  (append (rg-get-custom-type-aliases) rg-builtin-type-aliases
          (unless skip-internal rg-internal-type-aliases)))

(defun rg-default-alias ()
  "Return the default alias by matching alias globs with the buffer file name."
  (let* ((bufname (or (buffer-file-name)
                      (replace-regexp-in-string "<[0-9]+>\\'" "" (buffer-name))))
         (filename (and bufname
                        (stringp bufname)
                        (file-name-nondirectory bufname))))
    (or
     (when filename
       (cl-find-if
        (lambda (alias)
          (string-match (mapconcat 'wildcard-to-regexp
                                   (split-string (cdr alias) nil t)
                                   "\\|")
                        filename))
        (rg-get-type-aliases t)))
     ;; Default when an alias for the file can't be determined
     (or
      (cl-find-if
       (lambda (alias)
         (string= rg-default-alias-fallback (car alias)))
       (rg-get-type-aliases))
      (progn
        (message "Warning: rg-default-alias-fallback customization does not match any alias. Using \"all\".")
        (car rg-internal-type-aliases))))))

(defun rg-read-files ()
  "Read files argument for interactive rg."
  (let ((default-alias (rg-default-alias)))
    (completing-read
     (concat "Search in files"
             (if default-alias
                 (concat
                  " (default: [" (car default-alias) "] = "
                  (cdr default-alias) ")"))
             ": ")
     (rg-get-type-aliases)
     nil nil nil 'rg-files-history
     (car default-alias))))

(defun rg-read-pattern (literal &optional default)
  "Read search pattern argument from user.
If LITERAL is non nil prompt for literal string.  DEFAULT is the default pattern to use at the prompt."
  (let ((default (or default (grep-tag-default)))
        (prompt (concat (if literal "Literal" "Regexp")
                        " search for")))
    (read-regexp prompt default 'rg-pattern-history)))

(defun rg-project-root (file)
  "Find the project root of the given FILE."
  (or
   (when (and (require 'projectile nil t)
              (fboundp 'projectile-project-root))
     (projectile-project-root))
   (when (and (require 'find-file-in-project nil t)
              (fboundp 'ffip-project-root))
     (ffip-project-root))
   (when (and (require 'project nil t)
              (fboundp 'project-current)
              (fboundp 'project-roots))
     (car (project-roots (project-current))))
   (condition-case nil
       (let* ((file (or file default-directory))
              (backend (vc-responsible-backend file)))
         (vc-call-backend backend 'root file))
     (error (progn
              (file-name-directory file))))))

(defun rg-run (pattern files dir &optional literal confirm flags)
  "Execute rg command with supplied PATTERN, FILES and DIR.
If LITERAL is nil interpret PATTERN as regexp, otherwise as a literal.
CONFIRM allows the user to confirm and modify the command before
executing.  FLAGS is additional command line flags to use in the search."
  (unless (and (stringp pattern) (> (length pattern) 0))
    (signal 'user-error '("Empty string: No search done")))
  (wgrep-rg-warn-ag-setup)
  (unless (and (file-directory-p dir) (file-readable-p dir))
    (setq dir default-directory))
  (rg-apply-case-flag pattern)
  (let ((command (rg-build-command
                  pattern files literal
                  (append rg-toggle-command-line-flags flags)))
        confirmed)
    (setq dir (file-name-as-directory (expand-file-name dir)))
    (if confirm
        (setq confirmed
              (read-from-minibuffer "Confirm: "
                                    command nil nil 'rg-history))
      (add-to-history 'rg-history command))
    (let ((default-directory dir)
          (search (rg-search-create
                   :pattern pattern
                   :files files
                   :dir dir
                   :literal literal
                   :toggle-flags rg-toggle-command-line-flags
                   :flags flags)))
      (when (and confirmed
                 (not (string= confirmed command)))
        (setq command confirmed)
        ;; If user changed command we can't know the parts of the
        ;; search and needs to disable result buffer modifications.
        (setf (rg-search-full-command search) command))
      ;; Since `rg-buffer-name' can be changed through dir-locals file we
      ;; need to apply variables from dir-locals that resides in search dir.
      ;; This way the results buffer will receive correct name on `compilation-start'.
      ;; `hack-dir-local-variables' is searching dir-locals file in
      ;; `buffer-file-name' or `default-directory' directory.
      ;; Temporary buffer must be created here to make sure that dir-locals
      ;; file is loaded from `default-directory' defined above and
      ;; not from `buffer-file-name' in case search is started from file buffer.
      (with-temp-buffer
        (hack-dir-local-variables-non-file-buffer)
        ;; Setting process-setup-function makes exit-message-function work
        ;; even when async processes aren't supported.
        (with-current-buffer (compilation-start command 'rg-mode #'rg-buffer-name)
          (rg-mode-init search))))
    (if (eq next-error-last-buffer (current-buffer))
        (setq default-directory dir))))

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
            (add-to-list 'rg-toggle-command-line-flags "-i" ))
    (setq rg-toggle-command-line-flags
          (delete "-i" rg-toggle-command-line-flags))))

(defun rg-get-rename-target ()
  "Return the buffer that will be target for renaming."
  (let* ((buffer-name (rg-buffer-name))
         (buffer (if (eq major-mode 'rg-mode)
                     (current-buffer)
                   (get-buffer buffer-name))))
    (or buffer
        (error "Current buffer is not an rg-mode buffer and no buffer with name '%s'" buffer-name))))

(defalias 'kill-rg 'kill-compilation)
(defalias 'rg-kill-current 'kill-compilation "Kill the ongoing ripgrep search.")
(make-obsolete 'kill-rg 'rg-kill-current "1.7.1")

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
                   (add-to-list 'rg-toggle-command-line-flags ,flagvalue))
          `(setq rg-toggle-command-line-flags
                 (delete ,flagvalue rg-toggle-command-line-flags)))
       ,(when key
          `(define-key rg-mode-map ,key (quote ,(intern funname))))
       (defun ,(intern funname) ()
         ,(format "Rerun last search with flag '%s' toggled." flagvalue)
         (interactive)
         (rg-rerun-toggle-flag ,flagvalue)))))

(defun rg-save-search-as-name (newname)
  "Save the search result in current result buffer.
NEWNAME will be added to the result buffer name.  New searches will use the
standard buffer unless the search is done from a saved buffer in
which case the saved buffer will be reused."
  (interactive "sSave search as name: ")
  (let ((buffer (rg-get-rename-target)))
    (with-current-buffer buffer
      (rename-buffer (format "*%s %s*" (rg--buffer-name) newname)))))

(defun rg-save-search ()
  "Save the search result in current result buffer.
The result buffer will be renamed by the `rename-uniquify' function.
To choose a custom name, use `rg-save-search-as-name' instead.  New
searches will use the standard buffer unless the search is done from
a saved buffer in which case the saved buffer will be reused."
  (interactive)
  (let ((buffer (rg-get-rename-target)))
    (with-current-buffer buffer
      (rename-uniquely)
      ;; If the new buffer name became default result buffer name, just rename
      ;; again to make sure the result is saved.
      (when (equal (buffer-name) (rg-buffer-name))
        (rename-uniquely)))))

(defun rg-kill-saved-searches ()
  "Kill all saved rg buffers.  The default result buffer will be kept."
  (interactive)
  (when (y-or-n-p "Confirm kill all saved rg searches? ")
    (dolist (buf (buffer-list))
      (when (with-current-buffer buf
              (and (eq major-mode 'rg-mode)
                   (not (equal (buffer-name) (rg-buffer-name)))))
        (kill-buffer buf)))))

;;;###autoload
(defun rg-enable-default-bindings (&optional prefix)
  "Enable the global `rg' default key bindings under PREFIX key.
If prefix is not supplied `rg-keymap-prefix' is used."
  (interactive)
  (when-let ((prefix (or prefix rg-keymap-prefix)))
    (if rg-use-transient-menu
        (rg-enable-menu prefix)
      (global-set-key prefix rg-global-map)
      (message "Global key bindings for `rg' enabled with prefix: %s"
               (edmacro-format-keys prefix)))))

;;;###autoload
(defun rg-use-old-defaults ()
  "Restore default settings pre version 2.0.0."
  (define-key rg-mode-map "\C-f" 'rg-forward-history)
  (define-key rg-mode-map "\C-c>" nil)
  (define-key rg-mode-map "\C-b" 'rg-back-history)
  (define-key rg-mode-map "\C-c<" nil)
  (setf rg-group-result nil)
  (setf rg-align-position-numbers nil)
  (setf rg-align-line-column-separator nil)
  (setf rg-align-position-content-separator nil)
  (setf rg-use-transient-menu nil))


(eval-and-compile
  (defun rg-set-search-defaults (args)
    "Set defaults for required search options missing from ARGS.
If the :confirm option is missing, set it to NEVER, if
the :format option is missing, set it to REGEXP, and if
the :query option is missing, set it to ASK"
    (unless (plist-get args :confirm)
      (setq args (plist-put args :confirm 'never)))

    (unless (plist-get args :format)
      (setq args (plist-put args :format 'regexp)))

    (unless (plist-get args :query)
      (setq args (plist-put args :query 'ask)))

    (unless (plist-get args :files)
      (setq args (plist-put args :files 'ask)))

    (unless (plist-get args :dir)
      (setq args (plist-put args :dir 'ask)))
    args))

(eval-and-compile
  (defun rg-ensure-quoted (arg)
    "Ensure that ARG is quoted."
    (if (and (consp arg)
             (eq (car arg) 'quote))
        arg
      `(quote ,arg)))

  (defun rg-ensure-unquoted (arg)
    "Ensure that ARG is quoted."
    (if (and (consp arg)
             (eq (car arg) 'quote))
        (cadr arg)
      arg)))

(eval-and-compile
  (defun rg-search-parse-local-bindings (search-cfg)
    "Parse local bindings for search functions from SEARCH-CFG."
    (let* ((confirm-opt (plist-get search-cfg :confirm))
           (format-opt (plist-get search-cfg :format))
           (query-opt (plist-get search-cfg :query))
           (alias-opt (plist-get search-cfg :files))
           (dir-opt (plist-get search-cfg :dir))
           (flags-opt (plist-get search-cfg :flags))
           (binding-list `((literal ,(eq format-opt 'literal)))))

      ;; confirm binding
      (cond ((eq confirm-opt 'never)
             (setq binding-list (append binding-list `((confirm nil)))))

            ((eq confirm-opt 'always)
             (setq binding-list (append binding-list `((confirm t)))))

            ((eq confirm-opt 'prefix)
             (setq binding-list (append binding-list
                                        '((confirm (equal current-prefix-arg
                                                          '(4))))))))

      ;; query binding
      (unless (eq query-opt 'ask)
        (let ((query (cond ((eq query-opt 'point) '(grep-tag-default))
                           (t query-opt))))
          (setq binding-list (append binding-list `((query ,query))))))

      ;; dir binding
      (unless (eq dir-opt 'ask)
        (let ((dirs (cond ((eq dir-opt 'project) '(rg-project-root
                                                   buffer-file-name))
                          ((eq dir-opt 'current) 'default-directory)
                          (t dir-opt))))
          (setq binding-list (append binding-list `((dir ,dirs))))))

      ;; file alias binding
      (unless (eq alias-opt 'ask)
        (let ((files (if (eq alias-opt 'current)
                         '(car (rg-default-alias))
                       alias-opt)))
          (setq binding-list (append binding-list `((files ,files))))))

      (when (eq flags-opt 'ask)
        (setq flags-opt 'flags))

      (setq flags-opt (rg-ensure-quoted flags-opt))
      (setq binding-list
            (append binding-list
                    `((flags (funcall rg-command-line-flags-function ,flags-opt)))))
      binding-list)))

(eval-and-compile
  (defun rg-search-parse-interactive-args (search-cfg)
    "Parse interactive args from SEARCH-CFG for search functions."
    (let* ((query-opt (plist-get search-cfg :query))
           (format-opt (plist-get search-cfg :format))
           (literal (eq format-opt 'literal))
           (dir-opt (plist-get search-cfg :dir))
           (files-opt (plist-get search-cfg :files))
           (flags-opt (plist-get search-cfg :flags))
           (iargs '()))

      (when (eq query-opt 'ask)
        (setq iargs
              (append iargs `((query . (rg-read-pattern ,literal))))))

      (when (eq files-opt 'ask)
        (setq iargs
              (append iargs '((files . (rg-read-files))))))

      (when (eq dir-opt 'ask)
        (setq iargs
              (append iargs
                      '((dir . (read-directory-name
                                "In directory: " nil default-directory t))))))

      (when (eq flags-opt 'ask)
        (setq iargs
              (append iargs '((flags . (split-string
                                        (read-string "Command line flags: ")))))))
      iargs)))

(eval-and-compile
  (defun rg-search-parse-menu-arg (search-cfg name)
    "Parse :menu option in SEARCH-CFG.
Returns forms for binding function with NAME into rg-menu."
    (when-let ((menu-opt (rg-ensure-unquoted
                          (plist-get search-cfg :menu))))
      (unless (and (consp menu-opt)
                   (= (length menu-opt) 3))
        (user-error "'%S' should be a list of length 3" menu-opt))
      `((rg-menu-wrap-transient-search ,name)
        (rg-menu-transient-insert
         ,@menu-opt
         ',(intern (concat (symbol-name name) "--transient")))))))

(defconst rg-elisp-font-lock-keywords
  '(("(\\(rg-define-search\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode rg-elisp-font-lock-keywords)

;;;###autoload
(defmacro rg-define-search (name &rest args)
  "Define an rg search functions named NAME.
ARGS is a search specification that defines parameters of a search.
It optionally starts with a string that is used as the docstring for
the defined function.  The rest of ARGS contains key value pairs
according to the specification below.  All keys are optional with
specified default if left out.

:query      Method for retrieving the search string.  Allowed values
            are `point' which means extract thing at point and `ask'
            which means prompt the user for a string.  Any form that
            evaluates to a string is allowed.
            Default is `ask'.
:format     Specifies if :query is interpreted literally (`literal')
            or as a regexp (`regexp').
            Default is `regexp'.
:files      Form that evaluates to a file alias or custom file glob.
            `current' means extract alias from current buffer file name,
            `ask' will prompt the user.
            Default is `ask'.
:dir        Root search directory.  Allowed values are `ask' for user
            prompt, `current' for current dir and `project' for project
            root.  Any form that evaluates to a directory string is
            also allowed.
            Default is `ask'.
:confirm    `never', `always', or `prefix' are allowed values.  Specifies
            if the the final search command line string can be modified
            and confirmed the user.
            Default is `never'.
:flags      `ask' or a list of command line flags that will be used when
            invoking the search.
:menu       Bind the command into `rg-menu'.  Must be a list with three
            items in it.  The first item is the description of the
            group in witch the new command will appear.  If the group
            does not exist a new will be created.  The second item is
            the key binding for this new command (ether a key vector
            or a key description string) and the third item is the
            description of the command that will appear in the menu.

Example:
\(rg-define-search search-home-dir-in-elisp
  \"Doc string.\"
  :query ask
  :format literal
  :files \"elisp\"
  :dir (getenv \"HOME\"\)\)
  :menu (\"Custom\" \"H\" \"Home dir\")"
  (declare (indent defun))
  (let* ((body (macroexp-parse-body args))
         (decls (car body))
         (search-cfg (rg-set-search-defaults (cdr body)))
         (local-bindings (rg-search-parse-local-bindings search-cfg))
         (iargs (rg-search-parse-interactive-args search-cfg))
         (menu-forms (rg-search-parse-menu-arg search-cfg name)))
    `(progn
       (defun ,name ,(mapcar 'car iargs)
         ,@decls
         (interactive
          (list ,@(mapcar 'cdr iargs)))
         (let ,local-bindings
           (rg-run query files dir literal confirm flags)))
       ,@menu-forms)))

;;;###autoload (autoload 'rg-project "rg.el" "" t)
(rg-define-search rg-project
  "Run ripgrep in current project searching for REGEXP in FILES.
The project root will will be determined by either common project
packages like projectile and `find-file-in-project' or the source
version control system."
  :dir project)

;;;###autoload (autoload 'rg-dwim-project-dir "rg.el" "" t)
(rg-define-search rg-dwim-project-dir
  "Search for thing at point in files matching the current file
under the project root directory."
  :query point
  :format literal
  :files current
  :dir project)

;;;###autoload (autoload 'rg-dwim-current-dir "rg.el" "" t)
(rg-define-search rg-dwim-current-dir
  "Search for thing at point in files matching the current file
under the current directory."
  :query point
  :format literal
  :files current
  :dir current)

;;;###autoload (autoload 'rg-dwim-current-file "rg.el" "" t)
(rg-define-search rg-dwim-current-file
  "Search for thing at point in files matching the current file
name (as a pattern) under the current directory."
  :query point
  :format literal
  :files (file-name-nondirectory (buffer-file-name))
  :dir current)

;;;###autoload
(defun rg-dwim (&optional curdir)
  "Run ripgrep without user interaction figuring out the intention by magic(!).
The default magic searches for thing at point in files matching
current file under project root directory.

With \\[universal-argument] prefix (CURDIR), search is done in
current dir instead of project root.

With repeated \\[universal-argument] prefix, search is done in
the current dir and using the current variable `buffer-file-name'
as a pattern.  Subdirectories are still searched, so different
files with the same name pattern still will be searched."
  (interactive "P")
  (cond
   ((eq  4 (and (consp curdir) (car curdir))) (rg-dwim-current-dir))
   ((eq 16 (and (consp curdir) (car curdir))) (rg-dwim-current-file))
   (t     (rg-dwim-project-dir))))

;;;###autoload (autoload 'rg-literal "rg.el" "" t)
(rg-define-search rg-literal
  "Run ripgrep, searching for literal PATTERN in FILES in directory DIR.
With \\[universal-argument] prefix (CONFIRM), you can edit the
constructed shell command line before it is executed."
  :format literal
  :confirm prefix)

;;;###autoload (autoload 'rg "rg.el" "" t)
(rg-define-search rg
  "Run ripgrep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `rg-custom-type-aliases'
or ripgrep builtin type aliases, e.g. entering `elisp' is
equivalent to `*.el'. REGEXP is a regexp as defined by the
ripgrep executable. With \\[universal-argument] prefix (CONFIRM),
you can edit the constructed shell command line before it is
executed. Collect output in a buffer. While ripgrep runs
asynchronously, you can use \\[next-error] (M-x `next-error'), or
\\<grep-mode-map>\\[compile-goto-error] \ in the rg output
buffer, to go to the lines where rg found matches."
  :confirm prefix)

(provide 'rg)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; rg.el ends here
