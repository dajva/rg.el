;;; rg-result.el ---- Result buffer implementation for rg.el. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 David Landell <david.landell@sunnyhill.email>
;;
;; Author: David Landell <david.landell@sunnyhill.email>
;; URL: https://github.com/dajva/rg.el

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

;;; Code:

(require 'cl-lib)
(require 'grep)
(require 'mouse)
(require 'rg-header)
(require 'rg-history)
(require 'subr-x)
(require 'wgrep-rg)

;; Forward declarations.
(declare-function rg-build-command "rg.el")
(declare-function rg-get-type-aliases "rg.el")
(declare-function rg-list-searches "rg.el")
(declare-function rg-read-pattern "rg.el")
(declare-function rg-menu "rg-menu.el")


;; Customizations/public vars
(defcustom rg-use-transient-menu t
  "Use transient menu instead of a the global keymap."
  :type 'boolean
  :group 'rg
  :package-version '(rg . "2.0.0"))

(defcustom rg-show-columns nil
  "If t, show the columns of the matches in the output buffer."
  :type 'boolean
  :group 'rg)

(defcustom rg-group-result t
  "Group matches in the same file together.
If nil, the file name is repeated at the beginning of every match line."
  :type 'boolean
  :group 'rg
  :package-version '(rg . "2.0.0"))

(defcustom rg-show-header t
  "Show header in results buffer if non nil."
  :type 'boolean
  :group 'rg)

(defcustom rg-hide-command t
  "Hide most of rg command line when non nil."
  :type 'boolean
  :group 'rg)

(defcustom rg-align-position-numbers t
  "If non nil, alignment of line and column numbers is turned on."
  :type 'boolean
  :group 'rg
  :package-version '(rg . "2.0.0"))

(defcustom rg-align-line-number-field-length 4
  "Field length of aligned line numbers."
  :type 'integer
  :group 'rg)

(defcustom rg-align-column-number-field-length 3
  "Field length of aligned column numbers."
  :type 'integer
  :group 'rg)

(defcustom rg-align-line-column-separator " "
  "Separator used between line and column numbers.
Depends on `rg-show-columns'.  Default is ':'."
  :type 'string
  :group 'rg
  :package-version '(rg . "2.0.0"))

(defcustom rg-align-position-content-separator " "
  "Separator used between position numbers and the matching file content.
Default is ':'."
  :type 'string
  :group 'rg
  :package-version '(rg . "2.0.0"))

(defvar rg-filter-hook nil
  "Hook for new content in the rg buffer.
This hook is called every time the rg buffer has been updated with
new content and filtered through the `rg-filter' function.")


;; Faces
(defgroup rg-face nil
  "Settings for rg faces."
  :group 'rg)

(defface rg-match-face
  `((t :inherit ,grep-match-face))
  "Face for match highlight."
  :group 'rg-face)

(defface rg-error-face
  `((t :inherit ,grep-error-face))
  "Face for error."
  :group 'rg-face)

(defface rg-context-face
  `((t :inherit ,grep-context-face))
  "Face for context lines."
  :group 'rg-face)

(defface rg-info-face
  '((t :inherit compilation-info))
  "Face for info."
  :group 'rg-face)

(defface rg-warning-face
  '((t :inherit compilation-warning))
  "Face for warning."
  :group 'rg-face)

(defface rg-filename-face
  '((t :inherit rg-info-face))
  "Face for filename."
  :group 'rg-face)

(defface rg-file-tag-face
  '((t :inherit rg-info-face))
  "Face for file tag in grouped layout."
  :group 'rg-face)

(defface rg-line-number-face
  '((t :inherit compilation-line-number))
  "Face for line numbers."
  :group 'rg-face)

(defface rg-column-number-face
  '((t :inherit compilation-column-number))
  "Face for column numbers."
  :group 'rg-face)

(defface rg-match-position-face
  '((t :inherit default))
  "Face that is being appended to file positions.
This is the start of each matching line. This includes line number
and, depending on configuration, column number and file name."
  :group 'rg-face)


;; Internal vars and structs

(cl-defstruct (rg-search (:constructor rg-search-create)
                         (:constructor rg-search-new (pattern files dir))
                         (:copier rg-search-copy))
  pattern                ; search pattern
  files                  ; files to search
  dir                    ; base directory
  full-command           ; full-command (t or nil)
  literal                ; literal patterh (t or nil)
  flags)                 ; search specific flags

(defvar-local rg-cur-search nil
  "Stores parameters of last search.
Becomes buffer local in `rg-mode' buffers.")
(put 'rg-cur-search 'permanent-local t)

(defvar-local rg-search-history nil
  "Stores the search history per rg-mode buffer.")
(put 'rg-search-history 'permanent-local t)

(defvar-local rg-hit-count 0
  "Stores number of hits in a search.")

(defvar-local rg-recompile nil
  "Is `recompile' in progress or `compile-start'.")

(defconst rg-mode-font-lock-keywords
  '(;; Command output lines.
    (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or address\\)\\)$"
     1 'rg-error-face)
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
    ("^ *\\(?:.+?-\\)?[0-9]+-.*\n" (0 'rg-context-face))
    ("^.*rg \\(--color=always .*$\\)"
     (0 rg-command-line-properties)
     (1 (rg-hidden-command-line-properties)))
    ("^-\\*- mode: rg; default-directory: \"\\(.*\\)\" -\\*-$"
     (1 rg-directory-properties))))

(defvar rg-menu-map
  (let ((map (make-sparse-keymap "RipGrep")))
    (define-key map [rg-toggle-command-hiding]
      '(menu-item "Toggle command visibility"
                  rg-toggle-command-hiding
                  :help "Toggle showing verbose command options"))
    (define-key map [rg-enable-wgrep]
      '(menu-item "Edit buffer" wgrep-change-to-wgrep-mode
                  :help "Edit buffer and save changes."))
    (define-key map [rg-save-search]
      '(menu-item "Save search" rg-save-search-as-name
                  :help "Save current search."))
    (define-key map [rg-search-list]
      '(menu-item "List searches" rg-list-searches
                  :help "List all ripgrep search buffers."))
    (define-key map [rg-change-dir]
      '(menu-item "Change dir" rg-rerun-change-dir
                  :help "Rerun search in another directory."))
    (define-key map [rg-change-files]
      '(menu-item "Change file type" rg-rerun-change-files
                  :help "Rerun search on other file types."))
    (define-key map [rg-change-regexp]
      '(menu-item "Change regexp" rg-rerun-change-regexp
                  :help "Run regexp search with changed pattern."))
    (define-key map [rg-change-literal]
      '(menu-item "Change literal" rg-rerun-change-literal
                  :help "Run literal search with changed search string."))
    (define-key map [rg-kill-compilation]
      '(menu-item "Kill Ripgrep" kill-compilation
		  :help "Kill the currently running rg process"))
    (define-key map [rg-another]
      '(menu-item "Another search..."
                  (lambda ()
                    (interactive)
                    (rg-save-search)
                    (call-interactively 'rg))
		  :help "Save current search results and start a new search."))
    (define-key map [rg-recompile]
      '(menu-item "Go..." rg-recompile
		  :help "Rerun search"))
    map))

(defvar rg-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (define-key map " " 'scroll-up-command)
    (define-key map [?\S-\ ] 'scroll-down-command)
    (define-key map "\^?" 'scroll-down-command)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)
    (define-key map "\r" 'compile-goto-error)  ;; ?
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
    (define-key map "\t" 'compilation-next-error)
    (define-key map [backtab] 'compilation-previous-error)

    (define-key map "c" 'rg-rerun-toggle-case)
    (define-key map "d" 'rg-rerun-change-dir)
    (define-key map "f" 'rg-rerun-change-files)
    (define-key map "g" 'rg-recompile)
    (define-key map "i" 'rg-rerun-toggle-ignore)
    (define-key map "L" 'rg-list-searches)
    (define-key map "r" 'rg-rerun-change-regexp)
    (define-key map "s" 'rg-save-search)
    (define-key map "S" 'rg-save-search-as-name)
    (define-key map "t" 'rg-rerun-change-literal)
    (define-key map "e" 'wgrep-change-to-wgrep-mode)
    (define-key map "\M-N" 'rg-next-file)
    (define-key map "\M-P" 'rg-prev-file)
    (define-key map "\C-c>" 'rg-forward-history)
    (define-key map "\C-c<" 'rg-back-history)
    (when rg-use-transient-menu
      (define-key map "m" #'rg-menu))

    ;; Set up the menu-bar
    (define-key map [menu-bar rg]
      (cons "RipGrep" rg-menu-map))
    map)
  "The keymap for `rg-mode'.")

(defvar rg-ellipsis (if (char-displayable-p ?…) "[…]" "[...]")
  "Used when hiding command line.")

(defvar rg-finish-functions '()
  "Functions to call when a ripgrep process finishes.

Each function is called with two arguments: the compilation buffer,
and a string describing how the process finished.")


;; Defuns

(defun rg-create-mouse-map (command)
  "Create a mouse-map for the COMMAND.
This makes sure point moves to click and that the clicked window is
selected."
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-2] 'mouse-set-point)
    (define-key map [mouse-2] command)
    (define-key map "\C-m" command)
    map))

;; This solution was mostly copied from emacs grep.el and adjusted to
;; be more usable.
(defvar rg-command-line-properties
  (let ((map (rg-create-mouse-map 'rg-toggle-command-hiding)))
    `(face rg-context-face mouse-face highlight
           help-echo "RET, mouse-2: show unabbreviated command"
           keymap ,map))
  "Properties for graying out and keymap for hiding command line.")

(defun rg-hidden-command-line-properties ()
  "Return properties of button-like ellipsis on part of rg command line."
  (append
   '(face nil rg-command-hidden-part t)
   (when rg-hide-command
     `(display ,rg-ellipsis))))

(defun rg-toggle-command-hiding ()
  "Toggle showing the hidden part of rg command line."
  (interactive)
  (with-silent-modifications
    (let* ((beg (next-single-property-change (point-min) 'rg-command-hidden-part))
           (end (when beg
                  (next-single-property-change beg 'rg-command-hidden-part))))
      (if end
          (if (get-text-property beg 'display)
              (remove-list-of-text-properties beg end '(display))
            (add-text-properties beg end `(display ,rg-ellipsis)))
        (user-error "No abbreviated part to hide/show")))))

(defvar rg-directory-properties
  (let ((map (rg-create-mouse-map 'rg-rerun-change-dir)))
    `(face rg-filename-face mouse-face highlight
           help-echo "RET, mouse-2: Change search directory"
           keymap ,map))
  "Properties for `default-directory' in header.")

(defun rg-list-toggle (elem list)
  "Remove ELEM from LIST if present or add it if not present.
Returns the new list."
  (if (member elem list)
      (delete elem list)
    (push elem list)))

(defun rg-process-setup ()
  "Setup compilation variables and buffer for `rg'.
Set up `compilation-exit-message-function'."
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
           (cons msg code)))))

(defun rg-prepend-space (text length)
  "Prepend TEXT with LENGTH number of spaces."
  (let ((space-count (- length (length text))))
    (concat
     (when (> space-count 0) (make-string space-count ?\s))
     text)))

(defun rg-perform-position-numbers-alignment (line-number &optional column-number context-marker )
  "Return aligned LINE-NUMBER, COLUMN-NUMBER and CONTEXT-MARKER."
  (let* ((line-col-separator (or rg-align-line-column-separator ":"))
         (pos-content-separator (or rg-align-position-content-separator ":"))
         (pos-content-separator (if rg-show-columns
                                    pos-content-separator
                                  (propertize pos-content-separator 'invisible t)))
         (line-number-width
          (if (and rg-show-columns context-marker)
              ;; Context lines should be aligned to column numbers
              (+ rg-align-line-number-field-length
                 (1+ rg-align-column-number-field-length))
            rg-align-line-number-field-length))
         (column-number (when column-number
                          (if rg-show-columns
                              column-number
                            (propertize column-number 'invisible t)))))
    (cl-assert (if column-number (not context-marker) context-marker))
    (concat (rg-prepend-space line-number line-number-width)
            (if column-number
                (concat (if rg-show-columns
                            line-col-separator
                          pos-content-separator)
                        (rg-prepend-space column-number
                                          rg-align-column-number-field-length)
                        pos-content-separator)
              context-marker))))

(defun rg-format-line-and-column-numbers (beg end)
  "Align numbers in region defined by BEG and END."
  (goto-char beg)
  (while (re-search-forward
          "^\033\\[[0]*m\033\\[32m\\([0-9]*?\\)\033\\[[0]*m\\(:\\|-\\)\\(?:\033\\[[0]*m\\([0-9]*?\\)\033\\[[0]*m:\\)?"
          end 1)
    (let* ((line-match (match-string 1))
           (col-separator-match (match-string 2))
           (context-marker (when (equal col-separator-match "-")
                             col-separator-match))
           (column-match (match-string 3)))
    (cond
     (rg-align-position-numbers
      (replace-match
       (rg-perform-position-numbers-alignment
        line-match column-match context-marker)
       t t))
     ((and column-match (not rg-show-columns))
      (replace-match
       (propertize col-separator-match 'invisible t) t t nil 2)
      (replace-match (propertize column-match 'invisible t) t t nil 3))))))

(defun rg-filter ()
  "Handle match highlighting escape sequences inserted by the rg process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      (when (zerop rg-hit-count)
        (newline))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Add File: in front of filename
        (when rg-group-result
          (while (re-search-forward "^\033\\[[0]*m\033\\[35m\\(.*?\\)\033\\[[0]*m$" end 1)
            (replace-match (concat (propertize "File:"
                                               'rg-file-message t
                                               'face nil
                                               'font-lock-face 'rg-file-tag-face)
                                   " "
                                   (propertize (match-string 1)
                                               'face nil
                                               'font-lock-face 'rg-filename-face))
                           t t))
          (goto-char beg))

        ;; Highlight rg matches and delete marking sequences.
        (while (re-search-forward "\033\\[[0]*m\033\\[[3]*1m\033\\[[3]*1m\\(.*?\\)\033\\[[0]*m" end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face 'rg-match-face)
                         t t)
          (cl-incf rg-hit-count))

        (rg-format-line-and-column-numbers beg end)

        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[0mK]" end 1)
          (replace-match "" t t))

        (goto-char beg)
        (run-hooks 'rg-filter-hook)))))

;; The regexp and filter functions below were taken from ag.el
;; Kudos to the people from https://github.com/Wilfred/ag.el for these.
(defconst rg-file-line-column-pattern-nogroup
  "^\\([^\n:]+?\\):\\([1-9][0-9]*\\):\\([1-9][0-9]*\\):"
  "A regexp pattern that groups output.
Groups into filename,line number and column number.")

(defun rg-file-line-column-pattern-group ()
  "A regexp pattern to match line number and column number with grouped output."
  (concat "^ *\\([1-9][0-9]*\\)"
          (regexp-quote (or (and rg-align-position-numbers
                                 rg-align-line-column-separator) ":"))
          " *\\([1-9][0-9]*\\)"
          (regexp-quote (or (and rg-align-position-numbers
                                 rg-align-position-content-separator) ":"))))

(defconst rg-file-line-pattern-nogroup
  "^\\([^\n:]+?\\):\\([1-9][0-9]*\\):"
  "A regexp pattern that groups output into filename, line number.")

(defun rg-file-line-pattern-group ()
  "A regexp pattern to match line number with grouped output."
  (concat "^ *\\([1-9][0-9]*\\)"
          (regexp-quote (or (and rg-align-position-numbers
                                 rg-align-position-content-separator) ":"))))

(defun rg-match-grouped-filename ()
  "Match filename backwards when a line/column match is found."
  (save-match-data
    (save-excursion
      (when (re-search-backward "^File: \\(.*\\)$" (point-min) t)
        (list (match-string 1))))))

(defun rg-set-compilation-error-regexps ()
  "Set the compilation mode regexps for errors for rg-mode buffers."
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(rg-group-with-column
         rg-nogroup-with-column
         rg-group-no-column
         rg-nogroup-no-column))
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (list (cons 'rg-nogroup-no-column (list rg-file-line-pattern-nogroup 1 2))
             (cons 'rg-nogroup-with-column (list rg-file-line-column-pattern-nogroup 1 2 3))
             (cons 'rg-group-with-column (list (rg-file-line-column-pattern-group) 'rg-match-grouped-filename 1 2))
             (cons 'rg-group-no-column (list (rg-file-line-pattern-group) 'rg-match-grouped-filename 1)))))

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
  (set (make-local-variable 'tool-bar-map) grep-mode-tool-bar-map)
  (set (make-local-variable 'compilation-error-face) 'rg-filename-face)
  (set (make-local-variable 'compilation-message-face) 'rg-match-position-face)
  (set (make-local-variable 'compilation-line-face) 'rg-line-number-face)
  (set (make-local-variable 'compilation-column-face) 'rg-column-number-face)

  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match.
  (set (make-local-variable 'compilation-directory-matcher) '("\\`a\\`"))
  (set (make-local-variable 'compilation-process-setup-function)
       'rg-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns) nil)
  (unless rg-search-history
    (setq rg-search-history (rg-history-create)))
  (set (make-local-variable 'compilation-filter-hook) '(rg-filter))
  ;; Set compilation error regexps as the last compilation-mode-hook to be
  ;; able to override these if they were set by other hooks.
  (add-hook 'compilation-mode-hook
            'rg-set-compilation-error-regexps 90 'local))

(defun rg-maybe-show-header ()
  "Recreate header if enabled."
  (when rg-show-header
    (rg-create-header-line 'rg-cur-search
                           (rg-search-full-command rg-cur-search))))

(defun rg-mode-init (search)
  "Initiate `rg-mode' with SEARCH in current buffer."
  (unless (eq major-mode 'rg-mode)
    (error "Function rg-mode-init called in non rg mode buffer"))
  (hack-dir-local-variables-non-file-buffer)
  (setq rg-cur-search search)
  (rg-history-push (rg-search-copy rg-cur-search)
                   rg-search-history)
  (rg-maybe-show-header)
  (rg-configure-imenu))

(defun rg-recompile ()
  "Rerun the current search."
  (interactive)
  (let ((rg-recompile t))
    (recompile))
  (hack-dir-local-variables-non-file-buffer)
  (rg-maybe-show-header)
  (rg-configure-imenu))

(defun rg-rerun (&optional no-history)
  "Run `recompile' with `compilation-arguments' taken from `rg-cur-search'.
If NO-HISTORY is non nil skip adding the search to the search history."
  (let ((pattern (rg-search-pattern rg-cur-search))
        (files (rg-search-files rg-cur-search))
        (dir (rg-search-dir rg-cur-search))
        (literal (rg-search-literal rg-cur-search))
        (flags (rg-search-flags rg-cur-search)))
    ;; compilation-directory is used as search dir and
    ;; default-directory is used as the base for file paths.
    (setq compilation-directory dir)
    (setq default-directory compilation-directory)
    (setcar compilation-arguments
            (or (rg-search-full-command rg-cur-search)
                (rg-build-command pattern files literal flags)))
    (unless no-history
      (rg-history-push (rg-search-copy rg-cur-search)
                       rg-search-history))
    (rg-recompile)))

(defun rg-navigate-file-message (pos limit direction)
  "Return position of next 'rg-file-message text property.
POS is the start position of the search and LIMIT is the limit of the
search.  If the property is not found within LIMIT, LIMIT is returned.  If
DIRECTION is positive search forward in the buffer, otherwise search
backward."
  (let ((prop-change-func
         (if (> direction 0)
             'next-single-property-change
           'previous-single-property-change)))
    (while
        (progn
          (setq pos (funcall prop-change-func pos 'rg-file-message nil limit))
          (and (not (eq pos limit))
               (not (get-text-property pos 'rg-file-message)))))
    pos))

(defun rg-navigate-file-group (steps)
  "Move point to the a matched result group in the compilation buffer.
STEPS decides how many groups to move past.  Negative value means
backwards and positive means forwards."
  (let (move-to
        (pos (point))
        (steps-left (abs steps))
        (limit
         (if (< steps 0)
             (point-min)
           (point-max))))
    (while  (and (> steps-left 0) (not (equal pos limit)))
      (setq pos (rg-navigate-file-message pos limit steps))
      (unless (eq pos limit)
        (setq move-to pos))
      (setq steps-left (- steps-left 1)))
    (when move-to
      (goto-char move-to))))

(defun rg-rerun-toggle-flag (flag)
  "Toggle FLAG in `rg-cur-search`."
  (setf (rg-search-flags rg-cur-search)
        (rg-list-toggle flag (rg-search-flags rg-cur-search)))
  (rg-rerun))

(defun rg-rerun-toggle-case ()
  "Rerun last search with toggled case sensitivity setting."
  (interactive)
  (rg-rerun-toggle-flag "-i"))

(defun rg-rerun-toggle-ignore ()
  "Rerun last search with toggled '--no-ignore' flag."
  (interactive)
  (rg-rerun-toggle-flag "--no-ignore"))

(defun rg-rerun-toggle-rexexp-literal ()
  "Switch between literal and regexp and rerun last search."
  (interactive)
  (setf (rg-search-literal rg-cur-search)
        (not (rg-search-literal rg-cur-search)))
  (rg-rerun))

(defun rg-rerun-change-query ()
  "Rerun last search and change search string."
  (interactive)
  (rg-rerun-change-search-string
   (rg-search-literal rg-cur-search)))

(defun rg-rerun-change-search-string (literal)
  "Rerun last search but prompt for new search pattern.
IF LITERAL is non nil this will trigger a literal search,
otherwise a regexp search."
  (let ((pattern (rg-search-pattern rg-cur-search))
        (read-from-minibuffer-orig (symbol-function 'read-from-minibuffer)))
    ;; Override read-from-minibuffer in order to insert the original
    ;; pattern in the input area.
    (cl-letf (((symbol-function #'read-from-minibuffer)
               (lambda (prompt &optional _ &rest args)
                 (apply read-from-minibuffer-orig prompt pattern args))))
      (setf (rg-search-pattern rg-cur-search) (rg-read-pattern literal pattern)))
    (setf (rg-search-literal rg-cur-search) literal)
    (rg-rerun)))

(defun rg-rerun-change-regexp ()
  "Rerun last search but prompt for new regexp."
  (interactive)
  (rg-rerun-change-search-string nil))

(defun rg-rerun-change-literal ()
  "Rerun last search but prompt for new literal."
  (interactive)
  (rg-rerun-change-search-string t))

(defun rg-rerun-change-files()
  "Rerun last search but prompt for new files."
  (interactive)
  (let ((files (rg-search-files rg-cur-search)))
    (setf (rg-search-files rg-cur-search)
          (completing-read
           (concat "Repeat search in files (default: [" files "]): ")
           (rg-get-type-aliases)
           nil nil nil 'rg-files-history
           files))
    (rg-rerun)))

(defun rg-rerun-change-dir()
  "Rerun last search but prompt for new dir."
  (interactive)
  (setf (rg-search-dir rg-cur-search)
        (read-directory-name "In directory: "
                             (rg-search-dir rg-cur-search) nil))
  (rg-rerun))

(defun rg-next-file (n)
  "Move point to next file's first match.
Prefix arg N decides how many files to navigate.  When
`rg-group-result' is nil this is the same as invoking
`compilation-next-file', otherwise this will navigate to the
next file with grouped matches."
  (interactive "p")
  (if rg-group-result
      (when (rg-navigate-file-group n)
        (forward-line))
    (compilation-next-file n)))

(defun rg-prev-file (n)
  "Move point to previous file's first match.
Prefix arg N decides how many files to navigate.  When
`rg-group-result' is nil this is the same as invoking
`compilation-previous-file', otherwise this will navigate to the
previous file with grouped matches."
  (interactive "p")
  (if rg-group-result
      (let ((steps
             ;; On match rows we move 2 steps back to get to previous
             ;; file, otherwise 1 step.  The later is on file
             ;; headings, space between files and at the end of search results.
             (if (or (get-text-property (point) 'rg-file-message)
                     (save-excursion
                       (beginning-of-line)
                       (looking-at "^\\(?:rg finished .*\\)*$")))
                 n
               (+ n 1))))
        (when (rg-navigate-file-group (- steps))
          (forward-line)))
    (compilation-previous-file n)))

(defun rg-back-history ()
  "Navigate back in the search history."
  (interactive)
  (if-let (prev (rg-history-back rg-search-history))
      (progn
        (setq rg-cur-search (rg-search-copy prev))
        (rg-rerun 'no-history))
    (message "No more history elements for back.")))

(defun rg-forward-history ()
  "Navigate forward in the search history."
  (interactive)
  (if-let (next (rg-history-forward rg-search-history))
      (progn
        (setq rg-cur-search (rg-search-copy next))
        (rg-rerun 'no-history))
    (message "No more history elements for forward.")))

(defun rg-configure-imenu ()
  "Add files with matches to imenu if rg-group-result is enabled."
  (when rg-group-result
    (setq imenu-create-index-function
          (lambda ()
            (goto-char (point-min))
            (let ((elements nil)
                  (filepath nil)
                  (nextfile (point-min)))
              (while (setq nextfile (rg-navigate-file-message nextfile nil 1))
                (save-excursion
                  (goto-char nextfile)
                  (and (looking-at-p "^File: ") (forward-char 6))
                  (setq filepath (buffer-substring-no-properties (point) (line-end-position))))
                (push (cons filepath nextfile) elements))
              (nreverse elements))))))

(provide 'rg-result)

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:

;;; rg-result.el ends here
