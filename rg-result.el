;;; rg-result.el ---- Result buffer implementation for rg.el. *- lexical-binding: t; -*-

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
(require 'rg-compat)
(require 'rg-header)
(require 'rg-history)
(require 's)
(require 'wgrep-rg)

;; Forward declarations.
(declare-function rg-build-command "rg.el")
(declare-function rg-get-type-aliases "rg.el")
(declare-function rg-read-pattern "rg.el")


;; Customizations/public vars
(defcustom rg-show-columns nil
  "If t, show the columns of the matches in the output buffer."
  :type 'boolean
  :group 'rg)

(defcustom rg-group-result nil
  "Group matches in the same file together.
If nil, the file name is repeated at the beginning of every match line."
  :type 'boolean
  :group 'rg)

(defcustom rg-show-header t
  "Show header in results buffer if non nil."
  :type 'boolean
  :group 'rg)

(defcustom rg-hide-command t
  "Hide most of rg command line when non nil."
  :type 'boolean
  :group 'rg)

(defcustom rg-align-position-numbers nil
  "If non nil, alignment of line and colum numbers is turned on."
  :type 'boolean
  :group 'rg)

(defcustom rg-align-line-number-field-length 4
  "Field length of aligned line numbers."
  :type 'integer
  :group 'rg)

(defcustom rg-align-column-number-field-length 3
  "Field length of aligned column numbers."
  :type 'integer
  :group 'rg)

(defcustom rg-align-line-column-separator nil
  "Separator used between line and column numbers.
Depends on `rg-show-columns'.  Default is ':'."
  :type 'string
  :group 'rg)

(defcustom rg-align-position-content-separator nil
  "Separator used between position numbers and the matching file content.
Default is ':'."
  :type 'string
  :group 'rg)

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
  '((t :inherit font-lock-function-name-face))
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
  toggle-flags           ; toggle command line flags
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
    ("^ *\\(?:.+?-\\)?[0-9]+-.*\n" (0 'rg-context-face))
    ("^.*rg \\(--color always .*$\\)"
     (0 '(face rg-context-face))
     (1 (rg-hide-command-properties)))))

(defvar rg-mode-map
  (let ((map (copy-keymap grep-mode-map)))
    (define-key map "c" 'rg-rerun-toggle-case)
    (define-key map "d" 'rg-rerun-change-dir)
    (define-key map "f" 'rg-rerun-change-files)
    (define-key map "g" 'recompile)
    (define-key map "i" 'rg-rerun-toggle-ignore)
    (define-key map "l" 'rg-list-searches)
    (define-key map "r" 'rg-rerun-change-regexp)
    (define-key map "s" 'rg-save-search)
    (define-key map "S" 'rg-save-search-as-name)
    (define-key map "t" 'rg-rerun-change-literal)
    (define-key map "w" 'wgrep-change-to-wgrep-mode)
    (define-key map "\C-n" 'rg-next-file)
    (define-key map "\C-p" 'rg-prev-file)
    (define-key map "\C-f" 'rg-forward-history)
    (define-key map "\C-b" 'rg-back-history)
    map)
  "The keymap for `rg-mode'.")

(defvar rg-ellipsis (if (char-displayable-p ?…) "[…]" "[...]")
  "Used when hiding command line.")


;; Defuns

;; This solution was mostly copied from emacs grep.el and adjusted to
;; be more usable.
(defun rg-hide-command-properties ()
    "Return properties of button-like ellipsis on part of rg command line."
  (let ((map (make-sparse-keymap))
        properties)
    (define-key map [down-mouse-2] 'mouse-set-point)
    (define-key map [mouse-2] 'rg-toggle-command-hiding)
    (define-key map "\C-m" 'rg-toggle-command-hiding)
    (append
     `(face nil mouse-face highlight
            help-echo "RET, mouse-2: show unabbreviated command"
            keymap ,map rg-command t)
     (when rg-hide-command
         `(display ,rg-ellipsis)))))

(defun rg-toggle-command-hiding ()
  "Toggle showing the hidden part of rg command line."
  (interactive)
  (with-silent-modifications
    (let* ((beg (next-single-property-change (point-min) 'rg-command))
           (end (when beg
                  (next-single-property-change beg 'rg-command))))
      (if end
          (if (get-text-property beg 'display)
              (remove-list-of-text-properties beg end '(display))
            (add-text-properties beg end `(display ,rg-ellipsis)))
        (user-error "No abbreviated part to hide/show")))))

(defun rg-list-toggle (elem list)
  "Remove ELEM from LIST if present or add it if not present.
Returns the new list."
  (if (member elem list)
      (delete elem list)
    (push elem list)))

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

(defun rg-prepend-space (text length)
  "Prepend TEXT with LENGTH number of spaces."
  (let ((space-count (- length (length text))))
    (concat (s-repeat space-count " ") text)))

(defun rg-align-position-numbers-in-region (beg end)
  "Align numbers in region defined by BEG and END."
  (goto-char beg)
  (let ((line-col-separator (or rg-align-line-column-separator ":"))
        (pos-content-separator (or rg-align-position-content-separator ":")))
    (while (re-search-forward
            "^\033\\[[0]*m\033\\[32m\\([0-9]*?\\)\033\\[[0]*m\\(:\\|-\\)\\(?:\033\\[[0]*m\\([0-9]*?\\)\033\\[[0]*m:\\)?"
            end 1)
      (replace-match (concat
                      (rg-prepend-space
                       (match-string 1)
                       (+ rg-align-line-number-field-length
                          (if (and rg-show-columns
                                   (equal (match-string 2) "-"))
                              ;; Context lines
                              (1+ rg-align-column-number-field-length)
                            0)))
                      (rg-when-let (column-match (match-string 3))
                        (concat line-col-separator
                                (rg-prepend-space
                                 column-match
                                 rg-align-column-number-field-length)))
                      (if (equal (match-string 2) "-")
                          ;; Keep context separators
                          "-"
                        pos-content-separator))
                     t t))))

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
          (setq rg-hit-count (+ rg-hit-count 1)))

        ;; Align and format line and column numbers.
        (when rg-align-position-numbers
          (rg-align-position-numbers-in-region beg end))

        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[0mK]" end 1)
          (replace-match "" t t))))
    (run-hooks 'rg-filter-hook)))

;; The regexp and filter functions below were taken from ag.el
;; Kudos to the people from https://github.com/Wilfred/ag.el for these.
(defconst rg-file-line-column-pattern-nogroup
  "^\\(.+?\\):\\([1-9][0-9]*\\):\\([1-9][0-9]*\\):"
  "A regexp pattern that groups output into filename, line number and column number.")

(defun rg-file-line-column-pattern-group ()
  "A regexp pattern to match line number and column number with grouped output."
  (concat "^ *\\([1-9][0-9]*\\)"
          (regexp-quote (or rg-align-line-column-separator ":"))
          " *\\([1-9][0-9]*\\)"
          (regexp-quote (or rg-align-position-content-separator ":"))))

(defconst rg-file-line-pattern-nogroup
  "^\\(.+?\\):\\([1-9][0-9]*\\):"
  "A regexp pattern that groups output into filename, line number.")

(defun rg-file-line-pattern-group ()
  "A regexp pattern to match line number with grouped output."
  (concat "^ *\\([1-9][0-9]*\\)"
          (regexp-quote (or rg-align-position-content-separator ":"))))

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
  (set (make-local-variable 'compilation-error-face) 'rg-filename-face)
  (set (make-local-variable 'compilation-message-face) 'rg-match-position-face)
  (set (make-local-variable 'compilation-line-face) 'rg-line-number-face)
  (set (make-local-variable 'compilation-column-face) 'rg-column-number-face)

  (set (make-local-variable 'compilation-error-regexp-alist)
       '(rg-group-with-column rg-nogroup-with-column rg-group-no-column  rg-nogroup-no-column))
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (list (cons 'rg-nogroup-no-column (list rg-file-line-pattern-nogroup 1 2))
             (cons 'rg-nogroup-with-column (list rg-file-line-column-pattern-nogroup 1 2 3))
             (cons 'rg-group-with-column (list (rg-file-line-column-pattern-group) 'rg-match-grouped-filename 1 2))
             (cons 'rg-group-no-column (list (rg-file-line-pattern-group) 'rg-match-grouped-filename 1))))

  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match.
  (set (make-local-variable 'compilation-directory-matcher) '("\\`a\\`"))
  (set (make-local-variable 'compilation-process-setup-function)
       'rg-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns) nil)
  (unless rg-search-history
    (setq rg-search-history (rg-history-create)))
  (add-hook 'compilation-filter-hook 'rg-filter nil t))

(defun rg-maybe-show-header ()
  "Recreate header if enabled."
  (when rg-show-header
    (rg-create-header-line 'rg-cur-search
                           (rg-search-full-command rg-cur-search))))

(defun rg-mode-init (search)
  "Initiate rg-mode with SEARCH in current buffer."
  (unless (eq major-mode 'rg-mode)
    (error "Function rg-mode-init called in non rg mode buffer"))
  (setq rg-cur-search search)
  (rg-history-push (rg-search-copy rg-cur-search)
                   rg-search-history)
  (rg-maybe-show-header))

(defun rg-rerun (&optional no-history)
  "Run `recompile' with `compilation-arguments' taken from `rg-cur-search'.
If NO-HISTORY is non nil skip adding the search to the search history."
  (let ((pattern (rg-search-pattern rg-cur-search))
        (files (rg-search-files rg-cur-search))
        (dir (rg-search-dir rg-cur-search))
        (literal (rg-search-literal rg-cur-search))
        (toggle-flags (rg-search-toggle-flags rg-cur-search))
        (flags (rg-search-flags rg-cur-search)))
    (setcar compilation-arguments
            (or (rg-search-full-command rg-cur-search)
                (rg-build-command pattern files literal
                                  (append toggle-flags flags))))
    ;; compilation-directory is used as search dir and
    ;; default-directory is used as the base for file paths.
    (setq compilation-directory dir)
    (setq default-directory compilation-directory)
    (unless no-history
      (rg-history-push (rg-search-copy rg-cur-search)
                       rg-search-history))
    (recompile)
    (rg-maybe-show-header)))

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
  (let ((pos (point))
        (steps-left (abs steps))
        (limit
         (if (< steps 0)
             (point-min)
           (point-max))))
    (while  (and (> steps-left 0) (not (equal pos limit)))
      (setq pos (rg-navigate-file-message pos limit steps))
      (setq steps-left (- steps-left 1)))
    (unless (equal pos limit)
      (goto-char pos))))

(defun rg-rerun-toggle-flag (flag)
  "Toggle FLAG in `rg-cur-search`."
  (setf (rg-search-toggle-flags rg-cur-search)
        (rg-list-toggle flag (rg-search-toggle-flags rg-cur-search)))
  (rg-rerun))

(defun rg-rerun-toggle-case ()
  "Rerun last search with toggled case sensitivity setting."
  (interactive)
  (rg-rerun-toggle-flag "-i"))

(defun rg-rerun-toggle-ignore ()
  "Rerun last search with toggled '--no-ignore' flag."
  (interactive)
  (rg-rerun-toggle-flag "--no-ignore"))

(defun rg-rerun-change-search-string (literal)
  "Rerun last search but prompt for new search pattern.
IF LITERAL is non nil this will trigger a literal search, otherwise a regexp search."
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

(defun rg-back-history ()
  "Navigate back in the search history."
  (interactive)
  (rg-if-let (prev (rg-history-back rg-search-history))
      (progn
        (setq rg-cur-search (rg-search-copy prev))
        (rg-rerun 'no-history))
    (message "No more history elements for back.")))

(defun rg-forward-history ()
  "Navigate forward in the search history."
  (interactive)
  (rg-if-let (next (rg-history-forward rg-search-history))
      (progn
        (setq rg-cur-search (rg-search-copy next))
        (rg-rerun 'no-history))
    (message "No more history elements for forward.")))

(provide 'rg-result)

;;; rg-result.el ends here
