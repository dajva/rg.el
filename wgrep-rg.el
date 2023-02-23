;;; wgrep-rg.el --- Writable rg buffer and apply the changes to files

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Rewritten by: Dale Sedivec <dale@codefu.org>
;; Maintainer: David Landell <david.landell@sunnyhill.email>
;; Keywords: grep edit extensions
;; URL: http://github.com/dajva/rg.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; wgrep-rg allows you to edit a rg buffer and apply those changes to
;; the file buffer.

;;; Install:

;; 1. Install rg.el
;;
;;   https://github.com/dajva/rg.el

;; 2. Install wgrep.el

;; 3. Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (autoload 'wgrep-rg-setup "wgrep-rg")
;;     (add-hook 'rg-mode-hook 'wgrep-rg-setup)

;;; Usage:

;; See wgrep.el

;;; Code:

(require 'wgrep)

;; Forward declarations
(declare-function rg-file-line-column-pattern-group "rg-result.el")
(declare-function rg-file-line-pattern-group "rg-result.el")
(defvar rg-mode-hook)

(defvar wgrep-rg-grouped-result-file-regexp "^File:[[:space:]]+\\(.*\\)$"
  "Regular expression for the start of results for a file in grouped results.
\"Grouped results\" are what you get from rg.el when
`rg-group-result' is true or when you call rg with --heading.")

(defvar wgrep-rg-ungrouped-result-regexp
  "^\\(.+?\\)\\(?:-\\|:\\)\\([1-9][0-9]*\\)\\(?:-\\|:\\)\\(?:[1-9][0-9]*:\\)*"
  "Regular expression for an ungrouped result.
You get \"ungrouped results\" when `rg-group-result' is false or
when you manage to call rg with --no-heading.")

(defun wgrep-rg-prepare-header/footer ()
  "Add wgrep related text properties for header and footer.
This is needed in order for wgrep to ignore thos areas when parsing
the content."
  (save-excursion
    (goto-char (point-min))
    ;; Look for the first useful result line.
    (let ((result-line-regexp (concat wgrep-rg-grouped-result-file-regexp
                                      "\\|"
                                      wgrep-rg-ungrouped-result-regexp)))
      (while
          (progn
            (if (not (re-search-forward result-line-regexp nil t))
                ;; No results in this buffer, let's mark the whole thing as
                ;; header.
                (progn
                  (add-text-properties (point-min) (point-max)
                                       '(read-only t wgrep-header t))
                  nil)
              (save-excursion
                (beginning-of-line)
                ;; The ungrouped result regexp also match the "rg started"
                ;; line. If that happens continue searching.
                (if (looking-at-p "rg started.*")
                    'continue
                  (add-text-properties (point-min) (point)
                                       '(read-only t wgrep-header t))
                  nil)))))
        (goto-char (point-max))
        (re-search-backward "^rg finished .*$" nil t)
        ;; Move to the start of the line after the last result, and
        ;; mark everything from that line forward as wgrep-footer.
        (when (zerop (forward-line -1))
          (add-text-properties (point) (point-max)
                               '(read-only t wgrep-footer t))))))

(defun wgrep-rg-parse-command-results ()
  "Parse the rg results for wgrep usage.
This makes non content of the buffer readonly and marks specific areas
with wgrep text properties to allow for wgrep to do its job."
  ;; Note that this function is called with the buffer narrowed to
  ;; exclude the header and the footer.  (We're going to assert that
  ;; fact here, because we use (bobp) result a bit further down to
  ;; decide that we're not reading grouped results; see below.)
  (unless (bobp)
    (error "Expected to be called with point at beginning of buffer"))
  (save-excursion
    ;; First look for grouped results (`rg-group-result' is/was
    ;; probably true).
    (while (re-search-forward wgrep-rg-grouped-result-file-regexp nil t)
      ;; Ignore the line that introduces matches from a file, so that
      ;; wgrep doesn't let you edit it.
      (add-text-properties (match-beginning 0) (match-end 0)
                           '(wgrep-ignore t))
      (let ((file-name (match-string-no-properties 1)))
        ;; Note that I think wgrep uses this property to quickly find
        ;; the file it's interested in when searching during some
        ;; operation(s).  We stick it on the file name in the results
        ;; group header.
        (add-text-properties (match-beginning 1) (match-end 1)
                             (list (wgrep-construct-filename-property file-name)
                                   file-name))
        ;; Matches are like: 999{separator}55{separator}line content here
        ;; Context lines are like: 999-line content here
        (while (and
                (zerop (forward-line 1)) ; last line
                (not (looking-at-p "^$"))) ; Empty line between files
          (cond ((or (looking-at (rg-file-line-column-pattern-group))
                     (looking-at (rg-file-line-pattern-group))
                     ;; Context lines
                     (looking-at "^ *\\([1-9][0-9]*\\)-"))
                 (add-text-properties (match-beginning 0) (match-end 0)
                                      (list 'wgrep-line-filename file-name
                                            'wgrep-line-number
                                            (string-to-number (match-string 1)))))
                ;; Ignore context separator.
                ((looking-at "^--$")
                 (add-text-properties (match-beginning 0) (match-end 0)
                                      '(wgrep-ignore t)))))))
    (when (bobp)
      ;; Search above never moved point, so match non-grouped results
      ;; (`rg-group-result' is/was probably false).
      (let (last-file-name)
        ;; Matches are like: /foo/bar:999:55:line content here
        ;; Context lines are like: /foo/bar-999-line content here
        (while (re-search-forward (concat wgrep-rg-ungrouped-result-regexp
                                          "\\|\\(^--$\\)")
                                  nil t)
          (if (match-beginning 3)
              ;; Ignore context separator.
              (add-text-properties (match-beginning 0) (match-end 0)
                                   '(wgrep-ignore t))
            (let ((file-name (match-string-no-properties 1))
                  (line-number (string-to-number (match-string 2))))
              (unless (equal file-name last-file-name)
                ;; This line is a result from a different file than
                ;; the last match (or else this is the first match in
                ;; the results).  Write the special file name property
                ;; for wgrep.
                (let ((file-name-prop
                       (wgrep-construct-filename-property file-name)))
                  (add-text-properties (match-beginning 1) (match-end 1)
                                       (list file-name-prop file-name)))
                (setq last-file-name file-name))
              (add-text-properties (match-beginning 0) (match-end 0)
                                   (list 'wgrep-line-filename file-name
                                         'wgrep-line-number line-number)))))))))

;;;###autoload
(defun wgrep-rg-setup ()
  "Setup wgrep rg support."
  (set (make-local-variable 'wgrep-header&footer-parser)
       'wgrep-rg-prepare-header/footer)
  (set (make-local-variable 'wgrep-results-parser)
       'wgrep-rg-parse-command-results)
  (wgrep-setup-internal))

(defun wgrep-rg-warn-ag-setup ()
  "Print warning message if old ag setup is used."
  (when (memq 'wgrep-ag-setup rg-mode-hook)
    (message "Warning: wgrep-ag is no longer supported by this package. Please remove wgrep-ag-setup from rg-mode-hook.")))

;;;###autoload
(add-hook 'rg-mode-hook 'wgrep-rg-setup)

;; For `unload-feature'
(defun wgrep-rg-unload-function ()
  "Allow for unloading wgrep rg support."
  (remove-hook 'rg-mode-hook 'wgrep-rg-setup))

(provide 'wgrep-rg)

;;; wgrep-rg.el ends here
