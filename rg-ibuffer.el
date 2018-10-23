;;; rg-ibuffer.el --- List search buffers for rg-mode *- lexical-binding: t; -*-

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

;; Use ibuffer to list active searches.

;;; Code:

(require 'ibuf-ext)
(require 'ibuffer)
(require 'rg-result)

(defconst rg-search-list-buffer-name "*Searches rg*")

(defun rg-ibuffer-search-updated()
  "This function is executed when search list buffer is updated."
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
  (or (rg-search-pattern rg-cur-search) "N/A"))

(define-ibuffer-column rg-hit-count
  (:name "Hits")
  (ignore mark)
  (number-to-string rg-hit-count))

(define-ibuffer-column rg-search-dir
  (:name "Directory" :props ('face 'rg-filename-face))
  (ignore mark)
  (or (rg-search-dir rg-cur-search) "N/A"))

(define-ibuffer-column rg-file-types
  (:name "Type")
  (ignore mark)
  (or (rg-search-files rg-cur-search) "N/A"))

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

(provide 'rg-ibuffer)

;;; rg-ibuffer.el ends here
