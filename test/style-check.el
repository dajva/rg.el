;;; style-check.el --- rg.el: Check emacs lisp style of package files

;; Copyright (C) 2017 David Landell <david.landell@sunnyhill.email>
;;
;; Author: David Landell <david.landell@sunnyhill.email>
;; URL: https://github.com/davja/rg.el

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

(require 'flycheck)
(require 'package-lint)
(load "test-helper")

(defun rg-message ())

(defun get-compilation-buffer ()
  (let (buffer)
    (dolist (buf (buffer-list) buffer)
      (when (eq (with-current-buffer buf major-mode) 'compilation-mode)
        (setq buffer buf)))))

(defun run-flycheck (checker)
  (flycheck-compile checker)
  (let ((buffer (get-compilation-buffer)))
    (unless buffer
      (error "No compilation buffer"))
    (with-current-buffer buffer
      (rg-wait-for-search-result)
      (goto-char (point-min))
      (let (beg error-lines)
        (while (ignore-errors (compilation-next-error 1))
          (setq beg (point))
          (end-of-line)
          (push (buffer-substring-no-properties beg (point)) error-lines))
        (when error-lines
          (rg-message "%s" (mapconcat 'identity
                                      (reverse error-lines) "\n"))
          (kill-emacs 2))))))

(defun run-emacs-lisp-flycheck-and-exit ()
  (cl-letf (((symbol-function #'rg-message) (symbol-function #'message))
            ((symbol-function #'message) #'ignore))
     (dolist (file argv)
      (dolist (checker '(emacs-lisp-checkdoc emacs-lisp))
        (with-temp-buffer
          (insert-file-contents file t)
          (emacs-lisp-mode)
          (run-flycheck checker)))))
  (kill-emacs 0))

(defun run-package-lint-and-exit ()
  (setq package-user-dir "/tmp/rg-elpa")
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-refresh-contents)
  (package-lint-batch-and-exit))

;;; style-check.el ends here
