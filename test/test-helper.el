;;; test-helper.el --- rg.el: Helper for tests

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

(when (require 'undercover nil t)
  (undercover "rg.el"))

(require 'cl-lib)
(require 'ert)
(require 'rg)
(require 's)
(require 'seq)

(defun rg-regexp-anywhere (needle)
  (s-replace "%%%%" needle "\\( \\|^\\)%%%%\\( \\|$\\)"))

(defun rg-regexp-last (needle)
  (s-replace "%%%%" needle "\\( \\|^\\)%%%%$"))

(defun rg-regexp-anywhere-but-last (needle)
  (s-replace "%%%%" needle "\\( \\|^\\)%%%% "))

(defun rg-wait-for-search-result ()
"Wait for the rg process to finish searching and return non nil if the
search was successful. Timeout is 10 s."
  (let (search-finished)
    (add-hook 'compilation-finish-functions
              (lambda (buffer msg) (setq search-finished msg))
              t t)
    (with-timeout (10 nil)
      (while (not search-finished)
        (accept-process-output nil 0.1)))
    (when search-finished
      (equal (s-trim search-finished) "finished"))))

(defun rg-check-git-project-root ()
"Check that project root of rg.el-test.el file is main dir of
repository."
  (should (equal
           (expand-file-name (rg-project-root
                              (concat default-directory "/test/rg.el-test.el")))
           (expand-file-name default-directory))))

(defmacro rg-with-current-result (&rest body)
"Evaluate BODY in current result buffer when search has finished."
  (declare (indent 0))
  `(with-current-buffer "*rg*"
     (should (rg-wait-for-search-result))
     (let ((result (progn ,@body)))
       (kill-buffer "*rg*")
       result)))

(defmacro rg-with-temp-global-keymap (&rest body)
"Evaluate BODY with a temporary keymap as global map and then restore
the original global kemap"
  (declare (indent 0))
  (let ((saved-global-map (cl-gensym))
        (temp-global-map (cl-gensym)))
    `(let ((,saved-global-map (current-global-map))
           (,temp-global-map (make-sparse-keymap)))
       (use-global-map ,temp-global-map)
       ,@body
       (use-global-map ,saved-global-map))))

;;; test-helper.el ends here
