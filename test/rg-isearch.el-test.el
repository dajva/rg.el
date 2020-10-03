;;; rg-isearch.el-test.el --- rg-isearch.el tests -*- lexical-binding: t; -*-

;; Copyright (C) 2020 David Landell <david.landell@sunnyhill.email>
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

(ert-deftest rg-unit/isearch-current-file ()
  "Test `rg-isearch-current-file'."
  (cl-letf* ((called-pattern nil)
             (called-literal nil)
             (called-files nil)
             (called-dir nil)
             ((symbol-function #'rg-run)
              (lambda (pattern files dir &optional literal &rest _)
                (setq called-pattern pattern)
                (setq called-literal literal)
                (setq called-files files)
                (setq called-dir dir))))
    (find-file "test/data/bar.el")
    (let ((isearch-string "test"))
      (rg-isearch-current-file)
      (should (equal called-pattern "test"))
      (should (eq called-literal t))
      (should (equal called-files "bar.el"))
      (should (equal (expand-file-name called-dir) (expand-file-name default-directory))))))


(ert-deftest rg-unit/isearch-current-dir ()
  "Test `rg-isearch-current-dir'."
  (cl-letf* ((called-pattern nil)
             (called-literal nil)
             (called-files nil)
             (called-dir nil)
             ((symbol-function #'rg-run)
              (lambda (pattern files dir &optional literal &rest _)
                (setq called-pattern pattern)
                (setq called-literal literal)
                (setq called-files files)
                (setq called-dir dir))))
    (find-file "test/data/bar.el")
    (let ((isearch-string "test"))
      (rg-isearch-current-dir)
      (should (equal called-pattern "test"))
      (should (eq called-literal t))
      (should (equal called-files "elisp"))
      (should (equal (expand-file-name called-dir) (expand-file-name default-directory))))))

(ert-deftest rg-unit/isearch-project ()
  "Test `rg-isearch-project'."
  (cl-letf* ((called-pattern nil)
             (called-literal nil)
             (called-files nil)
             (called-dir nil)
             (project-dir (expand-file-name default-directory))
             ((symbol-function #'rg-run)
              (lambda (pattern files dir &optional literal &rest _)
                (setq called-pattern pattern)
                (setq called-literal literal)
                (setq called-files files)
                (setq called-dir dir))))
    (find-file "test/data/bar.el")
    (let ((isearch-string "test"))
      (rg-isearch-project)
      (should (equal called-pattern "test"))
      (should (eq called-literal t))
      (should (equal called-files "elisp"))
      (should (equal (expand-file-name called-dir) project-dir)))))

(ert-deftest rg-unit/isearch-regexp ()
  "Test that we do regexp search if required."
  (cl-letf* ((called-pattern nil)
             (called-literal nil)
             ((symbol-function #'rg-run)
              (lambda (pattern _files _dir &optional literal &rest _)
                (setq called-pattern pattern)
                (setq called-literal literal))))
    (find-file "test/data/bar.el")
    (let ((isearch-string "test")
          (isearch-regexp t))
      (rg-isearch-current-file)
      (should (equal called-pattern "test"))
      (should (eq called-literal nil)))))

(ert-deftest rg-unit/isearch-regexp-func ()
  "Test that we handle a regexp function correctly."
  (cl-letf* ((called-pattern nil)
             (called-literal nil)
             ((symbol-function #'rg-run)
              (lambda (pattern _files _dir &optional literal &rest _)
                (setq called-pattern pattern)
                (setq called-literal literal))))
    (find-file "test/data/bar.el")
    (let ((isearch-string "test")
          (isearch-regexp-function #'word-search-regexp))
      (rg-isearch-current-file)
      (should (equal called-pattern "test"))
      (should (eq called-literal nil)))))

(ert-deftest rg-unit/isearch-regexp-word ()
  "Test that we handle a regexp word search correctly."
  (cl-letf* ((called-pattern nil)
             (called-literal nil)
             ((symbol-function #'rg-run)
              (lambda (pattern _files _dir &optional literal &rest _)
                (setq called-pattern pattern)
                (setq called-literal literal))))
    (find-file "test/data/bar.el")
    (let ((isearch-string "test")
          (isearch-regexp-function t))
      (rg-isearch-current-file)
      (should (equal called-pattern "\btest\b"))
      (should (eq called-literal nil)))))

(provide 'rg-isearch.el-test)

;;; rg-isearch.el-test.el ends here
