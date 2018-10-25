;;; rg-history.el-test.el --- Tests for rg-history.el *- lexical-binding: t; -*-

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



;; Unit tests

(ert-deftest rg-unit/history-navigation ()
  (let ((instance (rg-history-create)))
    (should (equal (rg-history-back instance) nil))
    (rg-history-push 'search1 instance)
    (rg-history-push 'search2 instance)
    (rg-history-push 'search3 instance)
    (should (equal (rg-history-forward instance) nil))
    (should (equal (rg-history-back instance) 'search2))
    (should (equal (rg-history-back instance) 'search1))
    (should (equal (rg-history-back instance) nil))
    (should (equal (rg-history-forward instance) 'search2))
    (should (equal (rg-history-forward instance) 'search3))
    (should (equal (rg-history-forward instance) nil))))

(ert-deftest rg-unit/history-new-search ()
  (let ((instance (rg-history-create)))
    (should (equal (rg-history-back instance) nil))
    (rg-history-push 'search1 instance)
    (rg-history-push 'search2 instance)
    (rg-history-push 'search3 instance)
    (should (equal (rg-history-forward instance) nil))
    (should (equal (rg-history-back instance) 'search2))
    (rg-history-push 'search4 instance)
    (should (equal (rg-history-forward instance) nil))
    (should (equal (rg-history-back instance) 'search2))
    (should (equal (rg-history-back instance) 'search1))
    (should (equal (rg-history-back instance) nil))))

(provide 'rg-history.el-test)

;;; rg-history.el-test.el ends here
