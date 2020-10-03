;;; rg-history.el-test.el --- Tests for rg-history.el -*- lexical-binding: t; -*-

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

;; Tests for search history navigation.

;;; Code:



;; Unit tests

(ert-deftest rg-unit/history-navigation ()
  "Testing the history data structure for navigation."
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
  "Testing the history data structure behavior when inserting new
search after moving in history."
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


;; Integration tests

(ert-deftest rg-integration/history ()
  "Verify that simple history navigation works."
  :tags '(need-rg)
  (rg-run-and-wait #'rg-run
                   "foo" "all" (concat default-directory "test/data"))
  (rg-run-and-wait #'rg-run
                   "bar" "all" (concat default-directory "test/data"))
  (rg-run-and-wait #'rg-run
                   "baz" "all" (concat default-directory "test/data"))
  (with-current-buffer (rg-buffer-name)
     (rg-run-and-wait #'rg-back-history)
     (should (equal (rg-search-pattern rg-cur-search) "bar"))
     (rg-run-and-wait #'rg-back-history)
     (should (equal (rg-search-pattern rg-cur-search) "foo"))
     (rg-run-and-wait #'rg-forward-history)
     (should (equal (rg-search-pattern rg-cur-search) "bar"))
     (rg-run-and-wait #'rg-forward-history)
     (should (equal (rg-search-pattern rg-cur-search) "baz"))))

(provide 'rg-history.el-test)

;;; rg-history.el-test.el ends here
