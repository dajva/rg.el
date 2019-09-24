;;; rg-menu.el-test.el --- Tests for rg-menu.el *- lexical-binding: t; -*-

;; Copyright (C) 2018 David Landell <david.landell@sunnyhill.email>
;;
;; Author: David Landell <david.landell@sunnyhill.email>
;; URL: https://github.com/davja/

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
(ert-deftest rg-unit-test/enable-menu ()
  "Test `rg-enable-menu'."
  (rg-with-temp-global-keymap
    (rg-enable-menu)
    (should (eq 'rg-menu (lookup-key (current-global-map) "\C-cs"))))
  (rg-with-temp-global-keymap
   (rg-enable-menu "\M-s")
   (should (eq 'rg-menu (lookup-key (current-global-map) "\M-s"))))

  (let ((rg-keymap-prefix "\M-s"))
    ;; Default prefix
    (rg-with-temp-global-keymap
      (rg-enable-menu)
      (should (eq 'rg-menu (lookup-key (current-global-map) "\M-s"))))
    ;; Function supplied prefix
    (rg-with-temp-global-keymap
      (rg-enable-menu "\C-ct")
      (should (eq 'rg-menu (lookup-key (current-global-map) "\C-ct")))))

  (let ((rg-mode-map (make-sparse-keymap)))
    (rg-enable-menu)
    (should (eq 'rg-menu (lookup-key rg-mode-map "m")))))

(ert-deftest rg-unit/menu-transient-wrappers ()
  "Test the transient wrappers."
  (cl-letf* ((transient-flags '("a" "b" "c"))
             (func-flags '("d" "e"))
             (flags-result nil)

             (rg-command-line-flags-function (lambda (_) '("f")))
             ((symbol-function #'test-rerun) #'ignore)
             ((symbol-function #'test-search)
              (lambda ()
                (interactive)
                (setf flags-result (funcall rg-command-line-flags-function func-flags))))
             ((symbol-function #'transient-get-value) (lambda () transient-flags))

             (rg-cur-search (rg-search-create
                 :pattern "test"
                 :files "all"
                 :dir "/tmp"
                 :literal t
                 :toggle-flags nil
                 :flags nil)))

  (rg-menu-wrap-transient-search test-search)
  (should (commandp #'test-search--transient))
  (rg-menu-wrap-transient-rerun test-rerun)
  (should (commandp #'test-rerun--transient))

  (test-search--transient)
  (should-not (seq-difference flags-result '("a" "b" "c" "d" "e" "f")))

  (test-rerun--transient)
  (should (equal (rg-search-flags rg-cur-search) transient-flags))))

(ert-deftest rg-unit/menu-transient-insert ()
  "Test adding new items to the menu"
  (define-transient-command rg-menu ()
    [ "Switches" ]
    [ "Options" ]
    [[ "Search"
       (3 "s" "search" search--transient)]
     [ "Rerun"
       (3 "r" "rerun" rerun--transient)]])
  ;; Make sure we are not using some other rg-menu
  (should-not (ignore-errors (transient-get-suffix 'rg-menu '(2 2))))
  (should (transient-get-suffix 'rg-menu 'search--transient))
  (should (transient-get-suffix 'rg-menu 'rerun--transient))

  ;; Add to existing sub group
  (rg-menu-transient-insert "Search" "n" "new search" 'new--transient)
  (should (eq (plist-get
               (cl-third (transient-get-suffix 'rg-menu '(2 0 1))) :command)
              'new--transient))

  ;; Create a new sub group and add to that
  (rg-menu-transient-insert "Custom" "c" "custom search" 'custom--transient)
  (should (eq (plist-get
               (cl-third (transient-get-suffix 'rg-menu '(2 2 0))) :command)
              'custom--transient)))

(ert-deftest rg-unit/menu-define-search ()
  "Test adding new items to the menu"
  (define-transient-command rg-menu ()
    [ "Switches" ]
    [ "Options" ]
    [[ "Search"
       (3 "s" "search" search--transient)]
     [ "Rerun"
       (3 "r" "rerun" rerun--transient)]])
  ;; Make sure we are not using some other rg-menu
  (should-not (ignore-errors (transient-get-suffix 'rg-menu '(2 2))))

  (rg-define-search menu-search
    :menu ("Macro" "m" "macro search"))

  (let ((subgroup (seq-elt (transient-get-suffix 'rg-menu '(2 2)) 2))
        (entry (cl-third (transient-get-suffix 'rg-menu '(2 2 0)))))
    (should (equal (plist-get subgroup :description)
                   "Macro"))
    (should (eq (plist-get entry :command)
                'menu-search--transient))
    (should (equal (plist-get entry :description)
                   "macro search"))
    (should (equal (plist-get entry :key)
                   "m"))))

(ert-deftest rg-unit/menu-search-initial-value ()
  "Test extraction of flags into transient initial value."
  (let ((rg-cur-search (rg-search-create
                        :pattern "test"
                        :files "all"
                        :dir "/tmp"
                        :literal t
                        :toggle-flags '("a" "b" "c")
                        :flags '("d" "e"))))
    (with-temp-buffer
      (should (equal (rg-menu-search-initial-value) nil))
      (rg-mode)
      (should-not (seq-difference
                   (rg-menu-search-initial-value)
                   '("a" "b" "c" "d" "e"))))))

(provide 'rg-menu.el-test)

;;; rg-menu.el-test.el ends here
