;;; wgrep-rg.el-test.el --- wgrep-rg.el tests *- lexical-binding: t; -*-

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


;; Integration tests

(ert-deftest rg-integration-test/wgrep-nogroup ()
  "wgrep test with grouped result."
  :tags '(need-rg)
  (let ((rg-show-columns nil)
        (rg-group-result nil)
        (rg-align-position-numbers nil))
    (rg-run-wgrep-tests)
    (rg-run-wgrep-context-tests)))

(ert-deftest rg-integration-test/wgrep-nogroup-columns ()
  "wgrep test with grouped result."
  :tags '(need-rg)
  (let ((rg-show-columns t)
        (rg-group-result nil)
        (rg-align-position-numbers nil))
    (rg-run-wgrep-tests)
    (rg-run-wgrep-context-tests)))

(ert-deftest rg-integration-test/wgrep-group ()
  "wgrep test with grouped result."
  :tags '(need-rg)
  (let ((rg-show-columns nil)
        (rg-group-result t)
        (rg-align-position-numbers nil))
    (rg-run-wgrep-tests)
    (rg-run-wgrep-context-tests)))

(ert-deftest rg-integration-test/wgrep-group-columns ()
  "wgrep test with grouped result."
  :tags '(need-rg)
  (let ((rg-show-columns t)
        (rg-group-result t)
        (rg-align-position-numbers nil))
    (rg-run-wgrep-tests)
    (rg-run-wgrep-context-tests)))

(ert-deftest rg-integration-test/wgrep-group-align ()
  "wgrep test with aligned and grouped result."
  :tags '(need-rg)
  (let ((rg-show-columns nil)
        (rg-group-result t)
        (rg-align-position-numbers t))
    (rg-run-wgrep-tests)
    (rg-run-wgrep-context-tests)))

(ert-deftest rg-integration-test/wgrep-group-align-columns ()
  "wgrep test with aligned and grouped result including column numbers."
  :tags '(need-rg)
  (let ((rg-show-columns t)
        (rg-group-result t)
        (rg-align-position-numbers t))
    (rg-run-wgrep-tests)
    (rg-run-wgrep-context-tests)))

(ert-deftest rg-integration-test/wgrep-group-align-columns-custom-separators ()
  "wgrep test with aligned and grouped result including column numbers."
  :tags '(need-rg)
  (let ((rg-show-columns t)
        (rg-group-result t)
        (rg-align-position-numbers t)
        (rg-align-line-column-separator "   ")
        (rg-align-position-content-separator "#"))
    (rg-run-wgrep-tests)
    (rg-run-wgrep-context-tests)))


;; Helper functions

(defun rg-check-wgrep-header ()
  (goto-char (point-min))
  (should (get-text-property (point) 'wgrep-header))
  ;; Jump over "rg-started" line that could match error regexp.
  (goto-char (next-single-property-change (point-min) 'rg-command-hidden-part))
  (should (get-text-property (point) 'wgrep-header))
  (end-of-line)
  (should (get-text-property (point) 'wgrep-header))
  (rg-next-file 1)
  (should-not (get-text-property (point) 'wgrep-header)))

(defun rg-check-wgrep-footer ()
  (goto-char (1- (point-max)))
  (should (get-text-property (point) 'wgrep-footer))
  ;; "rg finished" line sometimes match error regexp so skip that.
  (forward-line -1)
  (compilation-previous-error 1)
  (should-not (get-text-property (point) 'wgrep-footer))
  (re-search-forward "^$")
  (should (get-text-property (point) 'wgrep-footer)))

(defun rg-check-wgrep-props-at-pos (pos)
  (let ((wgrep-expected-props '(wgrep-line-filename wgrep-line-number))
        (all-props (text-properties-at pos)))
    (dolist (expected wgrep-expected-props)
      (should (member expected all-props)))))

(defun rg-check-no-wgrep-prop-at-pos (pos)
  (let ((wgrep-expected-props '(wgrep-line-filename wgrep-line-number))
        (all-props (text-properties-at pos)))
    (dolist (expected wgrep-expected-props)
      (should-not (member expected all-props)))))

(defun rg-check-wgrep-current-line ()
  (rg-check-wgrep-props-at-pos (point))
  (should (get-text-property (point) 'read-only))
  (end-of-line)
  (backward-char)
  (rg-check-no-wgrep-prop-at-pos (point))
  (should-not (get-text-property (point) 'read-only)))

(defun rg-move-to-context-line ()
  (goto-char (point-min))
  ;; Jump over "rg-started" line that could match error regexp.
  (goto-char (next-single-property-change (point-min) 'rg-command-hidden-part))
  (compilation-next-file 1)
  (forward-line -1))

(defun rg-run-wgrep-context-tests ()
  (rg-test-with-fontified-buffer
      (rg-run "amid" "elisp"
              (concat default-directory "test/data")
              nil nil '("--context 3"))
    (wgrep-change-to-wgrep-mode)
    (rg-check-wgrep-current-buffer)
    (rg-move-to-context-line)
    (rg-check-wgrep-current-line)))

(defun rg-run-wgrep-tests()
  (rg-test-with-fontified-buffer "hello"
    (wgrep-change-to-wgrep-mode)
    (rg-check-wgrep-current-buffer)))

(defun rg-check-wgrep-current-buffer ()
  (rg-check-wgrep-header)
  (rg-check-wgrep-footer)

  (goto-char (point-min))
  ;; Jump over "rg-started" line that could match error regexp.
  (goto-char (next-single-property-change (point-min) 'rg-command-hidden-part))
  (compilation-next-file 1)
  (rg-check-wgrep-current-line)

  (goto-char (point-max))
  ;; "rg finished" line sometimes match error regexp so skip that.
  (forward-line -1)
  (compilation-previous-error 1)
  (rg-check-wgrep-current-line))

(provide 'wgrep-rg.el-test)

;;; wgrep-rg.el-test.el ends here
