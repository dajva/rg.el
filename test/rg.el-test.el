;;; rg.el-test.el --- rg.el: Tests

;; Copyright (C) 2017 David Landell <david.landell@sunnyhill.email>
;;
;; Author: David Landell <david.landell@sunnyhill.email>
;; Homepage: https://github.com/davja/rg.el

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

(ert-deftest rg-unit-test/case-expand-template ()
  "Test that `rg-set-case-sensitivity' handles case settings correctly."
  (let (rg-toggle-command-line-flags)
    (let ((case-fold-search t))
      (rg-set-case-sensitivity "foo")
      (should (member  "-i" rg-toggle-command-line-flags))
      (rg-set-case-sensitivity "fOo")
      (should-not (member  "-i" rg-toggle-command-line-flags))
    (let ((case-fold-search nil))
      (rg-set-case-sensitivity "foo")
      (should-not (member  "-i" rg-toggle-command-line-flags))
      (rg-set-case-sensitivity "fOo")
      (should-not (member  "-i" rg-toggle-command-line-flags))))))

(ert-deftest rg-unit-test/build-template ()
"Test `rg-build-template' template creation."
  (let* ((rg-command "rg")
        (rg-custom-type-aliases nil)
        (notype-template (rg-build-template))
        (type-template (rg-build-template t))
        (custom-template (rg-build-template t "glob")))
    (should (s-matches? (rg-regexp-last "<R>") notype-template))
    (should-not (s-matches? (rg-regexp-anywhere-but-last "--type <F>") notype-template))
    (should-not (s-matches? (rg-regexp-anywhere-but-last "--type-add 'custom:") notype-template))

    (should (s-matches? (rg-regexp-anywhere-but-last "--type <F>") type-template))
    (should-not (s-matches? (rg-regexp-anywhere-but-last "--type-add 'custom:") type-template))

    (should (s-matches? (rg-regexp-anywhere-but-last "--type-add 'custom: *glob'") custom-template))
    (should (s-matches? (rg-regexp-anywhere-but-last "--type <F>") custom-template))))

(ert-deftest rg-unit-test/custom-command-line-flags ()
"Test that `rg-command-line-flags' is added to the template."
  (let* ((rg-command "rg")
        (rg-custom-type-aliases nil)
        (rg-command-line-flags '("--foo" "--bar"))
        (template (rg-build-template)))
    (should (s-matches? (rg-regexp-anywhere-but-last "--foo --bar") template))))

(ert-deftest rg-unit-test/toggle-command-flag ()
"Test `rg-toggle-command-flag'."
  (let ((testflag "--foo")
        flaglist)
    (setq flaglist (rg-toggle-command-flag testflag flaglist))
    (should (member testflag flaglist))
    (setq flaglist (rg-toggle-command-flag testflag flaglist))
    (should-not (member testflag flaglist))))

(ert-deftest rg-unit-test/rerun-change-regexp ()
"Test result of `rg-rerun-change-regexp'."
  (let ((rg-last-search '("regexp" "elisp" "/tmp/test")))
    (noflet ((rg-recompile (&rest _) nil)
             (rg-read-regexp (&rest _) "new-regexp"))
            (rg-rerun-change-regexp)
            (should (cl-every 'equal '("new-regexp" "elisp" "/tmp/test") rg-last-search)))))

(ert-deftest rg-unit-test/read-regexp-correct-read-func ()
"Test that `rg-read-regexp' choose the correct read function depending
on emacs version."
  (let (called prompt-result)
    (noflet ((read-string (pr default &rest _)
                          (setq called 'read-string)
                          (setq prompt-result pr))
             (read-regexp (pr &rest _)
                          (setq called 'read-regexp)
                          (setq prompt-result pr)))
            (rg-read-regexp "Search for" "foo" 'bar)
            (if (and (<= emacs-major-version 24)
                     (<= emacs-minor-version 2))
                (progn
                  (should (eq called 'read-string))
                  (should (equal prompt-result "Search for (default \"foo\"): ")))
              (progn
                (should (eq called 'read-regexp))
                (should (equal prompt-result "Search for")))))))

(ert-deftest rg-unit-test/rerun-change-files ()
"Test result of `rg-rerun-change-files'."
  (let ((rg-last-search '("regexp" "elisp" "/tmp/test")))
    (noflet ((rg-recompile (&rest _) nil)
             (completing-read (&rest _) "cpp"))
            (rg-rerun-change-files)
            (should (cl-every 'equal '("regexp" "cpp" "/tmp/test") rg-last-search)))))

(ert-deftest rg-unit-test/rerun-change-dir ()
"Test result of `rg-rerun-change-dir'."
  (let ((rg-last-search '("regexp" "elisp" "/tmp/test")))
    (noflet ((rg-recompile (&rest _) nil)
             (read-directory-name (&rest _) "/tmp/new"))
            (rg-rerun-change-dir)
            (should (cl-every 'equal '("regexp" "elisp" "/tmp/new") rg-last-search)))))

(ert-deftest rg-unit-test/custom-toggle ()
"Test `rg-define-toggle' macro."
  (let ((rg-last-search '("regexp" "elisp" "/tmp/test")))
    (noflet ((rg-recompile (&rest _) nil))
            (rg-define-toggle "--foo")
            (should (functionp 'rg-custom-toggle-flag-foo))
            (rg-define-toggle "--bar" nil t)
            (should (functionp 'rg-custom-toggle-flag-bar))
            (should (member "--bar" rg-toggle-command-line-flags))
            (rg-custom-toggle-flag-foo)
            (should (member "--foo" rg-toggle-command-line-flags))
            (should (member "--bar" rg-toggle-command-line-flags))
            (rg-custom-toggle-flag-foo)
            (should-not (member "--foo" rg-toggle-command-line-flags))
            (should (member "--bar" rg-toggle-command-line-flags))
            (rg-custom-toggle-flag-bar)
            (should-not (member "--foo" rg-toggle-command-line-flags))
            (should-not (member "--bar" rg-toggle-command-line-flags))
            (rg-custom-toggle-flag-bar)
            (should-not (member "--foo" rg-toggle-command-line-flags))
            (should (member "--bar" rg-toggle-command-line-flags)))))

(ert-deftest rg-unit-test/custom-toggle-key-binding ()
"Test `rg-define-toggle' macro key bindings."
  (let ((rg-last-search '("regexp" "elisp" "/tmp/test")))
    (noflet ((rg-recompile (&rest _) nil))
            (rg-define-toggle "--baz" "b")
            (should (functionp 'rg-custom-toggle-flag-baz))
            (should (eq 'rg-custom-toggle-flag-baz (lookup-key rg-mode-map "b"))))))

(ert-deftest rg-unit-test/custom-toggle-double-definition ()
"Test multiple clashing definitions of same flag and bindings in
`rg-define-toggle' macro."
  (let ((rg-last-search '("regexp" "elisp" "/tmp/test")))
    (noflet ((rg-recompile (&rest _) nil))
            (rg-define-toggle "--qux" nil t)
            (should (functionp 'rg-custom-toggle-flag-qux))
            (should-not (eq 'rg-custom-toggle-flag-qux (lookup-key rg-mode-map "q")))
            (should (member "--qux" rg-toggle-command-line-flags))
            (rg-define-toggle "--qux" "q")
            (should (functionp 'rg-custom-toggle-flag-qux))
            (should (eq 'rg-custom-toggle-flag-qux (lookup-key rg-mode-map "q")))
            (should-not (member "--qux" rg-toggle-command-line-flags))
            (rg-define-toggle "--qux" "z")
            (should (functionp 'rg-custom-toggle-flag-qux))
            (should (eq 'rg-custom-toggle-flag-qux (lookup-key rg-mode-map "z")))
            (rg-define-toggle "--quux" "q")
            (should (eq 'rg-custom-toggle-flag-quux (lookup-key rg-mode-map "q")))
            (should-not (eq 'rg-custom-toggle-flag-qux (lookup-key rg-mode-map "q"))))))

(ert-deftest rg-unit-test/build-command ()
"Test that`rg-build-command' convert files argument to correct type
alias."
  (let ((rg-command "rg")
        (rg-custom-type-aliases nil)
        full-command)
    (setq full-command (rg-build-command "foo" "cpp"))
    (should (s-matches? "rg +--no-heading +--type +cpp +foo" full-command))
    (setq full-command (rg-build-command "foo" "everything"))
    (should (s-matches? "rg +--no-heading +foo" full-command))
    (setq full-command (rg-build-command "foo" "bar"))
    (should (s-matches? "rg +--no-heading +--type-add +'custom:bar' +--type +custom +foo" full-command))))

(ert-deftest rg-unit-test/default-alias ()
"Test that `rg-default-alias' detects the current file and selects
matching alias."
  (find-file "test/data/foo.el")
  (should (equal (car (rg-default-alias)) "elisp"))
  (find-file "test/data/foo.baz")
  (should (equal (car (rg-default-alias)) nil))
  (let ((rg-custom-type-aliases '(("test" . "*.baz"))))
    (find-file "test/data/foo.baz")
    (should (equal (car (rg-default-alias)) "test"))))


;; Integration tests

(ert-deftest rg-integration-test/search-alias-builtin () :tags '(need-rg)
"Test that rg builtin aliases works."
  (let ((case-fold-search t))
    (rg "hello" "elisp" (concat default-directory "test/data"))
    (with-current-buffer "*rg*"
      (should (rg-wait-for-search-result))
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 3 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.el.*hello" bufstr)))
        (should (= 0 (s-count-matches "foo.baz.*hello" bufstr)))
        (should (= 0 (s-count-matches "bar.baz.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-alias-custom () :tags '(need-rg)
"Test that aliases defined in `rg-custom-type-aliases' works if explicitly selected."
  (let ((case-fold-search t)
        (rg-custom-type-aliases '(("test" . "*.baz"))))
    (rg "hello" "test" (concat default-directory "test/data"))
    (with-current-buffer "*rg*"
      (should (rg-wait-for-search-result))
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 0 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 0 (s-count-matches "bar.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "foo.baz.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.baz.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-alias-all-custom () :tags '(need-rg)
"Test that aliases defined in `rg-custom-type-aliases' works if
  implicitly selected via '--type all'."
  (let ((case-fold-search t)
        (rg-custom-type-aliases '(("test" . "*.baz"))))
    (rg "hello" "all" (concat default-directory "test/data"))
    (with-current-buffer "*rg*"
      (should (rg-wait-for-search-result))
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 3 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "foo.baz.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.baz.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-no-alias() :tags '(need-rg)
"Test that custom file pattern that is not an alias works."
  (let ((case-fold-search t))
    (rg "hello" "*.baz" (concat default-directory "test/data"))
    (with-current-buffer "*rg*"
      (should (rg-wait-for-search-result))
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 0 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 0 (s-count-matches "bar.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "foo.baz.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.baz.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-uppercase-regexp () :tags '(need-rg)
"Test that uppercase search triggers case sensitive search."
  (let ((case-fold-search t))
    (rg "Hello" "all" (concat default-directory "test/data"))
    (with-current-buffer "*rg*"
      (should (rg-wait-for-search-result))
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 1 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 1 (s-count-matches "bar.el.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-case-sensitive-regexp () :tags '(need-rg)
"Test explicit case sensitive search."
  (let ((case-fold-search nil))
    (rg "hello" "all" (concat default-directory "test/data")))
  (with-current-buffer "*rg*"
    (should (rg-wait-for-search-result))
    (let ((case-fold-search t)
          (bufstr (buffer-substring-no-properties (point-min) (point-max))))
      (should (= 1 (s-count-matches "foo.el.*hello" bufstr)))
      (should (= 1 (s-count-matches "bar.el.*hello" bufstr))))))

(ert-deftest rg-integration-test/project-root ()
"Test that all paths in `rt-project-root' gives valid results."
  ;; projectile
  (rg-check-git-project-root)
  (eval-after-load 'projectile
    (fmakunbound 'projectile-project-root))

  ;; ffip requires emacs 24.3 so not possible to test with cask right now.
  ;; (rg-check-git-project-root)
  ;; (with-eval-after-load 'find-file-in-project
  ;;   (fmakunbound 'ffip-project-root))

  ;; vc-backend
  (rg-check-git-project-root)
  ;; default
  (should (equal (expand-file-name
                  (rg-project-root "/tmp/foo.el"))
                 "/tmp/")))

(provide 'rg.el-test)

;;; rg.el-test.el ends here
