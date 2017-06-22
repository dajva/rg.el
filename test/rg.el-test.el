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
        (custom-template (rg-build-template t "glob"))
        (type-pattern (rg-regexp-anywhere-but-last "--type <F>"))
        (type-add-pattern (rg-regexp-anywhere-but-last
                           (concat
                            "--type-add "
                            (s-replace "----" "[^ ]+"
                                       (regexp-quote
                                        (shell-quote-argument "custom:----")))))))
    (should (s-matches? (rg-regexp-last "<R>") notype-template))
    (should-not (s-matches? type-pattern notype-template))
    (should-not (s-matches? type-add-pattern notype-template))
    (should (s-matches? type-pattern type-template))
    (should-not (s-matches? type-add-pattern type-template))
    (should (s-matches? type-add-pattern custom-template))
    (should (s-matches? type-pattern custom-template))))

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
    (noflet ((rg-rerun () nil)
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
    (noflet ((rg-rerun () nil)
             (completing-read (&rest _) "cpp"))
            (rg-rerun-change-files)
            (should (cl-every 'equal '("regexp" "cpp" "/tmp/test") rg-last-search)))))

(ert-deftest rg-unit-test/rerun-change-dir ()
"Test result of `rg-rerun-change-dir'."
  (let ((rg-last-search '("regexp" "elisp" "/tmp/test")))
    (noflet ((rg-rerun () nil)
             (read-directory-name (&rest _) "/tmp/new"))
            (rg-rerun-change-dir)
            (should (cl-every 'equal '("regexp" "elisp" "/tmp/new") rg-last-search)))))

(ert-deftest rg-unit-test/custom-toggle ()
"Test `rg-define-toggle' macro."
  (let ((rg-last-search '("regexp" "elisp" "/tmp/test")))
    (noflet ((rg-rerun () nil))
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
    (should (s-matches? "rg.*? +--type +cpp.*? +foo" full-command))
    (setq full-command (rg-build-command "foo" "everything"))
    (should-not (s-matches? "rg.*? +--type.*? +foo" full-command))
    (setq full-command (rg-build-command "foo" "bar"))
    (should (s-matches? (concat "rg.*? +--type-add +"
                                (regexp-quote (shell-quote-argument "custom:bar"))
                                " +--type +\"?custom.*? +foo")
                        full-command))))

(ert-deftest rg-unit-test/default-alias ()
"Test that `rg-default-alias' detects the current file and selects
matching alias."
  (find-file "test/data/foo.el")
  (should (equal (car (rg-default-alias)) "elisp"))
  (find-file "test/data/foo.baz")
  (should (equal (car (rg-default-alias)) "all"))
  (let ((rg-custom-type-aliases '(("test" . "*.baz"))))
    (find-file "test/data/foo.baz")
    (should (equal (car (rg-default-alias)) "test"))))

(ert-deftest rg-unit-test/single-font-lock-match ()
"Test that `rg-single-font-lock-match' find font-lock-face matches correctly."
  (let (pos limit)
    (with-temp-buffer
      (insert
       "noproperty"
       (propertize "match1" 'font-lock-face 'rg-file-tag-face)
       (propertize "someotherproperty" 'face 'rg-match-face)
       (propertize "othermatch" 'font-lock-face 'rg-filename-face)
       (propertize "match2" 'font-lock-face 'rg-file-tag-face))
      (setq pos (rg-single-font-lock-match 'rg-file-tag-face (point-min) (point-max) 1))
      (goto-char pos)
      (should (looking-at "match1"))
      (setq pos (rg-single-font-lock-match 'rg-file-tag-face pos (point-max) 1))
      (goto-char pos)
      (should (looking-at "match2"))
      (setq pos (rg-single-font-lock-match 'rg-file-tag-face pos (point-min) -1))
      (goto-char pos)
      (should (looking-at "match1"))
      (setq pos (rg-single-font-lock-match 'rg-filename-face pos (point-max) 1))
      (goto-char pos)
      (should (looking-at "othermatch"))
      (setq limit (+ pos 4))
      (setq pos (rg-single-font-lock-match 'rg-file-tag-face pos limit 1))
      (should (eq pos limit)))))

(ert-deftest rg-unit/next-prev-file ()
"Test that `rg-next-file' and `rg-prev-file' dispatch calls as it should."
  (let (called arg)
    (noflet ((rg-navigate-file-group (n) (setq
                                          called 'rg-navigate-file-group
                                          arg  n))
             (compilation-next-error (n) (setq
                                          called 'compilation-next-error
                                          arg n))
             (compilation-previous-error (n) (setq
                                              called 'compilation-previous-error
                                              arg n)))
            (let ((rg-group-result t))
              (rg-next-file 1)
              (should (eq called 'rg-navigate-file-group))
              (should (eq arg 1))
              (rg-prev-file 1)
              (should (eq called 'rg-navigate-file-group))
              (should (eq arg (- 1))))
            (let ((rg-group-result nil))
              (rg-next-file 1)
              (should (eq called 'compilation-next-error))
              (should (eq arg 1))
              (rg-prev-file 1)
              (should (eq called 'compilation-previous-error))
              (should (eq arg 1))))))

(ert-deftest rg-unit/match-grouped-filename ()
"Test that `rg-match-grouped-filename' finds correct match and restores state."
  (let (saved-pos)
    (with-temp-buffer
      (insert
       "some random text\n"
       "File: matched/text/file.foo\n"
       "4:1: 	Some matched text")
      (goto-char (point-min))
      (re-search-forward "Some match")
      (setq saved-pos (point))
      (should (equal "matched/text/file.foo" (car (rg-match-grouped-filename))))
      (should (equal "Some match" (match-string 0)))
      (should (eq saved-pos (point))))))

(ert-deftest rg-unit/save-vars ()
"Test `rg-save-vars' macro."
  (let ((first "value1")
        (second "value2")
        (third "value3"))
    (rg-save-vars (first)
      (setq first "new value1"))
    (should (equal first "value1"))
    (rg-save-vars (first second)
      (setq second first)
      (setq first third)
      (setq third "newvalue"))
    (should (equal first "value1"))
    (should (equal second "value2"))
    (should-not (equal third "value3"))))

(ert-deftest rg-unit/regexp-quote ()
  "Test `rg-regexp-quote' with some 'random' strings."
  (should (equal (rg-regexp-quote ")-[abs|]^$_+=(^\?)")
                 "\\)-\\[abs\\|\\]\\^\\$_\\+=\\(\\^\\?\\)"))
  (should (equal (rg-regexp-quote "(?i)a+(?-i)b+")
                 "\\(\\?i\\)a\\+\\(\\?-i\\)b\\+"))
  (should (equal (rg-regexp-quote "AaAaAbbBBBb")
                 "AaAaAbbBBBb"))
  (should (equal (rg-regexp-quote "^(.*)|{}")
                 "\\^\\(\\.\\*\\)\\|\\{\\}")))

;; Integration tests

(ert-deftest rg-integration-test/search-alias-builtin () :tags '(need-rg)
"Test that rg builtin aliases works."
  (let ((case-fold-search t))
    (rg "hello" "elisp" (concat default-directory "test/data"))
    (rg-with-current-result
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
    (rg-with-current-result
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
    (rg-with-current-result
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 3 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "foo.baz.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.baz.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-no-alias() :tags '(need-rg)
"Test that custom file pattern that is not an alias works."
  (let ((case-fold-search t))
    (rg "hello" "*.baz" (concat default-directory "test/data"))
    (rg-with-current-result
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 0 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 0 (s-count-matches "bar.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "foo.baz.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.baz.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-uppercase-regexp () :tags '(need-rg)
"Test that uppercase search triggers case sensitive search."
  (let ((case-fold-search t))
    (rg "Hello" "all" (concat default-directory "test/data"))
    (rg-with-current-result
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 1 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 1 (s-count-matches "bar.el.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-case-sensitive-regexp () :tags '(need-rg)
"Test explicit case sensitive search."
  (let ((case-fold-search nil))
    (rg "hello" "all" (concat default-directory "test/data")))
  (rg-with-current-result
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

(ert-deftest rg-integration/navigate-file-group-in-grouped-result () :tags '(need-rg)
"Test group navigation in grouped result."
  (let ((rg-group-result t)
        (files '("foo.el" "bar.el"))
        first-file
        second-file
        pos)
    (rg "hello" "elisp" (concat default-directory "test/data"))
    (rg-with-current-result
      (goto-char (point-min))
      (rg-navigate-file-group 1)
      ;; The order of results is non deterministic
      ;; First match any of the files in `files'.
      (should (looking-at
               (concat "File: \\(" (mapconcat 'identity files "\\|") "\\)")))
      (setq first-file (match-string 1))
      ;; Filter out the already matched file.
      (setq second-file
            (car (seq-filter
                  (lambda (elm) (not (equal first-file elm))) files)))
      (rg-navigate-file-group 1)
      (should (looking-at (concat "File: " second-file)))
      (compilation-next-error 1)
      (setq pos (point))
      (rg-navigate-file-group -3)
      (should (eq pos (point)))
      (rg-navigate-file-group -2)
      (should (looking-at (concat "File: " first-file))))))

(ert-deftest rg-integration/navigate-file-group-in-ungrouped-result () :tags '(need-rg)
"Test group navigation in ungrouped result."
  (let ((rg-group-result nil))
    (rg "hello" "elisp" (concat default-directory "test/data"))
    (rg-with-current-result
      (goto-char (point-min))
      (rg-navigate-file-group 1)
      (should (eq (point) (point-min))))))

(defun rg-test-highlight-match (grouped)
"Helper for highlight testing.  If GROUPED is is non nil grouped
result are used."
  (let ((rg-group-result grouped)
        pos)
    (rg "hello" "elisp" (concat default-directory "test/data"))
    (rg-with-current-result
      (setq pos (rg-single-font-lock-match 'rg-match-face (point-min) (point-max) 1))
      (should-not (eq (point-max) pos)))))

(ert-deftest rg-integration/highlight-match-group () :tags '(need-rg)
"Test that highlighting of matches works."
  (rg-test-highlight-match t)
  (rg-test-highlight-match nil))

(defun rg-file-tag-face-exist-in-result (grouped)
"Make a search and return non nil if 'rg-file-tag-face exist in buffer, i.e. 'File:'."
  (let((rg-group-result grouped)
       pos)
    (rg "hello" "elisp" (concat default-directory "test/data"))
    (rg-with-current-result
      (setq pos (rg-single-font-lock-match 'rg-file-tag-face (point-min) (point-max) 1))
      (not (eq (point-max) pos)))))

(ert-deftest rg-integration/group-result-variable () :tags '(need-rg)
"Test that grouped result is triggered if `rg-group-result' is non nil
and ungrouped otherwise."
  (should-not (rg-file-tag-face-exist-in-result nil))
  (should (rg-file-tag-face-exist-in-result t)))

(ert-deftest rg-integration/recompile () :tags '(need-rg)
"Make sure that `rg-recompile' preserves search parameters."
  (let ((parent-dir (concat (expand-file-name default-directory) "test/")))
    (rg "hello" "elisp" (concat parent-dir "data"))
    (with-current-buffer "*rg*"
      (should (rg-wait-for-search-result))
      (noflet ((rg-read-regexp (&rest _) "Hello"))
              (rg-rerun-with-changes (:files files :dir dir :regexp regexp :flags flags)
                (setq files "all")
                (setq regexp "Hello")
                (setq dir parent-dir)
                (setq flags '("--text")))
              (should (rg-wait-for-search-result))
              (should (cl-every 'equal `("Hello" "all" ,parent-dir) rg-last-search))
              (should (cl-every 'equal '("--text") rg-toggle-command-line-flags))
              (rg-recompile)
              (should (rg-wait-for-search-result))
              (should (cl-every 'equal `("Hello" "all" ,parent-dir) rg-last-search))
              (should (cl-every 'equal '("--text") rg-toggle-command-line-flags))))))


(ert-deftest rg-integration/list-searches ()
  "Test `rg-list-searches'."
  :tags '(need-rg)
  (rg-run "hello" "all" (concat default-directory "test/data"))
  (rg-with-current-result
   (rg-list-searches)
   (with-current-buffer rg-search-list-buffer-name
     (ibuffer-forward-line)
     (search-forward "hello"))
   (kill-buffer rg-search-list-buffer-name)))

(provide 'rg.el-test)

;;; rg.el-test.el ends here
