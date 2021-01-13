;;; rg.el-test.el --- rg.el: Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2017 David Landell <david.landell@sunnyhill.email>
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

(ert-deftest rg-unit-test/case-expand-template ()
  "Test that `rg-apply-case-flag' handles case settings correctly."
  (let (rg-initial-toggle-flags)
    (let ((case-fold-search t))
      (rg-apply-case-flag "foo")
      (should (member  "-i" rg-initial-toggle-flags))
      (rg-apply-case-flag "fOo")
      (should-not (member "-i" rg-initial-toggle-flags)))
    (let ((case-fold-search nil))
      (rg-apply-case-flag "foo")
      (should-not (member "-i" rg-initial-toggle-flags))
      (rg-apply-case-flag "fOo")
      (should-not (member "-i" rg-initial-toggle-flags)))
    (let ((rg-ignore-case 'smart))
      (rg-apply-case-flag "foo")
      (should (member "-i" rg-initial-toggle-flags))
      (rg-apply-case-flag "fOo")
      (should-not (member "-i" rg-initial-toggle-flags)))
    (let ((rg-ignore-case 'force))
      (rg-apply-case-flag "foo")
      (should (member "-i" rg-initial-toggle-flags))
      (rg-apply-case-flag "fOo")
      (should (member "-i" rg-initial-toggle-flags)))
    (let ((rg-ignore-case nil))
      (rg-apply-case-flag "foo")
      (should-not (member "-i" rg-initial-toggle-flags))
      (rg-apply-case-flag "fOo")
      (should-not (member "-i" rg-initial-toggle-flags)))))

(ert-deftest rg-unit-test/custom-command-line-flags ()
  "Test that `rg-command-line-flags' is added to the template."
  (let ((rg-command "rg")
        (rg-custom-type-aliases nil)
        (rg-command-line-flags '("--foo" "--bar")))
    (should (s-matches? (rg-regexp-anywhere-but-last "--foo --bar")
                        (rg-build-command "" "" nil nil)))))

(ert-deftest rg-unit-test/custom-command-line-function ()
  "Test that when `rg-command-line-flags' is a function, the returned
  list of flags are added to the template."
  (let ((rg-command "rg")
        (rg-custom-type-aliases nil)
        (rg-command-line-flags (lambda () '("--foo" "--bar"))))
    (should (s-matches? (rg-regexp-anywhere-but-last "--foo --bar")
                        (rg-build-command "" "" nil nil)))))

(ert-deftest rg-unit-test/rg-buffer-name-string ()
  "Test that function `rg-buffer-name' will return correct buffer
name if variable `rg-buffer-name' is string."
  (let ((rg-buffer-name "rg results"))
    (should (string= "*rg results*" (rg-buffer-name)))))

(ert-deftest rg-unit-test/rg-buffer-name-function ()
  "Test that function `rg-buffer-name' will return correct buffer
name if variable `rg-buffer-name' is function."
  (let ((rg-buffer-name (lambda () "" "rg results")))
    (should (string= "*rg results*" (rg-buffer-name)))))

(ert-deftest rg-unit-test/rg-buffer-name-dirlocals ()
  "Test that `rg-buffer-name' is set from dir-locals.el."
  ;; This test covers two cases:
  ;; - that dir-locals from search dir is applied at all
  ;; - that dir-locals from search dir is applied if search is
  ;;   started from file buffers
  :tags '(need-rg)
  (let ((safe-local-variable-values '((rg-buffer-name . "from dir locals"))))
    (find-file (concat default-directory "test/data/foo.baz"))
    (rg-run "foo" "*.baz" (concat default-directory "dirlocals"))
    (should (get-buffer "*from dir locals*"))))

(ert-deftest rg-unit-test/save-search-as-name ()
  "Verify exit messages."
  :tags '(need-rg)
  (let ((rg-buffer-name "rg results"))
    (rg-run "foo" "*.baz" (concat default-directory "test/data"))
    (with-current-buffer (rg-buffer-name)
      (rg-save-search-as-name "bar")
      (should (string= "*rg results bar*" (buffer-name))))))

(ert-deftest rg-unit-test/toggle-command-flag ()
  "Test `rg-list-toggle'."
  (let ((testflag "--foo")
        flaglist)
    (setq flaglist (rg-list-toggle testflag flaglist))
    (should (member testflag flaglist))
    (setq flaglist (rg-list-toggle testflag flaglist))
    (should-not (member testflag flaglist))))

(ert-deftest rg-unit-test/rerun-change-regexp-literal ()
  "Test result of `rg-rerun-change-regexp' and `rg-rerun-change-literal'."
  (let ((rg-cur-search (rg-search-create :pattern "pattern" :files "elisp" :dir "/tmp/test")))
    (cl-letf (((symbol-function #'rg-rerun) #'ignore)
              ((symbol-function #'rg-read-pattern) (lambda (&rest _) "new-pattern")))
      (rg-rerun-change-regexp)
      (should (equal
               (rg-search-create :pattern "new-pattern"
                                 :files "elisp"
                                 :dir "/tmp/test"
                                 :literal nil)
               rg-cur-search))
      (rg-rerun-change-literal)
      (should (equal
               (rg-search-create :pattern "new-pattern"
                                 :files "elisp"
                                 :dir "/tmp/test"
                                 :literal t)
               rg-cur-search))
      (rg-rerun-change-regexp)
      (should (eq (rg-search-literal rg-cur-search) nil)))))

(ert-deftest rg-unit-test/read-pattern-correct-read-func ()
  "Test that `rg-read-pattern' choose the correct read function depending
on emacs version."
  (let (called prompt-result)
    (cl-letf (((symbol-function #'read-string) (lambda (pr default &rest _)
                                                 (setq called 'read-string)
                                                 (setq prompt-result pr)))
              ((symbol-function #'read-regexp) (lambda (pr &rest _)
                                                 (setq called 'read-regexp)
                                                 (setq prompt-result pr))))
      (rg-read-pattern nil "foo")
      (should (eq called 'read-regexp))
      (should (equal prompt-result "Regexp search for")))))

(ert-deftest rg-unit-test/rerun-change-files ()
  "Test result of `rg-rerun-change-files'."
  (let ((rg-cur-search (rg-search-new "regexp" "elisp" "/tmp/test")))
    (cl-letf (((symbol-function #'rg-rerun) #'ignore)
              ((symbol-function #'completing-read) (lambda (&rest _) "cpp")))
      (rg-rerun-change-files)
      (should (equal (rg-search-new "regexp" "cpp" "/tmp/test") rg-cur-search)))))

(ert-deftest rg-unit-test/rerun-change-dir ()
  "Test result of `rg-rerun-change-dir'."
  (let ((rg-cur-search (rg-search-new "regexp" "elisp" "/tmp/test")))
    (cl-letf (((symbol-function #'rg-rerun) #'ignore)
              ((symbol-function #'read-directory-name) (lambda (&rest _) "/tmp/new")))
      (rg-rerun-change-dir)
      (should (equal (rg-search-new "regexp" "elisp" "/tmp/new") rg-cur-search)))))

(ert-deftest rg-unit-test/custom-toggle ()
  "Test `rg-define-toggle' macro."
  (let ((rg-cur-search (rg-search-create :pattern "regexp" :files "elisp" :dir "/tmp/test")))
    (cl-letf (((symbol-function #'rg-rerun) #'ignore))
      (rg-define-toggle "--foo")
      (should (functionp 'rg-custom-toggle-flag-foo))
      (should-not (member "--foo" rg-initial-toggle-flags))
      (rg-define-toggle "--bar" nil t)
      (should (functionp 'rg-custom-toggle-flag-bar))
      (should (member "--bar" rg-initial-toggle-flags))

      (simulate-rg-run (rg-search-pattern rg-cur-search)
                       (rg-search-files rg-cur-search)
                       (rg-search-dir rg-cur-search))

      (rg-custom-toggle-flag-foo)
      (should (member "--foo" (rg-search-flags rg-cur-search)))
      (should (member "--bar" (rg-search-flags rg-cur-search)))

      (rg-custom-toggle-flag-foo)
      (should-not (member "--foo" (rg-search-flags rg-cur-search)))
      (should (member "--bar" (rg-search-flags rg-cur-search)))
      (rg-custom-toggle-flag-bar)
      (should-not (member "--foo" (rg-search-flags rg-cur-search)))
      (should-not (member "--bar" (rg-search-flags rg-cur-search)))

      (rg-custom-toggle-flag-bar)
      (should-not (member "--foo" rg-initial-toggle-flags))
      (should (member "--bar" rg-initial-toggle-flags)))))

(ert-deftest rg-unit-test/custom-toggle-key-binding ()
  "Test `rg-define-toggle' macro key bindings."
  (let ((rg-cur-search (rg-search-create :pattern "regexp" :files "elisp" :dir "/tmp/test")))
    (cl-letf (((symbol-function #'recompile) #'ignore))
      (rg-define-toggle "--baz" "b")
      (should (functionp 'rg-custom-toggle-flag-baz))
      (should (eq 'rg-custom-toggle-flag-baz (lookup-key rg-mode-map "b"))))))

(ert-deftest rg-unit-test/custom-toggle-double-definition ()
  "Test multiple clashing definitions of same flag and bindings in
`rg-define-toggle' macro."
  (let ((rg-cur-search (rg-search-create :pattern "regexp" :files "elisp" :dir "/tmp/test")))
    (cl-letf (((symbol-function #'recompile) #'ignore))
      (rg-define-toggle "--qux" nil t)
      (should (functionp 'rg-custom-toggle-flag-qux))
      (should-not (eq 'rg-custom-toggle-flag-qux (lookup-key rg-mode-map "q")))
      (should (member "--qux" rg-initial-toggle-flags))
      (rg-define-toggle "--qux" "q")
      (should (functionp 'rg-custom-toggle-flag-qux))
      (should (eq 'rg-custom-toggle-flag-qux (lookup-key rg-mode-map "q")))
      (should-not (member "--qux" rg-initial-toggle-flags))
      (rg-define-toggle "--qux" "z")
      (should (functionp 'rg-custom-toggle-flag-qux))
      (should (eq 'rg-custom-toggle-flag-qux (lookup-key rg-mode-map "z")))
      (rg-define-toggle "--quux" "q")
      (should (eq 'rg-custom-toggle-flag-quux (lookup-key rg-mode-map "q")))
      (should-not (eq 'rg-custom-toggle-flag-qux (lookup-key rg-mode-map "q"))))))

(ert-deftest rg-unit-test/build-command-files ()
  "Test that`rg-build-command' convert files argument to correct type
alias."
  (let ((rg-command "rg")
        (rg-custom-type-aliases nil)
        full-command)
    (setq full-command (rg-build-command "foo" "cpp" nil nil))
    (should (s-matches? "rg.*? +--type=cpp.*? +foo" full-command))
    (setq full-command (rg-build-command "foo" "everything" nil nil))
    (should-not (s-matches? "rg.*? +--type=.*? +foo" full-command))
    (setq full-command (rg-build-command "foo" "bar" nil nil))
    (should (s-matches? (concat "rg.*? +--type-add="
                                (regexp-quote (shell-quote-argument "custom:bar"))
                                " +--type=\"?custom.*? +foo")
                        full-command))))

(ert-deftest rg-unit-test/build-command-type ()
  "Test `rg-build-template' template creation."
  (let* ((rg-command "rg")
         (rg-custom-type-aliases nil)
         (notype-command (rg-build-command "query" "everything" nil nil))
         (builtin-type-command (rg-build-command "query" "elisp" nil nil))
         (custom-type-command (rg-build-command "query" "glob" nil nil))
         (type-pattern (rg-regexp-anywhere-but-last "--type=[^ ]+"))
         (type-add-pattern (rg-regexp-anywhere-but-last
                            (concat
                             "--type-add="
                             (s-replace "----" "[^ ]+"
                                        (regexp-quote
                                         (shell-quote-argument "custom:----")))))))
    (should (s-matches? (rg-regexp-anywhere "-e query") notype-command))
    (should-not (s-matches? type-pattern notype-command))
    (should-not (s-matches? type-add-pattern notype-command))
    (should (s-matches? type-pattern builtin-type-command))
    (should-not (s-matches? type-add-pattern builtin-type-command))
    (should (s-matches? type-add-pattern custom-type-command))
    (should (s-matches? (rg-regexp-anywhere-but-last "--type=custom") custom-type-command))))

(ert-deftest rg-unit-test/default-alias ()
  "Test that `rg-default-alias' detects the current file and selects
matching alias."
  (find-file "test/data/foo.el")
  (should (equal (car (rg-default-alias)) "elisp"))
  (find-file "test/data/foo.baz")
  (should (equal (car (rg-default-alias)) rg-default-alias-fallback))
  (let ((rg-custom-type-aliases '(("test" . "*.baz"))))
    (find-file "test/data/foo.baz")
    (should (equal (car (rg-default-alias)) "test"))))

(ert-deftest rg-unit-test/navigate-file-message ()
  "Test that `rg-navigate-file-message' find font-lock-face matches correctly."
  (let (pos limit)
    (with-temp-buffer
      (insert
       "noproperty"
       (propertize "match1" 'rg-file-message t)
       (propertize "someotherproperty" 'rg-file-message nil)
       "noproperty2"
       (propertize "othermatch" 'rg-file-message t))
      (setq pos (rg-navigate-file-message (point-min) (point-max) 1))
      (goto-char pos)
      (should (looking-at "match1"))
      (setq pos (rg-navigate-file-message pos (point-max) 1))
      (goto-char pos)
      (should (looking-at "othermatch"))
      (setq pos (rg-navigate-file-message pos (point-min) -1))
      (goto-char pos)
      (should (looking-at "match1"))
      (setq pos (rg-navigate-file-message pos (point-max) 1))
      (goto-char pos)
      (should (looking-at "othermatch"))
      (setq limit (+ pos 4))
      (setq pos (rg-navigate-file-message pos limit 1))
      (should (eq pos limit)))))

(ert-deftest rg-unit/next-prev-file ()
  "Test that `rg-next-file' and `rg-prev-file' dispatch calls as it should."
  (let (called arg)
    (cl-letf (((symbol-function #'rg-navigate-file-group)
               (lambda (n)
                 (setq called 'rg-navigate-file-group
                       arg n)))
              ((symbol-function #'compilation-next-file)
               (lambda (n)
                 (setq called 'compilation-next-file
                       arg n)))
              ((symbol-function #'compilation-previous-file)
               (lambda (n)
                 (setq called 'compilation-previous-file
                       arg n))))
      (let ((rg-group-result t))
        (rg-next-file 1)
        (should (eq called 'rg-navigate-file-group))
        (should (eq arg 1))
        (rg-prev-file 1)
        (should (eq called 'rg-navigate-file-group))
        (should (eq arg (- 1))))
      (let ((rg-group-result nil))
        (rg-next-file 1)
        (should (eq called 'compilation-next-file))
        (should (eq arg 1))
        (rg-prev-file 1)
        (should (eq called 'compilation-previous-file))
        (should (eq arg 1))))))

(ert-deftest rg-unit/match-grouped-filename ()
  "Test that `rg-match-grouped-filename' finds correct match and restores state."
  (let (saved-pos)
    (with-temp-buffer
      (insert
       "some random text\n"
       "File: matched/text/file.foo\n"
       "4:1:   Some matched text")
      (goto-char (point-min))
      (re-search-forward "Some match")
      (setq saved-pos (point))
      (should (equal "matched/text/file.foo" (car (rg-match-grouped-filename))))
      (should (equal "Some match" (match-string 0)))
      (should (eq saved-pos (point))))))

(ert-deftest rg-unit-test/global-keymap ()
  "Test global keymap."
  (let ((rg-use-transient-menu nil))
    ;; Default prefix
    (rg-with-temp-global-keymap
     (rg-enable-default-bindings)
     (should (eq rg-global-map (lookup-key (current-global-map) "\C-cs"))))
    ;; Function supplied prefix
    (rg-with-temp-global-keymap
     (rg-enable-default-bindings "\M-s")
     (should (eq rg-global-map (lookup-key (current-global-map) "\M-s"))))
    ;; Customized prefix
    (let ((rg-keymap-prefix "\M-s"))
      ;; Default prefix
      (rg-with-temp-global-keymap
       (rg-enable-default-bindings)
       (should (eq rg-global-map (lookup-key (current-global-map) "\M-s"))))
      ;; Function supplied prefix
      (rg-with-temp-global-keymap
       (rg-enable-default-bindings "\C-ct")
       (should (eq rg-global-map (lookup-key (current-global-map) "\C-ct")))))
    ;; Make sure rg-global-map comes into play
    (rg-with-temp-global-keymap
     (rg-enable-default-bindings)
     (should (eq 'rg (lookup-key (current-global-map) "\C-csr")))
     ;; Modify global map
     (define-key rg-global-map "0" 'foo)
     (should (eq 'foo (lookup-key (current-global-map) "\C-cs0"))))))

(ert-deftest rg-unit/literal-search ()
  "Test `rg-literal'."
  (cl-letf* ((called-literal nil)
             ((symbol-function #'rg-run)
              (lambda (_pattern _files _dir &optional literal &rest _)
                (setf called-literal literal))))
    (rg-literal "foo" "elisp" "/tmp/test")
    (should-not (eq called-literal nil))))

(ert-deftest rg-unit/regexp-search ()
  "Test `rg'."
  (cl-letf* ((called-literal nil)
             ((symbol-function #'rg-run)
              (lambda (_pattern _files _dir &optional literal &rest _)
                (setq called-literal literal))))
    (rg "foo" "elisp" "/tmp/test")
    (should (eq called-literal nil))))

(ert-deftest rg-unit/set-search-defaults ()
  "Test rg-define-search helper defun rg-set-search-defaults."
  (let  ((defaults (rg-set-search-defaults nil)))
    (should (eq (plist-get defaults :confirm) 'never))
    (should (eq (plist-get defaults :format) 'regexp))
    (should (eq (plist-get defaults :query) 'ask))
    (should (eq (plist-get defaults :files) 'ask))
    (should (eq (plist-get defaults :dir) 'ask))))

(ert-deftest rg-unit/search-parse-local-bindings-confirm ()
  "Test rg-define-search helper defun rg-search-parse-local-bindings.
Test `:confirm' directive."
  (let  ((prefix (rg-search-parse-local-bindings '(:confirm prefix)))
         (never (rg-search-parse-local-bindings '(:confirm never)))
         (always (rg-search-parse-local-bindings '(:confirm always))))
    (should (null (cadr (assq 'confirm never))))
    (should (cadr (assq 'confirm always)))
    (should (eq (cadr (cadr (assq 'confirm prefix))) 'current-prefix-arg))))

(ert-deftest rg-unit/search-parse-local-bindings-query ()
  "Test rg-define-search helper defun rg-search-parse-local-bindings.
Test `:query' directive."
  (let  ((ask (rg-search-parse-local-bindings '(:query ask)))
         (form (rg-search-parse-local-bindings '(:query (form))))
         (point (rg-search-parse-local-bindings '(:query point))))
    (should (null (assq 'query ask)))
    (should (equal (cadr (assq 'query form)) '(form)))
    (should (equal (cadr (assq 'query point)) '(grep-tag-default)))))

(ert-deftest rg-unit/search-parse-local-bindings-dir ()
  "Test rg-define-search helper defun rg-search-parse-local-bindings.
Test `:dir' directive."
  (let  ((ask (rg-search-parse-local-bindings '(:dir ask)))
         (project (rg-search-parse-local-bindings '(:dir project)))
         (current (rg-search-parse-local-bindings '(:dir current)))
         (form (rg-search-parse-local-bindings '(:dir (form)))))
    (should (null (assq 'dir ask)))
    (should (equal (cadr (assq 'dir project)) '(rg-project-root buffer-file-name)))
    (should (equal (cadr (assq 'dir current)) 'default-directory))
    (should (equal (cadr (assq 'dir form)) '(form)))))

(ert-deftest rg-unit/search-parse-local-bindings-files ()
  "Test rg-define-search helper defun rg-search-parse-local-bindings.
Test `:files' directive."
  (let  ((ask (rg-search-parse-local-bindings '(:files ask)))
         (current (rg-search-parse-local-bindings '(:files current)))
         (form (rg-search-parse-local-bindings '(:files (form)))))
    (should (null (assq 'files ask)))
    (should (equal (cadr (assq 'files current)) '(car (rg-default-alias))))
    (should (equal (cadr (assq 'files form)) '(form)))))

(ert-deftest rg-unit/search-parse-local-bindings-flags ()
  "Test rg-define-search helper defun rg-search-parse-global-bindings.
Test `:flags' directive."
  (let  ((ask (rg-search-parse-local-bindings '(:flags ask)))
         (form (rg-search-parse-local-bindings '(:flags '("--flag")))))
    (should (cadr (assq 'flags ask)))
    (should (equal (cadr (assq 'flags form))
                   (quote (funcall rg-command-line-flags-function '("--flag")))))))

(ert-deftest rg-unit/search-parse-interactive-args ()
  "Test rg-define-search helper defun rg-search-parse-interactive-args."
  (let  ((empty (rg-search-parse-interactive-args nil))
         (query (rg-search-parse-interactive-args '(:query ask)))
         (files (rg-search-parse-interactive-args '(:files ask)))
         (dir (rg-search-parse-interactive-args '(:dir ask)))
         (flags (rg-search-parse-interactive-args '(:flags ask))))
    (should (null empty))
    (should (equal (cadr (assq 'query query)) 'rg-read-pattern))
    (should (equal (cadr (assq 'files files)) 'rg-read-files ))
    (should (equal (cadr (assq 'dir dir)) 'read-directory-name))
    (should (equal (caar (cddr (assq 'flags flags))) 'read-string))))

(ert-deftest rg-unit/builtin-aliases-empty-strings ()
  "Test that empty strings in builtin aliases are filtered out."
  (cl-letf (((symbol-function #'rg-invoke-rg-type-list)
             (lambda () "\n\n foo: *.foo,*.fo\n\n bar: *.bar,*.ba\n")))
    (equal (rg-list-builtin-type-aliases)
           '(("foo" . "*.foo *.fo") ("bar" . "*.bar *.ba")))))

(ert-deftest rg-unit/prepend-space ()
  "Test rg-prepend-space."
  (should (equal (rg-prepend-space "foo" (length "foo")) "foo"))
  (should (equal (rg-prepend-space "foo" (1+ (length "foo"))) " foo"))
  (should (equal (rg-prepend-space "foo" (1- (length "foo"))) "foo")))


;; Integration tests

(ert-deftest rg-integration-test/search-alias-builtin ()
  "Test that rg builtin aliases works."
  :tags '(need-rg)
  (let ((rg-group-result nil)
        (rg-ignore-case 'force))
    (rg-run "hello" "elisp" (concat default-directory "test/data"))
    (rg-with-current-result
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 3 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.el.*hello" bufstr)))
        (should (= 0 (s-count-matches "foo.baz.*hello" bufstr)))
        (should (= 0 (s-count-matches "bar.baz.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-alias-custom ()
  "Test that aliases defined in `rg-custom-type-aliases' work if explicitly selected."
  :tags '(need-rg)
  (let ((rg-group-result nil)
        (rg-ignore-case 'force)
        (rg-custom-type-aliases '(("test" . "*.baz"))))
    (rg-run "hello" "test" (concat default-directory "test/data"))
    (rg-with-current-result
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 0 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 0 (s-count-matches "bar.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "foo.baz.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.baz.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-alias-all-custom ()
  "Test that aliases defined in `rg-custom-type-ailiases' work if
  implicitly selected via '--type=all'."
  :tags '(need-rg)
  (let ((rg-group-result nil)
        (rg-ignore-case 'force)
        (rg-custom-type-aliases '(("test" . "*.baz"))))
    (rg-run "hello" "all" (concat default-directory "test/data"))
    (rg-with-current-result
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 3 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "foo.baz.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.baz.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-alias-custom-lambda ()
  "Test that aliases defined via lambdas in `rg-custom-type-aliases' work."
  :tags '(need-rg)
  (let ((rg-group-result nil)
        (rg-ignore-case 'force)
        (rg-custom-type-aliases '((lambda () '("test" . "*.baz")))))
    (rg-run "hello" "test" (concat default-directory "test/data"))
    (rg-with-current-result
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 0 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 0 (s-count-matches "bar.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "foo.baz.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.baz.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-no-alias()
  "Test that custom file pattern that is not an alias works."
  :tags '(need-rg)
  (let ((rg-group-result nil)
        (rg-ignore-case 'force))
    (rg-run "hello" "*.baz" (concat default-directory "test/data"))
    (rg-with-current-result
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 0 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 0 (s-count-matches "bar.el.*hello" bufstr)))
        (should (= 3 (s-count-matches "foo.baz.*hello" bufstr)))
        (should (= 3 (s-count-matches "bar.baz.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-history ()
  "Test that `rg-history' gets updated."
  :tags '(need-rg)
  (let ((rg-history nil))
    (rg-run "hello" "all" "/tmp/test")
    (rg-with-current-result
      (should (member (car compilation-arguments) rg-history)))))

(ert-deftest rg-integration-test/search-uppercase-regexp ()
  "Test that uppercase search triggers case sensitive search."
  :tags '(need-rg)
  (let ((rg-group-result nil)
        (rg-ignore-case 'smart))
    (rg-run "Hello" "all" (concat default-directory "test/data"))
    (rg-with-current-result
      (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 1 (s-count-matches "foo.el.*hello" bufstr)))
        (should (= 1 (s-count-matches "bar.el.*hello" bufstr)))))))

(ert-deftest rg-integration-test/search-case-sensitive-regexp ()
  "Test explicit case sensitive search."
  :tags '(need-rg)
  (let ((rg-group-result nil)
        (rg-ignore-case 'nil))
    (rg-run "hello" "all" (concat default-directory "test/data")))
  (rg-with-current-result
    (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
      (should (= 1 (s-count-matches "foo.el.*hello" bufstr)))
      (should (= 1 (s-count-matches "bar.el.*hello" bufstr))))))

(ert-deftest rg-integration-test/project-root ()
  "Test that all paths in `rt-project-root' gives valid results.
This test abuse the internal priority of `rg-project-root', by first
checking the root and then successively disable the internally used
method. "
  (rg-check-git-project-root)
  (with-eval-after-load 'projectile
    (fmakunbound 'projectile-project-root))

  ;; ffip
  (rg-check-git-project-root)
  (with-eval-after-load 'find-file-in-project
    (fmakunbound 'ffip-project-root))

  ;; project.el
  (rg-check-git-project-root)
  (with-eval-after-load 'project
    (fmakunbound 'project-current))

  ;; vc-backend
  (rg-check-git-project-root)
  ;; default
  (should (equal (expand-file-name
                  (rg-project-root "/tmp/foo.el"))
                 "/tmp/")))

(ert-deftest rg-integration/command-hiding-hide ()
  "Test command hiding when `rg-hide-command` is non nil."
  :tags '(need-rg)
  (rg-test-with-command-start "hello"
     (should (get-text-property (point) 'display))
     (rg-toggle-command-hiding)
     (should-not (get-text-property (point) 'display))))

(ert-deftest rg-integration/command-hiding-nohide ()
  "Test command hiding when `rg-hide-command` is nil."
  :tags '(need-rg)
  (let ((rg-hide-command nil))
    (rg-test-with-command-start "hello"
        (should-not (get-text-property (point) 'display))
        (rg-toggle-command-hiding)
        (should (get-text-property (point) 'display)))))

(ert-deftest rg-integration/nogroup-show-columns ()
  "Test that column numbers are shown in no group mode if enabled."
  :tags '(need-rg)
  (let ((rg-group-result nil)
        (rg-show-columns t))
    (rg-test-with-first-error "hello"
     (should (looking-at ".*:[0-9]:[0-9]")))))

(ert-deftest rg-integration/nogroup-hide-columns ()
  "Test that column numbers are hidden in no group mode if disabled."
  :tags '(need-rg)
  (let ((rg-group-result nil)
        (rg-show-columns nil))
    (rg-test-with-first-error "hello"
     (should (looking-at ".*:[0-9]:[^0-9]")))))

(ert-deftest rg-integration/positions-line-only ()
  "Test line position format without alignment."
  :tags '(need-rg)
  (let ((rg-align-position-numbers nil))
    (rg-test-with-first-error "hello"
     (should (looking-at "[0-9]:")))))

(ert-deftest rg-integration/positions-line-column ()
  "Test line and column position format without alignment."
  :tags '(need-rg)
  (let ((rg-align-position-numbers nil)
        (rg-show-columns t))
    (rg-test-with-first-error "hello"
     (should (looking-at "[0-9]+:[0-9]+:")))))

(ert-deftest rg-integration/positions-align-line ()
  "Test line position format with alignment."
  :tags '(need-rg)
  (let ((rg-align-position-numbers t))
    (rg-test-with-first-error "hello"
     (should (looking-at (format
                          " \\{0,3\\}[0-9]\\{1,4\\}%s"
                          rg-align-position-content-separator)))
     (should (equal (length (match-string 0))
                    (1+ rg-align-line-number-field-length))))
    (let ((rg-align-position-content-separator "#"))
      (rg-test-with-first-error "hello"
       (should (looking-at (format " \\{0,4\\}[0-9]\\{1,5\\}%s"
                                   rg-align-position-content-separator)))))))

(ert-deftest rg-integration/positions-align-line-column ()
  "Test line and column position format with alignment."
  :tags '(need-rg)
  (let ((rg-show-columns t)
        (rg-align-position-numbers t))
    (rg-test-with-first-error "hello"
     (should (looking-at (format " \\{0,3\\}[0-9]\\{1,4\\}%s \\{0,2\\}[0-9]\\{1,3\\}%s"
                                 rg-align-line-column-separator
                                 rg-align-position-content-separator)))
     (should (equal (length (match-string 0))
                    (+ rg-align-line-number-field-length
                       rg-align-column-number-field-length
                       2))))
    (let ((rg-align-position-content-separator ";")
          (rg-align-line-column-separator "&"))
      (rg-test-with-first-error "hello"
       (should (looking-at (format " \\{0,3\\}[0-9]\\{1,4\\}%s \\{0,2\\}[0-9]\\{1,3\\}%s"
                                   rg-align-line-column-separator
                                   rg-align-position-content-separator)))
       (should (equal (length (match-string 0))
                      (+ rg-align-line-number-field-length
                         rg-align-column-number-field-length
                         2)))))))

(ert-deftest rg-integration/positions-align-context-line ()
  "Test line position format with alignment."
  :tags '(need-rg)
  (let ((rg-align-position-numbers t)
        (ctx-line-rx " \\{0,4\\}[1-9]-")
        (match-line-rx (format " \\{0,3\\}[0-9]\\{1,4\\}%s"
                               rg-align-position-content-separator))
        ctx-match line-match)
    (rg-test-with-first-error
      (rg-run "amid" "elisp"
              (concat default-directory "test/data")
              nil nil '("--context=3"))
      (forward-line 1)
      (should (looking-at ctx-line-rx))
      (setq ctx-match (match-string 0))
      (forward-line -1)
      (should (looking-at match-line-rx))
      (setq line-match (match-string 0))
      (should (equal (length ctx-match) (length line-match)))
      (forward-line -1)
      (should (looking-at ctx-line-rx))
      (setq ctx-match (match-string 0))
      (should (equal (length ctx-match) (length line-match))))))

(ert-deftest rg-integration/positions-align-context-line-column ()
  "Test line position format with alignment."
  :tags '(need-rg)
  (let ((rg-show-columns t)
        (rg-align-position-numbers t)
        (contex-line-rx " \\{0,9\\}[1-9]-")
        (match-line-rx (format " \\{0,3\\}[0-9]\\{1,4\\}%s \\{0,2\\}[0-9]\\{1,3\\}%s"
                               rg-align-line-column-separator
                               rg-align-position-content-separator))
        ctx-match line-match)
    (rg-test-with-first-error
      (rg-run "amid" "elisp"
              (concat default-directory "test/data")
              nil nil '("--context=3"))
      (forward-line 1)
      (should (looking-at contex-line-rx))
      (setq ctx-match (match-string 0))
      (forward-line -1)
      (should (looking-at match-line-rx))
      (setq line-match (match-string 0))
      (should (equal (length ctx-match) (length line-match)))
      (forward-line -1)
      (should (looking-at contex-line-rx))
      (setq ctx-match (match-string 0))
      (should (equal (length ctx-match) (length line-match))))))

(ert-deftest rg-integration/navigate-file-group-in-grouped-result ()
  "Test file navigation in grouped result."
  :tags '(need-rg)
  (let ((rg-group-result t)
        (files '("bar.el" "foo.el"))
        pos)
    (rg-run "hello" "elisp" (concat default-directory "test/data") nil nil (list "--sort=path"))
    (rg-with-current-result
      (goto-char (point-min))
      (rg-navigate-file-group 1)
      (should (looking-at (concat "File: " (car files))))
      (rg-navigate-file-group 1)
      (should (looking-at (concat "File: " (cadr files))))
      (compilation-next-error 1)
      (setq pos (point))

      ;; Move exactly to first match
      (rg-navigate-file-group -2)
      (should (looking-at (concat "File: " (car files))))
      (goto-char pos)

      ;; Move past first match should move to first match
      (rg-navigate-file-group -3)
      (should (looking-at (concat "File: " (car files))))

      ;; rg-navigate-file-group should indicate when it didn't move at all
      (should-not (rg-navigate-file-group -1)))))

(ert-deftest rg-integration/next-prev-file ()
  "Test file navigation in grouped result."
  :tags '(need-rg)
  (let ((rg-group-result t)
        (files '("bar.el" "foo.el"))
        pos)
    (rg-run "hello" "elisp" (concat default-directory "test/data") nil nil (list "--sort=path"))
    (rg-with-current-result
      (goto-char (point-min))
      (rg-next-file 1)
      (forward-line -1)
      (should (looking-at-p (concat "File: " (car files))))
      (rg-next-file 1)
      (forward-line -1)
      (should (looking-at-p (concat "File: " (cadr files))))
      ;; prev from header
      (rg-prev-file 1)
      (forward-line -1)
      (should (looking-at-p (concat "File: " (car files))))
      ;; prev from match
      (rg-next-file 1)
      (rg-prev-file 1)
      (forward-line -1)
      (should (looking-at-p (concat "File: " (car files))))
      ;; prev from last line
      (goto-char (- (point-max) 5))
      (rg-prev-file 1)
      (forward-line -1)
      (should (looking-at-p (concat "File: " (cadr files))))
      ;; prev from empty separator line
      (forward-line 4)
      (should (looking-at-p "^$"))
      (rg-prev-file 1)
      (forward-line -1)
      (should (looking-at-p (concat "File: " (cadr files)))))))

(ert-deftest rg-integration/navigate-file-group-in-ungrouped-result ()
  "Test file navigation in ungrouped result."
  :tags '(need-rg)
  (let ((rg-group-result nil))
    (rg-run "hello" "elisp" (concat default-directory "test/data"))
    (rg-with-current-result
      (goto-char (point-min))
      (rg-navigate-file-group 1)
      (should (eq (point) (point-min))))))

(defun rg-test-highlight-match (grouped)
  "Helper for highlight testing.
If GROUPED is is non nil grouped result are used."
  (let ((rg-group-result grouped)
        pos)
    (rg-run "hello" "elisp" (concat default-directory "test/data"))
    (rg-with-current-result
      (setq pos (rg-single-font-lock-match 'rg-match-face (point-min) (point-max) 1))
      (should-not (eq (point-max) pos)))))

(ert-deftest rg-integration/highlight-match-group ()
  "Test that highlighting of matches works."
  :tags '(need-rg)
  (rg-test-highlight-match t)
  (rg-test-highlight-match nil))

(ert-deftest rg-integration/group-result-variable ()
  "Test that grouped result is triggered if `rg-group-result' is non nil
and ungrouped otherwise."
  :tags '(need-rg)
  (should-not (rg-file-message-exist-in-result nil))
  (should (rg-file-message-exist-in-result t)))

(ert-deftest rg-integration/rg-recompile ()
  "Make sure that `rg-recompile' preserves search parameters."
  :tags '(need-rg)
  (let ((parent-dir (concat (expand-file-name default-directory) "test/")))
    (rg-run "hello" "elisp" (concat parent-dir "data"))
    (rg-with-current-result
      (cl-letf (((symbol-function #'rg-read-pattern) #'ignore))
        (setf (rg-search-files rg-cur-search) "all")
        (setf (rg-search-pattern rg-cur-search) "Hello")
        (setf (rg-search-dir rg-cur-search) parent-dir)
        (setf (rg-search-flags rg-cur-search) '("--text"))
        (rg-rerun))
        (should (rg-wait-for-search-result))
        (should (equal
                 (list "Hello" "all" parent-dir)
                 (list (rg-search-pattern rg-cur-search)
                       (rg-search-files rg-cur-search)
                       (rg-search-dir rg-cur-search))))
        (should (equal '("--text") (rg-search-flags rg-cur-search)))
        (rg-recompile)
        (should (rg-wait-for-search-result))
        (should (equal
                 (list "Hello" "all" parent-dir)
                 (list (rg-search-pattern rg-cur-search)
                       (rg-search-files rg-cur-search)
                       (rg-search-dir rg-cur-search))))
        (should (equal '("--text") (rg-search-flags rg-cur-search))))))

(ert-deftest rg-integration/display-exit-message ()
  "Verify exit messages."
  :tags '(need-rg)
  (rg-run "foo" "*.baz" (concat default-directory "test/data"))
  (with-current-buffer (rg-buffer-name)
    (rg-wait-for-search-result)
    (s-matches-p "no matches found" (buffer-substring-no-properties (point-min) (point-max))))
  (rg-run "hello" "*.baz" (concat default-directory "test/data"))
  (rg-with-current-result
    (should (equal rg-hit-count 6))
    (s-matches-p "(6 matches found)" (buffer-substring-no-properties (point-min) (point-max)))))

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

(ert-deftest rg-integration/run-confirm-unchanged-command ()
  "Test confirm and full command search"
  :tags '(need-rg)
  (cl-letf* ((rg-history nil)
             (called-prompt nil)
             (called-history nil)
             ((symbol-function #'read-from-minibuffer)
              (lambda (prompt command _ign1 _ign2 history)
                (setq called-prompt prompt)
                (setq called-history history)
                command)))
    (rg-run "hello" "all" "tmp/test" nil 'confirm)
    (rg-with-current-result
      (should (eq called-history 'rg-history))
      (should (equal called-prompt "Confirm: "))
      (should-not (null rg-cur-search))
      ;; We use the stub about which does not update the history
      (should (null rg-history)))))

(ert-deftest rg-integration/run-confirm-changed-command ()
  "Test confirm and full command search"
  :tags '(need-rg)
  (cl-letf* ((rg-history nil)
             (changed-command nil)
             (original-command nil)
             ((symbol-function #'read-from-minibuffer)
              (lambda (_ign1 command _ign2 _ign3 _ign4)
                (setq changed-command (concat command "\\ world"))
                (setq original-command command)
                changed-command)))
    (rg-run "hello" "all" "tmp/test" nil 'confirm)
    (rg-with-current-result
      (should (equal changed-command (concat original-command "\\ world")))
      (should (rg-search-full-command rg-cur-search))
      ;; We use the stub above which does not update the history
      (should (null rg-history)))))

(ert-deftest rg-integration/dwim-search ()
  "Test `rg-dwim'."
  (cl-letf* ((called-pattern nil)
             (called-files nil)
             (called-dir nil)
             (called-literal nil)
             (project-dir (expand-file-name default-directory))
             ((symbol-function #'rg-run)
              (lambda (pattern files dir &optional literal &rest _)
                (setq called-pattern pattern)
                (setq called-files files)
                (setq called-dir dir)
                (setq called-literal literal))))
    (find-file "test/data/foo.el")
    (rg-dwim)
    (should (equal called-pattern "hello"))
    (should (equal called-files "elisp"))
    (should (equal (expand-file-name called-dir) project-dir))
    (should-not (eq called-literal nil))
    (rg-dwim '(4))
    (should (equal (expand-file-name called-dir) (expand-file-name default-directory)))
    (rg-dwim '(16))
    (should (equal called-files "foo.el"))))

(ert-deftest rg-integration/project-search ()
  "Test `rg-project'."
  (cl-letf* ((called-pattern nil)
             (called-files nil)
             (called-dir nil)
             (called-literal nil)
             (project-dir (expand-file-name default-directory))
             ((symbol-function #'rg-run)
              (lambda (pattern files dir &optional literal &rest _)
                (setq called-pattern pattern)
                (setq called-files files)
                (setq called-dir dir)
                (setq called-literal literal))))
    (find-file "test/data/foo.baz")
    (rg-project "hello" "elisp")
    (should (equal called-pattern "hello"))
    (should (equal called-files "elisp"))
    (should (equal (expand-file-name called-dir) project-dir))
    (should (eq called-literal nil))))

(provide 'rg.el-test)

;;; rg.el-test.el ends here
