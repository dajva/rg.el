;;; org-bootstrap.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2019 David Landell <david.landell@sunnyhill.email>
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

(require 'ox-rst)
(require 'package)
(require 'rg)

;; General org mode settings

(setq org-odd-levels-only t)
(setq org-confirm-babel-evaluate nil)

;; org ox-rst settings
(setq org-rst-pygments-langs
      '((lisp "common-lisp")
        (cc "c++")
        (cperl "perl")
        (latex "tex")
        (shell-script "bash")
        (caml "ocaml")
        (sqlite3 "sqlite")))

(setq org-rst-code-block 'code-block)
(setq org-rst-link-use-ref-role t)
(setq org-rst-file-link-use-ref-role t)

(defun rg-format-todo (todo todo-type priority title tags contents)
  (when (eq todo-type 'todo)
    (let* ((padding "   ")
           (formated-contents
            (when contents
              (concat padding
                      (mapconcat 'identity (split-string contents "\n")
                                 (concat "\n" padding))))))
      (concat (format (concat ".. todo:: %s\n\n") title)
              formated-contents "\n"))))
(setq org-rst-format-inlinetask-function #'rg-format-todo)

(defun rg-format-drawer (name contents info)
  (let* ((lines (split-string contents "\n"))
         (heading (car lines))
         (body (cdr lines)))
    (format ".. %s:: %s\n%s\n\n"
            (downcase name)
            heading
            (concat "   " (mapconcat #'identity body (concat "\n" "   "))))))
(setq org-rst-format-drawer-function #'rg-format-drawer)

(defun rg-func-role-link (path desc backend)
  (rg-ref-role-link "func" path desc backend))

(defun rg-cmd-role-link (path desc backend)
  (rg-ref-role-link "cmd" path desc backend))

(defun rg-opt-role-link (path desc backend)
  (rg-ref-role-link "opt" path desc backend))

(defun rg-var-role-link (path desc backend)
  (rg-ref-role-link "var" path desc backend))

(defun rg-ref-role-link (role path desc backend)
  (when (eq backend 'rst)
    (if desc
        (format ":%s:`%s <%s>`" role desc path)
      (format ":%s:`%s`" role path))))

(push '("func" :export rg-func-role-link ) org-link-parameters)
(push '("cmd" :export rg-cmd-role-link ) org-link-parameters)
(push '("opt" :export rg-opt-role-link ) org-link-parameters)
(push '("var" :export rg-var-role-link ) org-link-parameters)

(add-to-list 'load-path nil)

(defvar rst-out-dir (getenv "RST_OUT_DIR"))

(defun rg-export-to-rst ()
  (interactive)
  (let ((outfile (org-export-output-file-name ".rst" nil rst-out-dir)))
    (org-export-to-file 'rst outfile)))

(provide 'org-bootstrap)

;;; org-bootstrap.el ends here
