;;; rg-compat.el --- Compatibility layer for older emacsen *- lexical-binding: t; -*-

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

;; subr-x is not available in Emacs 24.4.
(if (and (require 'subr-x nil 'noerror)
         (fboundp 'when-let))
    (defalias 'rg-when-let 'when-let)
  ;; Somewhat simplified version of emacs 25 when-let
  ;; TODO: Drop with emacs 24 support.
  (defmacro rg-when-let (bindings &rest body)
    "Process BINDINGS and if all values are non-nil eval BODY.
Argument BINDINGS is a list of tuples whose car is a symbol to be
bound and (optionally) used in BODY, and its cadr is a sexp to be
evalled to set symbol's value.  In the special case you only want
to bind a single value, BINDINGS can just be a plain tuple."
    (declare (indent 2)
             (debug ([&or (&rest (symbolp form)) (symbolp form)] body)))
    (when (and (<= (length bindings) 2)
               (not (listp (car bindings))))
      ;; Adjust the single binding case
      (setq bindings (list bindings)))
    (let ((binding-ok t)
           checked-bindings)
      (dolist (binding bindings)
        (push `(,(car binding) (and ,binding-ok ,(cadr binding))) checked-bindings)
        (setq binding-ok (car binding)))
      `(let* ,(nreverse checked-bindings)
         (when ,binding-ok
           ,@body)))))

(provide 'rg-compat)

;;; rg-compat.el ends here
