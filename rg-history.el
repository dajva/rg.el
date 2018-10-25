;;; rg-history.el --- History navigation in rg.el *- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'rg-compat)

(cl-defstruct (rg-history (:constructor rg-history-create)
                          (:copier nil))
  past                    ; list of searches for backward navigation
  present                 ; current search
  future)                 ; list of searches for forward navigation

(defun rg-history-push (item instance)
  "Push a new ITEM to the rg-history INSTANCE."
  (rg-when-let (present (rg-history-present instance))
    (push present (rg-history-past instance)))
  (setf (rg-history-present instance) item)
  (setf (rg-history-future instance) nil))

(defun rg-history-back (instance)
  "Move back in the rg-history INSTANCE.
Return the new current search."
  (rg-when-let (prev (pop (rg-history-past instance)))
    (push (rg-history-present instance) (rg-history-future instance))
    (setf (rg-history-present instance) prev)))

(defun rg-history-forward (instance)
  "Move forward in the rg-history INSTANCE.
Return the new current search."
  (rg-when-let (next (pop (rg-history-future instance)))
    (push (rg-history-present instance) (rg-history-past instance))
    (setf (rg-history-present instance) next)))

(provide 'rg-history)

;;; rg-history.el ends here
