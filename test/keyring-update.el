;;; rg.el --- Helper for updating keyring on github -*- lexical-binding: t -*-

;; Copyright (C) 2024 David Landell <david.landell@sunnyhill.email>

;; Author: David Landell <david.landell@sunnyhill.email>
;; URL: https://github.com/dajva/rg.el

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

(defun rg-update-elpa-keyring ()
  (let ((package-check-signature nil))
    (package-initialize)
    (package-refresh-contents)
    (package-install 'gnu-elpa-keyring-update)))

;;; keyring-update.el ends here
