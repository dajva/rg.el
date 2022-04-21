;;; rg-isearch.el --- rg integration into isearch -*- lexical-binding: t; -*-

;; Copyright (C) 2020 David Landell <david.landell@sunnyhill.email>
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

;; This file provides rg isearch integration.
;; Add this to your configuration to bind chosen key in isearch minibuffer:
;;
;; (define-key isearch-mode-map "\M-sr" 'rg-isearch-menu)
;;

;; See info node `(rgel)Isearch search' for documentation or online at https://rgel.readthedocs.io/.

;;; Code:

(require 'rg)

(defun rg-get-isearch-string ()
  "Extract the isearch string from the last isearch."
  ;; Mimic what `isearch-occur' does.
  (or (cond
       ;; We can't handle arbitrary functions so just use the string.
       ((functionp isearch-regexp-function) isearch-string)
       ;; isearch-occur handles this as word search so insert word boundary
       (isearch-regexp-function (concat "\b" isearch-string "\b"))
       (isearch-regexp isearch-string)
       (t isearch-string))
      ""))

;;;###autoload (autoload 'rg-isearch-current-file "rg-isearch.el" "" t)
(rg-define-search rg-isearch-current-file
  "Run ripgrep on current file searching for latest isearch string."
  :dir current
  :query (rg-get-isearch-string)
  :format literal
  :files (rg-get-buffer-file-name)
  :dir current)

;;;###autoload (autoload 'rg-isearch-current-dir "rg-isearch.el" "" t)
(rg-define-search rg-isearch-current-dir
  "Run ripgrep in current directory searching for latest isearch string
in files matching the current file type."
  :query (rg-get-isearch-string)
  :format literal
  :files current
  :dir current)

;;;###autoload (autoload 'rg-isearch-project "rg-isearch.el" "" t)
(rg-define-search rg-isearch-project
  "Run ripgrep in current project searching for latest isearch string
in files matching the current file type."
  :dir project
  :query (rg-get-isearch-string)
  :format literal
  :files current)

 ;;;###autoload (autoload 'rg-isearch-menu "rg-isearch.el" "" t)
(transient-define-prefix rg-isearch-menu ()
  "Show menu for rg isearch commands."
  [:description "Search with ripgrep"
    (3 "f" "File" rg-isearch-current-file--transient)
    (3 "d" "Dir" rg-isearch-current-dir--transient)
    (3 "p" "Project" rg-isearch-project--transient)])

(provide 'rg-isearch)

;;; rg-isearch.el ends here
