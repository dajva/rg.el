;;; rg-header.el --- Header line for rg-mode -*- lexical-binding: t; -*-

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

;; Header line format for rg-mode result buffer.

;;; Code:

(require 'mouse)

;; Faces
(defface rg-toggle-on-face
  '((t :inherit rg-file-tag-face))
  "face for toggle 'on' text in header."
  :group 'rg-face)

(defface rg-toggle-off-face
  '((t :inherit rg-error-face))
  "face for toggle 'off' text in header."
  :group 'rg-face)

(defface rg-literal-face
  '((t :inherit rg-filename-face))
  "face for literal label in header."
  :group 'rg-face)

(defface rg-regexp-face
  '((t :inherit compilation-line-number))
  "face for regexp label in header."
  :group 'rg-face)


;; Defuns
(defun rg-header-render-label (labelform)
  "Return a fontified header label.
LABELFORM is either a string to render or a form where the `car' is a
conditional and the two following items are then and else specs.
Specs are lists where the the `car' is the labels string and the
`cadr' is font to use for that string."
  (list '(:propertize "[" font-lock-face (header-line bold))
        (cond
         ((stringp labelform)
          `(:propertize ,labelform font-lock-face (header-line bold)))
         ((listp labelform)
          (let* ((condition (nth 0 labelform))
                 (then (nth 1 labelform))
                 (else (nth 2 labelform)))
            `(:eval (if ,condition
                        (propertize ,(nth 0 then) 'font-lock-face '(,(nth 1 then) header-line bold))
                      (propertize ,(nth 0 else) 'font-lock-face '(,(nth 1 else) header-line bold))))))
         (t (error "Not a string or list")))
        '(:propertize "]" font-lock-face (header-line bold))
        '(": ")))

(defun rg-header-render-toggle (on)
  "Return a fontified toggle symbol.
If ON is non nil, render \"on\" string, otherwise render \"off\"
string."
  `(:eval (let* ((on ,on)
                 (value (if on "on " "off"))
                 (face (if on 'rg-toggle-on-face 'rg-toggle-off-face)))
            (propertize value 'font-lock-face `(bold ,face)))))

(defun rg-header-mouse-action (command help &rest items)
  "Add a keymap with mouse click action for COMMAND.
When hoovering HELP is shown as a tooltip.  ITEMS is the header line
items that the map will be applied to."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-2]
      (lambda (click)
        (interactive "e")
        (mouse-select-window click)
        (call-interactively command)))
    `(:propertize ,items mouse-face header-line-highlight
                  help-echo ,help
                  keymap ,map)))

;; Use full-command here to avoid dependency on rg-search
;; struct. Should be properly fixed.
(defun rg-create-header-line (search full-command)
  "Create the header line for SEARCH.
If FULL-COMMAND specifies if the full command line search was done."
  (let ((itemspace "  "))
    (setq header-line-format
          (if full-command
              (list (rg-header-render-label "command line") "no refinement")
            (list
             (rg-header-mouse-action
              'rg-rerun-toggle-rexexp-literal "Toggle literal/regexp"
              (rg-header-render-label `((rg-search-literal ,search)
                                        ("literal" rg-literal-face)
                                        ("regexp" rg-regexp-face))))
             (rg-header-mouse-action
              'rg-rerun-change-query "Change search string"
              `(:eval (rg-search-pattern ,search)))
             itemspace
             (rg-header-render-label "files")
             (rg-header-mouse-action
              'rg-rerun-change-files "Change file types"
              `(:eval (rg-search-files ,search)))
             itemspace
             (rg-header-render-label "case")
             (rg-header-mouse-action
              'rg-rerun-toggle-case "Toggle case"
              (rg-header-render-toggle
               `(not (member "-i" (rg-search-flags ,search)))))
             itemspace
             (rg-header-render-label "ign")
             (rg-header-mouse-action
              'rg-rerun-toggle-ignore "Toggle ignore"
              (rg-header-render-toggle
               `(not (member "--no-ignore" (rg-search-flags ,search)))))
             itemspace
             (rg-header-render-label "hits")
               '(:eval (format "%d" rg-hit-count)))))))

(provide 'rg-header)

;;; rg-header.el ends here
