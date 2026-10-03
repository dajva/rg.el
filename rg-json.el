;;; rg-json.el --- json parsing for rg.el -*- lexical-binding: t; -*-

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

;; This file contains parsing code for the ripgrep json output.  It recreates the
;; native ripgrep ouputs we use in non json parsing code paths.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

;; Forward declarations
(defvar rg-align-position-numbers)
(defvar rg-group-result)
(defvar rg-show-columns)
(defvar rg-hit-count)
(declare-function rg-perform-position-numbers-alignment "rg-result")

(defvar-local rg-json-last-context-line-number nil
  "Keeps track of the last context line.
Used to insert ending marker with newline.")
(defvar-local rg-json-file-printed nil
  "Triggers new lines at the correct places.")

;; Markers to dispatch methods for grouped and ungrouped output.
(cl-defstruct (grouped-output))
(cl-defstruct (ungrouped-output))

;; Handles the different ripgrep json output types.
(cl-defgeneric rg-json-transform-begin (output-type object))
(cl-defgeneric rg-json-transform-match (output-type object))
(cl-defgeneric rg-json-transform-end (output-type object))
(cl-defgeneric rg-json-transform-context (output-type object))
(cl-defgeneric rg-json-transform-unhandled (output-type object))

(defun rg-json-plist-path (object &rest keys)
  "Return the sub object of OBJECT at the path represented by KEYS."
  (dolist (key keys object)
    (setq object (plist-get object key))))

(defun rg-json-format-positions (line-number column-number)
  "Format LINE-NUMBER and COLUMN-NUMBER.
Formatting is based on alignment and display settings."
  (if rg-align-position-numbers
      (rg-perform-position-numbers-alignment
       (number-to-string line-number)
       (and column-number (number-to-string column-number)))
    (if column-number
        (if rg-show-columns
            (format "%d:%d:" line-number column-number)
          (concat (number-to-string line-number)
                  (propertize (format ":%d" column-number) 'invisible t)
                  ":"))
      (format "%d:" line-number))))

(cl-defmethod rg-json-transform-begin ((_output-type ungrouped-output) _object))

(cl-defmethod rg-json-transform-match ((_output-type ungrouped-output) object)
  (let* ((matches (rg-json-plist-path object :data :submatches))
         (line-number (rg-json-plist-path object :data :line_number))
         (first-match (seq-first matches))
         (column-number (when first-match
                          (1+ (rg-json-plist-path first-match :start))))
         (text (rg-json-plist-path object :data :lines :text))
         (path (rg-json-plist-path object :data :path :text)))
    (setf rg-json-last-context-line-number line-number)
    (mapc
     (lambda (match)
       (cl-incf rg-hit-count)
       (add-text-properties
        (rg-json-plist-path match :start) (rg-json-plist-path match :end)
        (list 'face nil 'font-lock-face 'rg-match-face) text))
     matches)
    (insert
     path ":"
     (if (and rg-show-columns column-number)
         (format "%d:%d:" line-number column-number)
       (format "%d:" line-number))
     text)))

(cl-defmethod rg-json-transform-end ((_output-type ungrouped-output) _object)
  (setf rg-json-last-context-line-number nil))

(cl-defmethod rg-json-transform-context ((_output-type ungrouped-output) object)
  (let ((line-number (rg-json-plist-path object :data :line_number))
        (path (rg-json-plist-path object :data :path :text)))
    (when (and rg-json-last-context-line-number
               (not (equal (1+ rg-json-last-context-line-number) line-number)))
      (insert "--\n"))
    (setf rg-json-last-context-line-number line-number)
    (insert
     path "-"
     (format "%d-" line-number)
     (rg-json-plist-path object :data :lines :text))))

(cl-defmethod rg-json-transform-unhandled ((_output-type ungrouped-output) _object))

(cl-defmethod rg-json-transform-begin ((_output-type grouped-output) object)
  (when rg-json-file-printed
    (newline))
  (setq rg-json-file-printed t)
  (insert
   (propertize "File:"
               'rg-file-message t
               'face nil
               'font-lock-face 'rg-file-tag-face)
   " "
   (propertize (rg-json-plist-path object :data :path :text)
               'face nil
               'font-lock-face 'rg-filename-face)
   "\n"))

(cl-defmethod rg-json-transform-end ((_output-type grouped-output) _object)
  (setf rg-json-last-context-line-number nil))

(cl-defmethod rg-json-transform-match ((_output-type grouped-output) object)
  (let* ((matches (rg-json-plist-path object :data :submatches))
         (line-number (rg-json-plist-path object :data :line_number))
         (first-match (seq-first matches))
         (column-number (when first-match
                          (1+ (rg-json-plist-path first-match :start))))
         (text (rg-json-plist-path object :data :lines :text)))
    (setf rg-json-last-context-line-number line-number)
    (mapc
     (lambda (match)
       (cl-incf rg-hit-count)
       (add-text-properties
        (rg-json-plist-path match :start) (rg-json-plist-path match :end)
        (list 'face nil 'font-lock-face 'rg-match-face) text))
     matches)
    (insert
     (rg-json-format-positions line-number column-number)
     text)))

(cl-defmethod rg-json-transform-context ((_output-type grouped-output) object)
  (let ((line-number (rg-json-plist-path object :data :line_number)))
    (when (and rg-json-last-context-line-number
               (not (equal (1+ rg-json-last-context-line-number) line-number)))
      (insert "--\n"))
    (setf rg-json-last-context-line-number line-number)
    (insert
     (if rg-align-position-numbers
         (rg-perform-position-numbers-alignment
          (number-to-string line-number) nil "-")
       (format "%d-" line-number))
     (rg-json-plist-path object :data :lines :text))))

(cl-defmethod rg-json-transform-unhandled ((_output-type grouped-output) _object))

(defun rg-json-filter ()
  ;; Process marks default to insertion-type nil.  When a chunk ends on a
  ;; complete JSON line, delete-line leaves point and the process mark at the
  ;; same position; insert would then put the replacement from the json parsing
  ;; after the mark, and later process output pushes that line to the end of the
  ;; buffer.
  ;; Set marker insertion type to t to move the marker to the end of the
  ;; insterted text to have new process output be inserted after our replacements.
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-marker-insertion-type (process-mark proc) t))
  (let (object
        (output-type (if rg-group-result (make-grouped-output) (make-ungrouped-output))))
    (while (setf object
                 (condition-case nil
                     (json-parse-buffer :object-type 'plist)
                   (error nil)))
      (delete-line)
      (pcase (plist-get object :type)
        ("begin" (rg-json-transform-begin output-type object))
        ("match" (rg-json-transform-match output-type object))
        ("end" (rg-json-transform-end output-type object))
        ("context" (rg-json-transform-context output-type object))
        (_ (rg-json-transform-unhandled output-type object))))))

(provide 'rg-json)

;;; rg-json.el ends here
