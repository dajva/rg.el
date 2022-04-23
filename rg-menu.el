;;; rg-menu.el --- Menu interface for rg -*- lexical-binding: t; -*-

;; Copyright (C) 2017 David Landell <david.landell@sunnyhill.email>
;;
;; Author: David Landell <david.landell@sunnyhill.email>

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

;; Support a menu interface for rg.el based on transient.

;;; Code:

(require 'rg-result)
(require 'seq)
(require 'transient)
(require 'cl-lib)

;; Forward declarations
(declare-function rg "rg.el")
(declare-function rg-literal "rg.el")
(declare-function rg-dwim "rg.el")
(declare-function rg-project "rg.el")
(declare-function rg-dwim-current-dir "rg.el")
(declare-function rg-dwim-current-file "rg.el")
(declare-function rg-list-searches "rg.el")
(declare-function rg-save-search "rg.el")
(declare-function rg-save-search-as-name "rg.el")
(defvar rg-keymap-prefix)
(defvar rg-command-line-flags-function)

(defun rg-menu-search-initial-value ()
  "Provide initial value for `rg-menu' transient command."
  (when (eq major-mode 'rg-mode)
    (rg-search-flags rg-cur-search)))

(eval-and-compile
  (defun rg-menu-create-search-body (func)
    "Call FUNC from search menu with the flags set in the transient menu."
    `((let* ((transient-flags (transient-get-value))
             (function-flags (funcall rg-command-line-flags-function nil))
             (rg-command-line-flags-function
              (lambda (flags)
                (append function-flags transient-flags flags))))
        (call-interactively #',func))))

  (defun rg-menu-create-rerun-body (func)
    "Call FUNC from rerun menu with flags extracted with ARGFUNC.
If INTERACTIVE is non nil, call func interactively, otherwise call it
regularly."
    `((setf (rg-search-flags rg-cur-search) (transient-get-value))
      (if (commandp #',func)
           (call-interactively #',func)
         (,func))))

  (defun rg-menu-assemble-transient-wrapper (func body)
    "Create a defun with name 'FUNC--transient' with BODY."
    (let ((func-name (concat (symbol-name func) "--transient"))
          (doc-string (format "Transient wrapper around `%s' for `rg-menu'."
                              (symbol-name func))))
      `(progn
         (defun ,(intern func-name) ()
           (interactive)
           ,@body)
         (put ',(intern func-name) 'function-documentation
              ;; quote to defer evalutation until func is available
              '(concat ,doc-string "\n\n" (documentation ',func)))))))

(defmacro rg-menu-wrap-transient-search (func)
  "Wrap FUNC with a command that apply transient arguments to the search.
FUNC is an initial search function and not a rerun function."
  (rg-menu-assemble-transient-wrapper
   func
   (rg-menu-create-search-body func)))

(defmacro rg-menu-wrap-transient-rerun (func)
  "Wrap FUNC with a command that apply transient arguments to the rerun.
FUNC is an rerun function invoked from an `rg-mode' buffer."
  (rg-menu-assemble-transient-wrapper
   func
   (rg-menu-create-rerun-body func)))

;; Regular rg entries
(rg-menu-wrap-transient-search rg)
(rg-menu-wrap-transient-search rg-literal)
(rg-menu-wrap-transient-search rg-dwim)
(rg-menu-wrap-transient-search rg-project)
(rg-menu-wrap-transient-search rg-dwim-current-dir)
(rg-menu-wrap-transient-search rg-dwim-current-file)
(rg-menu-wrap-transient-search rg-list-searches)
(rg-menu-wrap-transient-search rg-save-search)
(rg-menu-wrap-transient-search rg-save-search-as-name)
(rg-menu-wrap-transient-search rg-back-history)
(rg-menu-wrap-transient-search rg-forward-history)

;; Rerun rg entries
(rg-menu-wrap-transient-rerun rg-rerun)
(rg-menu-wrap-transient-rerun rg-rerun-change-regexp)
(rg-menu-wrap-transient-rerun rg-rerun-change-literal)
(rg-menu-wrap-transient-rerun rg-rerun-change-files)
(rg-menu-wrap-transient-rerun rg-rerun-change-dir)

 ;;;###autoload (autoload 'rg-menu "rg-menu.el" "" t)
(transient-define-prefix rg-menu ()
  "Show menu buffer for rg commands."
  :man-page "rg"
  :value 'rg-menu-search-initial-value
  ["Switches"
   (3 "-h" "Search hidden files" "--hidden")
   (6 "-a" "Search binary files" "--text")
   (4 "-z" "Search zipped files" "--search-zip")
   (4 "-v" "Invert match" "--invert-match")
   (4 "-U" "Multi line" "--multiline")
   (4 "-." "Dot all" "--multiline-dotall")
   (3 "-w" "Search words" "--word-regexp")
   (5 "-x" "Search lines" "--line-regexp")
   (5 "-P" "Use PCRE2 regexps" "--pcre2")
   (4 "-1" "Don't cross file system" "--one-file-system")
   (6 "-L" "Follow symlinks" "--follow")
   (3 "-n" "Override ignore files" "--no-ignore")]
  ["Options"
   (3 "-C" "Show context" "--context=")
   (6 "-B" "Show context before" "--before-context=")
   (6 "-A" "Show context after" "--after-context=")
   (4 "-M" "Omit long lines" "--max-columns=")
   (4 "-m" "Max matches per file" "--max-count=")
   (6 "-s" "Ignore large files" "--max-filesize=")
   (4 "-g" "Filter files glob" "--glob=")
   (6 "-i" "Filter files glob (no case)" "--iglob=")
   (4 "-T" "Exclude files types" "--type-not=")
   (5 "-S" "Sort result" "--sort=")
   (5 "-R" "Reverse sort result" "--sortr=")
   (6 "-E" "Force encoding" "--encoding=")
   (6 "-r" "Replace match" "--replace=")]
  [[:if-not-mode rg-mode
    :description "Search"
    (3 "d" "Dwim project" rg-dwim--transient)
    (4 "c" "Dwim current directory" rg-dwim-current-dir--transient)
    (4 "f" "Dwim current file" rg-dwim-current-file--transient)
    (3 "r" "Regex" rg--transient)
    (3 "t" "Literal" rg-literal--transient)
    (3 "p" "Project" rg-project--transient)]
   [:if-mode rg-mode
    :description "Rerun"
    (3 "g" "Go" rg-rerun--transient)
    (3 "r" "Change regex" rg-rerun-change-regexp--transient)
    (3 "t" "Change literal" rg-rerun-change-literal--transient)
    (3 "f" "Change files" rg-rerun-change-files--transient)
    (3 "d" "Change directory" rg-rerun-change-dir--transient)]
   ["Manage"
    (4 "l" "List" rg-list-searches--transient)
    (4 "s" "Save" rg-save-search--transient)
    (4 "S" "Save as name" rg-save-search-as-name--transient)]
   [:if-mode rg-mode
    :description "History"
    (3 "b" "Back" rg-back-history--transient)
    (3 "w" "Forward" rg-forward-history--transient)]])

(eval-and-compile
  (defun rg-menu-group-at-location-p (desc loc)
    "Check if group with description DESC exist at location LOC."
    (let ((suffix (transient-get-suffix 'rg-menu loc)))
      (and suffix
           (string= (plist-get (seq-elt suffix 2) :description) desc))))

  (defun rg-menu-get-loc-of-group (desc)
    "Find the location of the group with description DESC.
Returns the transient location coordinates for the group or NIL if not found."
    (let ((loc (list 2 0)))
      (ignore-errors
        (while (not (rg-menu-group-at-location-p desc loc))
          (cl-incf (cl-second loc)))
        loc)))

  (defun rg-menu-transient-insert (group key description command)
    "Insert a new suffix into the rg-menu under GROUP.
GROUP is the description of an existing group. If the group does not exist
A new group will be created. KEY and DESCRIPTION defines the binding
and description of the new menu entry. COMMAND is a transient wrapped
command."
    (unless (stringp group)
      (user-error "'%S' is not a string" group))
    (unless (or (stringp key)
                (vectorp key))
      (user-error "'%S' should be a key description string or a key vector" key))
    (unless (stringp description)
      (user-error "'%S' is not a string" description))
    (if-let (group-loc (rg-menu-get-loc-of-group group))
        (transient-append-suffix 'rg-menu (append group-loc '(-1))
          (list 3 key description command))
      (transient-append-suffix 'rg-menu '(-1 -1)
        (vector group (list 3 key description command))))))

;;;###autoload
(defun rg-enable-menu (&optional prefix)
  "Bind `rg-menu' to PREFIX key.
If prefix is not supplied `rg-keymap-prefix' is used."
  (interactive)
  (setq prefix (or prefix rg-keymap-prefix))
  (when prefix
    (global-set-key prefix #'rg-menu)
    ;; If it's already bound it might have been rebound so keep that
    ;; instead of overriding.
    (unless (where-is-internal #'rg-menu (list rg-mode-map) t)
      (define-key rg-mode-map "m" #'rg-menu))))

(provide 'rg-menu)

;;; rg-menu.el ends here

