;;; test-helper.el --- rg.el: Helper for tests

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

(when (require 'undercover nil t)
  (undercover "*.el"
              (:report-format 'lcov)
              (:send-report nil)))

(require 'cl-lib)
(require 'ert)
(require 'rg)
(require 'rg-history)
(require 'rg-isearch)
(require 'rg-menu)
(require 's)
(require 'seq)
(require 'tramp)
(require 'wgrep-rg)

(defun rg-regexp-anywhere (needle)
  (s-replace "%%%%" needle "\\( \\|^\\)%%%%\\( \\|$\\)"))

(defun rg-regexp-last (needle)
  (s-replace "%%%%" needle "\\( \\|^\\)%%%%$"))

(defun rg-regexp-anywhere-but-last (needle)
  (s-replace "%%%%" needle "\\( \\|^\\)%%%% "))

(defun rg-run-and-wait (fn &rest args)
  "Run FN  with ARGS and then wait for search to be done."
  (apply fn args)
  (with-current-buffer (rg-buffer-name)
    (rg-wait-for-search-result)))

(defun rg-wait-for-search-result ()
  "Wait for the rg process to finish searching and return non nil if the
search was successful. Timeout is 10 s."
  (let (search-finished)
    (add-hook 'compilation-finish-functions
              (lambda (buffer msg) (setq search-finished msg))
              t t)
    (with-timeout (10 nil)
      (while (not search-finished)
        (accept-process-output nil 0.1)))
    (when search-finished
      (equal (string-trim search-finished) "finished"))))

(defun rg-check-git-project-root ()
"Check that project root of rg.el-test.el file is main dir of
repository."
  (should (equal
           (expand-file-name (rg-project-root
                              (concat default-directory "/test/rg.el-test.el")))
           (expand-file-name default-directory))))

(defmacro rg-with-current-result (&rest body)
  "Evaluate BODY in current result buffer when search has finished."
  (declare (indent 0) (debug t))
  `(with-current-buffer (rg-buffer-name)
     (font-lock-ensure)
     (rg-wait-for-search-result)
     (let ((result (progn ,@body)))
       (kill-buffer)
       result)))

(defmacro rg-with-temp-global-keymap (&rest body)
  "Evaluate BODY with a temporary keymap as global map.
Restore original global keymap afterwards."
  (declare (indent 0) (debug t))
  (let ((saved-global-map (cl-gensym))
        (temp-global-map (cl-gensym)))
    `(let ((,saved-global-map (current-global-map))
           (,temp-global-map (make-sparse-keymap)))
       (use-global-map ,temp-global-map)
       ,@body
       (use-global-map ,saved-global-map))))

(defmacro rg-test-with-fontified-buffer (search &rest body)
  "Run SEARCH and make sure buffer is fontified when executing BODY.
SEARCH can either be a search string or a form invocating `rg-run'."
  (declare (indent 1) (debug t))
  (let ((invocation
         (if (stringp search)
             `(rg-run ,search "elisp" (concat default-directory "test/data"))
           search)))
    (cl-assert (consp invocation))
    `(progn
       ,invocation
       (rg-with-current-result
         ;; font-lock-mode is disabled by default in batch mode so
         ;; request explicit fontification
         (font-lock-ensure)
         ,@body))))

(defmacro rg-test-with-command-start (search &rest body)
  "Run search and put point to beginning of rg command when running BODY."
  (declare (indent 0) (debug t))
  (let ((command-start (cl-gensym)))
    `(rg-test-with-fontified-buffer ,search
       (let ((,command-start (next-single-property-change (point-min) 'rg-command-hidden-part)))
         (should ,command-start)
         (should-not (eq ,command-start (point-max)))
         (goto-char ,command-start)
         ,@body))))

(defmacro rg-test-with-first-error (search &rest body)
  "Run search and put point at start of first error line when running BODY."
  (declare (indent 0) (debug t))
  `(rg-test-with-fontified-buffer ,search
     (compilation-next-error 1)
     (should-not (eq (point) (point-max)))
     (beginning-of-line)
     ,@body))

(defmacro rg-with-executable-find-mock (&rest body)
  "Mock `executable-find' in BODY."
  (declare (indent 0) (debug t))
  `(cl-letf* (((symbol-function #'executable-find)
               (lambda (command &optional remote)
                 (let ((tramp-name (tramp-dissect-file-name default-directory)))
                   (concat "remote-rg-" (tramp-file-name-host-port tramp-name)))))
              (rg-executable "local-rg"))
     ,@body))

(defun simulate-rg-run (pattern files dir)
  (setq-default rg-cur-search
                (rg-search-create
                 :pattern pattern
                 :files files
                 :flags rg-initial-toggle-flags)))

(defun rg-file-message-exist-in-result (grouped)
  "Search and return non nil if 'rg-file-tag-face exist in buffer.
GROUPED control if `rg-group-result' is used."
  (let((rg-group-result grouped)
       pos)
    (rg-run "hello" "elisp" (concat default-directory "test/data"))
    (rg-with-current-result
      (not (eq (point-max) (next-single-property-change (point-min)
                                             'rg-file-message
                                             nil (point-max)))))))

(defun rg-single-font-lock-match (face pos limit direction)
  "Return position of next match of 'font-lock-face property that equals FACE.
POS is the start position of the search and LIMIT is the limit of the
search.  If FACE is not found within LIMIT, LIMIT is returned.  If
DIRECTION is positive search forward in the buffer, otherwise search
backward."
  (let ((single-property-change-func
         (if (> direction 0)
             'next-single-property-change
           'previous-single-property-change)))
    (while
        (progn
          (setq pos (funcall single-property-change-func pos 'font-lock-face nil limit))
          (and (not (equal pos limit))
               (not (let ((properties (get-text-property pos 'font-lock-face)))
                      (if (listp properties)
                          (member face properties)
                        (eq face properties))))))))
  pos)

;;; test-helper.el ends here
