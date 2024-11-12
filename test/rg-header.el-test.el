(require 'cl-lib)

(defvar rg-unit/long-search-pattern "everything everywhere all at once")

;;; Mocks.

(cl-defmacro rg-unit/mock-truncation-predicate ((&key max predicate) &rest body)
  "Execute BODY with truncation predicate mocked by PREDICATE.

MAX is the value `rg-header-max-search-string-length' will be set to."
  (declare (indent defun))
  `(cl-letf* ((rg-header-max-search-string-length ,max)
              ((symbol-function 'rg-header-truncates-p) #',predicate))
     ,@body))

(cl-defmacro rg-unit/mock-rg-cur-search-pattern ((&key do-return) &rest body)
  "Execute BODY with `rg-cur-search-pattern' mocked.

Instead DO-RETURN will be returned when the function is called."
  (declare (indent defun))

  `(cl-letf* (((symbol-function 'rg-cur-search-pattern) (lambda (&rest _) ,do-return)))
     ,@body))

;;; Tests.

(ert-deftest rg-unit/header-truncation-predicate ()
  "Test `rg-header-truncates-p'."

  ;; Not set.
  (let ((rg-header-max-search-string-length nil))

    (should-not (rg-header-truncates-p rg-unit/long-search-pattern)))

  ;; Erroneous value.
  (let ((rg-header-max-search-string-length 'none))

    (should-not (rg-header-truncates-p rg-unit/long-search-pattern)))

  ;; The length of the string passed.
  (let ((rg-header-max-search-string-length (length rg-unit/long-search-pattern)))

    (should-not (rg-header-truncates-p rg-unit/long-search-pattern)))

  ;; One below string length.
  (let ((rg-header-max-search-string-length (1- (length rg-unit/long-search-pattern))))

    (should (rg-header-truncates-p rg-unit/long-search-pattern)))

  ;; At 0.
  (let ((rg-header-max-search-string-length 0))

    (should (rg-header-truncates-p rg-unit/long-search-pattern)))

  ;; Below 0.
  (let ((rg-header-max-search-string-length -1))

    (should (rg-header-truncates-p rg-unit/long-search-pattern))))


(ert-deftest rg-unit/search-pattern-truncation-in-header ()
  "Tests `rg-header-truncate-search-pattern'."
  ;; When predicate is true.
  (let ((ellipsis-len (length (rg-truncate-string-ellipsis))))
    (rg-unit/mock-truncation-predicate (:max 11 :predicate always)
      (should (string=
               (concat (substring "everything" 0 (- 11 ellipsis-len)) (rg-truncate-string-ellipsis))
               (rg-header-truncate-search-pattern rg-unit/long-search-pattern))))

    (rg-unit/mock-truncation-predicate (:max 5 :predicate always)
      (should (string=
               (concat (substring "ever" 0 (- 5 ellipsis-len)) (rg-truncate-string-ellipsis))
               (rg-header-truncate-search-pattern rg-unit/long-search-pattern))))

    (rg-unit/mock-truncation-predicate (:max (length rg-unit/long-search-pattern) :predicate always)
      (should (string=
               "everything everywhere all at once"
               (rg-header-truncate-search-pattern rg-unit/long-search-pattern))))

    ;; When predicate is false.
    (rg-unit/mock-truncation-predicate (:max 11 :predicate ignore)
      (should (string=
               "everything everywhere all at once"
               (rg-header-truncate-search-pattern rg-unit/long-search-pattern))))

    (rg-unit/mock-truncation-predicate (:predicate ignore)
      (should (string=
               "everything everywhere all at once"
               (rg-header-truncate-search-pattern rg-unit/long-search-pattern))))))

(ert-deftest rg-unit/search-help-for-header ()
  "Tests `rg-header-search-help'."
  (rg-unit/mock-rg-cur-search-pattern (:do-return "this is the full string")

    (rg-unit/mock-truncation-predicate (:predicate always)
      (should (string= "Change search string: this is the full string" (rg-header-search-help))))

    (rg-unit/mock-truncation-predicate (:predicate ignore)
      (should (string= "Change search string" (rg-header-search-help))))))
