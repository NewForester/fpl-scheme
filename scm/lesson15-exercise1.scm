; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Macro that will evaluate several expressions when the predicate is false
(define-syntax unless
  (syntax-rules ()
    ((_ pred b1 ...)
     (if (not pred) (begin b1 ...)))))

;; Derived from the when example in the tutorial; same as the tutorial answer.

; EOF
