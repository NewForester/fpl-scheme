; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Macro to decrement a variable by the amount given or 1 if none given
(define-syntax decf
  (syntax-rules ()
    ((_ x) (begin (set! x (- x 1)) x))
    ((_ x ii) (begin (set! x (- x ii)) x))))

;; Derived from the incf example in the tutorial; same as the tutorial answer.

; EOF
