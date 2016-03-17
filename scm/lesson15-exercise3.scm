; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Macro to implement a for loop with a step, 1 if none given
(define-syntax for
  (syntax-rules ()
    ((_ (ii from to) b1 ...)
      (for (ii from to 1) b1 ...))
    ((_ (ii from to step) b1 ...)
      (let loop((ii from))
        (if (< ii to)
          (begin
            b1 ...
            (loop (+ ii step))))))))

;; Derived from the for example in the tutorial; the example in the tutorial writes out the first case in full.

; EOF
