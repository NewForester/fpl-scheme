; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Function that takes three real numbers and returns their product if all are positive
(define (pos a b c)
  (if (and (> a 0) (> b 0) (> c 0)) (* a b c) #f))

;;; Function that takes three real numbers and returns their product if any is negative
(define (neg a b c)
  (if (or (< a 0) (< b 0) (< c 0)) (* a b c) #f))

;; My answers uses if and return #f if the condition is not met whereas the tutorial answer used (and without the (if.

; EOF
