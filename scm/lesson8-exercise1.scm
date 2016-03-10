; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; A function that doubles a list of numbers
(define (double ls)
  (map (lambda (x) (* 2 x)) ls))

;;; A function that subtracts two lists of numbers
(define (subtract lhs rhs)
  (map - lhs rhs))

;; My answers are the same as given in the tutorial

; EOF
