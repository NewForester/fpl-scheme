; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;; Calculate the following using the Scheme interpreter

;;; (1+39) * (53-45)
(* (+ 1 39) (- 53 45))

;;; (1020 / 39) + (45 * 2)
(+ (/ 1020 39) (* 45 2))

;;; Sum of 39, 48, 72, 23, and 91
(+ 39 48 72 23 91)

;;; Average of 39, 48, 72, 23, and 91 as a floating point.
(/ (+ 39 48 72 23 91) 5.)

;; My answers match those in the tutorial except for the last where exact->inexact is used explicitly

; EOF
