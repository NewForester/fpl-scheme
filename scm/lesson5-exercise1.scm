; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Function to return the absolute value of a real number
(define (abs x)
  (if (>= x 0) x (- x)))

;;; Function to return the reciprocal of a real number
(define (reciprocal x)
  (if (= x 0) #f (/ 1 x)))

;;; Function to convert an integer to a graphical character
(define (int2char nn)
  (if (or (< nn 33) (> nn 126)) #f (integer->char nn)))

;; My first two answers differ from the tutorial answers for they do not use predicates introduced later on.
;; My final answer fails to use the cool Scheme syntax (<= 33 n 126).

; EOF
