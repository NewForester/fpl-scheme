; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Reverse the order of list items.
(define (my-reverse ls)
  (do (
    (result '() (cons (car ls) result))
    (ls ls (cdr ls)))
      ((null? ls) result)))

;;; Sum a list of numbers
(define (my-sum ls)
  (do (
    (result 0 (+ result (car ls)))
    (ls ls (cdr ls)))
      ((null? ls) result)))

;;; Converting a string that represents a positive integer to the corresponding integer, i.e. "1232" → 1232
(define (convert str)
  (let ((zero (char->integer #\0)))
    (do (
      (result 0
        (let ((digit (- (char->integer (car ls)) zero)))
          (+ (* result 10) digit)))
      (ls (string->list str) (cdr ls)))
        ((null? ls) result))))

;; My answers are similar to those in the tutorial but I've chosen the order the parameters differently

; EOF
