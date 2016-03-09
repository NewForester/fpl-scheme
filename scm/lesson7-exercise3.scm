; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; A function that takes a list and an object as arguments and returns a list removing x from ls
(define (remove ls x)
  (let tail ((result '()) (ls ls))
    (if (null? ls)
      (reverse result)
      (let ((y (car ls)))
        (tail
          (if (eqv? y x) result (cons y result))
          (cdr ls))))))

;;; A function that takes a list and an object and returns the first position of x in ls
;;; The position is counted from 0. If x is not found in ls, the function returns #f
(define (index ls x)
  (let tail ((result 0) (ls ls))
    (cond
      ((null? ls) #f)
      ((eqv? (car ls) x) result)
      (else (tail (+ result 1) (cdr ls))))))

;;; Reverse the order of list items
(define (my-reverse ls)
  (let tail ((result '()) (ls ls))
    (if (null? ls)
      result
      (tail (cons (car ls) result) (cdr ls)))))

;;; Sum a list of numbers
(define (my-sum ls)
  (let tail ((result 0) (ls ls))
    (if (null? ls)
      result
      (tail (+ result (car ls)) (cdr ls)))))

;;; Convert a string that represents a positive integer to the corresponding integer, i.e. "1232" → 1232
(define (convert str)
  (let ((zero (char->integer #\0)))
    (let tail ((result 0) (ls (string->list str)))
      (if (null? ls)
        result
        (let ((digit (- (char->integer (car ls)) zero)))
          (tail (+ (* result 10) digit) (cdr ls)))))))

;;; A function that returns a list of numbers from 0 to n (not including n)
(define (range n)
  (let tail ((result '()) (n (- n 1)))
    (if (< n 0)
      result
      (tail (cons n result) (- n 1)))))

;; My answers are, for all intents and purposes, the same as those in the tutorials except for the last.
;; For the last, I count down, not up, so avoid having to reverse the list.

; EOF
