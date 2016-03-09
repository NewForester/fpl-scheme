; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; A function that reverses the order of items on a list
(define (reverse-tail sl ls)
  (if (null? ls)
    sl
    (reverse-tail (cons (car ls) sl) (cdr ls))))

(define (my-reverse ls)
  (reverse-tail '() ls))

;;; Summarizing items of a list consisting of numbers
(define (sum-tail sum ls)
  (if (null? ls)
    sum
    (sum-tail (+ sum (car ls)) (cdr ls))))

(define (my-sum ls)
  (sum-tail 0 ls))

;;; Convert a string that represents a positive integer to the corresponding integer, i.e. "1232" → 1232
(define (convert-tail int ls)
  (if (null? ls)
    int
    (let ((digit (- (char->integer (car ls)) (char->integer #\0))))
      (convert-tail (+ (* 10 int) digit) (cdr ls)))))

(define (convert str)
  (let ((ls (string->list str)))
     (convert-tail 0 ls)))

;; My answers have the same form as those in the tutorial but I'd reversed the order of the pairs of functions.

; EOF
