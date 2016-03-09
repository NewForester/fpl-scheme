; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Reverse the order of items in a list
(define (my-reverse ls)
  (letrec
    ((tail (lambda (result ls)
      (if (null? ls)
        result
        (tail (cons (car ls) result) (cdr ls))))))
    (tail '() ls)))

;;; Sum a list of numbers
(define (my-sum ls)
  (letrec
    ((tail (lambda (result ls)
      (if (null? ls)
        result
        (tail (+ result (car ls)) (cdr ls))))))
    (tail 0 ls)))

;;; Converting a string that represents a positive integer to the corresponding integer, i.e. "1232" → 1232
(define (convert str)
  (let ((zero (char->integer #\0)))
    (letrec
      ((tail (lambda (result ls)
        (if (null? ls)
          result
          (let ((digit (- (char->integer (car ls)) zero)))
            (tail (+ (* result 10) digit) (cdr ls)))))))
      (tail 0 (string->list str)))))

;; My answers are the same as those in the tutorial apart from the order of parameters, which is about the only difference you could have.

; EOF
