; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; A function that counts the number of list items
(define (my-length ls)
  (if (null? ls)
    0
    (+ 1 (my-length (cdr ls)))))

;;; A function that summarizes numbers in a list
(define (summary ls)
  (if (null? ls)
    '()
    (let ((result (summary (cdr ls))))
      (if (index result (car ls))
        result
        (cons (car ls) result)))))

;;; A function that takes a list and an object as arguments and returns a list removing x from ls
(define (remove ls x)
  (if (null? ls)
    '()
    (if (= (car ls) x)
       (remove (cdr ls) x)
       (cons (car ls) (remove (cdr ls) x)))))

;;; A function that takes a list and an object and returns the first position of x in ls
(define (index ls x)
  (cond
    ((null? ls) #f)
    ((= (car ls) x) 0)
    (else
      (let ((result (index (cdr ls) x)))
         (if result
           (+ result 1)
           #f)))))

;; Oh boy!
;; My first answer matches that given in the tutorial.
;; My second answer is to a different question:  in English as I speak it "sum" is not the same a "summarise".
;; My third answer has two evaluations of (remove (cdr ls) while the tutor has only one but introduct two lambda functions,
;;   one of which is the identity to get there.
;; My fourth answer is in one function whereas the answer in the tutorial uses two functoins.

; EOF
