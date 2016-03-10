; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Define keep-matching-items yourself
(define (my-keep-matching ls predicate?)
  (let tail ((result '()) (ls ls))
    (if (null? ls)
      (reverse result)
      (let ((y (car ls)))
        (tail
          (if (not (predicate? y)) result (cons y result))
          (cdr ls))))))

;;; Define map yourself.  Accepting arguments of more than one list may be difficult.
(define (my-map fn ls)
  (let tail ((result '()) (ls ls))
    (if (null? ls)
      (reverse result)
      (tail (cons (fn (car ls)) result) (cdr ls)))))

;; My version of my-keep-matching uses tail recurison using named let and has only one call to itself;
;; the answer in the tutorial has two recursive calls, which is the reverse of previous exercises.

;; My version of my-map only works for one list and monadic functions; the answer in the tutorial introduces
;; a new use of . (dot) notation and the function memq, which is not described.

; EOF
