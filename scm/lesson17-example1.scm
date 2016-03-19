; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Basic functions and a macro for lazy evaluation

;;; car for lazy evaluation
(define lazy-car car)

;;; cdr for lazy evaluation
(define (lazy-cdr ls)
  (force (cdr ls)))

;;; lazy cons
(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

;;; lazy map
(define (lazy-map fn . lss)
  (if (memq '() lss)
    '()
    (lazy-cons
      (apply fn (map lazy-car lss))
      (apply lazy-map fn (map lazy-cdr lss)))))

;;; lazy filter
(define (lazy-filter pred ls)
  (if (null? ls)
    '()
    (let ((obj (lazy-car ls)))
      (if (pred obj)
        (lazy-cons obj (lazy-filter pred (lazy-cdr ls)))
        (lazy-filter pred (lazy-cdr ls))))))

;;; returns n-th item of the lazy list
(define (lazy-ref ls nn)
  (if (= nn 0)
    (lazy-car ls)
    (lazy-ref (lazy-cdr ls) (- nn 1))))

;;; returns first n items of the ls
(define (lazy-head ls nn)
  (if (= nn 0)
    '()
    (cons (lazy-car ls) (lazy-head (lazy-cdr ls) (- nn 1)))))

;;; Fibonacci series generated using lazy evaluation
(define fib
  (lazy-cons 1
    (lazy-cons 1
      (lazy-map + fib (lazy-cdr fib)))))


; EOF
