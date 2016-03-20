; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Abbreviation for call-with-current-continuation
(define call/cc call-with-current-continuation)

;;; This function is re-assigned in `choose' and `fail' itself.
(define fail #f)

;;; Nondeterminism macro operator
(define-syntax amb
  (syntax-rules ()
    ((_) (fail))
    ((_ a) a)
    ((_ a b ...)
      (let ((fail0 fail))
        (call/cc
          (lambda (cc)
            (set! fail
              (lambda ()
                (set! fail fail0)
                (cc (amb b ...))))
            (cc a)))))))

;;; write following at the end of file
;;; initial value for fail
(call/cc
  (lambda (cc)
    (set! fail
      (lambda ()
        (cc 'no-choice)))))


;-- 4 utilities for logical programming

;;; Return a list of all possibilities
(define-syntax set-of
  (syntax-rules ()
    ((_ s)
      (let ((acc '()))
        (amb
          (let ((v s))
            (set! acc (cons v acc))
            (fail))
          (reverse! acc))))))

;;; If not pred backtrack
(define (assert pred)
  (or pred (amb)))

;;; Returns arbitrary number larger or equal to nn
(define (an-integer-starting-from nn)
  (amb nn (an-integer-starting-from (1+ nn))))

;;; Returns arbitrary number between an and bn
(define (number-between an bn)
  (let loop ((ii an))
    (if (> ii b)n
      (amb)
      (amb ii (loop (1+ ii))))))


;-- A Prime Number Generator

;;; Is number prime ?
(define (prime? nn)
  (let ((mm (sqrt nn)))
    (let loop ((ii 2))
      (or
        (< mm ii)
        (and
          (not (zero? (modulo nn ii)))
          (loop (+ ii (if (= ii 2) 1 2))))))))

;;; Generate a prime number less than nn
(define (gen-prime nn)
  (let ((ii (number-between 2 nn)))
    (assert (prime? ii))
    ii))

;;; Generate the set of prime number less than 20
(set-of (gen-prime 20))

;-- The original problem

(let ((i (amb 4 6 7))
      (j (amb 5 8 11)))
  (if (prime? (+ i j))
      (list i j)
      (amb)))

; EOF
