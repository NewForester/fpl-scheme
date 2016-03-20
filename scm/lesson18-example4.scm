; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;-- Simplistic macro implementation of amb

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


;-- The school girls and their exam example

(define (xor a b)
  (if a (not b) b))

(define (all-different? . ls)
  (let loop ((obj (car ls)) (ls (cdr ls)))
    (or (null? ls)
        (and (not (memv obj ls))
             (loop (car ls) (cdr ls))))))

;;; SICP Exercise 4.42
(define (girls-exam)
  (let ((kitty (number-between 1 5))
        (betty (number-between 1 5)))
    (assert (xor (= kitty 2) (= betty 3)))
    (let ((mary (number-between 1 5)))
      (assert (xor (= kitty 2) (= mary 4)))
      (assert (xor (= mary 4) (= betty 1)))
      (let ((ethel (number-between 1 5))
            (joan (number-between 1 5)))
        (assert (xor (= ethel 1) (= joan 2)))
        (assert (xor (= joan 3) (= ethel 5)))
        (assert (all-different? kitty betty ethel joan mary))
        (map list '(kitty betty ethel joan mary) (list kitty betty ethel joan mary))))))

;;; Poor answer for ex 4.42
(define (girls-exam-x)
  (let ((kitty (number-between 1 5))
        (betty (number-between 1 5))
        (mary (number-between 1 5))
        (ethel (number-between 1 5))
        (joan (number-between 1 5)))
    (assert (xor (= kitty 2) (= betty 3)))
    (assert (xor (= kitty 2) (= mary 4)))
    (assert (xor (= mary 4) (= betty 1)))
    (assert (xor (= ethel 1) (= joan 2)))
    (assert (xor (= joan 3) (= ethel 5)))
    (assert (all-different? kitty betty ethel joan mary))
    (map list '(kitty betty ethel joan mary) (list kitty betty ethel joan mary))))

; EOF
