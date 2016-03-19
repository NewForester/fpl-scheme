; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; -----------------------------------------------
;;; Basic functions and a macro for lazy evaluation
;;; -----------------------------------------------

;;; car for lazy evaluation
(define lazy-car car)

;;; cdr for lazy evaluation
(define (lazy-cdr ls)
  (force (cdr ls)))

  ;;; car of the cdr (next approximation)
(define (lazy-second ls)
  (lazy-car (lazy-cdr ls)))

;;; lazy cons
(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

;;; lazy map
(define (lazy-map fn . lss)
  (if (memq '() lss)
    '()
    (lazy-cons (apply fn (map lazy-car lss))
               (apply lazy-map fn (map lazy-cdr lss)))))

;;; ----------------------------
;;; Sequences by lazy evaluation
;;; ----------------------------

;;; infinite sequences represented by a_(n+1) = f(a_n)
(define (inf-seq a0 f)
  (lazy-cons a0 (inf-seq (f a0) f)))

;;; arithmetic sequence
(define (ari a0 d)
  (inf-seq a0 (lambda (x) (+ x d))))

;;; geometric sequence
(define (geo a0 r)
  (inf-seq a0 (lambda (x) (* x r))))

;;; Evaluate the infinite series ls until the ratio of successive terms is less than eps
(define (lazylist->answer ls eps)
  (let ((e1 (- 1.0 eps))
        (e2 (+ 1.0 eps)))
    (let loop ((val (lazy-car ls))
               (ls1 (lazy-cdr ls)))
      (let ((val2 (lazy-car ls1)))
        (if (or (zero? val2) (< e1 (/ val val2) e2))
            (exact->inexact val2)
            (loop val2 (lazy-cdr ls1)))))))

;;; ---------------------
;;; Newton-Raphson method
;;; ---------------------

;;; the infinite series, which is known to converge
(define (newton-raphson n)
  (inf-seq 1 (lambda (x) (/ (+ x (/ n x)) 2))))

;;; top level square root routines
(define (my-sqrt n eps)
  (lazylist->answer (newton-raphson n) eps))

;;; -----------------------------------------
;;; Common to differentiation and integration
;;; -----------------------------------------

; This implements a convergence acceleration algorithm that is unfamiliar.
; I really cannot fathom what the algorithm is from the Scheme source code.

; Furthermore I have no idea what fix:lsh means in terms of the language.
; If it is Scheme syntax then is was mentioned only briefly in the tutorial
; (if at all).  There is nothing in my notes.

;;; eliminate error from the approximation
(define (elimerror n ls)
  (let ((a (lazy-car ls))
        (b (lazy-second ls))
        (c (fix:lsh 1 n)))   ; (expt 2 n)
    (lazy-cons
      (/ (- (* b c) a) (- c 1))
      (elimerror n (lazy-cdr ls)))))

;;;
(define (log2 x)
  (/ (log x) (log 2)))

;;; estimate 'n' in elimerror
(define (order ls)
  (let* ((a (lazy-car ls))
         (b (lazy-second ls))
         (c (lazy-ref ls 2))
         (d (- (/ (- a c) (- b c)) 1.0)))
    (cond
     ((< d 2) 1)
     ((<= 2 d 16) (inexact->exact (round (log2 d))))
     (else 4))))

;;; improve convergence of the lazy list of the approximation
(define (improve ls)
  (elimerror (order ls) ls))

;;; further improve the convergence of the list
(define (super ls)
  (lazy-map lazy-second (inf-seq ls improve)))

;;; --------------------------------
;;; Differentiation by approximation
;;; --------------------------------

;;; Primitive differentiation - assume f(x) linear over the range [x,x+h]
(define (easydiff f x h)
  (/ (- (f (+ x h)) (f x)) h))

;;; Create a lazy list of approximations for differentiation
(define (lazylist-diff h0 f x)
  (lazy-map (lambda (h) (easydiff f x h)) (geo h0 0.5)))


;;; Differentiate function `f' at x to within an error of 'eps' with an initial window h0
(define (diff f x h0 eps)
  (lazylist->answer (super (lazylist-diff h0 f x)) eps))

;;; ----------------------------
;;; Integration by approximation
;;; ----------------------------

;;; Primitive integration - assume f(x) linear over the range [a,b]
(define (easyintegrate f a b)
  (* (/ (+ (f a) (f b)) 2) (- b a)))

;;; Create a lazy list of approximations for the integration
(define (lazylist-integrate f a b)
  (let ((mid (/ (+ a b) 2)))
    (lazy-cons (easyintegrate f a b)
               (lazy-map + (lazylist-integrate f a mid)
                           (lazylist-integrate f mid b)))))

;;; Integrate function `f' in the range [a,b] to within an error of 'eps'
(define (integrate f a b eps)
  (lazylist->answer (super (lazylist-integrate f a b)) eps))

; EOF
