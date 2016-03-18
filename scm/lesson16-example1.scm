; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Generator that returns leaves of a binary tree one-by-one
(define (leaf-generator tree)
  (let ((return '()))
    (letrec
      ((continue
        (lambda ()
          (let loop ((tree tree))
            (cond
              ((null? tree) 'skip)
              ((pair? tree) (loop (car tree)) (loop (cdr tree)))
              (else
                (call/cc (lambda (lap-to-go)
                  (set! continue (lambda () (lap-to-go 'restart)))
                  (return tree))))))
            (return '()))))
      (lambda ()
        (call/cc
          (lambda (where-to-go)
            (set! return where-to-go)
            (continue)))))))

;;; Define a little binary tree
(define tr '((1 2) (3 (4 5))))

;;; Define a little generator
(define p (leaf-generator tr))

;;; Invoke is with (p)

; EOF
