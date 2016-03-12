; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;; Modify make-bank-account so that withdrawing more than balance causes error.
(define (make-bank-account balance)
  (lambda (n)
    (begin
      (if (< (+ balance n) 0)
        'error
        (begin
          (set! balance (+ balance n))
          balance)))))

;; Pinched the use of 'error from the tutorial answer (so far not introduced)
;; Once again the tutorial avoids evaluating the same, trivial, expression more than once.

;; Note: 'error is not part of the language.
;; It is a reference to a symbol.  Scheme symbols are introduced in a couple of chapters time.
;; It is a reference to a symbol that may not exist but is created on the fly and becomes the result of evaluating the function.
;; Try replacing it with 'joy or "Oops" and you will see what I mean.

; EOF
