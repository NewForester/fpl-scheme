; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; My definition of the let* macro
(define-syntax my-let*
  (syntax-rules ()
    ((_ (var) exp1 ...)
      (let (var) exp1 ...))
    ((_ (var1 var2 ...) exp1 ...)
      (let (var1) (my-let* (var2 ...) exp1 ...))
    )))

;; My answer has the same form as that given in the tutorial but it uses (p v) where I have just var.

; EOF
