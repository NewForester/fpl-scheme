; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Examination grades determined by score
(define (grade score)
  (cond
    ((>= score 80) #\A)
    ((>= score 60) #\B)
    ((>= score 40) #\C)
    (else #\D)))

;; My answer relies on the order of predicate evaluation whereas the tutorial answer explictly uses full range conditions e.g. (<= 60 score 79).

; EOF
