; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Filter out the odd numbers in a list)
(define (evens ls)
  (keep-matching-items ls even?))

;;; Filter outt numbers (x) that are not 10 ≤ x ≤ 100
(define (middle ls)
  (delete-matching-items ls (lambda (x) (or (< x 10) (> x 100)))))

;; My answers are those of the tutorial exacept that once again I did use the cool Scheme syntax (<= 10 x 100)

; EOF
