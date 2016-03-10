; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Sort by magnitude of sin(x) in ascending order
(define (sort-by-magnitude ls)
  (sort ls (lambda (x y) (< (sin x) (sin y)))))

;;; Sort by length of list in descending order
(define (sort-by-length list-ls)
  (sort list-ls (lambda (x y) (> (length x) (length y)))))

;; My answers are the same as those given in the tutorial

; EOF
