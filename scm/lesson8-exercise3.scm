; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Function that squares each item of a list, sums them and then takes the square root
(define (sigma ls)
    (sqrt (reduce + 0 (map * ls ls))))

;; My answer uses ls twice whereas the answer in the tutorial uses a lambda function


; EOF
