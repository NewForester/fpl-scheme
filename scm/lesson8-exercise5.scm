; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; A function that squares each item of a list, sums them and then takes the square root using apply.
(define (sigma ls)
    (sqrt (apply + (map * ls ls))))

;; As before, my answer uses ls twice whereas the answer in the tutorial uses a lambda function

; EOF
