; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Calculate the inner product of two vectors
(define (vector-inner-product v1 v2)
    (let ((len (vector-length v1)))
      (if (= len (vector-length v2))
        (let loop ((ii 0) (result 0))
          (if (= ii len)
             result
             (loop (1+ ii) (+ result (* (vector-ref v1 ii) (vector-ref v2 ii))))))
        (error "different dimensions."))))

;; My answer takes the same approach as the answer given in the tutorial.
;; Note, this time (error is a function but not one that has been introduced.


; EOF
