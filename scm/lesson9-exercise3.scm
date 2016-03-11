; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; A function that takes arbitrary number of strings as arguments and outputs them to the standard output.
(define (print-lines . ls)
    (let tail ((ls ls))
      (if (null? ls)
        '()
        (begin
          (display (car ls))
          (newline)
          (tail (cdr ls))))))

;; My solution takes the same approach as that given in the tutorial
;; I've stuck to using (null ? ls) whereas the tutorial uses (pair ?),
;; perhaps to emphasise the `. ls` parameter form as opposed to `ls` ?

; EOF
