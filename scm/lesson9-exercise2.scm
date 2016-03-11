; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; A function to copy a file
(define (my-copy-file from to)
  (let ((in (open-input-file from)) (out (open-output-file to)))
    (begin
      (let tail ((cc (read-char in)))
        (if (eof-object? cc)
          '()
          (begin
            (write-char cc out)
            (tail (read-char in)))))
    (close-output-port out)
    (close-input-port in)
    #t)))

;; My answer is similar to the answer in the tutorial.
;; Mine has an outer (begin in order to close the files, while the tutorial has an inner (begin.
;; I prefer the tutorial solution.

; EOF
