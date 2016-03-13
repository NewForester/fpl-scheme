; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;; Capitalise the first character of each word
(define (title-style str)
  (let ((len (string-length str)) (promote #t))
    (let loop ((ii 0) (promote #t))
      (if (= ii len)
        str
        (let ((cc (string-ref str ii)))
          (if promote (string-set! str ii (char-upcase cc)))
          (loop (+ ii 1) (char-whitespace? cc)))))))

(title-style "the cathedral and the bazaar")

;; My answer is essentially the second approach given in the tutorial
;; I gave up trying the first approach.

; EOF
