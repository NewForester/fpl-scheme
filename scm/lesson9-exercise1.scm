; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; A function returns a list of strings which correspond to each line of a file
(define (read-lines file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop((ls2 '()) (ls1 '()) (c (read-char)))
        (cond
          ((eof-object? c) (reverse ls2))
          ((eqv? c #\Linefeed) (loop (cons (list->string (reverse ls1)) ls2) '() (read-char)))
          (else  (loop ls2 (cons c ls1) (read-char))))))))

;; My answer does in one function what the answer in the tutorial does in two.
;; My function has the same number of lines as the shorter of the tutorial's.
;; The tutorial and I still have a way to go on agreeing on what makes a good program.

; EOF
