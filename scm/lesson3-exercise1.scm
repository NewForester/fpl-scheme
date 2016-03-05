; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;; Make data structures using cons that the front end prints as:

;;; ("hi" . "everybody")
(cons "hi" "everybody")

;;; (0)
(cons 0 '())

;;; (1 10 . 100)
(cons 1 (cons 10 100))

;;; (1 10 100)
(cons 1 (cons 10 (cons 100 '())))

;;; (#\I "saw" 3 "girls")
(cons #\I (cons "saw" (cons 3 (cons "girls" '()))))

;;; ("Sum of" (1 2 3 4) "is" 10)
(cons "Sum of" (cons '(1 2 3 4) (cons "is" (cons 10 '()))))

;; My answers match those in the tutorial except that I didn't expand '(1 2 3 4).  In practice, no one would write such expressions

; EOF
