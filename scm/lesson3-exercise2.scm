; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;; Evaluate the following S-expressions:

;;; (car '(0))  expect 0
(car '(0))

;;; (cdr '(0))  expect ()
(cdr '(0))

;;; (car '((1 2 3) (4 5 6)))    expect (1 2 3)
(car '((1 2 3) (4 5 6)))

;;; (cdr '(1 2 3 . 4))          expect (2 3 . 4)
(cdr '(1 2 3 . 4))

;;; (cdr (cons 3 (cons 2 (cons 1 '()))))        expect (2 1)
(cdr (cons 3 (cons 2 (cons 1 '()))))

;; My guesses were spot on

; EOF
