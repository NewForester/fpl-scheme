; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Here is my reformatting of the tutorial's answer to lesson 8, exercise 6, question 2.

;; The answer introduces for the first time in the tutorial the . notation in a function definition parameter list
;; that turns all arguments to its right into a list.

;; The answer also introduces the `memq` function.  This apparently looks to see if the first argument is a member of
;; the second.  The second is a list.  The shortening of the name member to `memq` says use `eq?` for comparison.

(define (their-map fun . lss)
  (letrec (
    (iter (lambda (fun lss)
      (if (null? lss)
        '()
        (cons (fun (car lss))
          (iter fun (cdr lss))))))
    (map-rec (lambda (fun lss)
      (if (memq '() lss)
        '()
        (cons
          (apply fun (iter car lss))
          (map-rec fun (iter cdr lss)))))))
    (map-rec fun lss)))

;; The first thing to note is that `letrec` is used to defined two, not one, lambda functions and give them names.
;; The second thing to note is that `car` and `cdr` are passed as functions to `iter` by `map-rec`.

;; Remember, `lss` is a list of lists, so `(car lss)` is the first list and `(car (car lss))` is the first value of the first list.

;; The lines
;;
;;   (cons
;;     (apply fun (iter car lss))
;;     (map-rec fun (iter cdr lss)))))))
;;
;; are in four parts:
;;
;;   (iter car lss)
;;
;; call `iter` to construct and return a list comprising the first item of each list in `lss`.
;; That's the knot.  It is now easy to see that:
;;
;;   (apply fun (...))
;;
;; applies the function to the list returned, giving the first item of the result.
;; It is also clear that:
;;
;;   (iter cdr lss)
;;
;; returns the `lss` less the first item of each list in `lss`.
;; And lastly:
;;
;;   (cons (apply fun (...)) (map-rec fun (...)))
;;
;; recursively builds the answer.
;;
;; There is a lot of list construction going on.
;; If there are m lists each of length n, there are:
;;
;;   (* (- n 1) m) calls to `fun` but
;;   (* 2 (+ (* (- n 1) m)) (- m 1)) calls to `cons`
;;
;; so I suspect the proper Scheme implementation avoids the construction of intermediary lists.

; EOF
