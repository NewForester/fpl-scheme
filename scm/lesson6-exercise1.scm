; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Calculate the flying distance given initial velocity and angle to the surface

(define (flying_distance velocity theta)
  (let* (
    (pi (* 4 (atan 1.0)))
    (radians (/ (* theta pi) 180))
    (flight_time ((/ (* 2 velocity (sin radians)) 9.8))))
    (* velocity (cos radians) flight_time)))

;; My answer calculates three intermediary results in an effort to express the solution clearly in terms of the problem.
;; The answer in the tutorial has only one local variable in order to avoid repeating a common expression.

; EOF
