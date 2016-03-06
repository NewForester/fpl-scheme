; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;; Build a function that calculates the flying distance of a projectile.

;;; The constant pi
(define pi (* 4 (atan 1.0)))

;;; Convert degrees to radians
(define (degrees_to_radians degrees)
  (/ (* degrees pi) 180))

;;; Convert radians to degrees

(define (radians_to_degrees radians)
  (/ (* radians 180) pi))

;;; Calculate the distance moved in t seconds at a constant velocity vx
(define (distance vx t)
  (* vx t))

;;; Calculate time of descent of an object launched with vertical velocity of vy
(define g 9.8)

(define (terminal_velocity t)
  (/ (* t g) 2))

(define (flight_time vy)
  (/ (* 2 vy) g))

;;; Calculate the flying distance of a ball thrown with an initial velocity v and an angle theta degrees
(define (flying_distance v theta)
  (distance
    (* v (cos (degrees_to_radians theta)))
    (flight_time (* v (sin (degrees_to_radians theta))))))

;;; Calculate the distance of a ball thrown with a initial velocity of 40 m s-1 and an angle of 30 degree.
(flying_distance 40 30)

;; My answers are the same as the tutorial answers but I was rather told what to do.
;; A good example of building from the ground up.  Choosing names gets hard with a busy function namespace.

; EOF
