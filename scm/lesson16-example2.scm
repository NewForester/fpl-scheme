; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Need FIFO queue impementation
(load "lesson10-example1.scm")

;;; Standard short hand
(define call/cc call-with-current-continuation)

;;; Generator to run co-routines using a FIFO queue
(define process-queue (make-queue))

(define (coroutine thunk)
  (enqueue! process-queue thunk))

(define (start)
  ((dequeue! process-queue)))

(define (pause)
  (call/cc
    (lambda (k)
      (coroutine (lambda () (k #f)))
      (start))))

;;; Define routines to flash a virutal LED
(define (flash onoff)
  (begin
    (display onoff)
    (display " ")))

(define (eg nn onoff)
  (let loop ((ii 0))
    (if (< ii nn)
      (begin
        (flash onoff)
        (pause)
        (loop (1+ ii))))))

;;; Define a pair of co-routines
(coroutine (lambda () (eg 10 0)))
(coroutine (lambda () (eg 10 1)))

;;; Run the co-routines
(newline)
(start)

; EOF
