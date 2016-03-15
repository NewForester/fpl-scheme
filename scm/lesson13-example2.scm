; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; Password generateor after the  the example by T.Shido

;; The function pw-candidates will generate 10 hard-to-crack passwords:
;;   - a random string of between 9 and 13 characters
;;   - spaces in the string are randomly converted to a punctuation character
;;   - a space may be randomly converted to a space
;;   - to which a two digit random number is appended
;;
;; The string between 9 and 13 character is generated from the letter pair frequency counts
;; created by `ss-make-dat` of first example (lesson13example1.scm).
;; Each character in the string is used to generate the next.  The seed is always #\Space.
;;
;; The generation process take a character and selects from that character's frequency pairs.
;; The selection is random but is weighted by the frequency counts.
;; Thus it favours pairs of characters than occurred most often in the original text.

;;; Convert an association list to to a hash table (general purpose)
(define (alist->hash al mode)
  (let ((h-table (case mode
             ((eq) (make-eq-hash-table))
             ((eqv) (make-eqv-hash-table))
             ((equal) (make-equal-hash-table))
             ((string) (make-string-hash-table)))))
    (for-each (lambda (p)
                (hash-table/put! h-table (car p) (cdr p)))
              al)
    h-table))

;;; Select at random from a vector
(define (pw-random-select vec)
  (vector-ref vec (random (vector-length vec))))

;;; Select a pair of digits at random
(define (random00)
  (let loop ((ii 0) (acc '()))
    (if (= ii 2)
      (list->string acc)
      (loop (1+ ii) (cons (pw-random-select '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) acc)))))

;;; Randomly convert character to upper case
(define (occasional-upcase cc)
  (if (< (random 10) 3)
    (char-upcase cc)
    cc))

;;; Reverse and convert a list to a string but apply password enhancment to the individual characters
(define (pw-enhance ls)
  (list->string
    (map
      (lambda (cc)
        (cond
          ((char=? cc #\Space)
            (pw-random-select '#(#\- #\_ #\/  #\Space  #\. #\, #\@ #\? #\( #\))))
          ((char-alphabetic? cc)
            (occasional-upcase cc))
          (else cc)))
      (cdr (reverse! ls)))))

;;; Choose at random from an alist - this is the tricky bit
(define (random-following alist)
  (let ((nn (random (apply + (map cdr alist)))))
    (let loop ((jj 0) (alist alist))
      (if (pair? alist)
          (let* ((pair (car alist))
                 (kk (+ jj (cdr pair))))
            (if (> kk nn)
                (car pair)
                (loop kk (cdr alist))))))))

;;; Generate a password - select n character at random, enhance it and append a pair of digits
(define (make-pw h-table nn)
  (let loop ((ii 0) (cc #\Space) (acc '()))
    (if (= ii nn)
        (string-append
         (pw-enhance (cons #\Space (cons cc acc)))
         (random00))
      (loop (1+ ii)
        (random-following (hash-table/get h-table cc '((#\Space . 1))))
        (cons cc acc)))))

;;; Print 10 password suggestion picked at random from *stat-spell-hash*
(define (pw-candidates source-data)
  (begin
    (load source-data)
    (let loop ((ii 0))
      (if (< ii 10)
        (begin
          (display ii)
          (display ": ")
          (write (make-pw (alist->hash *stat-spell* 'eqv) (+ 9 (random 4))))
          (newline)
          (loop (1+ ii)))
        'done))))

(pw-candidates "stat-spell.dat")

; EOF
