; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; An exercise in generating character pair frequency counts after the example by T.Shido

;; An input is read, analysed and the result written to a second file.
;; What is counted is the frequency counts of pairs of letters.
;; The output is ordered by the first character.
;; For each character is listed the characters that follow it and how many times they do so.
;; The counts are ordered in descending count order.

;;; Predicate - true if cc is one of a list of punctuation characters or a non-printing character
(define (skip-char? cc)
  (or (not (char-graphic? cc)) (memv cc '(#\: #\; #\' #\" #\`))))

;;; Update frequency count alist for character cc
(define (ss-make-alist cc alist)
  (let ((cc-count (assv cc alist)))
    (if cc-count
      (begin
         (set-cdr! cc-count (1+ (cdr cc-count)))
         alist)
      (cons (cons cc 1) alist))))

;;; Read characters from a file and populate the hash table with frequency alists
(define (ss-input char-hash path)
  (with-input-from-file path
    (lambda ()
      (let loop ((c0 #\Space) (c1 (read-char)))
        (if (not (eof-object? c1))
          (if (skip-char? c1)
            (loop c0 (read-char))
            (let ((c1 (char-downcase c1)))
              (hash-table/put! char-hash c0 (ss-make-alist c1 (hash-table/get char-hash c0 '())))
              (loop c1 (read-char)))))))))

;;; Write the hash table with frequency alists to a file
(define (ss-output char-hash path)
  (with-output-to-file path
    (lambda ()
      (display "(define *stat-spell* \'(")
      (let loop
        ((alist (sort
                  (hash-table->alist char-hash)
                  (lambda (x y) (char<? (car x) (car y))))))
        (if (pair? alist)
          (begin
            (write (car alist))
            (newline)
            (loop (cdr alist)))))
      (display "))")
      (newline))))

;;; Generate character frequency counts for the file inpath and write them to outpath
(define (ss-make-dat inpath outpath)
   (let ((char-hash (make-eqv-hash-table)))
     (ss-input char-hash inpath)
     (ss-output char-hash outpath)
     1))

; EOF
