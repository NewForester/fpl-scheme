; Exercises and examples from Yet Another Scheme Tutorial
; Copyright (C) 2016 NewForester
; Available under MIT Licence

;;; An exercise in counting words after the example by T.Shido

;; Words are delimited by white space and the common punctuation characters but not apostrophe or quotes.
;; The code produces a list of word + frequency counts in descending frequency order.

;;; Create a symbol from a list of characters (in reverse order as it happens)
(define (list->symbol ls)
  (string->symbol (string-downcase (list->string (reverse! ls)))))

;;; Predicate that is true if the first argument is repeated somewhere in the remaining arguments
(define (char-in? c . ls)
  (let loop ((ls0 ls))
    (if (null? ls0)
      #f
      (or (char=? c (car ls0))
        (loop (cdr ls0))))))

;;; Read a word from file and return is as a symbol
(define (read-word)
  (let loop ((w '()))
    (let ((c (read-char)))
      (cond
        ((eof-object? c)
          (if (pair? w)
            (list->symbol w)
            c))
        ((char-in? c #\Space #\Linefeed #\Tab #\, #\.  #\ #\( #\) #\= #\? #\! #\; #\:)
          (if (pair? w)
            (list->symbol w)
            (loop '())))
        (else
            (loop (cons c w)))))))

;;; Return word list sorted by frequency
(define (sort-by-frequency ls)
  (sort ls (lambda (x y) (> (cdr x) (cdr y)))))

;;; Generate word count from file ordered by word frequency
(define (wc fname)
  (let ((word-hash (make-eq-hash-table)))
    (with-input-from-file fname
      (lambda ()
        (let loop ((word (read-word)))
          (if (eof-object? word)
            (sort-by-frequency (hash-table->alist word-hash))
            (begin
              (hash-table/put! word-hash word (1+ (hash-table/get word-hash word 0)))
              (loop (read-word)))))))))

;; My example read a word at a time and adds it to the hash table.
;; The tutorial's example read the entire file producing a list of words that were then added to the hash table.
;; For large files, my example will use less memory.

; EOF
