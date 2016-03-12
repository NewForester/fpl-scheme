<!DOCTYPE html>
<html lang="en-GB">
    <!-- scheme notes by NewForester is licensed under a Creative Commons Attribution-ShareAlike 4.0 International Licence. -->
<head>
    <meta charset="UTF-8" />
    <meta name="description" content="Notes on the Yet Another Scheme Introduction tutorial" />
    <meta name="keywords" content="Scheme" />
    <meta name="author" content="NewForester" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <link rel="stylesheet" href="../styles/style-sheet.css" />

    <title>Scheme Notes: Assignment</title>
</head>

<body>
# Scheme

## Assignment

Scheme is a functional programming language:  it is possible to write non-trivial programs without assignment.
However, assignment is required for continuations and state and is convenient for some algorithms.

The function `set!` can be used to assign a value to a previously declared symbol:

```scheme
    (define var 1)
    (set! var (+ var 10))
```

The exclamation mark `!` is used to warn the reader that the function modifies state.

In Scheme, constructs such as `lambda`, `let` and so on involve the definition of symbols.
The symbols are valid until the end of the construct that defines them.
That is until the closing parenthesis of the `lambda`, `let` and so on.

The symbols are said to have (static) scope or lexical closure.
Lexical closure is considered a good thing.

Internal state can be implemented within a lexical closure by using assignment.

```scheme
    (define bank-account
      (let ((balance 10))       ; initial balance
        (lambda (n)
        (set! balance (+ balance n))
        balance)))

    (bank-account 20)           ; deposit

    (bank-account -25)          ; withdrawal
```

In Scheme, as in other functional programming languages, a function can return another function complete with lexical closure.

```scheme
    (define (make-bank-account balance)
      (lambda (n)
        (set! balance (+ balance n))
        balance))

    (define my-bank-account (make-bank-account 10))     ; create account with initial deposit

    (my-bank-account 50)        ; deposit

    (my-bank-account -50)       ; withdrawal
```

Note that `set!` is restricted:  it cannot assign a value to an S-expression, only a named variable.

Note a goal of functional programming is to avoid side-effects (as these a viewed as difficult to debug).
Assignment (modification of state) and I/O are both side effects.

<hr /><!-- Destructive List Operations -->

The functions `set-car!` and `set-cdr!` may be used to assign values to the `car` and `cdr` parts of a `cons` cell.

They can be used to implement a FIFO queue.
A `cons` holds pointers to the front and back of the queue, which is itself a conventional  list.

```scheme
    (define (make-queue)
      (cons '() '()))

    (define (enqueue! queue obj)
      (let ((lobj (cons obj '())))
        (if (null? (car queue))
          (begin
            (set-car! queue lobj)
            (set-cdr! queue lobj))
          (begin
            (set-cdr! (cdr queue) lobj)
            (set-cdr! queue lobj)))
        (car queue)))

    (define (dequeue! queue)
      (let ((obj (car (car queue))))
        (set-car! queue (cdr (car queue)))
        obj))
```

Note that `set-car!` and `set-cdr!` can assign a value to an S-expression:  there is one example above.

Note:  ordinary lists in Scheme are, in effect, LIFO structures.

</body>
</html>
