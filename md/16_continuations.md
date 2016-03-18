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

    <title>Scheme Notes: Continuations</title>
</head>

<body>
# Scheme

## Continuations

Continuations are a term I am not familiar with not having done Computer Science at school.

The tutorial thinks it is a difficult concept to grasp.
The example I find easiest to grasp is the execution context that the OS stores when switching tasks or processes:
The context that needs to be stored in order to resume (continue) execution of the task or process later is called the continuation.

Continuations may also used for co-routines, generators, callbacks and exception handling.

Scheme recognises a continuation as a data type.
Scheme defines a continuation as the calculation that needs to be performed before returning to the top-level.

It is related to the concept of 'closure' used in other functional languages but I am not sure that they are the same.


<hr /><!-- The Continuation Passing Style -->

The tutorial talks of Continuation Passing Style (CPS).
This first emerged from Scheme in 1975.
It is in contrast to the more usual Direct Passing Style.

Essentially, with CPS, each function has an extra parameter, which is a function that takes a single value.
Functions do not return but rather call this extra parameter, passing it the result they would otherwise return.

Here is a simple example:

```scheme
    (define (return x)
      x)

    (define (k+ a b k)
      (k (+ a b)))

    (define (k* a b k)
      (k (* a b)))      ; *)

    (k+ 1 2 (lambda (x) (k* x 3 return)))

    ; is equivalent to

    (* (+ 1 2) 3)      ; *)
```

The function k+ adds its first two parameters and calls the lambda function with its result.
The lambda function calls k* which multiplies its first two parameters and calls return, which puts us out of our misery.

What is remarked about the CPS is that the expression is inside out with respect to the conventional expression.


<hr /><!-- Recursive functions in CPS -->

An example of recursion using the CPS style:

```scheme
    ;;; normal factorial
    (define (fact n)
      (if (= n 1)
        1
        (* n (fact (- n 1))))) ; *)

    ;;; CPS factorial
    (define (kfact n k)
      (if (= n 1)
        (k 1)
        (kfact (- n 1) (lambda (x) (k (* n x)))))) ; *)

```

CPS is not useful for simple functions and even for complex function it is cumbersome.


<hr /><!-- Exceptions in CPS -->


In a very real sense, with CPS, where to 'return' is passed explicitly to each function.

Exceptions can be programmed by passing two alternative 'return' points, one for success, the other for fail.

```scheme
    (define (non-number-value-error x)
      (display "Value error: ")
      (display  x)
      (display " is not number.")
      (newline)
      'error)


    (define (kproduct ls k k-value-error)
      (let ((break k))
        (let loop ((ls ls) (k k))
          (cond
           ((null? ls) (k 1))
           ((not (number? (car ls))) (k-value-error (car ls)))
           ((zero? (car ls)) (break 0))
           (else (loop (cdr ls) (lambda (x) (k (* (car ls) x)))))))))   ; *)
```

Here the alternative 'return' is `k-value-error` and is called when a non-number is encountered.
The non-number is passed to  `k-value-error`.

Also note that for convenience the initial good 'return' is saved in `break`
allowing an immediate exit with the result 0 should a 0 be encountered.

The tricky part remains the normal execution, which is essentially a sequence of incomplete multiplications that all pancake when the last element of the list is reached.


<hr /><!-- Scheme Continuations -->

Apparently the examples above show that a continuation is also a 'chain of closure'.  What ?

At every stage of execution there existing an implicit continuation.

In Scheme, a continuation is a first class object.
The current continuation may be instantiated by calling `call-with-current-continuation`, which is usually abbreviated:

```scheme
    (define call/cc call-with-current-continuation)
```

This `call-with-current-continuation` function takes a single parameter
that is itself a function that takes a single parameter
that is the current continuation.

Until you are comfortable with continuations, the result may be unexpected.

Consider (the misleading expressions):

```scheme
    (* 3 (call/cc (lambda (k) (+ 1 2))))        ; *) ⇒ 9
    (* 3 (call/cc (lambda (k) (+ 1 (k 2)))))    ; *) ⇒ 6
```

Here `k` is the continuation that is passed to the lambda function.

In the first case, `k` is not used by the lambda function so what is evaluated as `(* 3 (+ 1 2)`,
<!-- *) --> which yields 9.

In the second case, `k` is evaluated and 'returns' 2 to the top level so what is evaluated is `(* 3 2)`,
<!-- *) --> which yields 6.

Since continuations are first class, they may be saved and reused:

```scheme
    (define add4)
      (+ 4 (call/cc (lambda (k)
        (set! add4 k)
        0)))
```

This defines a continuation that can be used to add 4.
Defining the continuation seems to require the evaluation of (+ 4 x),
hence the dummy 0 at the end (because call/cc return nothing ?).

I seriously do not grok this - the parentheses appear wrong to me.

How is this different from an ordinary function that adds 4 ?
It returns to the top level so the evaluation of any enclosing S-expression stops:

```scheme
    (+ 100 (add4 10))                   ; ⇒ 14, not 114
```

This return to the top level is very different from what I read described for Elm.


<hr /><!-- Returning to the Top Level -->

Returning to the top level can be put to good use in recursive functions:

```scheme
    (define (find-leaf obj tree)
      (call/cc
        (lambda (cc)
          (letrec ((iter
            (lambda (tree)
              (cond
                ((null? tree) #f)
                ((pair? tree)
                  (iter (car tree))
                  (iter (cdr tree)))
                (else
                  (if (eqv? obj tree)
                    (cc obj)))))))
            (iter tree)))))
```

This function searches a binary tree for `obj`.
When it find `obj`, `(cc obj)` invokes the continuation established at the start.
This throws `obj`, so returning from the tree search in one action.

Returning to the top level can be put to good use in breaking out of list processing when an error occurs:

```scheme
    ;; macro to enclose a block of code in a continuation
    (define-syntax block
      (syntax-rules ()
        ((_ tag e1 ...)
          (call-with-current-continuation
          (lambda (tag)
            e1 ...)))))

    ;; normally map will return list of square roots
    ;; but when passed a wobbly like -2 in the middle
    ;; this block will return the wobbly value
    (block break
      (map
        (lambda (x)
          (if (positive? x)
            (sqrt x)
            (break x)))
        '(1 -2 3)))
```

Note that here `break` is a name with no significance to the language.


<hr /><!-- Generator Functions -->

A generator returns the next item (of a list) each time it is invoked by creating the item on the fly.
This avoids the possibly expensive up front creation of long lists.

Scheme continuations may be used to implement generators:  with each call the continuation picks up where the last one left off.
Each call involves the creation a new continuation.

Here the leaves of a binary tree are returned one-by-one, one for each call to the function.

```scheme
    (define (leaf-generator tree)
      (let ((return '()))
        (letrec
          ((continue
            (lambda ()
              (let loop ((tree tree))
                (cond
                  ((null? tree) 'skip)
                  ((pair? tree) (loop (car tree)) (loop (cdr tree)))
                  (else
                    (call/cc (lambda (lap-to-go)
                      (set! continue (lambda () (lap-to-go 'restart)))
                      (return tree))))))
                (return '()))))
          (lambda ()
            (call/cc
              (lambda (where-to-go)
                (set! return where-to-go)
                (continue)))))))
```

On first examination, I think that `'skip` and `'restart` are just place-holders because the continuation syntax requires a parameter.
They are symbols with names chosen to be helpful but without a good explanation they are just plain confusing.

On second examination, the continuations are named `return` and `continue`.
The first is the 'found it, get me out of here quick' trick of `find-leaf` example above.
The second is where to resume.  This bit is not obvious at all.

The `letrec` statement initialises `continue` with the 'initial state' continuation and later on the `set!` assigns it a new continuation for next time.
What is at first baffling is how `continue` is not re-initialised next time.
The answer is not here but in the caller:

```scheme
    ;;; Define a little binary tree
    (define tr '((1 2) (3 (4 5))))

    ;;; Define a little generator
    (define p (leaf-generator tr))
```

Here `p` is the generator.
When it is defined, `(leaf-generator tr)` is evaluated and `continue` is initialised.
Later, each time `p` is re-evaluated, `(leaf-generator tr)` is not:  it is just run.
So whatever value `continue` had last time it still current and the whole thing works.
Ouch !


<hr /><!-- Coroutines -->

Coroutines are routines that executed in parallel but one at a time.
Coroutine pass control by yielding the processor rather than calling each other.

Using the FIFO queue from earlier (see chapter 10 - Assignment), here is an implementation of coroutines based on continuations:

```scheme
    (define process-queue (make-queue))

    (define (coroutine thunk)
      ( enqueue! process-queue thunk))

    (define (start)
       ((dequeue! process-queue)))

    (define (pause)
      (call/cc
        (lambda (k)
          (coroutine (lambda () (k #f)))
          (start))))
```

Apparently simple and straight forward.
It is interesting that the dequeue action seems to resume execution implicitly.
It does not ... the extra pair of parentheses evaluate the result of the dequeue action.

Here is a simple example of use:

```scheme
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

    (coroutine (lambda () (eg 10 0)))
    (coroutine (lambda () (eg 10 1)))

    (newline)
    (start)
```

<hr />

</body>
</html>
