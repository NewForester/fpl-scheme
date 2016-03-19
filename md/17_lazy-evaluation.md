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

    <title>Scheme Notes: Lazy Evaluation</title>
</head>

<body>
# Scheme

## Lazy Evaluation

Reference: [Why Functional Programming Matters](http://www.md.chalmers.se/~rjmh/Papers/whyfp.html).

Haskell is the champion of lazy evaluation.
Scheme has only partial support for the concept and
the examples given imply that it is all about numerical approximation using infinite series.

This is not quite what I was expecting but then I am not from a universe without side affects.

In Scheme, the not-evaluated-yet state is called a `promise`.

The following three operations provide the basis of lazy evaluation in Scheme:

```scheme
    (delay proc)        ; do not evaluate proc but turn it into a promise

    (promise? obj)      ; true if obj is a promise

    (force promise)     ; calculate a value from the promise
```

A promise is not consumed by `force` ... you can call it repeatedly to get successive values.
At this stage a promise looks a lot like a generator.

These primitives can be defined and used to construct the basis for handling infinite sequences:

```scheme
    (lazy-car ls)                       ; car for a lazy list
    (lazy-cdr ls)                       ; cdr for a lazy list
    (lazy-cons obj next)                ; cons for a lazy list
    (lazy-map proc ls ...)              ; map for a lazy list
    (lazy-filter pred ls)               ; filter for a lazy list
    (lazy-ref ls nn)                    ; return nth element of a lazy list
    (lazy-head ls nn                    ; return first n elements of a lazy list
```

All but `lazy-cons` are functions.  `lazy-cons` invokes `(delay ...)` and `lazy-cdr` invokes `(force (cdr ...))`
but the others are all straight forward.

These primitives allow the definition of infinite lists with items in the list only being evaluated as and when required.
How it seems to differ from a generator is that a generator evaluates and returns successive values one at a time
while this lazy evaluation evaluates the first nn values in one go.

Using this as a base, it is simple to define infinite arithmetic and geometric sequences:

```scheme
    ;;;;  sequences

    ;;; infinite sequences represented by a_(n+1) = f(a_n)
    (define (inf-seq a0 f)
      (lazy-cons a0 (inf-seq (f a0) f)))

    ;;; arithmetic sequence
    (define (ari a0 d)
      (inf-seq a0 (lambda (x) (+ x d))))

    ;;; geometric sequence
    (define (geo a0 r)
      (inf-seq a0 (lambda (x) (* x r)))) ; *)

```

A simple example of use:

```scheme
    (define ar1 (ari 1 1))
    ;;Value: ar1

    (lazy-head ar1 10)
    ;;Value 15: (1 2 3 4 5 6 7 8 9 10)

    (lazy-head (lazy-filter even? ar1) 10)
    ;;Value 16: (2 4 6 8 10 12 14 16 18 20)
```

A less straight forward example is the Fibonacci series:

```scheme
    (define fib
      (lazy-cons 1
        (lazy-cons 1
          (lazy-map + fib (lazy-cdr fib)))))
```

I am baffled by this:  to me the parentheses appear to be in the wrong place.

The tutorial then continues with examples of:

  * Newton-Raphson method for finding the square root of n
  * numerical differentiation
  * numerical integration

The second two rely on a 'convergent acceleration function', which, the tutorial claims,
is easier to implement in Scheme than it is in a conventional programming language.

I grasp all the parts used to construct the Newton-Raphson method even though I don't remember the method itself.
Leaving out the 'convergent acceleration function', I grasp what the numerical differentiation and integration examples are doing.
Perhaps I have never met the 'convergent acceleration function'.
I certainly do not see what it is doing.

There seems to be a lot of lazy evaluation.
It is not clear whether, for each iteration, the next term of each of a several infinite series are evaluated or
whether there are infinite series within infinite series each subject to lazy evaluation.

The code ought to make sense (but run with infinite loops) without the lazy evaluation.
Not for me it doesn't.
I suspect this means that I am still not familiar enough with the Scheme programming language to be able reap the benefits of 'simpler to reason about' that is claimed.

---

</body>
</html>
