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

    <title>Scheme Notes: Defining Syntax</title>
</head>

<body>
# Scheme

## Defining Syntax

The proposition here is that Scheme offers a very powerful macro facility that allows programmers to define their own syntax.

A macro may be thought of as a transformation of source code before it is evaluated or compiled.

A good number of the operations already described are, in fact, macros, not functions.

In Scheme, simple macros may be defined using `syntax-rules`, which is defined R<sub>5</sub>RS.
This mechanism is easier than the equivalent in Common Lisp for simple macros but a lot harder for complex ones.

A simple example:

```scheme
    (define-syntax nil!
      (syntax-rules ()
        ((_ x)
          (set! x '()))))
```

The second formal parameter of `syntax-rules` is a list of pairs of 'before' and 'after' expressions with '_' standing for the name of the macro.

What the simple example transforms:

```scheme
    (nil! x)                            ; before transformation

    (set! x '())                        ; after transformation
```

The macro `nil!` cannot be written as a function because altering the value of an actual parameter is a side affect that functions are not permitted.

Another simple example:

```scheme
    (define-syntax when
      (syntax-rules ()
        ((_ predicate b1 ...)
          (if predicate (begin b1 ...)))))
```

Here ... represents an arbitrary number of expressions.

The tutorial claims `when` cannot be written as a function because `if` is a special form but does not explain why.
I think it is this.  `if` is a special form that deliberately does not evaluate all its arguments.
The macro does not evaluate any arguments but a function evaluates them all.

Here are two more potentially useful macros:

```scheme
    (define-syntax while
      (syntax-rules ()
        ((_ predicate b1 ...)
          (let loop () (if predicate (begin b1 ... (loop)))))))

    (define-syntax for
      (syntax-rules ()
        ((_ (ii from to) b1 ...)
          (let loop((ii from)) (if (< ii to) (begin b1 ... (loop (+ ii 1))))))))
```

Here b1 ... are evaluated for each iteration, whereas, with a function, they would be evaluated once before the call.

<hr />

The second formal parameter of `syntax-rules` is just the first of any arbitrary number of substitutions.
The macro substitutes the first matching pattern:

```scheme
    (define-syntax inc
      (syntax-rules ()
        ((_ x) (begin (set! x (+ x 1)) x))
        ((_ x ii) (begin (set! x (+ x ii)) x))))

    (inc ii)            ; increment ii (by 1)
    (inc ii jj)         ; increment ii by jj)
```

Macros can substitute recursive code and that code may involve recursive macro substitution.

```scheme
    (define-syntax my-and
      (syntax-rules ()
        ((_) #t)
        ((_ e) e)
        ((_ e1 e2 ...)
          (if e1
            (my-and e2 ...)
            #f))))

    (define-syntax my-or
      (syntax-rules ()
        ((_) #f)
        ((_ e) e)
        ((_ e1 e2 ...)
          (if e1
            #t
            (my-or e2 ...)))))
```

The `and` and `or` of Scheme have similar definitions.

The first formal parameter of `syntax-rules` is a list of 'reserved words'.

```scheme
    (define-syntax my-cond
      (syntax-rules (else)
        ((_ (else e1 ...))
         (begin e1 ...))
        ((_ (e1 e2 ...))
         (if e1 (begin e2 ...)))
        ((_ (e1 e2 ...) c1 ...)
         (if e1 (begin e2 ...) (my-cond c1 ...)))))
```

This appears to mean 'words' in the 'before' expression that are not to be substituted.

<hr />

Scheme also has `let-syntax` and `letrec-syntax` whose use is similar to `define-syntax` but no examples are given.

Scheme also has `sc-macro-transformer` available for more complex macros.
This provides a mechanism similar to that of Common Lisp.

The tutorial example suggests that this is the basis of the implementation of structures
and the automatic generation of accessors and setters.

<hr />

</body>
</html>
