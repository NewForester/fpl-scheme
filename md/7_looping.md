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

    <title>Scheme Notes: Looping and Recursion</title>
</head>

<body>
# Scheme

## Looping and Recursion

This is a difficult topic.

In Scheme, repetition is usually handled by recursion although `do` expressions provide the iterative alternative.

One reason for recursion is the use of lists and a list is essentially a recursive data structure.
Here recursion used to process a list.
It returns a new list where each element is twice that of the original list:

```scheme
    (define (list*2 ls)
      (if (null? ls)
        '()
        (cons (* 2 (car ls))            ; *)
          (list*2 (cdr ls)))))
```

Another simple example:

```scheme
   (define (factorial n)
     (if (= n 1)
       1
       (* n (fact (- n 1)))))           ; *)
```

This arrangement is inefficient.
It maximises stack usage.
The intermediary result is factorial n-1, which seems natural, but means
keeping the stack context of each level until the lowest level is reached.

<hr /><!-- Tail Recursions -->

A technique called tail recursion can be used to avoid this.

```scheme
    (define (fact-tail n)
      (fact-rec n n))

    (define (fact-rec n p)
      (if (= n 1)
        p
        (let ((m (- n 1)))
          (fact-rec m (* p m)))))       ; *)
```

In terms of calculation, the first example does 1 * 2 * 3 ... while this example does n * n-1 * n-2 ...

The recursive call is the very last thing the function does so no stack context need be maintained.
The compiler can turn the recursion into a loop.

This does require a second parameter in lieu of the stack context.
It, inconveniently, requires two routines.

<hr /><!-- Named Tail Recursions -->

The named `let` offers a way of implementing tail recursion within a single routine:

```scheme
    (define (fact-let n)
      (let loop((n1 n) (p n))
        (if (= n1 1)
          p
          (let ((m (- n1 1)))
            (loop m (* p m))))))        ; *)
```

Here the `let` is named 'loop' but the named has no meaning to the compiler.

An alternative is `letrec` (recursive let),
which allows the declaration of a local variable that can refer to itself.
This uses a recursive `lambda` function:  the local variable, in effect, gives the lambda function a local name.

```scheme
    (define (fact-letrec n)
      (letrec (
        (iter (lambda (n1 p)
          (if (= n1 1)
            p
            (let ((m (- n1 1)))
              (iter m (* p m)))))))     ; *)
        (iter n n)))
```

<hr /><!-- Iterative Loops -->

The `do` expression is not used very often.
Some say it is less clear than recursion.

The definition is:

```scheme
    (do binds (predicate value) body)
```

Variables are bound in the 'binds' part and the 'predicate' is evaluated.
If true, the loop terminates and the final result is the result of evaluating 'body'.
Otherwise the variables are rebound and the predicate re-evaluated.

The `binds` part has the form:

```scheme
    ((p1 i1 u1) (p2 i2 u2) ...)
```
where p1 ... are the names of local variables, i1 ... the initial values and u1 ... the update values.

The factorial may be expressed thus:

```scheme
   define (fact-do n)
     (do (
       (n1 n (- n1 1))
       (p n (* p (- n1 1))))            ; *)
       ((= n1 1)
       p)))
```

In this case, the loop is all in the 'binds' part and the 'body' simply returns the result.

</body>
</html>
