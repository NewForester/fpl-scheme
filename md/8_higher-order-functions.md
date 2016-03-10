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

    <title>Scheme Notes: Higher Order Functions</title>
</head>

<body>
# Scheme

## Higher Order Functions

Higher Order Functions are functions that take functions as arguments or return them.

Higher order functions may be used to promote modularity.
For example, a sort operation involves the data to be sorted, the sort algorithm (e.g. quick sort)
and the ordering function.

Separating the ordering function from the sort algorithm is an example of modularity
that is possible with higher order functions.
It also improves readability since recursive algorithms are called, instead of implemented, many times.

Scheme does not distinguish between values and functions (or data structures and procedures),
which means you can create your own higher order functions by simply passing functions to other functions.
This is what is meant by 'functions are first class citizens'.

Higher order functions are a integral part of the language.
Scheme does not have a block structured syntax.
Programmers tend to use lambda expressions as blocks.
These 'anonymous' functions and also, in effect, higher order functions.

<hr /><!-- Mapping -->

R<sub>5</sub>RS defines two mapping functions:

  1. `map`      - generates a list from one or more lists
  1. `for-each` - generates side effects of each item in a list

<!-- -->
The general form of the `map` function is:

```scheme
    (map procedure list1 list2 ...)
```

The `procedure` is a symbol bound to a function or a lambda expression.
There will be a list argument for each parameter of `procedure`.

<!-- -->

```scheme
    (map + '(1 2 3) '(4 5 6))
```

will produce a list of sums.

<!-- -->
```scheme
    (map (lambda (x) (* x x)) '(1 2 3)) ; *)
```

will produce a list of squares.

<!-- -->
The general form of the `for-each` function is the same as for `map` but it does not return a value.
It is typically be used for output or other side effects.

<hr /><!-- Filtering -->

MIT-Scheme defines two filter functions:

  1. `keep-matching-items`      - retain list items that satisfy a predicate
  1. `delete-matching-items`    - retain list items that do not satisfy a predicate

These functions take a list and a predicate as first and second arguments.

<!-- -->
```scheme
    (keep-matching-items '(1 2 -3 -4 5) positive?)
```

<hr /><!-- Folding -->

MIT-Scheme defines two folding functions:

  1. `reduce`   - `(reduce proc acc list)`
  1. `apply`    - `(apply proc [int ...] list)`

<!-- -->
The classic use of `reduce` is to sum the elements in a list:

```scheme
    (reduce + 0 '(1 2 3 4))     ; yields 10
```

The initial value for the accumulator is zero.

The classic use of `apply` is to pass a list to a function that expects an atom sequence:

```scheme
    (apply max 6 7 '(10 9 8))      ; yields 10
```

<hr /><!-- Sorting -->

MIT-Scheme provides two sorting functions:

  1. `sort`         - an implementation of the merge sort algorithm
  1. `quick-sort`   - an implementation of the quick sort algorithm

<!-- -->
```scheme
    (sort '(3 5 1 4 -1) <)
```

These functions take a list and a predicate as first and second arguments.

<hr /><!-- Making Higher Order Functions -->

Here is a quick example of a code-it-yourself higher order function that returns a sub-list
starting with the first element that satisfies the predicate:

```scheme
    (define (member-if predicate ls)
      (cond
        ((null? ls) #f)
        ((predicate (car ls)) ls)
        (else (member-if predicate (cdr ls)))))
```

<!-- -->
Here is another that returns a sub-list of `ls` that starts with the first element that matches the criterion `obj` according to the condition `proc`:

```scheme
    (define (member proc obj ls)
      (cond
      ((null? ls) #f)
      ((proc obj (car ls)) ls)
      (else (member proc obj (cdr ls)))))
```

<!-- -->
The solution to the second question of exercise 6 is important for it introduces another trick.
It involves transposing lists, something that at first glance was not obvious.
My notes on the solution are in [lesson8-example6.scm](../scm/lesson8-example6.scm)

</body>
</html>
