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

    <title>Scheme Notes: Making Lists</title>
</head>

<body>
# Scheme

## Making Lists

At heart, Scheme is Lisp and so a list processor.

<hr /><!-- Cons cells --->

The fundamental building block of Lisp list is the `cons` cell (for construction cell).

It consists of two pointers, the, `car` and `cdr`,
whose names originate with a hardware construct of the machine on which the first Lisp processor was built.

The `car` references the value of this cell, while the `cdr` points to the next cell of a list.
The term beaded references to cells referencing other cells.

At end of a string of beaded cells, the `cdr` may also address a value and, naturally,
any `car` may reference another string of beaded cells.

Values that are not themselves cells are called atoms.
Numbers, characters, string and vectors are all atoms as well as an empty list.

The syntax for a cons cell is:

```scheme
    (cons 1 2)
```

which Scheme prints as:

```scheme
    (1 . 2)
```

The beading of a pair of cons cells might look like:

```scheme
    (cons 3 (cons 1 2))
```

A list is a string of beaded `const` cells where the `cdr` part of the last cell is '(), the empty list.

The definition is recursive:

  1. '() is a list
  1. if ls is a list and obj is data of any kind, then (cons obj ls) is a list

This makes list essentially LIFO data structures.


<hr /><!-- Aside on Quote and Special Forms --->

Why the apostrophe in:

```scheme
    '()                         ; represents an empty list
    '(1 2 3)                    ; represents the list 1 2 3
```

Scheme evaluation rules mean that it tries to evaluate whatever occurs between parentheses.
The number 1 evaluates to 1 so you get what you want but (1 2 3) looks to Scheme like a request to apply a function
named 1 to the arguments 2 and 3.

To stop this happening you need to quote the parenthesised expression:

```scheme
    (quote ())                  ; represents an empty list
    (quote (1 2 3))             ; represents the list 1 2 3
```

The apostrophe is simply shorthand for a very common expression.

Also note that `quote` is one a limited number of 'special forms' that are used like functions but which aren't
because they do not evaluate all their arguments.
Most of the time the distinction is not obvious but the special forms are the Scheme analogue of reserved keywords in other languages.

<hr /><!-- List Functions --->

The `list` function will take an arbitrary number of arguments and return them as a list:

```scheme
    (list 1 2 3 4)              ; a list of the first four natural number
    '(1 2 3 4)                  ; ditto - the usual way of entering a list

    (list)                      ; an empty list
    '()                         ; ditto - the usual way of denoting an empty list

    (list '(1 2) '(3 4))        ; a list comprising two lists of two number each
```

The `car` and `cdr` functions return the two parts of a cons cell.

```scheme
    (car '(1 2 3 4))            ; returns 1

    (cdr '(1 2 3 4))            ; return (2 3 4)
```

These functions are important for list traversal

</body>
</html>
