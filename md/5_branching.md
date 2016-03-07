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

    <title>Scheme Notes: Conditional Evaluation</title>
</head>

<body>
# Scheme

## Conditional Evaluation

In Scheme, the Boolean values true and false are represented by `#t` and `#f`.

<hr /><!-- Conditional Expressions -->

The `if` special form is the primary mean of achieving conditional evaluation.

```scheme
    (if predicate then_value else_value)
```

The predicate is evaluated:

  1. if false, the else_value is evaluated
  1. otherwise the result is taken to be true and the then_value is evaluated
  1. only one of then_value and else_value is evaluated
  1. the result of the `if` expression is the result of whichever is evaluated
  1. each is a single S-expression
  1. the else_value may be omitted but then the `if` expression result may be undefined
  1. if you need the result to be false then use an explicit `#f`


Note, in MIT-Scheme, `#f` and `'()` (the empty list) are both false but
do not rely on this as under R<sub>5</sub>RS they are not.
Use the `null?` predicate instead (see below).

The `not` function may be used to negate predicates:

```scheme
    (if (not predicate) then_value else_value)
```

The `and` and `or` expressions (special forms ?) may be used to combine predicate conditions.

 1. each takes an arbitrary number of arguments
 1. an `and` expression returns `#f` if one of its arguments evaluates to false otherwise is returns the value of its last argument.
 1. an `or` expression returns the value of the first argument that does not evaluate to false or `#f` is they all do.
 1. arguments are evaluated left to right until a result is obtained.

The `cond` expression generalises branching:

```scheme
    (cond
      (predicate_1 clause_1)
      (predicate_2 clause_2)
           ...
      (predicate_n clause_n)
      (else  clause_else))
```

 1. predicates are evaluated top to bottom until one does not evaluate to false or the `else` predicate is reached
 1. the corresponding clause is then evaluated
 1. a clause may consist of more than one S-expression
 1. the result of the `cond` expression is the result of evaluating the last S-expression of the clause.

<hr /><!-- Predicates -->

Scheme has rich set of predicates.  All have names that end in `?`.

```scheme
    (eq? a b)           ; true iff a and b are the same object
    (eqv? a b)          ; true iff a and b are of the same type and have the same atomic value
    (equal? a b)        ; true iff a and b are equal lists or strings (sequences)
```

Predicates that check data types are:

```scheme
    (pair? x)           ; true if x is one or more a `cons` cells
    (list? x)           ; true if x is a list - '() is a list but not a pair
    (null? x)           ; true if x is the empty list '()
    (symbol? x)         ; true if x is a symbol
    (char? x)           ; true if x is a character
    (string? x)         ; true if x is a string
    (number? x)         ; true if x is any kind of number
    (complex? x)        ; true if x is a complex number
    (real? x)           ; true if x is a real number
    (rational? x)       ; true if x is a rational number
    (integer? x)        ; true if x is an integer
    (exact? x)          ; true if x is not a floating point number
    (inexact? x)        ; true if x is a floating pointer number
```

Presumably a number can be both complex and rational.

The predicates that compare numbers are: `=`, `<`, `>`, `<=`, `>=`.
These take an arbitrary number of arguments and return `#t` if the arguments are suitably ordered.
Wow.  They do what you would expect.

```scheme
    (> 1 -0 -1)
    ;Value: #t
```

How this works for complex numbers is not said.

These predicates take only one parameter:

```scheme
    (odd? x)            ; true if x is an odd number
    (even? x)           ; true if x is an even number
    (positive? x)       ; true if x is greater than 0
    (negative? x)       ; true if x is less than 0
    (zero? x)           ; true if x is zero
```

Scheme has a great many more type specific predicates some of which are introduced later in these notes.

</body>
</html>
