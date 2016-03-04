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

    <title>Scheme Notes: Using Scheme as a Calculator</title>
</head>

<body>
# Scheme

## Using Scheme as a Calculator

Scheme expressions are call S-expressions.

The basic sum is expressed this:

```scheme
    (+ 1 1)
```scheme

So, parentheses around operator following by operands.

```scheme
    (+ 1 2 3)
```scheme

Evaluates to sum of 1, 2 and 3.

Parentheses can be use to alter the order of evaluation:

```scheme
    (/ (+ 6 4) (- 7 2))
```scheme

<hr /><!-- Numeric Types -->

Integer arithmetic produces exact results:

```scheme
    (/ 101 13)
```scheme

yields `101/3` (a fractional number) rather than truncating to the nearest integer.

Floating point produces inexact results:

```scheme
    (/ 101. 13.)
```scheme

yields `7.769230769230769.`

There are functions that perform type conversions:

```scheme
    (exact->inexact 1/2)
```scheme

yields `.5`.

Scheme has a good many type conversion functions with names with the form from->to.

Complex numbers are also permitted:

```scheme
    (* 1+1i 1-1i)       ; *)
```scheme

yields `2` and

```scheme
    (sqrt -1)
```scheme

yields `+i`.

<hr /><!-- Arithmetic Operations -->

The basic arithmetic functions: `+`, `-`, `*`, `/`.

Here are four more:  `quotient`, `remainder`, `modulo`, `sqrt`.

There are trigonometric functions: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`.

For example:

```scheme
   (* 4 (atan 1.0))     ; *)
```scheme

yields 3.141592653589793.

There are exponential and power functions:  `exp`, `log` and `expt`.

The last of these takes two parameters.

</body>
</html>
