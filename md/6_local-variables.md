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

    <title>Scheme Notes: Local Variables</title>
</head>

<body>
# Scheme

## Local Variables

Local variables ease the definition of complex functions.
Scheme provides a mechanism through eye candy and `lambda` functions.

Consider:

```scheme
    ((lambda (p1 p2 ...)
      exp1 exp2 ...) v1 v2 ...)
```

By definition, parameters p1, p2 ... are assigned the values v1, v2, ... and
the assignments are in scope during the evaluation of expr1, expr2, ...

Thus:

```scheme
    (let ((p1 v1) (p2 v2) ...) exp1 exp2 ...)
```
may been seen as eye candy:
the local variables p1, p2 ... are assigned the values v1, v2, ... and
the assignments are in scope during the evaluation of expr1, expr2, ...

This definition of scope means that the value v2, for example, cannot be an expression that involves p1.
It is always possible to nest `let` expressions:

``` scheme
    (let ((i 1))
      (let ((j (+ i 2)))
      (* i j)))                         ; *)
```

but this is cumbersome.

Further eye-candy allows this to expressed as:

```
    (let* ((i 1) (j (+ i 2)))
      (* i j))                          ; *)
```

The scope of the `lambda` function (or the `let` expression) is referred to as the lexical closure.
Good luck with all the parentheses.

</body>
</html>
