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

    <title>Scheme Notes: Defining Functions</title>
</head>

<body>
# Scheme

## Defining Functions

The `define` operator is used to bind a symbol to a value:

```scheme
    (define hello "Hello world")        ; Hello world as a variable
```

The `cdr` half can be an S-expression, which is effectively a function without parameters:

```scheme
    (define pi (* 4 (atan 1.0)))        ; *)
```

The `lambda` special form is used to define a function.
It takes two parameters: the formal parameters and the function body.
It does not name the function, so it often is used in conjunction with define:

```scheme
    (define hello                       ; hello with name
      (lambda (name)
              (string-append "Hello " name "!")))
```

There is a short form that purists look down on:

```scheme
    (define (hello name)                ; hello with name
            (string-append "Hello " name "!")))
```

Note that the definition has the same form as an invocation.


### Aside: Editors

The tutorial recommends emacs for Windows but also describes edwin.
The latter comes with Scheme is a based on and older version of emacs.

The tutorial suggest emacs is more up to date than edwin and will provide a better experience.
Things may have moved on since then.

The tutorial says how to invoke edwin from within Scheme under Windows.
I do not see how to do this under Linux but apparently you can run Scheme from within emacs under Linux.

For more information see: [here](http://www-swiss.ai.mit.edu/projects/scheme/documentation/user_8.html).

Both editors may involve lots of control characters, which I do not get on with.


### Aside: Useful functions

Two useful functions:

```scheme
    (cd "new-pwd")                      ; change pwd
```

and


```scheme
    (load "file.scm")                   ; load a file
```

The file will probably contain Scheme function definitions (hence the extension).

It is evaluated as it is read.
Reading is aborted if a syntax error is encountered.

This usually means a pesky parenthesis has moved itself to the wrong place.

</body>
</html>
