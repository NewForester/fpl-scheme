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

    <title>Scheme Notes: Symbols</title>
</head>

<body>
# Scheme

## Symbols

Lists and numbers are the most commonly used data types in Scheme but there are others.

Scheme (and Lisp) have a data type `symbol`.
The tutorials explanation of this is poor.

I think it goes something like this.

Imagine a separate name space for symbols.
It could be just a simple list with `car` addressing the name of a symbol and `cdr` address the next symbol.
The symbol name is a string but, since it occurs only once in the name space list, its address is unique.

Addresses may be compared as numbers, so comparison is O(1), whereas string comparison is O(n).
Symbols are therefore convenient keys in associative lists and other data structure.

I think that is all there is to it (although the internal implementation is probably not a list).

A symbol may be created and referred to with:

```scheme
    'name                               ; short-hand for (quote name)
```

The only snag seems to be symbols should be lower case.

The symbols functions are:

```scheme
    (symbol? x)                 ; #t if x is a symbol
    (string->symbol str)        ; convert string to symbol
    (symbol->string sym)        ; convert symbol to string
```

The tutorial example of symbol use is was word counting program.
I rephrased it to process the file a word at a time, incidentally eliminating the long list of words.
My rephrasing is in [lesson12-example1.scm](../scm/lesson12-example1.scm).


</body>
</html>
