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

    <title>Scheme Notes: Characters and Strings</title>
</head>

<body>
# Scheme

## Characters and Strings

Lists and numbers are the most commonly used data types in Scheme but there are others.

<hr /><!-- Characters -->

Characters are indicated by the prefix `#\`:  `#\a` and `#\A` representing the characters 'a' and 'A'.

Four non-printing characters are also defined: `#\Space`. `#\Tab`, `#\Linefeed` and `#\Return`.

A good number of functions are also available:

```scheme
    (char? obj)                         ; #t is obj is a character

    (char->integer c)                   ; convert character to (ASCII) integer
    (integer->char c)                   ; convert (ASCII) integer to character

    (char=? c1 c2)                      ; #t is c1 and c2 are the same character
    (char<? c1 c2)                      ; comparison of characters after conversion to integer
                                        ; etc.

    (char-ci=? c1 c2)                   ; case insensitive comparison
                                        ; etc.

    (char-numeric? c)                   ; #t if character is a numeral
                                        ; also alphabetic, whitespace, upper-case, lower-case

    (char-upcase c)                     ; return upper case character
    (char-downcase c)                   ; return lower case character
```

<hr /><!-- Strings -->

String are enclosed in quotes.  Thus `"abc"`.

They are sequences of characters.
The time taken to compare strings is proportional to their length.

The following functions are defined:

```scheme
    (string? s)                         ; #t if s is a string
    (make-string n c)                   ; returns a string of n characters c)

    (string->list s)                    ; convert a string to a list of characters
    (list->string s)                    ; convert list of characters to a string

    (string-length s)                   ; eponymous
    (string-copy s)                     ; eponymous
    (string-append s1 s2)               ; append s2 to s1

    (string=? s1 s2)                    ; #t if the strings are the same

    (string-ref s n)                    ; get nth character from string (0 based)
    (string-set! s n c)                 ; get nth character in string (0 based) to character

    (substring s start end)             ; return substring [start,end)
```

</body>
</html>
