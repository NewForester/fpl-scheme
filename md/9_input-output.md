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

    <title>Scheme Notes: Input and Output</title>
</head>

<body>
# Scheme

## Input and Output

Without the ability to read and write files the interactive front end of Scheme is just a toy.

The functions to open and close files are:

```scheme
    (open-input-file filename)
    (open-output-file filename)

    (close-input-port port)
    (close-output-port port)
```

The open functions return a `port`, which is passed to other input/output routines.
The `filename` is operating system dependent.

<hr /><!-- Input -->

An input file may be read a single character at a time:

```scheme
    (define (read-file file-name)
      (let ((p (open-input-file file-name)))
      (let loop((ls1 '()) (c (read-char p)))
        (if (eof-object? c)
          (begin
            (close-input-port p)
            (list->string (reverse ls1)))
          (loop (cons c ls1) (read-char p))))))
```

Evaluating this function on a file will print one long string with `\n` to denote line breaks.
Use `display` to, in effect, `cat` the file.

Two other functions are used more often because they handle errors such as file-not-found for you:

```scheme
    (call-with-input-file filename procedure)
    (with-input-from-file filename procedure)
```

The first passes the `port` to the `procedure`, which is responsible for closing the file.

```scheme
  (define (read-file file-name)
    (call-with-input-file file-name
      (lambda (p)
        (let loop((ls1 '()) (c (read-char p)))
          (if (eof-object? c)
            (begin
              (close-input-port p)
              (list->string (reverse ls1)))
            (loop (cons c ls1) (read-char p)))))))
```

The second opens the file as standard input so no parameter and no close.

```scheme
  (define (read-file file-name)
    (with-input-from-file file-name
      (lambda ()
        (let loop((ls1 '()) (c (read-char)))
          (if (eof-object? c)
            (list->string (reverse ls1))
            (loop (cons c ls1) (read-char)))))))
```

There is a function `(read port)` that will read an S-expression.

<hr /><!-- Output -->

The analogous functions for output are:

```scheme
    (call-with-output-file filename procedure)
    (with-output-to-file filename procedure)
```

The simple output routines are:

```scheme
    (write-char char port)
    (newline port)
```

The slightly more interesting routines are:

```scheme
    (write obj port)
    (display obj port)
```

The first print strings in double quotes and characters have the `#\` prefix.
The second does not.

</body>
</html>
