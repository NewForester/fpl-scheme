<!DOCTYPE html>
<html lang="en-GB">
    <!-- scheme notes by NewForester is licensed under a Creative Commons Attribution-ShareAlike 4.0 International Licence. -->
<head>
    <meta charset="UTF-8" />
    <meta name="description" content="Notes on the Scheme programming language made while learning a bit about Functional Programming" />
    <meta name="keywords" content="Scheme" />
    <meta name="author" content="NewForester" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <link rel="stylesheet" href="../styles/style-sheet.css" />

    <title>Scheme Notes: Introduction</title>
</head>

<body>
# Scheme

## Introduction

Scheme was chosen for study because there are those that describe it as the easiest Functional Programming language to learn.

Scheme is one of the two main dialects of Lisp.
It follows a minimalist design philosophy with a small standard core and powerful tools for language extension.

The language first appeared in 1970 and with it some of the terms common to Functional Programming languages.

There is an official IEEE standard but divergence between implementations are such that Scheme has been described by its own Steering Committee
as the world's least portable programming language.

The most widely implemented standard is R<sub>5</sub>RS from 1998;  the most recent is R<sub>7</sub>RS from 2013.

The language features functional, procedural and meta programming.
It has strong dynamic typing and uses lexical scope.
Syntactically it uses lots of parentheses and little else.
The lambda keyword is used heavily.


### MIT-Scheme

The original Scheme language was developed at MIT.
MIT-Scheme pre-dates the FSF and the GPL but is now MIT/GNU Scheme.

The current version is 9.2 from May 2014.
It is not fully R<sub>5</sub>RS compliant.
There are plans to implement the minimalist R<sub>7</sub>RS standard and a module system.

It features a rich run-time library, a powerful source-level debugger, a native code compiler and the built-in emacs like editor called Edwin.

The only one of these I may have used is the run-time library.
None were covered in the course I followed and Edwin was not viewed favourably.

Documentation is available on-line:

 1. [MIT/GNU Scheme User's Manual](http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-user/)
 1. [MIT/GNU Scheme Reference](http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/)

There are for Version 9.2 based on R<sub>4</sub>RS.

The install description is very brief and straight forward but the web page instructions are required.
There is a "scheme.ini" file somewhere where settings can be customised.


### User Interface

Scheme has an command line interpreter or `read-eval-print loop`.
Input is in the form of S-expressions.

The S stands for symbolic, not Scheme, but the idea of expressions represented by nested, tree-structured, lists originated with Lisp.

It has no special characters (so not APL-like).
It uses parentheses a great deal but very few other punctuation characters.

Backspace may used to erase characters.
Arrow keys are not recognised.
I have yet to find how to invoke the editor Edwin.

For future reference:

  1. `^C h` for help
  1. `^C q` to quit
  1. `^C^C` cancel bad input and start again

The command line interprets S-expressions, which is bewildering first time in.

  1. 1 + 1      ; simply does not work
  1. (+ 1 1)    ; is that you should type
  1. ; indicates the rest of the line is a comment

When something goes wrong, execution is suspended.
To simply clear and try again enter:

```scheme
    (RESTART 1)
```

There are more advanced options but I have not tried them.

Take note that:

  1. 1+ is a valid function name, so
  1. a single space is significant in many contexts
  1. extra white space and line breaks are of no significance
  1. hit return by mistake and the interpreter just sits there waiting for you to complete your input

The contents of files (that define functions etc.) are interpreted as they are read in.
It is possible to compile them and afterwards read in the compiled form.


### xUnit Test Framework

I looked for a xUnit test framework for Scheme and found [SchemeUnit](http://schematics.sourceforge.net/schemeunit/).
It dates from 2003 and is aimed at PLT Scheme now named [Racket](http://racket-lang.org/).

The source should be available from [SourceForge](https://sourceforge.net/projects/schematics/).
I would not expect it to just work.

### Scheme Tutorial

An Internet search for MIT-Scheme based tutorials turned up this [Yet Another Scheme Tutorial](http://www.shido.info/lisp/idx_scm_e.html).

It seems to have been written in 2006 but, since MIT-Scheme has little changed in that time, this is not important.

The author claims this is a simple tutorial for beginners.
The author says there are many other tutorials but these assume too much of the reader.

It covers Scheme in 18 chapters, which is perhaps evidence of the language minimalist approach.
Its aim is the provide a starting point for reading [SICP](http://mitpress.mit.edu/sicp/), a famous text book on Computer Science.
I was impressed by this approach although I might have appreciated something more pragmatic.

The tutorial is in English but the author (T. Shido) I believe is Japanese.
It is clear the text has not been proof read by a native English speaker and there are sentences that I could not make sense of without
researching the topic at hand elsewhere.

Notwithstanding, some chapters are much more difficult than others.
Chapter 7 on Repetition or Looping took a lot more effort that its predecessors.
I also found the last four difficult to grok.

Shido himself does not believe that Scheme is suitable for writing "practical applications" and suggests that Common Lisp is.

</body>
</html>
