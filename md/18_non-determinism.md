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

    <title>Scheme Notes: Non-determinism</title>
</head>

<body>
# Scheme

## Non-determinism

The tutorial clams that non-determinism suites logical programming.
The tutorial might make a lot more sense if the chapter was called Logical Programming
and suggested that a valid approach to logical programming is pseudo non-determinism.

A [Non-deterministic Algorithm](https://en.wikipedia.org/wiki/Nondeterministic_algorithm)
is an algorithm than may return a different result, even for the same input.
This sounds like very opposite of pure functional programming and it is not what is mean here.

[Non-deterministic Programming](https://en.wikipedia.org/wiki/Nondeterministic_programming)
is a programming form where a program encounters 'choice points'.
These represent program flow alternatives (as do if-then-else statements)
but the choice is determined at run-time by some general mechanism (such as a virtual tossing of a coin).

One method of choice is known as backtracking:  the chosen 'alternative' may fail causing the program to backtrack and try other 'alternatives'.
This is what is considered in this chapter.
I see this a deterministic but, heck, what do I know.

Since backtracking implies restoring the state of choice points, languages that implement continuations are particularly suited to this method.

The chapter considers the `amb` evaluator.
This is a special form that is not part of the standard MIT-Scheme.

The `amb` evaluator chooses to evaluate one from a set of expressions "ambiguously":

```scheme
    (amb exp1 exp2 ...)
```

If the list of expression has just one expression, then it has no choice but to evaluate than expression.
If the list is empty, the evaluation is said to 'fail'.

In practice `amb` does not choose "ambiguously" but "systematically".
Deterministic choice points substitute for non-deterministic ones.
Thus:

```scheme
    (list (amb 1 2 3) (amb 'a 'b))
```

would return all six possible values in the same order every time.

Turn this around, `amb` can be used to search a tree structure for an item that matches and arbitrary condition.
In principle, you do not care which item it returns but in practice it will always return the same one.

How is this different from any other systematic search ?
I am not sure other than the backtracking is automagical through its use of continuations.
Perhaps the point is that you don't have to write the code to do the systematic search
and can concentrate on expressing the (real world) problem rather than the (programming language specific) solution.

Let us now consider the tutorial.

The following will return a pair of numbers whose sum is a prime number:

```scheme
    (let ((i (amb 4 6 7)) (j (amb 5 8 11)))
      (if (prime? (+ i j))
        (list i j)
        (amb)))
```

How do we get here ?

This, I think, is the inspiration:  [Amb and Search](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-28.html#%_sec_4.3.1).
In this, `amb` is a special form.
It is not present in MIT-Scheme.
At least not the version I downloaded.

The tutorial instead implements it as a function and then as a macro.

Our copy of the function implementation and example are in [lesson18-example1.scm](../scm/lesson18-example1.scm).
It is a bit gauche:  the use of a global variable and side effects; the need for `apply` and the call to `choice` inside `pythag`
raise more questions than they answer.

Our copy of the macro implementation and example are in [lesson18-example2.scm](../scm/lesson18-example2.scm).
This is a slight improvement since S-expressions are handled properly.
Perhaps the ugliness is inevitable unless you can implement `amb` as a special form.

Next are four utilities for "logical programming".
I like the `assert` function.
Given the substitution of "systematically" for "ambiguously", I am not convinced the example offers any advantage over a generator.
The use of one global variable `fail` by any number of functions concurrently looks very contrary to functional programming principles.

The four utilities are then used to implement `prime?` and so implement the support required for the example given at the start of the chapter.
See [lesson18-example3.scm](../scm/lesson18-example3.scm).

The final school girl example is probably a classic logical programming exercise.

The synthesis has been a long time coming.
What we have here is a reasonably straight forward implementation that is easy to following provided you have grasped all the rest.

A logic problem is something like a Suduko puzzle.
There are rules or constraints (such as the sums down each column and across rows) - and there are facts - the numbers already given

Logical programming is a way of programming solutions to logic problems by stating the facts and rules
and allowing the programming language to find solutions that do not violate these.

In our case here, by using the backtracking ("systematic") `amb` implementation.

The school girl example ([lesson18-example4.scm](../scm/lesson18-example4.scm)) shows two implementations.
The first implementation is apparently an order of magnitude faster than the second.

The second implementations is a brute force algorithm that will consider all possibilities with five unknowns.
The first implementations cuts down the number of possibilities by initially considering just two unknowns with one constraint,
only going further for those possibilities that work for this simpler problem.

Well, d'oh, you might think but for complex, real world problems, finding the short-cuts may not be so obvious.

Note that the two implementations demonstrate that the algorithm is not entirely separate from the rules and facts
as a pure logical programming language would require.

---

</body>
</html>
