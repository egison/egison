============
Basic Syntax
============

.. highlight:: haskell

Top-level Expressions
=====================

Definition
----------

You can bind an expression to a variable by connecting them with ``:=``.
When defining functions, the argument variable can be placed in the left hand side of the ``:=``.

::

   def x := 1

   -- The following two definitions are identical.
   def f := \x -> x + 1
   def f x := x + 1

Expression
----------

You can also write arbitrary expressions at the top level of programs.
These expressions are evaluated only when the ``-t`` option (see :ref:`command-options-test`) is specified.

::

   -- definitions
   def x := 1
   def y := 2

   -- A top-level expression which will evaluate to 3.
   x + y

``load`` and ``loadFile``
-------------------------
We can load Egison libraries with ``load``.
To load your own program, call ``loadFile`` with a full-path or a relative path to the file.

::

   -- Load Egison library.
   load "lib/core/number.egi"

   -- Load your program.
   loadFile "myfile.egi"

Infix Declaration
-----------------
(From version 4.0.4)

You can define your own infixes (binary operators) for expressions and patterns.
In Egison, infix declaration consists of the following 4 parts.

* Associativity ... ``infix`` (non associative), ``infixl`` (left associative) or ``infixr`` (right associative)
* Infix type ... ``expression`` (for functions) or ``pattern`` (for pattern constructors)
* Priority of the infix
* Representation of infix

::

   -- Define a right-associative infix '&&' of priority 5.
   infixr expression 5 &&

   -- Definition of the semantics of '&&'.
   def (&&) a b := match (a, b) as (eq, eq) with
                 | (#True, #True) -> True
                 | _              -> False

   -- Define a left-associative infix '<>' of priority 7.
   infixl pattern 7 <>

   def exampleMatcher := matcher
     | $ <> $ as (integer, integer) with
       | $x :: $y :: [] -> [(x, y)]
       | _              -> []

   match [1, 2] as exampleMatcher with $x <> $y -> x + y
   ---> 3


Basic Expressions
=================

.. _anonymous-function:

Anonymous function
------------------

An anonymous function consists of two parts: arguments and a body.
The arguments are written between ``\`` and ``->``, and the body is written at the right of ``->``.

::

   -- A function of one argument.
   \x -> x + 1

   -- A function of two arguments.
   \x y -> x + y

   -- A function of no arguments.
   \() -> 1

   -- Function application
   (\x y -> x + y) 3 7  ---> 10

The arguments can be simply aligned (separated with whitespace) or packed in a tuple.
Namely, the following two notations are identical.

::

   (\x y -> x + y)) 3 7    ---> 10
   (\(x, y) -> x + y) 3 7  ---> 10


Anonymous parameter function
----------------------------

Egison has a shorthand notation for the anonymous function.
In this syntax, the function body is prefixed with a ``n#``, where ``n`` indicates the arity of the function.
There must not be any spaces between the arity number ``n`` and the ``#``.
Also, the arguments are specified by numbers, where ``%i`` refers to the i-th argument.

This syntax is inspired by `the anonymous function syntax of Clojure <https://clojure.org/guides/learn/functions#_anonymous_function_syntax>`_.

::

   -- The followings are identical.
   2#(%1 + %2)
   \x y -> x + y

Section
-------

Egison has a special syntax for the partial application of infix operators,
which is inspired by `the section notation of Haskell <https://wiki.haskell.org/Section_of_an_infix_operator>`_.

* ``(+)`` is desugared into ``\x y -> x + y``
* ``(+ 1)`` is desugared into ``\x -> x + 1``
* ``(1 +)`` is desugared into ``\x -> 1 + x``


``let`` ... ``in`` expression
-----------------------------

A ``let`` ... ``in`` expression (or simply a ``let`` expression) locally binds expressions to variables.
Bindings defined in a ``let`` expression cannot be referred to outside of the ``let`` expression.

::

   let x := 1 in x + 1 ---> 2

You can write multiple bindings in a single ``let`` expression.
Note that the head of the binding must be aligned vertically in order to be parsed correctly.

::

   let x := 1
       y := 2
    in x + y
   ---> 3

The above expression can be written in a single line as follows.
The bindings must be wrapped with ``{`` ``}`` and separated with ``;``.

::

   let { x := 1 ; y := 2 } in x + y

Bindings in the same ``let`` expression can depend on each other.
The bindings do not necessarily be aligned in the order of dependency.

::

   let y := x  -- 'x' is defined in the next binding
       x := 1
    in y
   ---> 1

   -- We can even define mutual-recursive functions.
   let isOdd n := if n = 0 then False else isEven (n - 1)
       isEven n := if n = 0 then True else isOdd (n - 1)
    in isOdd 5
   ---> True

As a result, note the following behavior.

::

   x := 3

   let y := x
       x := 1
    in y
   ---> 1 (not 3)


``where`` expression
--------------------

``where`` is a syntax sugar for the above ``let`` expression.
Unlike the ``let`` expression, the bindings in ``where`` expressions come after the body expression.

For example, the following two expressions are identical.

::

   -- local bindings with `where`
   expression
     where
       x1 := expr1
       x2 := expr2

   -- local bindings with `let`
   let x1 := expr1
       x2 := expr2
    in expression

``if`` expression
-----------------

It is the ordinary ``if`` expression.
The guard expression (the one right after ``if``) must be evaluated to a boolean (``True`` or ``False``).

::

   if True then "Yes" else "No"  ---> "Yes"
   if False then "Yes" else "No" ---> "No"

.. _do-expression:

``do`` expression
-----------------

A ``do`` expression can group several IO functions into one IO function.
You can bind expressions to values with ``let`` in the ``do`` expression as well.
Every lines in the ``do`` block must either be an expression that evaluates to an IO function or a ``let`` binding.
Note that all the lines in the ``do`` block must be aligned vertically.

::

   def repl := do
     write ">>> "
     flush ()
     let line := readLine ()
     write line
     flush ()
     repl

A ``do`` expression can be written in one line as follows.
The expressions needs to be wrapped with ``{`` ``}`` and separated by ``;``.

::

   do { print "foo" ; print "bar" ; print "baz" }


The last statement in a ``do`` block must be an expression.
The last expression in a ``do`` block is interpreted as the evaluation result of the ``do`` expression.

::

   > io do { return 1; return 2; return 3 }
   3

``seq`` expression
------------------

This expression is inspired by the ``seq`` function in Haskell.

A ``seq`` expression takes two arguments.
The first argument of ``seq`` is strictly evaluated.
The most popular use case of seq is in the definition of the foldl function.

::

   def foldl $fn $init $ls :=
     match ls as list something with
       | [] -> init
       | $x :: $xs ->
         let z := fn init x
          in seq z (foldl fn z xs)
