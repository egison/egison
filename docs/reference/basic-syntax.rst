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

   x := 1

   -- The following two definitions are identical.
   f := \x -> x + 1
   f x := x + 1

Expression
----------

You can also write arbitrary expressions at the top level of programs.
These expressions are evaluated only when the ``-t`` option (see :ref:`command-options-test`) is specified.

::

   -- definitions
   x := 1
   y := 2

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

This syntax is inspired by `the anonymous function syntax of Clojure`_.

.. _`the anonymous function syntax of Clojure`: https://clojure.org/guides/learn/functions#_anonymous_function_syntax

::

   -- The followings are identical.
   2#(%1 + %2)
   \x y -> x + y

   -- The followings are identical.
   1#1
   \x -> 1


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

``do`` expression
-----------------

A ``do`` expression can group several IO functions into one IO function.
You can bind expressions to values with ``let`` in the ``do`` expression as well.
Every lines in the ``do`` block must either be an expression that evaluates to an IO function or a ``let`` binding.
Note that all the lines in the ``do`` block must be aligned vertically.

::

   repl := do
     write "> "
     flush ()
     let line := readLine ()
     write line
     flush ()
     repl

A ``do`` expression can be written in one line as follows.
The expressions needs to be wrapped with ``{`` ``}`` and separated by ``;``.

::

   do { print "foo" ; print "bar" ; print "baz" }


You can return values from ``do`` expressions with ``return``, a primitive function.

::

   > io do { return 1 }
   1

.. warning::

   As of version 4.0.0, the ``return`` must be placed at the last line of the ``do`` block,
   and the last line must be of the form ``return ...`` (``return`` being applied to some argument) in order to be recognized as the return expression.

   When the last line is not of the form ``return ...``, an expression ``return ()`` is appended to the tail of the ``do`` block.
   Applications of the function ``return`` in non-tail position of ``do`` blocks is evaluated, but the evaluation result is discarded.

   ::

      > io do { return 1 }
      1
      > -- Applications of 'return' in then/else branches are not recognized.
      > -- Therefore, the following is parsed as
      > --   io do { if True then return 1 else return 2 ; return () }
      > io do { if True then return 1 else return 2 }
      ()
      > io do { if True then print "foo" else return 2 }
      foo
      ()
      > io do { if True then return 1 else return 2 ; return 4 }
      4

.. _io-expression:

``io`` expression
-----------------

An ``io`` expression takes an IO function and executes it.
This is similar to the ``unsafePerformIO`` in Haskell.

::

   > io print "hoge"
   hoge
   ()

``procedure`` expression
------------------------

``seq`` expression
------------------

This expression is inspired by the ``seq`` function in Haskell.

A ``seq`` expression takes two arguments.
The first argument of ``seq`` is strictly evaluated.
The most popular use case of seq is in the definition of the foldl function.

::

   foldl $fn $init $ls :=
     match ls as list something with
       | [] -> init
       | $x :: $xs ->
         let z := fn init x
          in seq z (foldl fn z xs)
