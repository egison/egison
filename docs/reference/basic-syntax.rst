============
Basic Syntax
============

.. highlight:: haskell

Top-level Expression
====================

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

.. TODO

``do`` expression
-----------------

``io`` expression
-----------------

``procedure`` expression
------------------------

``seq`` expression
------------------

Expressions for tensor computation
==================================

``generateTensor`` expression
-----------------------------

``contract`` expression
-----------------------

``tensorMap`` expression
------------------------

``tensorMap2`` expression
-------------------------

``transpose`` expression
------------------------

``subrefs``, ``suprefs`` and ``userRefs``
-----------------------------------------

Wedge application expression
----------------------------

.. TODO arguments (scalar arguments, tensor arguments, inverted scalar arguments)

Exprsesions for symbolic computation
====================================

``withSymbols`` expression
--------------------------

``function`` expression
-----------------------

Quoted expression
-----------------
