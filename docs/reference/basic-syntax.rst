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

You can also write arbitrary expresions at the top level of programs.
These expressions are evaluated only when the ``-t`` option (see :ref:`command-options-test`) is specified.

::

   -- definitions
   x := 1
   y := 2

   -- (top-level) expression which will evaluate to 3
   x + y

``load`` and ``loadFile``
-------------------------
We can load Egison libraries with ``load``.
To load your own program, call ``loadFile`` with a full-path or a relative path to the file.

::

   -- Load Egison library
   load "lib/core/number.egi"

   -- Load your program
   loadFile "myfile.egi"

Expression
==========

Lambda expression
-----------------

