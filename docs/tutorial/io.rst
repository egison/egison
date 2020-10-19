=============
Basics of I/O
=============

.. highlight:: haskell

This chapter explains how to use IO operation in Egison.

Hello World!
============

Let's start this tutorial by greeting the world. The following is the "Hello world" program in Egison.

::

   -- Save this code as a "hello.egi" file
   def main args :=
     write "Hello, World!\n"

We can execute the above program as follows.

.. code-block:: text

   $ egison hello.egi
   Hello, World!

Egison I/O works via a function named ``main``.
In main, we can use I/O functions such as ``write``.
(The list of I/O primitive functions is given in :doc:`../reference/primitive-functions`.)

Command Line Arguments
======================

Command line arguments are given to the ``main`` as its argument as a collection of strings.

For instance, assume the following program.

::

   -- Save this code as a "args.egi" file
   def main args :=
     write (show args)

If you execute the following commands, you will see that the arguments are given to ``main`` as ``args``.

.. code-block:: text

   $ egison args.egi
   []
   $ egison args.egi We can write scripts in Egison
   ["We", "can", "write", "scripts", "in", "Egison"]

``do`` expressions
==================

To combine several I/O operations to one, we can use ``do`` expressions.
The feature of ``do`` expressions is *serial* execution; the I/O functions in the ``do`` expressions are executed from the top in order.
If you know Haskell, you probably notice that it is the same with the ``do`` expressions in Haskell.

::

   -- Save this code as a "repl.egi" file
   def repl := do
     write "input: "
     flush ()
     let input := readLine ()
     write input
     print ""
     repl

   def main args := repl

Then, execute it as follow.
Note that ``write "input: "``, ``flush ()``, ``readLine ()`` and ``write input`` are executed in the order.

.. code-block:: text

   $ egison repl.egi
   input: Hi
   Hi
   input: Hello
   Hello
   input: Repl
   Repl
   input: ^C
   $

Check out :ref:`do-expression` for more detail.

``io`` primitive function
=========================

We can use ``io`` (primitive function) to execute IO functions anywhere.
For example, the following is a definition of the ``pureRand`` function in ``lib/core/io.egi``.

::

   def pureRand s e := io (rand s e)
