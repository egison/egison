====================
Command-Line Options
====================

``-l`` / ``--load-file``
========================

Load definitions from the given file.
::

   $ cat name-of-file-to-load.egi
   x := 1

   $ egison -l name-of-file-to-load.egi
   > x
   1


.. _command-options-test:

``-t`` / ``--test``
===================

Evaluate expressions in the given file.
::

   $ cat name-of-file-to-test.egi
   x := 1
   x + 2
   "This is the third line"

   $ egison -t name-of-file-to-test.egi
   3
   "This is the third line"


``-e`` / ``--eval``
===================

Output the evaluation result of the given Egison expression.
::

   $ egison -e 'matchAll [1,2,3] as list something with $x ++ _ -> x'
   [[], [1], [1, 2], [1, 2, 3]]

``-c`` / ``--command``
======================

Execute the given expression, which should evaluate to an IO function.
::

   $ egison -c 'print (show 1)'
   1

``-T`` / ``--tsv``
==================

Output the evaluation result in the TSV form.

When the evaluation result is a colleciton, each of its elements is printed in a single line.
::

   $ egison -T -e 'take 10 primes'
   2
   3
   5
   7
   11
   13
   17
   19
   23
   29

When the evaluation result is a collection of collections or a collection of tuples, the elements of the inner collections are separated by a tab.
::

   $ egison -T -e '[[1, 2, 3], [4, 5]]'
   1       2       3
   4       5
   $ egison -T -e '[(1, 2, 3), (4, 5, 6)]'
   1       2       3
   4       5       6

``-M`` / ``--math``
===================

Output the evaluation result in the specified format.
The format can be chosen from ``latex``, ``asciimath``, ``mathematica`` and ``maxima``.

::

   $ egison -M latex
   > x / y
   #latex|\frac{x}{y}|#

``-S`` / ``--sexpr-syntax``
===========================

Use the old S-expression syntax in REPL.

::

   $ egison -S
   > (+ 1 2)
   3


.. note::

   When parsing programs in files, Egison switches the parser by the file extension.
   If the source file has extension ``.egi``, it is interpreted in the new syntax,
   and if the source file has extension ``.segi``, it is interpreted in the old (S-expression) syntax.

.. warning::

   Since we are no longer taking care of the backward compatibility with the old syntax (before version 4.0.0),
   we recommend using the new syntax if possible.

   As for Egison programs written in the old syntax, we have a tool to translate them in the new syntax.
   Please see :doc:`migration-guide-for-new-syntax` for details.
