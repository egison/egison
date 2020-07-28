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


Display results in TSV
======================

``-T`` / ``--tsv``
------------------

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

Reading TSV input
=================

The following 3 options ( ``-s``, ``-m`` and ``-f``) allow you to work on the input in TSV format.
Each line of the input is interpreted as a tuple.


``-s`` / ``--substitute``
-------------------------

Regard the input as a collection and apply the given function to it.

::

   $ seq 10 20 | egison -s '\matchAll as list integer with _ ++ $x :: _ ++ #(x + 5) :: _ -> x'
   10
   11
   12
   13
   14
   15

``-m`` / ``--map``
------------------

Read the input line by line, apply the given function and print out the result.
``egison -m 'f'`` is equivalent to ``egison -s '\x -> map f x'``.

::

   $ seq 1 5 | egison -m '\x -> x + 2'
   3
   4
   5
   6
   7

``-f`` / ``--filter``
---------------------

Regard the input line by line, apply the given function and display the input only if the result is ``True``.
``egison -f 'f'`` is equivalent to ``egison -s '\x -> filter f x'``.

::

   $ seq 1 10 | egison -f 'isPrime'
   2
   3
   5
   7

``-F`` / ``--field``
--------------------

You can specify how the TSV should be interepreted with this option.

For example, if you give ``-F 2c`` to the interpreter, all the elements from the 2nd row is packed up in a collection.

::

      $ seq 10 15 | egison -T -m '\x -> x :: pF x'
      10	2	5
      11	11
      12	2	2	3
      13	13
      14	2	7
      15	3	5
      $ seq 10 15 | egison -T -m '\x -> x :: pF x' | egison -F 2c -m '\x -> x'
      (10, [2, 5])
      (11, [11])
      (12, [2, 2, 3])
      (13, [13])
      (14, [2, 7])
      (15, [3, 5])

If you give ``-F 2,4c``, elements in 2nd, 3rd and 4th column is packed in a collection.

::

   $ seq 10 20 | egison -T -m '\x -> x :: pF x' | egison -F 2,4c -m '\x -> x'
   (10, [2, 5])
   (11, [11])
   (12, [2, 2, 3])
   (13, [13])
   (14, [2, 7])
   (15, [3, 5])
   (16, [2, 2, 2], 2)
   (17, [17])
   (18, [2, 3, 3])
   (19, [19])
   (20, [2, 2, 5])

If you replace the ``c`` with ``s``, the elements are respectively interpreted as strings (i.e. surrounded with ``""``).
For instance, if you give ``-F 2,2s``, the elements in the 2nd column is stringified.

::

   $ seq 10 15 | egison -T -m '\x -> x :: pF x' | egison -F 2,2s -m '\x -> x'
   (10, "2", 5)
   (11, "11")
   (12, "2", 2, 3)
   (13, "13")
   (14, "2", 7)
   (15, "3", 5)

Likewise, giving ``-F 2,4s`` will stringify all the elements in the 2nd, 3rd and 4th columns.


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
