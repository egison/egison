=============
Built-in Data
=============

.. highlight:: haskell

Boolean
=======

``True`` and ``False`` are booleans.

Numbers
=======

Integer
-------

::

   1
   0
   -100

Rational number
---------------

Egison supports rational numbers.

::

   1 / 3
   4 / 6 ---> 2 / 3

Float
-----

::

   1.0
   1e-1 ---> 0.1
   2e3  ---> 2000.0

Mathematical expression
=======================

.. TODO

Character
=========

Characters are enclosed in single quotes.

::

   'a'
   ' '
   '\n'

String
======

Strings are enclosed in double quotes.

::

   "Hello, world"


.. _inductive-data:

Inductive data
==============

A variable starting with an upper-case letter is interpreted as a constructor of inductive data.
You don't need to define the inductive data before using it.

::

   Leaf
   Node 1 Leaf (Node 2 Leaf Leaf)


Tuple (Multiple values)
=======================

A tuple is denoted as a sequence of elements enclosed in parentheses and separated by ``,``.
Tuples of single element cannot be written.

::

   () -- zero-element tuple
   (1, 2)
   (2, "foo", True)


Collection
==========

A collection is a sequence of elements that are enclosed in brackets and separated by ``,``.

::

   []
   [1]
   [1, 2]
   [1, 2, 3]

Tensor
======

A tensor is a sequence of elements enclosed in double brackets ``[|`` ``|]`` and separated by ``,``.
The :math:`i`-th element of a tensor ``t`` can be retrieved by ``t_i``. Note that it is 1-indexed.

::

   t := [| 1, 2, 3, 4, 5 |]

   t_1 ---> 1

   -- The index can be any expression that evaluates to an integer.
   t_(2+3) ---> 5

   t_6 ---> Error: Tensor index out of bounds

You can get the shape of a tensor with ``tensorShape``.

::

   tensorShape [| 1, 2, 3, 4, 5 |] ---> [5]

Multi-dimensional tensors can be defined by nesting tensors.

::

   [| [| 1, 2, 3 |], [| 4, 5, 6 |], [| 7, 8, 9 |] |]_1   ---> [| 1, 2, 3 |]
   [| [| 1, 2, 3 |], [| 4, 5, 6 |], [| 7, 8, 9 |] |]_2_3 ---> 6

Egison prepares special syntax for tensors.
See :ref:`tensor-syntax` for detail.


.. _hash-maps:

Hash Maps
=========

A hash map is a sequence of key-value pairs enclosed in double braces ``{|`` ``|}``.
The value of a key ``k`` in a hash map ``h`` can be retrieved by ``h_k``.
If the key is not included in the keys of the hash map, the result will be ``undefined``.

::

   {| (1, 11) (2, 12) (3, 13) (4, 14) (5, 15) |}_1 ---> 11
   {| (1, 11) (2, 12) (3, 13) (4, 14) (5, 15) |}_4 ---> 14
   {| (1, 11) (2, 12) (3, 13) (4, 14) (5, 15) |}_8 ---> undefined

IO Function
===========

IO functions are functions that will yield IO operation when executed.

Any IO functions can be executed with :ref:`io expressions<io-expression>`.

::

   print "foo" ---> #<io-function>

Undefined
=========

``undefined`` is a useful built-in data you can put where you have not written yet.
