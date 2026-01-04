=============
Built-in Data
=============

.. highlight:: haskell

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


Boolean
=======

``True`` and ``False`` are booleans.

Scalar Values
=============

In Egison, numeric scalar values (except for floats) are treated as polynomials
and represented in the mathematical canonical form.

Integer
-------

::

   1
   0
   -100

Rational number
---------------

::

   1 / 3
   4 / 6 ---> 2 / 3

Symbols
-------

Unbound variables are interpreted as symbols.

::

   > x + 1
   x + 1
   > f 2  -- uninterpreted functions
   f 2

Mathematical expressions with symbols are automatically normalized into the normal form.

::

   > (x + 1) ^ 2
   x^2 + 2 * x + 1

Special symbols
```````````````

Egison implements normalization algorithm for some of the common mathematical symbols.

``i`` (imaginary unit):
::

   > i * i
   -1
   > (1 + i)^2
   2 * i

``sqrt`` and ``rt`` (``sqrt n`` denotes :math:`\sqrt{n}` and ``rt m n`` denotes :math:`\sqrt[m]{n}`):
::

   > (sqrt 2) ^ 2
   2
   > (rt 3 2) ^ 3
   2

``sin`` and ``cos``:
::

   > (sin x)^2 + (cos x)^2
   1


Float
=====

::

   1.0
   1e-1 ---> 0.1
   2e3  ---> 2000.0

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

   def t := [| 1, 2, 3, 4, 5 |]

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

Any IO functions can be executed with a primitive function :ref:`io<io-primitive-function>`.

::

   print "foo" ---> #<io-function>

Port
====

A port has information of a file and its access mode (input/output).
You can create a port with ``openInputFile`` or ``openOutputFile``.

Undefined
=========

``undefined`` is a useful built-in data you can put where you have not written yet.
