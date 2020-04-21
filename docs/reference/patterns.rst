================
Pattern Matching
================

.. highlight:: haskell

Expressions for pattern matching
================================

``matchAll`` expression
-----------------------

A ``matchAll`` expression takes a target, a matcher and one or more match clauses.
It tries pattern matching for all match clauses, and returns a collection of the evaluation result of the body for all successful result of pattern matching.

::

   matchAll [1, 2, 3] as list integer with
   | $x :: $xs -> (x, xs)
   ---> [(1, [2, 3])]

   matchAll [1, 2, 3] as multiset integer with
   | $x :: $xs -> (x, xs)
   ---> [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]

   matchAll [1, 2, 3] as set integer with
   | $x :: $xs -> (x, xs)
   ---> [(1, [1, 2, 3]), (2, [1, 2, 3]), (3, [1, 2, 3])]

When none of the match clauses successfully pattern-matches, ``matchAll`` returns an empty collection ``[]``.

::

   matchAll [] as list integer with
   | $x :: $xs -> (x, xs)
   ---> []

You can write more than one match clauses.
In that case, every match clause must start with ``|`` and the ``|`` of all match clauses must be vertically aligned.

::

   matchAll [1, 2, 3] as multiset integer with
   | [] -> -1
   | $x :: $xs -> x


When there is only one match clause, the ``|`` can be omitted.

::

   matchAll [1, 2, 3] as multiset integer with $x :: _ -> x

``match`` expression
--------------------

``match`` expressions are similar to ``matchAll`` expressions except that it returns only one value.
In fact, the return value of a ``match`` expression is defined as the first element of the return value of its corresponding ``matchAll`` expression.

::

   match [1, 2, 3] as multiset integer with
   | $x :: $xs -> (x, xs)
   ---> (1, [2, 3])

When none of the match clauses successfully pattern-matches, it will raise an error.

::

   match [1, 2, 3] as multiset integer with
   | [] -> "OK"
   ---> Failed pattern match in: <stdin>


``\matchAll`` and ``\match``
----------------------------

``\matchAll`` and ``\match`` are handy syntax sugar for the combination of anonymous function and ``matchAll``/``match`` expressions.

The syntax of ``\matchAll`` expression is similar to that of ``matchAll`` except that it doesn't need the target.
A ``\matchAll`` expression is desugared into an anonymous function whose body is ``matchAll`` and whose argument is the target of ``matchAll``.

For example,

::

   \matchAll as matcher with
    | pattern1 -> expr1
    | pattern2 -> expr2

is desugared into the following expression.

::

   \x ->
     matchAll x as matcher with
     | pattern1 -> expr1
     | pattern2 -> expr2


The semantics of ``\match`` is similar.


``matchAllDFS`` and ``matchDFS``
--------------------------------

``matchAllDFS`` and ``matchDFS`` are variants of ``matchAll`` and ``match``, respectively.
See :ref:`label-match-search-order` for the description.

Pattern functions
-----------------

``matcher`` and ``algebraicDataMatcher``
----------------------------------------

Patterns
========

Wildcard pattern
----------------

Pattern variable
----------------

Indexed pattern variable
------------------------

Inductive pattern
-----------------

Value pattern
-------------

Predicate pattern
-----------------

Logical patterns: and-, or- and not-pattern
-------------------------------------------

Sequential pattern
------------------

Loop pattern
------------

Let pattern
-----------

