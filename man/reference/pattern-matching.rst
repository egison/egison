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
See :ref:`match-search-order` for the description.

Pattern functions
-----------------

A pattern function is a function that takes patterns and returns a pattern.
Pattern functions allows us to reuse useful combination of patterns.

The syntax of pattern function is similar to that of :ref:`anonymous function <anonymous-function>` except that it uses double arrow ``=>`` instead of the sigle arrow ``->``.
Also, the argument pattern must be prefixed with a ``~`` in the body of the pattern function.
This is to distinguish the argument with nullary pattern constructor.

The application of pattern functions is written in the same manner as the application of pattern constructors.

::

   -- Defining a pattern function 'twin'
   def twin := \ pat1 pat2 => ($pat & ~pat1) :: #pat :: ~pat2

   matchAll [1, 2, 1, 3] as multiset integer with twin $n _ -> n
   ---> [1, 1]

   matchAll [2, 2, 1, 3] as multiset integer with _ :: twin #1 _ -> True
   ---> []

Like anonymous functions, a pattern function has lexical scope for the pattern variables.
Therefore, bindings for pattern variables in the argument patterns and the body of pattern functions don't conflict.

Patterns
========

Wildcard pattern
----------------

Wildcard patterns are denoted by ``_``.
It can match with any values and the matched value will be discarded.

::

   match [1, 2, 3] as list something with
   | _ -> "OK"
   ---> "OK"

Pattern variable
----------------

We can bind values to variables in pattern matching with pattern variables.
It is denoted as a variable prefixed with ``$``.
Any object matches pattern variables and the variable is locally bound to the object.

::

   match True as bool with
   | $x -> x
   ---> True

   match [1, 2, 3] as list integer with
   | $x :: $xs -> (x, xs)
   ---> (1, [2, 3])


Indexed pattern variable
------------------------

Indexed pattern variables ``$x_n`` (``n`` denotes integers) are special pattern variables.
When an indexed pattern variable ``$x_n`` appears in the pattern, Egison creates a :ref:`hash map <hash-maps>` and binds it to the variable ``x``.
An object matched to ``$x_i`` is associated with the key ``i`` in the hash ``x``.

::

   match 1 as something with $x_1 -> x
   ---> {| (1, 1) |}

   match [1, 2, 3] as list integer with $x_1 :: $x_2 -> x
   ---> {| (1, 1), (2, [2, 3]) |}

Inductive pattern
-----------------

Inductive pattern is an analogy of :ref:`inductive data <inductive-data>`.
An inductive pattern consists of a **pattern constructor** and multiple (zero or more) argument patterns.
The names and behaviors of pattern constructors are defined by matchers.

In the following example, ``*:`` is a pattern constructor defined in the ``list`` matcher, and ``$xs`` and ``$x`` is applied to the pattern constructor.

::

   matchAll [1, 2, 3] as list integer with $xs *: $x -> (xs, x)
   ---> ([1, 2], 3)

The nil pattern ``[]`` and the pattern infixes such as ``::`` and ``++`` are also implemented as pattern constructors.

Value pattern
-------------

A value pattern is written as ``#expr``, where ``expr`` can be any expression.
An object ``obj`` can match a value pattern ``#expr`` only if the evaluation result of ``obj`` is equal to that of ``expr``.
This equality is defined by the given matcher.

::

   match 1 as integer with
   | #1 -> OK
   | _  -> KO
   ---> OK

   match 0 as integer with
   | #1 -> OK
   | _  -> KO
   ---> KO

   match [1, 2, 3] as list integer with
   | #[1, 2, 3] -> OK
   ---> OK

   match [1, 2, 3] as multiset integer with
   | #[2, 1, 3] -> OK
   ---> OK

Predicate pattern
-----------------

A predicate pattern is a pattern that matches with an object when it satisfies the predicate following ``?``.
The expression following ``?`` should be a unary function that returns a boolean.

::

   matchAll [1..6] as list integer with
   | $xs ++ ?(< 4) :: $ys -> xs ++ ys
   ---> [[2, 3, 4, 5, 6], [1, 3, 4, 5, 6], [1, 2, 4, 5, 6]]

   matchAll [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377] as multiset integer with
   | ?(\x -> modulo x 2 == 0) & $x -> x
   ---> [2, 8, 34, 144]


And-pattern
-----------

An and-pattern ``p1 & p2`` is a pattern that matches the object if and only if both of the pattern ``p1`` and ``p2`` are matched.

::

   match [1, 3, 2] as list integer with
   | (#1 :: _) & _ *: #2 -> OK
   | _                   -> KO
   ---> OK

We can use and-patterns like as-patterns in Haskell.
For example, a pattern ``(_ :: _) & $xs`` matches with any non-empty collections and binds it to the variable ``xs``.

::

   match [1, 2] as list integer with
   | (_ :: _) & $xs -> xs
   ---> [1, 2]

   match [] as list integer with
   | (_ :: _) & $xs -> xs
   ---> pattern match failure

Or-pattern
----------

An or-pattern ``p1 | p2`` matches with the object if the object matches with ``p1`` or ``p2``.

::

   match [1, 3, 3] as list integer with
   | (#1 :: _) | _ *: #2 -> OK
   | _                   -> KO
   ---> OK

Not-pattern
-----------

A not-pattern ``!p`` matches with the object if the object does not match the pattern ``p``.

::

   match 1 as integer with !#2 -> True
   ---> True

   -- Returns True if and only if the collection does not contain 1
   def f :=
     \match as multiset integer with
      | !(#1 :: _) -> True
      | _          -> False

   -- Returns True if and only if the collection has an element other than 1
   def g :=
     \match as multiset integer with
      | !#1 :: _ -> True
      | _        -> False

   f [2, 3, 4] ---> True
   f [1, 2, 3] ---> False
   g [1, 2, 3] ---> True
   g [1, 1, 1] ---> False


Sequential pattern
------------------

See :ref:`sequential-patterns` in the tutorial.

Loop pattern
------------

See :ref:`loop-patterns` in the tutorial.

Let pattern
-----------

A let pattern allows binding expressions to variables inside the pattern.
The variables bound in the ``let`` pattern can be used in the body of the ``let`` pattern.

::

   def f x :=
     match x as multiset integer with
     | let n := length x in #n :: #n :: _ -> True
     | _                                  -> False

   f [1, 2, 2] ---> False
   f [3, 3, 2] ---> True
   f [1, 2, 3, 4] ---> False
   f [1, 4, 3, 4] ---> True

Matchers
========

``something`` matcher
---------------------

``something`` is the only built-in matcher.
Only variable pattern and wildcard patterns can be used for ``something`` matcher; it does not decompose the target object.

::

   match [1, 2, 3] as something with $x -> x ---> [1, 2, 3]
   match [1, 2, 3] as something with _  -> True ---> True
   match [1, 2, 3] as something with $x :: _  -> x ---> Error

.. _matcher:

Defining matcher with ``matcher`` expression
--------------------------------------------

This subsection describes how to define a matcher with ``matcher`` expression.

Let's think about defining a matcher ``unorderedIntegerPair``, which matches with a tuple of 2 integers ignoring the order.

::

   matchAll (1, 2) as unorderedIntegerPair with pair $a $b -> (a, b)
   ---> [(1, 2), (2, 1)]

This ``unorderedIntegerPair`` matcher can be defined as follows.

::

   def unorderedIntegerPair :=
     matcher
       | pair $ $ as (integer, integer) with
         | ($x, $y) -> [(x, y), (y, x)]
       | $ as something with
         | $tgt -> [tgt]

Line 3 and 4 corresponds with the case where we want to decompose the tuple, and line 5 and 6 is for the case where we don't want to.
The expression ``pair $ $`` in line 3 is a **primitive pattern pattern** (pattern for patterns) and it defines a pattern constructor named ``pair``, which enables the pattern expression like ``pair $a $b``.
The following ``(integer, integer)`` indicates that the both of matched 2 terms should be recursively pattern-matched by using ``integer`` matcher.
The expression ``($x, $y) -> [(x, y), (y, x)]`` in line 4 defines the correspondense between the syntactic representation of the target data and pattern matching results.
The ``($x, $y)`` in line 4 is called **primitive data pattern**.
In the example above, the target data ``(1, 2)`` is *syntactically* matched with ``($x, $y)``, making the variable ``x`` bound to ``1`` and ``y`` to ``2``.
As a result, the pattern matching result (specified with ``[(x, y), (y, x)]``) will be ``[(1, 2), (2, 1)]``.
Then, variable ``a`` and ``b`` in the pattern expression ``pair $a $b`` are bound to one of the pattern matching result.
Since it is a ``matchAll`` expression, this binding enumrates for the entire results, meaning that the first ``a`` is bound to ``1`` and ``b`` to ``2``, and secondly ``a`` to ``2`` and ``b`` to ``1``.

This ``unorderedIntegerPair`` matcher only works for integer tuples;
however, we can make it "polymorphic" by making it a function that takes matchers and returns a matcher.
For example, ``unorderedPair`` for an arbitrary matcher can be defined as follows:

::

   def unorderedPair m :=
     matcher
       | pair $ $ as (m, m) with
         | ($x, $y) -> [(x, y), (y, x)]
       | $ as something with
         | $tgt -> [tgt]

   -- Examples
   match ([1, 2], [3, 4]) as unorderedPair (multiset integer) with
   | pair (#4 :: _) _ -> True
   ---> True


``algebraicDataMatcher`` expression
-----------------------------------

``algebraicDataMatcher`` is a convenient syntax sugar for defining normal matchers, which decompose data accordingly to their data structure.
For example, the following code defines a matcher for terms in untyped lambda calculus.
The first identifiers in each line of the ``algebraicDataMatcher`` (``var``, ``abs`` and ``app``) must start with a lower case alphabet.

::

   def term :=
     algebraicDataMatcher
       | var string       -- variable
       | abs string term  -- lambda abstraction
       | app term term    -- application

The above definition is desugared into the following one:

::

   def term :=
     matcher
       | var $ as string with
         | Var $x -> [x]
         | _      -> []
       | abs $ $ as (string, term) with
         | Abs $x $t -> [(x, t)]
         | _         -> []
       | app $ $ as (term, term) with
         | App $s $t -> [(s, t)]
         | _         -> []
       | $ as something with
         | $tgt -> [tgt]

.. Primitive Pattern Pattern
.. =========================
..
.. As explained in :ref:`matcher`, primitive pattern patterns are patterns for patterns.
..
.. This section gives the syntax for primitive pattern patterns.
..
.. Primitive Data Pattern
.. ======================
..
.. As explained in :ref:`matcher`, primitive pattern patterns are used to express decomposition of Egison objects.
.. The pattern matching for primitive data pattern is conducted in a similar way as the pattern matching in standard programming languages.
..
.. This section gives the syntax for primitive data patterns.
