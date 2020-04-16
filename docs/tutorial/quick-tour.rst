=================
Egison Quick Tour
=================

This section introduces basic functionalities of Egison for pattern-matching oriented programming.


``matchAll`` and matchers
=========================

Egison provides some syntactic constructs for expressing pattern-matching.
The most basic one among them is ``matchAll``.

::

   matchAll [1,2,3] as list something with
     | $x :: $xs -> (x, xs)
   -- [(1,[2,3])]

A ``matchAll`` expression consists of the following elements.

* a **target** (``[1,2,3]`` in the above example)
* a **matcher** (``list something``)
* more than one **match clauses** (``$x :: $xs -> (x, xs)``)

A match clause contains a **pattern** (``$x :: $xs`` in the above example) and a **body** (``(x, xs)``).
Just like the pattern matching in other programming languages, the ``matchAll`` expression attempts pattern-matching of the target and the pattern, and if it succeeds, evaluates the body of the match clause.

The unique feature of the ``matchAll`` expression is twofold: (1) it returns a list, and (2) it takes additional argument called matchers.

(1) is for supporting pattern-matching with multiple results.
Since there can be multiple ways to match the pattern for the target data, the ``matchAll`` expression evaluates the body for all of these pattern-matching results and returns a list of the evaluation results.
In the above example, the ``::`` is what we call a **cons pattern** which decomposes a list into the first element and the others.
Because there is only one way to decompose the list ``[1, 2, 3]`` in this manner, the ``matchAll`` returns a singleton list.

The feature (2) realizes extensible pattern matching algorithm and pattern polymorphism.
Matcher is an Egison-specific object that retains pattern-matching algorithms.
See :ref:`label-pattern-polymorphism` for the description of pattern polymorphism.

Lines starting with ``--`` are comments.
In this tutorial, a line comment right after a program shows the execution result of the program.

We will explain more on the syntax of ``matchAll``.
A matcher is sandwitched between two keywords ``as`` and ``with``.
A ``matchAll`` expression can take multiple match clauses.
Match clauses are precedented with ``|``, which enhances the readability of program when a match clause occupy multiple lines.
In a match clause, the pattern and the body is separated with ``->``.

::

   matchAll target as matcher with
   | pattern1 -> body1
   | pattern2 -> body2
   ...

When there is only one match clause, we can omit the ``|`` before the match clause.

::

   matchAll ターゲット as マッチャー with パターン -> ボディ


The following is an example of pattern-matching with multiple results.
``++`` is called **join pattern**, which splits a list into two segments.
The ``matchAll`` evaluates the body for every possible matching result of the join pattern.

::

   matchAll [1,2,3] as list something with
   | $hs ++ $ts -> (hs, ts)
   -- [([], [1, 2, 3]), ([1], [2, 3]), ([1, 2], [3]), ([1, 2, 3], [])]


Non-linear pattern with value pattern and predicate pattern
===========================================================

``matchAll`` gets even more powerful when combined with non-linear patterns.
For example, the following non-linear pattern matches when the target collection contains a pair of identical elements.

::

   matchAll [1,2,3,2,4,3] as list integer with
   | _ ++ $x :: _ ++ #x :: _ -> x
   -- [2,3]

**Value patterns** play an important role in representing non-linear patterns.
A value pattern matches the target if the target is equal to the content of the value pattern.
A value pattern is prepended with ``#`` and the expression after ``#`` is evaluated referring to the value bound to the pattern variables that appear on the left side of the patterns.
As a result, for example, ``$x : #x : _`` is valid while ``#x : $x : _`` is invalid.

Let us show pattern matching for twin primes as a sample of non-linear patterns.
A twin prime is a pair of prime numbers of the form :math:`(p, p+2)`.
``primes`` is an infinite list of prime numbers which is defined in one of Egison standard libraries.
This ``matchAll`` extracts all twin primes from this infinite list of prime numbers in order.

::

   twinPrimes := matchAll primes as list integer with
     | _ ++ $p :: #(p + 2) :: _ -> (p, p + 2)

   take 8 twinPrimes
   -- [(3, 5), (5, 7), (11, 13), (17, 19), (29, 31), (41, 43), (59, 61), (71, 73)]


There are cases that we might want to use more general predicates in patterns than equality.
**Predicate patterns** are provided for such a purpose.
A predicate pattern matches the target if the predicate returns true for the target.
A predicate pattern is prepended with ``?``, and a unary predicate follows after ``?``.

::

   twinPrimes := matchAll primes as list integer with
     | _ ++ $p :: ?(\q -> q = p + 2) :: _ -> (p, p + 2)


Efficient pattern-matching with backtracking
============================================

The pattern-matching algorithm inside Egison includes a backtracking mechanism for efficient non-linear pattern matching.

::

   matchAll [1..n] as list integer with _ ++ $x :: _ ++ #x :: _ -> x
   -- returns [] in O(n^2) time
   matchAll [1..n] as list integer with _ ++ $x :: _ ++ #x :: _ ++ #x :: _ -> x
   -- returns [] in O(n^2) time

The above expressions match a collection that consists of integers from 1 to n as a list of integers for enumerating identical pairs and triples, respectively.
Since this target collection contains neither identical pairs nor triples, both expressions return an empty collection.

When evaluating the second expression, Egison interpreter does not try pattern matching for the second ``#x`` because pattern matching for the first ``#x`` always fails.
Therefore, the time complexities of the above expressions are identical.
.. The pattern-matching algorithm inside Egison is discussed in [9] in detail.


.. _label-pattern-polymorphism:

Ad-hoc polymorphism of patterns by matchers
===========================================

Another merit of matchers, in addition to the extensibility of pattern-matching algorithms, is the **ad-hoc polymorphism of patterns**.
The ad-hoc polymorphism of patterns allows us to use the same pattern constructors such as ``::`` and ``++`` for different matchers like ``list`` and ``multiset``.
It is important for non-free data types because some data are pattern-matched as various non-free data types at the different parts of a program.
For example, a list can be pattern-matched as a multiset or a set.
Polymorphic patterns reduce the number of names for pattern constructors.

In the following sample, a **collection** ``[1,2,3]`` is pattern-matched using different matchers with the same cons pattern.
The "collection" is actually what we have been calling "list" so far.
In Egison, collection refers to the sequential data that can be pattern-matched as lists, multisets or sets.

When we use ``multiset`` matcher, the cons pattern decomposes a collection into one element and the others ignoring the order of the elements.
When we use ``set`` matcher, the right hand side of the cons pattern is matched with the original collection.
This behavior comes from the idea that a set can be seen as a collection which contains infinitely many copies of each element.

::

   matchAll [1,2,3] as list something with $x :: $xs -> (x,xs)
   -- [(1,[2,3])]

   matchAll [1,2,3] as multiset something with $x :: $xs -> (x,xs)
   -- [(1,[2,3]),(2,[1,3]),(3,[1,2])]

   matchAll [1,2,3] as set something with $x :: $xs -> (x,xs)
   -- [(1,[1,2,3]),(2,[1,2,3]),(3,[1,2,3])]


Controlling the order of pattern-matching
=========================================

The ``matchAll`` expression is designed to enumerate all countably infinite pattern-matching results.
For this purpose, users sometimes need to care about the order of pattern-matching results.

Let us start by showing a typical example.
The ``matchAll`` expression below enumerates all pairs of natural numbers.
We extract the first 8 elements with the ``take`` function.
``matchAll`` uses breadth-first search to traverse all the nodes in the reduction tree of pattern matching. .. TODO: Refer to the chapter of pattern-matching mechanism
As a result, the order of the pattern-matching results is as follows.

::

   take 8 (matchAll [1..] as set something with
           | $x :: $y :: _ -> (x,y))
   -- [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(2,3),(3,2)]

The above order is suitable for traversing an infinite reduction tree.
However, sometimes this order is not preferable. .. (see section 3.1.2 and section 3.4.1).
``matchAllDFS``, which traverses a reduction tree in depth-first order, is provided for this reason.

::

   take 8 (matchAllDFS [1..] as set something with
   | $x :: $y :: _ -> (x,y))
   -- [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8)]


For instance, think about defining ``concat`` with pattern-matching.
If we use ``matchAll``, the outcome will be the alternation of the elements in the sublists, which is not what we expect of ``concat``.

::

   concat' xss := matchAll xss as list (list something) with
     | _ ++ (_ ++ $x :: _) :: _ -> x

   concat' [[1,2,3],[4,5,6],[7,8,9]]
   -- [1, 2, 4, 3, 5, 7, 6, 8, 9]

To fix this, we should use ``matchAllDFS`` instead.

::

   concat xss := matchAllDFS xss as list (list something) with
     | _ ++ (_ ++ $x :: _) :: _ -> x

   concat [[1,2,3],[4,5,6],[7,8,9]]
   -- [1, 2, 3, 4, 5, 6, 7, 8, 9]


And-Patterns, Or-Patterns, and Not-Patterns
===========================================

Logical patterns such as **and-patterns**, **or-patterns** and **not-patterns** play an important role in enriching the expressivity of patterns.

An and-pattern ``p1 & p2`` pattern-matches when *both* of the patterns ``p1`` and ``p2`` succeeds in pattern-matching.
Similarly, an or-pattern ``p1 | p2`` pattern-matches when *either* of the patterns ``p1`` and ``p2`` succeeds in pattern-matching.
A not-pattern ``!p`` pattern-matches when the pattern ``p`` fails to pattern-match.

We start by showing pattern matching for prime triples as an example of and-patterns and or-patterns.
A prime triple is a triple of primes of the form :math:`(p, p + 2, p + 6)` or :math:`(p, p + 4, p + 6)`.
The or-pattern ``#(p + 2) | #(p + 4)`` is used to match :math:`p+2` or :math:`p+4`.
The and-pattern ``(#(p + 2) | #(p + 4)) & $m`` binds the value matched by ``(#(p + 2) | #(p + 4))`` to a new variable ``m``.
This usage of and-pattern is similar to the as-pattern in Haskell.

::

   primeTriples := matchAll primes as list integer with
     | _ ++ $p :: ((#(p + 2) | #(p + 4)) & $m) :: #(p + 6) :: _
     -> (p, m, p + 6)

   take 6 primeTriples
   -- [(5,7,11),(7,11,13),(11,13,17),(13,17,19),(17,19,23),(37,41,43)]


As an example of not-patterns, the following ``matchAll`` enumerates sequential pairs of prime numbers that are not twin primes.
The not-pattern ``!#(p + 2)`` matches values other than :math:`p + 2`.

::

   take 10 (matchAll primes as list integer with
            | _ ++ $p :: (!#(p + 2) & $q) :: _ -> (p, q))
   -- [(2,3),(7,11),(13,17),(19,23),(23,29),(31,37),(37,41),(43,47),(47,53),(53,59)]


Loop Patterns
=============

A loop pattern is a pattern construct for representing a pattern that repeats itself multiple times.
It is an extension of Kleene star operator of regular expressions for general non-free data types.

Let us start by considering pattern matching for enumerating all combinations of two elements from a target collection.
It can be written using ``matchAll`` as follows.

::

   comb2 xs := matchAll xs as list something with
     | _ ++ $x_1 :: _ ++ $x_2 :: _ -> [x_1, x_2]

   comb2 [1,2,3,4] -- [[1,2],[1,3],[2,3],[1,4],[2,4],[3,4]]

Egison allows users to append indices to a pattern variable as ``$x_1`` and ``$x_2`` in the above sample.
They are called **indexed variables** and represent :math:`x_1` and :math:`x_2` in mathematical expressions.
The expression after ``_`` must be evaluated to an integer and is called an **index**.
We can append as many indices as we want like ``x_i_j_k``.
When a value is bound to an indexed pattern variable ``$x_i``, the system initiates an abstract map consisting of key-value pairs if ``x`` is not bound to a map, and bind it to ``x``.
If x is already bound to a map, a new key-value pair is added to this map.

Now, we generalize ``comb2``. The loop patterns can be used for this purpose.

::

   comb n xs := matchAll xs as list something with
     | loop $i                 -- index variable
            (1, n)             -- index range
            (_ ++ $x_i :: ...) -- repeat pattern
            _                  -- final pattern
     -> map (\i -> x_i) [1..n]

   comb 2 [1,2,3,4] -- [[1,2],[1,3],[2,3],[1,4],[2,4],[3,4]]
   comb 3 [1,2,3,4] -- [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]

A loop pattern consists of the following four elements.

* An **index variable** is a variable to hold the current repeat count.
* An **index range** is a tuple of an initial number and final number which specifies the range of the index variable.
* A **repeat pattern** is a pattern repeated when the index variable is in the index range.
* A **final pattern** is a pattern expanded when the index variable gets out of the index range.

Inside of the repeat patterns, we can use the ellipsis pattern ``...``.
The repeat pattern or the final pattern is expanded at the location of the ellipsis pattern.
The repeat pattern is expanded replacing the ellipsis pattern incrementing the value of the index variable.
For example, when ``n`` is 3, the above loop pattern is unfolded into as follows.

::

   (loop $i (1, 3) (_ ++ $x_i :: ...) _)
   _ ++ $x_1 :: (loop $i (2, 3) (_ ++ $x_i :: ...) _)
   _ ++ $x_1 :: _ ++ $x_2 :: (loop $i (3, 3) (_ ++ $x_i :: ...) _)
   _ ++ $x_1 :: _ ++ $x_2 :: _ ++ $x_3 :: (loop $i (4, 3) (_ ++ $x_i :: ...) _)
   _ ++ $x_1 :: _ ++ $x_2 :: _ ++ $x_3 :: _

The repeat count of the loop patterns in the above example is constant.
However, we can also write a loop pattern whose repeat count varies depending on the target by specifying a pattern instead of an integer as the final number.
When the final number is a pattern, the ellipsis pattern is replaced with both the repeat pattern and the final pattern, and the repeat count when the ellipsis pattern is replaced with the final pattern is pattern-matched with that pattern.
The following loop pattern enumerates all initial prefixes of the target collection.

::

   matchAll [1,2,3,4] as list something with
   | loop $i (1, $n) ($x_i :: ...) _ -> map (\i -> x_i) [1..n]
   -- [[],[1],[1,2],[1,2,3],[1,2,3,4]]

.. TODO
.. Loop patterns are heavily used especially for trees and graphs.
.. We work on pattern matching for trees in section 3.4.1.
.. More formal specification of syntax and semantics of loop patterns is shown in the author’s previous paper [6].


Sequential Patterns
===================

The pattern-matching system of Egison processes patterns from left to right.
However, there are some cases where we want to change this order, for example, to refer to a pattern variable bound in the right side of a pattern.
**Sequential patterns** are provided for such cases.

Sequential patterns allow users to control the order of the pattern-matching process.
A sequential pattern is represented as a list of patterns.
Pattern matching is executed for each pattern in order.
In the following sample, the target list is pattern-matched from the third, first, and second element in order.

::

   matchAll [2,3,1,4,5] as list integer with
     | [    @    ::    @    :: $x :: _,
        (#(x + 1),     @    ),
                    #(x + 2)]
     -> "Matched"
   -- ["Matched"]

``@`` that appears in a sequential pattern is called **later pattern variable**.
The target data bound to later pattern variables are pattern-matched in the next sequence.
When multiple later pattern variables appear, they are pattern-matched as a tuple in the next sequence.

Sequential patterns allow us to apply not-patterns for different parts of a pattern at the same time.
For example, the following pattern matches when ``xs`` and ``ys`` have only one element in common.
The use of the sequential pattern in this example allows us to first check that the two collections have at least one element in common, and then make sure that there is no more common element in the remaining part of the collections.
Such combination of sequential patterns and not patterns is often useful when writing a mathematical algorithm.

.. TODO : example

::

   singleCommonElem :=
     match (xs, ys) as (multiset eq, multiset eq) with
       | [($x :: @, #x :: @),
         !($y :: _, #y :: _)] -> True


Some readers might wonder if sequential patterns can be implemented using nested ``matchAll`` expressions.
There are at least two reasons why it is impossible.
First, a nested ``matchAll`` expression breaks breadth-first search strategy:
the inner ``matchAll`` for the second result of the outer ``matchAll`` is executed only after the inner ``matchAll`` for the first result of the outer ``matchAll`` is finished.
Second, a later pattern variable retains the information of not only a target but also a matcher.
There are cases that the matcher of ``matchAll`` is a parameter passed as an argument of a function, and a pattern is polymorphic.
Therefore, it is impossible to determine the matchers of inner ``matchAll`` expressions syntactically.


Pattern functions
=================

It is sometimes the case that the same combination of patterns appears at multiple locations of a program.
In such case, we can use **pattern functions** to give names to the combinations of patterns and avoid repetition.

A pattern function is a function which takes patterns as its argument and returns a pattern.
Its syntax is similar to that of lambda functions except that it uses ``=>`` instead of ``->``.

The ``twin`` in the following program is a pattern function and modularizes the double nested cons pattern.
The argument of pattern functions are called **variable patterns**, which are ``pat1`` and ``pat2`` in the following case.
Variable patterns must be prefixed with ``~`` when referred to in the body of pattern functions.
This is necessary for distinguishing variable patterns from nullary pattern constructors.

::

   twin := \pat1 pat2 => (~pat1 & $x) :: #x :: ~pat2

   match [1, 1, 2, 3] as list integer with
   | twin $n $ns -> [n, ns]
   -- [1, [2, 3]]


Matcher compositions
====================

.. TODO : link to ``matcher`` expression

All the matchers presented so far can be defined by users, except for the only built-in matcher ``something``.
Matchers are usually defined by the ``matcher`` expressions, but users can define matchers by composing the existing matchers.
This way, we can for example define matchers for tuples of multisets and multisets of multisets.

First, we can define a matcher for tuples by a tuple of matchers.
A tuple pattern is used for pattern matching using such a matcher.
For example, we can define the intersect function using a matcher for tuples of two multisets.

.. We work on pattern matching for tuples of collections more in section 3.3.

::

   intersect xs ys := matchAll (xs,ys) as (multiset eq, multiset eq) with
     | ($x : _, #x : _) -> x

``eq`` is a user-defined matcher for data types for which equality is defined.
When it is used, equality is checked for a value pattern.
By passing a tuple matcher to a function that takes and returns a matcher, we can define a matcher for various non-free data types.
For example, we can define a matcher for a graph as a set of edges as follows, where the nodes are represented by integers.

::

   graph := multiset (integer, integer)

A matcher for adjacency graphs can also be defined.
An adjacency graph is defined as a multiset of tuples of an integer and a multiset of integers.

::

   adjacencyGraph := multiset (integer, multiset integer)

Egison provides a handy syntactic sugar for defining a matcher for algebraic data types,
while it can laso be defined with ``matcher`` expressions.
For example, a matcher for binary trees can be defined using ``algebraicDataMatcher``.

::

   binaryTree a := algebraicDataMatcher
     | bLeaf a
     | bNode a (binaryTree a) (binaryTree a)

Matchers for algebraic data types and matchers for non-free data types also can be composed.
For example, we can define a matcher for trees whose nodes have an arbitrary number of children whose order is ignorable.

.. We show pattern matching for these trees in section 3.4.1.

::

   tree a := algebraicDataMatcher
     | leaf a
     | node a (multiset (tree a))
