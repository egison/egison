===================
lib/core/random.egi
===================

.. highlight:: haskell

R.multiset
   ::

      matchAll [1, 2] as R.multiset integer with
      | $n :: $ns -> (n, ns)
      ---> [(1, [2]), (2, [1])] or [(2, [1]), (1, [2])]

      matchAll [1, 2] as R.multiset integer with
      | #1 :: $ns -> ns
      ---> [[2]]

R.set
   ::

      matchAll [1, 2] as R.set integer with
      | $n :: $ns -> (n, ns)
      ---> [(1, [1, 2]), (2, [1, 2])] or [(2, [1, 2]), (1, [1, 2])]

      matchAll [1, 2] as R.set integer with
      | #1 :: $ns -> ns
      ---> [[1, 2]]


pureRand
   ::

      pureRand 1 6
      ---> returns a random integer uniformly distributed over the closed interval [1, 6]

rands
   ::

      take 10 (rands 1 6)
      ---> returns a collection of 10 random integers
      ---> uniformly distributed over the closed interval [1, 6]
      ---> [3, 3, 6, 3, 3, 4, 5, 5, 1, 4] for example

randomize
   ::

      randomize [1, 2, 3]
      ---> [1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2] or [3, 2, 1]

R.between
   ::

      R.between 1 3
      ---> [1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2] or [3, 2, 1]

R.uncons
   ::

      R.uncons [1, 2]
      ---> (1, [2]) or (2, [1])

R.head
   ::

      R.head [1, 2]
      ---> 1 or 2

R.tail
   ::

      R.tail [1, 2]
      ---> [2] or [1]

f.pureRand
   ::

      f.pureRand 1.0 2.0
      ---> returns a random double value uniformly distributed over the
      ---> closed interval [1.0, 2.0].

f.rands
   ::

      take 10 (f.rands 1.0 2.0)
      ---> returns a collection of 10 random double values uniformly distributed
      ---> over the closed interval [1.0, 2.0].
