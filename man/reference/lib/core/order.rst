==================
lib/core/order.egi
==================

.. highlight:: haskell

.. BEGIN docsgen

compare
   ::

      compare 10 10 ---> Equal
      compare 11 10 ---> Greater
      compare 10 11 ---> Less
      compare [1, 2] [1] ---> Greater
      compare [1] [1, 2] ---> Less
      compare [1, 2] [1, 3] ---> Less
      compare [1, 3] [1, 3] ---> Equal

min
   ::

      min 20 5 ---> 5

max
   ::

      max 5 30 ---> 30

minimum
   ::

      minimum [20, 5, 12] ---> 5

maximum
   ::

      maximum [5, 30, 23] ---> 30

splitByOrdering
   ::

      splitByOrdering 2 [1, 2, 3, 2, 3, 4, 5]
      ---> ([1], [2, 2], [3, 3, 4, 5])

sort
   ::

      sort [10, 20, 5, 20, 30] ---> [5, 10, 20, 20, 30]
      sort ["banana", "apple", "chocolate"] ---> ["apple", "banana", "chocolate"]

minimize
   ::

      minimize S.length ["banana", "apple", "chocolate"]
      ---> "apple"

maximize
   ::

      maximize S.length ["banana", "apple", "chocolate"]
      ---> "chocolate"

.. END docsgen
