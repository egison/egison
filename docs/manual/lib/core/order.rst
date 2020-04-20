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

min
   ::

      min 20 5 ---> 5

minimum
   ::

      minimum [20, 5, 12] ---> 5

min/fn
   ::

      min/fn compare [10, 20, 5, 20, 30] ---> 5

max
   ::

      max 5 30 ---> 30

maximum
   ::

      maximum [5, 30, 23] ---> 30

max/fn
   ::

      max/fn compare [10, 20, 5, 20, 30] ---> 30

sort
   ::

      sort [10, 20, 5, 20, 30] ---> [5, 10, 20, 20, 30]

sort/fn
   ::

      sort/fn compare [10, 20, 5, 20, 30]
      ---> [5, 10, 20, 20, 30]

.. END docsgen
