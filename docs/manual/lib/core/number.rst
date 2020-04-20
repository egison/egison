===================
lib/core/number.egi
===================

.. BEGIN docsgen

nats
   ::

      take 10 nats --> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

nats0
   ::

      take 10 nats0 --> [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

odds
   ::

      take 10 odds --> [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]

evens
   ::

      > take 10 evens
      --> [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

primes
   ::

      > take 10 primes
      --> [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

divisor
   ::

      divisor 10 5 --> True

find-factor
   ::

      findFactor 100 --> 2

p-f
   ::

      pF 100 --> [2, 2, 5, 5]

isOdd
   ::

      isOdd 3 --> True
      isOdd 4 --> False

isEven
   ::

      isEven 4 --> True
      isEven 5 --> False

isPrime
   ::

      isPrime 17 --> True
      isPrime 18 --> False

perm
   ::

      perm 5 2 --> 20

comb
   ::

      comb 5 2 --> 10

nAdic
   ::

      nAdic 10 123 --> [1, 2, 3]
      nAdic 2 10 --> [1, 0, 1, 0]

rtod
   ::

      > 2#(%1, take 10 %2) (rtod (6 / 35))
      --> (0, [1, 7, 1, 4, 2, 8, 5, 7, 1, 4])

rtod'
   ::

      rtod' (6 / 35) --> (0, [1], [7, 1, 4, 2, 8, 5])

showDecimal
   ::

      showDecimal 10 (6 / 35) --> "0.1714285714"

showDecimal'
   ::

      showDecimal' (6 / 35) --> "0.1 714285 ..."

.. END docsgen
