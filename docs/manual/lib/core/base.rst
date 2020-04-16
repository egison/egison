=================
lib/core/base.egi
=================

.. BEGIN docsgen

id
   ::

      > id 1 --> 1

fst
   ::

      > fst (1, 2) --> 1

snd
   ::

      > snd (1, 2) --> 2

compose
   ::

      > (compose fst snd) ((1, 2), 3) --> 2

eqAs
   ::

      > eqAs integer 1 1 --> True

and
   ::

      > [True && True, True && False, False && True, False && False]
      --> [True, False, False, False]

or
   ::

      > [True || True, True || False, False || True, False || False]
      --> [True, True, True, False]

not
   ::

      > [not True, not False] --> [False, True]

.. END docsgen
