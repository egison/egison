==================
lib/core/maybe.egi
==================

.. highlight:: haskell

.. BEGIN docsgen

maybe
   ::

      matchAll Just 1 as maybe integer with
        | just $x -> x
        | nothing -> "error"
      ---> [1]
      matchAll Nothing as maybe integer with
        | just _ -> "error"
        | nothing -> True
      ---> [True]

.. END docsgen
