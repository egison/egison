==================
lib/core/assoc.egi
==================

.. highlight:: haskell

.. BEGIN docsgen

toAssoc
   ::

      toAssoc [x, x, y, z] ---> [(x, 2), (y, 1), (z, 1)]
      toAssoc [x, y, x] ---> [(x, 1), (y, 1), (x, 1)]

fromAssoc
   ::

      fromAssoc [(x, 2), (y, 1)] ---> [x, x, y]

assocList
   ::

      matchAll [(x, 2), (y, 1)] as assocList something with
          | $a :: _ -> a
      ---> [x]
      matchAll [(x, 3), (y, 2), (z, 1)] as assocList something with
          | ncons $a #2 $r -> (a, r)
      ---> [(x, [(x, 1), (y, 2), (z, 1)])]
      matchAll [(x, 1), (y, 2), (z, 3)] as assocList something with
          | ncons $a #2 $r -> (a, r)
      ---> []
      matchAll [(x, 3), (y, 2), (z, 1)] as assocList something with
          | ncons $a $n $r -> (a, n, r)
      ---> [(x, 3, [(y, 2), (z, 1)])]
      matchAll [(x, 3), (y, 2), (z, 1)] as assocList something with
          | ncons $a $n $r -> (a, n, r)
      ---> [(x, 3, [(y, 2), (z, 1)])]

assocMultiset
   ::

      matchAll [(x, 2), (y, 1)] as assocMultiset something with
          | $a :: _ -> a
      ---> [x, y]
      matchAll [(x, 3), (y, 2), (z, 1)] as assocMultiset something with
          | ncons #z $n $r -> (n, r)
      ---> [(1, [(x, 3), (y, 2)])]
      matchAll [(x, 3), (y, 2), (z, 1)] as assocMultiset something with
          | ncons $a #2 $r -> (a, r)
      ---> [(x, [(x, 1), (y, 2), (z, 1)]), (y, [(x, 3), (z, 1)])]
      matchAll [(x, 3), (y, 2), (z, 1)] as assocMultiset something with
          | ncons #y #1 $r -> r
      ---> [[(x, 3), (y, 1), (z, 1)]]
      matchAll [(x, 3), (y, 2), (z, 1)] as assocMultiset something with
          | ncons $a $n $r -> (a, n, r)
      ---> [(x, 3, [(y, 2), (z, 1)]),

AC.intersect
   ::

      AC.intersect [(x, 2), (y, 1)] [(x, 1), (y, 2)]
      ---> [(x, 1), (y, 1)]
      AC.intersect [(x, 2), (y, 2)] [(x, 1), (y, 1)]
      ---> [(x, 1), (y, 1)]
      AC.intersect [(x, 1), (y, 1)] [(x, 2), (y, 2)]
      ---> [(x, 1), (y, 1)]

.. END docsgen
