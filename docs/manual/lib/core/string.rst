===================
lib/core/string.egi
===================

.. highlight:: haskell

.. BEGIN docsgen

S.isEmpty
   ::

      S.isEmpty "" ---> True
      S.isEmpty "Egison" ---> False

S.head
   ::

      S.head "Egison" ---> 'E'

S.tail
   ::

      S.tail "Egison" ---> "gison"

S.last
   ::

      S.last "Egison" ---> 'n'

S.map
   ::

      S.map id "Egison" ---> "Egison"

S.length
   ::

      S.length "Egison" ---> 6

S.split
   ::

      S.split "," "Lisp,Haskell,Egison"
      ---> ["Lisp", "Haskell", "Egison"]

S.append
   ::

      S.append "Egi" "son" ---> "Egison"

S.concat
   ::

      S.concat ["Egi", "son"] ---> "Egison"

S.intercalate
   ::

      S.intercalate "," ["Lisp", "Haskell", "Egison"]
      ---> "Lisp,Haskell,Egison"

C.between
   ::

      C.between 'a' 'c' ---> ['a', 'b', 'c']

C.isBetween
   ::

      C.isBetween 'a' 'c' 'b' ---> True

isAlphabet
   ::

      isAlphabet 'a' ---> True

isAlphabetString
   ::

      isAlphabetString "Egison" ---> True

upper-case
   ::

      upperCase 'e' ---> 'E'

lower-case
   ::

      lowerCase 'E' ---> 'e'

.. END docsgen
