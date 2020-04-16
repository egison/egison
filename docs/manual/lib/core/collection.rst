=======================
lib/core/collection.egi
=======================

.. BEGIN docsgen

nth
   ::

      > nth 1 [1, 2, 3] --> 1

take
   ::

      > take 2 [1, 2, 3] --> [1, 2]

drop
   ::

      > drop 2 [1, 2, 3] --> [3]

takeAndDrop
   ::

      > takeAndDrop 2 [1, 2, 3] --> ([1, 2], [3])

takeWhile
   ::

      > takeWhile (< 10) primes --> [2, 3, 5, 7]

head
   ::

      > head [1, 2, 3] --> 1

tail
   ::

      > tail [1, 2, 3] --> [2, 3]

last
   ::

      > last [1, 2, 3] --> 3

init
   ::

      > init [1, 2, 3] --> [1, 2]

uncons
   ::

      > uncons [1, 2, 3] --> (1, [2, 3])

unsnoc
   ::

      > unsnoc [1, 2, 3] --> ([1, 2], 3)

isEmpty
   ::

      > isEmpty [] --> True
      > isEmpty [1] --> False

length
   ::

      > length [1, 2, 3] --> 3

map
   ::

      > map (* 2) [1, 2, 3] --> [2, 4, 6]

map2
   ::

      > map2 (*) [1, 2, 3] [10, 20, 30] --> [10, 40, 90]

filter
   ::

      > filter (\n -> n % 2 = 1) [1, 2, 3] --> [1, 3]

zip
   ::

      > zip [1, 2, 3] [10, 20, 30]
      --> [(1, 10), (2, 20), (3, 30)]

lookup
   ::

      > lookup 2 [(1, 10), (2, 20), (3, 30)] --> 20

foldr
   ::

      > foldr (\n ns -> n :: ns) [] [1, 2, 3]
      --> [1, 2, 3]

foldl
   ::

      > foldl (\ns n -> n :: ns) [] [1, 2, 3]
      --> [3, 2, 1]

scanl
   ::

      > scanl (*) 2 [2, 2, 2] --> [2, 4, 8, 16]

concat
   ::

      > concat [[1, 2], [3, 4, 5]] --> [1, 2, 3, 4, 5]

reverse
   ::

      > reverse [1, 2, 3] --> [3, 2, 1]

intersperse
   ::

      > intersperse [0] [[1, 2], [3, 3], [4], []]
      --> [[1, 2], [0], [3, 3], [0], [4], [0], []]

intercalate
   ::

      > intercalate [0] [[1, 2], [3, 3], [4], []]
      --> [1, 2, 0, 3, 3, 0, 4, 0]

split
   ::

      > split [0] [1, 2, 0, 3, 3, 0, 4, 0]
      --> [[1, 2], [3, 3], [4], []]

splitAs
   ::

      > splitAs integer [0] [1, 2, 0, 3, 3, 0, 4, 0]
      --> [[1, 2], [3, 3], [4], []]

findCycle
   ::

      > findCycle [1, 3, 4, 5, 2, 7, 5, 2, 7, 5, 2, 7]
      --> ([1, 3, 4], [5, 2, 7])

repeat
   ::

      > take 5 (repeat [1, 2, 3]) --> [1, 2, 3, 1, 2]

repeat1
   ::

      > take 5 (repeat1 2) --> [2, 2, 2, 2, 2]

all
   ::

      > all (= 1) [1, 1, 1] --> True
      > all (= 1) [1, 1, 2] --> False

any
   ::

      > any (= 1) [0, 1, 0] --> True
      > any (= 1) [0, 0, 0] --> False

from
   ::

      > take 3 (from 2) --> [2, 3, 4]

between
   ::

      > between 2 5 --> [2, 3, 4, 5]

add
   ::

      > add 1 [2, 3] --> [2, 3, 1]
      > add 1 [1, 2, 3] --> [1, 2, 3]

addAs
   ::

      > addAs integer 1 [2, 3] --> [2, 3, 1]
      > addAs integer 1 [1, 2, 3] --> [1, 2, 3]

deleteFirst
   ::

      > deleteFirst 2 [1, 2, 3, 2] --> [1, 3, 2]

deleteFirstAs
   ::

      > deleteFirstAs integer 2 [1, 2, 3, 2] --> [1, 3, 2]

delete
   ::

      > delete 2 [1, 2, 3, 1, 2, 3] --> [1, 3, 1, 3]

deleteAs
   ::

      > deleteAs integer 2 [1, 2, 3, 1, 2, 3]
      --> [1, 3, 1, 3]

difference
   ::

      > difference [1, 2, 3] [1, 3] --> [2]

differenceAs
   ::

      > differenceAs integer [1, 2, 3] [1, 3] --> [2]

union
   ::

      > union [1, 2, 3] [1, 3, 4] --> [1, 2, 3, 4]

unionAs
   ::

      > unionAs integer [1, 2, 3] [1, 3, 4]
      --> [1, 2, 3, 4]

intersect
   ::

      > intersect [1, 2, 3] [1, 3, 4] --> [1, 3]

intersectAs
   ::

      > intersectAs integer [1, 2, 3] [1, 3, 4] --> [1, 3]

member
   ::

      > member 1 [1, 3, 1, 4] --> True
      > member 2 [1, 3, 1, 4] --> False

memberAs
   ::

      > memberAs integer 1 [1, 3, 1, 4] --> True
      > memberAs integer 2 [1, 3, 1, 4] --> False

count
   ::

      > count 1 [1, 3, 1, 4] --> 2

countAs
   ::

      > countAs integer 1 [1, 3, 1, 4] --> 2

frequency
   ::

      > frequency [1, 3, 1, 4] --> [(1, 2), (3, 1), (4, 1)]

frequencyAs
   ::

      > frequencyAs integer [1, 3, 1, 4]
      --> [(1, 2), (3, 1), (4, 1)]

unique
   ::

      > unique [1, 2, 3, 2, 1, 4] --> [1, 2, 3, 4]

uniqueAs
   ::

      > uniqueAs integer [1, 2, 3, 2, 1, 4]
      --> [1, 2, 3, 4]

.. END docsgen
