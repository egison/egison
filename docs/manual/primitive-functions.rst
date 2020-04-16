===================
Primitive Functions
===================

.. BEGIN docsgen

numerator
   ::

      > numerator (13 / 21) --> 13

denominator
   ::

      > denominator (13 / 21) --> 21

modulo
   ::

      > modulo (-21) 13 --> 5

quotient
   ::

      > quotient (-21) 13 --> -1

remainder
   ::

      > remainder (-21) 13 --> -8

neg
   ::

      > neg (-89) --> 89

abs
   ::

      > abs 0 --> 0
      > abs 15 --> 15
      > abs (-89) --> 89

lt
   ::

      > 0.1 < 1.0 --> True
      > 1.0 < 0.1 --> False
      > 1.0 < 1.0 --> False

lte
   ::

      > 0.1 <= 1.0 --> True
      > 1.0 <= 0.1 --> False
      > 1.0 <= 1.0 --> True

gt
   ::

      > 0.1 > 1.0 --> False
      > 1.0 > 0.1 --> True
      > 1.0 > 1.0 --> False

gte
   ::

      > 0.1 >= 1.0 --> False
      > 1.0 >= 0.1 --> True
      > 1.0 >= 1.0 --> True

round
   ::

      > round 3.1 --> 3
      > round 3.7 --> 4
      > round (-2.2) --> -2
      > round (-2.7) --> -3

floor
   ::

      > floor 3.1 --> 3
      > floor 3.7 --> 3
      > floor (-2.2) --> -3
      > floor (-2.7) --> -3

ceiling
   ::

      > ceiling 3.1 --> 4
      > ceiling 3.7 --> 4
      > ceiling (-2.2) --> -2
      > ceiling (-2.7) --> -2

truncate
   ::

      > truncate 3.1 --> 3
      > truncate 3.7 --> 3
      > truncate (-2.2) --> -2
      > truncate (-2.7) --> -2

sqrt
   ::

      > sqrt 4 --> 2
      > sqrt 4.0 --> 2.0

itof
   ::

      > itof 4 --> 4.0
      > itof (-1) --> -1.0

rtof
   ::

      > rtof (3 / 2) --> 1.5
      > rtof 1 --> 1.0

ctoi
   ::

      > ctoi '1' --> 49

itoc
   ::

      > itoc 49 --> '1'

pack
   ::

      > pack [] --> ""
      > pack ['E', 'g', 'i', 's', 'o', 'n'] --> "Egison"

unpack
   ::

      > unpack "Egison" --> ['E', 'g', 'i', 's', 'o', 'n']
      > unpack "" --> []

unconsString
   ::

      > unconsString "Egison" --> 'E', "gison"

lengthString
   ::

      > lengthString "" --> 0
      > lengthString "Egison" --> 6

appendString
   ::

      > appendString "" "" --> ""
      > appendString "" "Egison" --> "Egison"
      > appendString "Egison" "" --> "Egison"
      > appendString "Egi" "son" --> "Egison"

splitString
   ::

      > splitString "," "" --> [""]
      > splitString "," "2,3,5,7,11,13" --> ["2", "3", "5", "7", "11", "13"]

regex
   ::

      > regex "cde" "abcdefg" --> [("ab", "cde", "fg")]
      > regex "[0-9]+" "abc123defg" --> [("abc", "123", "defg")]
      > regex "a*" "" --> [("", "", "")]

regexCg
   ::

      > regexCg "([0-9]+),([0-9]+)" "abc,123,45,defg" --> [("abc,", ["123", "45"], ",defg")]

read
   ::

      > read "3" --> 3
      > read "3.14" --> 3.14
      > read "[1, 2]" --> [1, 2]
      > read "\"Hello world!\"" --> "Hello world!"

show
   ::

      > show 3 --> "3"
      > show 3.14159 --> "3.14159"
      > show [1, 2] --> "[1, 2]"
      > show "Hello world!" --> "\"Hello world!\""

isBool
   ::

      > isBool False --> True

isInteger
   ::

      > isInteger 1 --> True

isRational
   ::

      > isRational 1 --> True
      > isRational (1 / 2) --> True
      > isRational 3.1 --> False

isScalar
   ::

      > isScalar 1 --> True
      > isScalar [| 1, 2 |] --> False

isFloat
   ::

      > isFloat 1.0 --> True
      > isFloat 1 --> False

isChar
   ::

      > isChar 'c' --> True

isString
   ::

      > isString "hoge" --> True

isCollection
   ::

      > isCollection [] --> True
      > isCollection [1] --> True

isHash
   ::

      > isHash {| |} --> True
      > isHash {| (1, 2) |} --> True

isTensor
   ::

      > isTensor 1 --> False
      > isTensor [| 1 |] --> True
      > isTensor (generateTensor (+) [1, 2]) --> True

.. END docsgen
