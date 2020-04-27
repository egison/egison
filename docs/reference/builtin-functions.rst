==========================
List of Built-in Functions
==========================

.. highlight:: haskell

Pure built-in functions
=======================

.. BEGIN docsgen

numerator
   ::

      numerator (13 / 21) ---> 13

denominator
   ::

      denominator (13 / 21) ---> 21

modulo
   ::

      modulo (-21) 13 ---> 5

quotient
   ::

      quotient (-21) 13 ---> -1

remainder
   ::

      remainder (-21) 13 ---> -8

neg
   ::

      neg (-89) ---> 89

abs
   ::

      abs 0 ---> 0
      abs 15 ---> 15
      abs (-89) ---> 89

lt
   ::

      0.1 < 1.0 ---> True
      1.0 < 0.1 ---> False
      1.0 < 1.0 ---> False

lte
   ::

      0.1 <= 1.0 ---> True
      1.0 <= 0.1 ---> False
      1.0 <= 1.0 ---> True

gt
   ::

      0.1 > 1.0 ---> False
      1.0 > 0.1 ---> True
      1.0 > 1.0 ---> False

gte
   ::

      0.1 >= 1.0 ---> False
      1.0 >= 0.1 ---> True
      1.0 >= 1.0 ---> True

round
   ::

      round 3.1 ---> 3
      round 3.7 ---> 4
      round (-2.2) ---> -2
      round (-2.7) ---> -3

floor
   ::

      floor 3.1 ---> 3
      floor 3.7 ---> 3
      floor (-2.2) ---> -3
      floor (-2.7) ---> -3

ceiling
   ::

      ceiling 3.1 ---> 4
      ceiling 3.7 ---> 4
      ceiling (-2.2) ---> -2
      ceiling (-2.7) ---> -2

truncate
   ::

      truncate 3.1 ---> 3
      truncate 3.7 ---> 3
      truncate (-2.2) ---> -2
      truncate (-2.7) ---> -2

sqrt
   ::

      sqrt 4 ---> 2
      sqrt 4.0 ---> 2.0
      sqrt (-1) ---> i

exp
   ::

      exp 1 ---> e
      exp 1.0 ---> 2.718281828459045
      exp (-1.0) ---> 0.36787944117144233

log
   ::

      log e ---> 1
      log 10.0 ---> 2.302585092994046

sin
   ::

      sin 2.71828 ---> 0.41078295757034533

cos
   ::

      cos 2.71828 ---> -0.9117331636886747

tan
   ::

      tan 2.71828 ---> -0.4505517336984941

asin
   ::

      asin 0.41078 ---> 0.42330940969311176

acos
   ::

      acos 0.41078 ---> 1.1474869171017847

atan
   ::

      atan 0.41078 ---> 0.38976479923936064

sinh
   ::

      sinh 2.71828 ---> 7.5441231880274415

cosh
   ::

      cosh 2.71828 ---> 7.610111344529286

tanh
   ::

      tanh 2.71828 ---> 0.9913288842285756

asinh
   ::

      asinh 3.14159 ---> 1.862294938438355

acosh
   ::

      acosh 3.14159 ---> 1.811525381452312

atanh
   ::

      atanh 0.41078 ---> 0.43654919678194676

itof
   ::

      itof 4 ---> 4.0
      itof (-1) ---> -1.0

rtof
   ::

      rtof (3 / 2) ---> 1.5
      rtof 1 ---> 1.0

ctoi
   ::

      ctoi '1' ---> 49

itoc
   ::

      itoc 49 ---> '1'

pack
   ::

      pack [] ---> ""
      pack ['E', 'g', 'i', 's', 'o', 'n'] ---> "Egison"

unpack
   ::

      unpack "Egison" ---> ['E', 'g', 'i', 's', 'o', 'n']
      unpack "" ---> []

unconsString
   ::

      unconsString "Egison" ---> ('E', "gison")

lengthString
   ::

      lengthString "" ---> 0
      lengthString "Egison" ---> 6

appendString
   ::

      appendString "" "" ---> ""
      appendString "" "Egison" ---> "Egison"
      appendString "Egison" "" ---> "Egison"
      appendString "Egi" "son" ---> "Egison"

splitString
   ::

      splitString "," "" ---> [""]
      splitString "," "2,3,5,7,11,13"
      ---> ["2", "3", "5", "7", "11", "13"]

regex
   ::

      regex "cde" "abcdefg" ---> [("ab", "cde", "fg")]
      regex "[0-9]+" "abc123defg"
      ---> [("abc", "123", "defg")]
      regex "a*" "" ---> [("", "", "")]

regexCg
   ::

      regexCg "([0-9]+),([0-9]+)" "abc,123,45,defg"
      ---> [("abc,", ["123", "45"], ",defg")]

read
   ::

      read "3" ---> 3
      read "3.14" ---> 3.14
      read "[1, 2]" ---> [1, 2]
      read "\"Hello world!\"" ---> "Hello world!"

show
   ::

      show 3 ---> "3"
      show 3.14159 ---> "3.14159"
      show [1, 2] ---> "[1, 2]"
      show "Hello world!" ---> "\"Hello world!\""

isBool
   ::

      isBool False ---> True

isInteger
   ::

      isInteger 1 ---> True

isRational
   ::

      isRational 1 ---> True
      isRational (1 / 2) ---> True
      isRational 3.1 ---> False

isScalar
   ::

      isScalar 1 ---> True
      isScalar [| 1, 2 |] ---> False

isFloat
   ::

      isFloat 1.0 ---> True
      isFloat 1 ---> False

isChar
   ::

      isChar 'c' ---> True

isString
   ::

      isString "hoge" ---> True

isCollection
   ::

      isCollection [] ---> True
      isCollection [1] ---> True

isHash
   ::

      isHash {| |} ---> True
      isHash {| (1, 2) |} ---> True

isTensor
   ::

      isTensor 1 ---> False
      isTensor [| 1 |] ---> True
      isTensor (generateTensor (+) [1, 2]) ---> True

.. END docsgen

IO built-in functions
=====================

return
   Takes a pure value and return an IO function that returns the value.
   ::

      io return 1 ---> 1

openInputFile
   Takes a name of a file (string) and opens the file in read-only mode.
   Returns a port of the opened file.
   ::

      let inport := openInputFile "file.txt"

openOutputFile
   Takes a name of a file (string) and opens the file in write-only (truncate) mode.
   Returns a port of the opened file.
   ::

      let outport := openOuputFile "file.txt"

closeInputPort, closeOutputPort
   Takes a port and closes it.
   ::

      closeInputPort inport
      closeOutputPort outport

readChar
   Reads one character from the standard input and returns it.
   ::

      let c := readChar ()

readLine
   Reads one line from the standard input and returns it.
   ::

      let line := readLine ()

writeChar
   Output a given character to the standard input.
   ::

      writeChar 'a'

write
   Output a given string to the standard input.
   ::

      write "string"

readCharFromPort
   Reads one character from the given port and returns it.
   ::

      let c := readCharFromPort inport

readLineFromPort
   Reads one line from the given port and returns it.
   ::

      let line := readLineFromPort inport

writeCharToPort
   Output a given character to the given port.
   ::

      writeCharToPort 'a' outport

writeToPort
   Output a given string to the given port.
   ::

      writeToPort "string" outport

isEof
   Returns ``True`` if an EOF is given in the standard input.
   ::

      let b := isEof ()

flush
   Flushes the standard output.
   ::

      flush ()

isEofPort
   Returns ``True`` if an EOF is given in the specified port.
   ::

      let b := isEofPort inport

flushPort
   Flushes the given port.
   ::

      flushPort outport

readFile
   Takes a name of a file (string) and returns its content as a string.
   ::

      let lines := readFile "file.txt"

rand
   ``rand n m`` returns an integer in the range :math:`[n, m]` (including :math:`m`).

f.rand
   Float version of ``rand``.

.. newIORef
.. writeIORef
.. readIORef
