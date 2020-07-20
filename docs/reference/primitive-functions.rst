===========================
List of Primitive Functions
===========================

.. highlight:: haskell

The following is the list of primitive (built-in) functions.

Pure primitive functions
========================

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

      (-21) % 13 ---> -8

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

      sin 0.0 ---> 0.0

cos
   ::

      cos 0.0 ---> 1.0

tan
   ::

      tan 0.0 ---> 0.0

asin
   ::

      asin 0.0 ---> 0.0

acos
   ::

      acos 1.0 ---> 0.0

atan
   ::

      atan 0.0 ---> 0.0

sinh
   ::

      sinh 0.0 ---> 0.0

cosh
   ::

      cosh 0.0 ---> 1.0

tanh
   ::

      tanh 0.0 ---> 0.0

asinh
   ::

      asinh 0.0 ---> 0.0

acosh
   ::

      acosh 1.0 ---> 0.0

atanh
   ::

      atanh 0.0 ---> 0.0

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

Primitive functions for IO operations
=====================================

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
   A variant of ``readChar`` that reads from the given port.
   ::

      let c := readCharFromPort inport

readLineFromPort
   A variant of ``readLine`` that reads from the given port.
   ::

      let line := readLineFromPort inport

writeCharToPort
   A variant of ``writeChar`` that writes to the given port.
   ::

      writeCharToPort 'a' outport

writeToPort
   A variant of ``write`` that writes to the given port.
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
