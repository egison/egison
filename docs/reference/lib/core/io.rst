===============
lib/core/io.egi
===============

.. highlight:: haskell

print
   Prints the given string and a newline to the standard output.
   ::

      > io print "foo"
      foo
      ()

printToPort
   A variant of ``print``. The output is written to the given port.
   ::

      printToPort outport "foo"


display
   Prints the given string to the standard output.
   ::

      > io display "foo"
      foo()

displayToPort
   A variant of ``display``. The output is written to the given port.
   ::

      displayToPort outport "foo"

eachLine
   Repeat reading a single line from the standard input and applying the given function to it.
   ::

      -- Repeats reading one line and printing it out
      eachLine print

eachLineFromPort
   A variant of ``eachLine``. The input is read from the given port.
   ::

      eachLineFromPort inport print

eachFile
   Takes a collection of file names (in string) and a function,
   and apply the function for each line in each of the files.
   ::

      eachFile ["in1.txt", "in2.txt"] print

each
   This function corresponds to the ``mapM_`` in Haskell.
   ::

      each print ["foo", "bar"]

debug
   Prints out the argument value and returns it.
   This function is useful for debugging.
   ::

      debug x

debug2
   A variant of ``debug``. The first argument is a string.
   ::

      debug2 "x = " x
