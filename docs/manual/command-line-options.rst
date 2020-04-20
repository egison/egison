====================
Command-Line Options
====================

``-l`` / ``--load-file``
   Load definitions from the given file.
   ::

      $ cat name-of-file-to-load.egi
      x := 1

      $ egison -l name-of-file-to-load.egi
      > x
      1

``-t`` / ``--test``
   Evaluate expressions in the given file.
   ::

      $ cat name-of-file-to-test.egi
      x := 1
      x + 2
      "This is the third line"

      $ egison -t name-of-file-to-test.egi
      3
      "This is the third line"


``-e`` / ``--eval``
   Output the evaluation result of the given Egison expression.
   ::

      $ egison -e 'matchAll [1,2,3] as list something with $x ++ _ -> x'
      [[], [1], [1, 2], [1, 2, 3]]

``-c`` / ``--command``
   Execute the given expression, which should evaluate to an IO function.
   ::

      $ egison -c 'print (show 1)'
      1

``-T`` / ``--tsv``
   Output the evaluation result in the TSV form.

   When the evaluation result is a colleciton, each of its elements is printed in a single line.
   ::

      $ egison -T -e 'take 10 primes'
      2
      3
      5
      7
      11
      13
      17
      19
      23
      29

   When the evaluation result is a collection of collections or a collection of tuples, the elements of the inner collections are separated by a tab.
   ::

      $ egison -T -e '[[1, 2, 3], [4, 5]]'
      1       2       3
      4       5
      $ egison -T -e '[(1, 2, 3), (4, 5, 6)]'
      1       2       3
      4       5       6
