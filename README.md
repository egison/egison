## The Egison Programming Language

Egison is the pattern-matching oriented pure functional programming language.
We can directly represent pattern-matching against lists, multisets, sets, trees, graphs and any kind of data types.
This is the repository of the interpreter of Egison.

For more information, visit [Egison web site](http://www.egison.org).

If you get interested in Egison, please mail to [Satoshi Egi](http://www.egison.org/~egi/) or tweet to [@__Egi](https://twitter.com/__Egi) or [@Egison_Lang](https://twitter.com/Egison_Lang).

### Getting Started!

At first, you should install [Haskell Platform](http://www.haskell.org/platform/).

After you installed Haskell Platform, run the following commands on the terminal.

```
% cabal update
% cabal install egison
```

Now, you can try Egison.

```
% egison
Egison Version X.X.X(C) 2011-2014 Satoshi Egi
http://www.egison.org
Welcome to Egison Interpreter!
> ^D
Leaving Egison Interpreter.
```

If you are a beginner of Egison, it would be better to install [`egison-tutorial`](https://github.com/egisatoshi/egison-tutorial).

```
% cabal update
% cabal install egison-tutorial
% egison-tutorial
Egison Tutorial for Version X.X.X (C) 2013-2014 Satoshi Egi
http://www.egison.org
Welcome to Egison Tutorial!
==============================
List of sections in the tutorial
1: Calculate numbers
2: Basics of functional programming
3: Define your own functions
4: Basic of pattern-matching
5: Pattern-matching against infinite collections
6: Writing scripts in Egison
==============================
Choose a section to learn.
(1-6): 5
====================
We can write a pattern-matching against infinite lists even if that has infinite results.
Note that Egison really enumurate all pairs of two natural numbers in the following example.

Examples:
  (take 10 (match-all nats (set integer) [<cons $m <cons $n _>> [m n]]))
====================
>
```

Enjoy!

### Demonstration

We can do non-linear pattern-matching with backtracking in Egison.
The following code is the program that determines poker-hands written in Egison.
All hands are expressed in a single pattern.

```
(define $poker-hands
  (lambda [$cs]
    (match cs (multiset card)
      {[<cons <card $s $n>
         <cons <card ,s ,(- n 1)>
          <cons <card ,s ,(- n 2)>
           <cons <card ,s ,(- n 3)>
            <cons <card ,s ,(- n 4)>
             <nil>>>>>>
        <Straight-Flush>]
       [<cons <card _ $n>
         <cons <card _ ,n>
          <cons <card _ ,n>
            <cons <card _ ,n>
              <cons _
                <nil>>>>>>
        <Four-of-Kind>]
       [<cons <card _ $m>
         <cons <card _ ,m>
          <cons <card _ ,m>
           <cons <card _ $n>
            <cons <card _ ,n>
              <nil>>>>>>
        <Full-House>]
       [<cons <card $s _>
         <cons <card ,s _>
           <cons <card ,s _>
             <cons <card ,s _>
               <cons <card ,s _>
                 <nil>>>>>>
        <Flush>]
       [<cons <card _ $n>
         <cons <card _ ,(- n 1)>
          <cons <card _ ,(- n 2)>
           <cons <card _ ,(- n 3)>
            <cons <card _ ,(- n 4)>
             <nil>>>>>>
        <Straight>]
       [<cons <card _ $n>
         <cons <card _ ,n>
          <cons <card _ ,n>
           <cons _
            <cons _
             <nil>>>>>>
        <Three-of-Kind>]
       [<cons <card _ $m>
         <cons <card _ ,m>
          <cons <card _ $n>
            <cons <card _ ,n>
             <cons _
               <nil>>>>>>
        <Two-Pair>]
       [<cons <card _ $n>
         <cons <card _ ,n>
          <cons _
           <cons _
            <cons _
             <nil>>>>>>
        <One-Pair>]
       [<cons _
         <cons _
          <cons _
           <cons _
            <cons _
             <nil>>>>>>
        <Nothing>]})))
```

Isn't it exciting?
The pattern-matching of Egison is very powerful.
We can use it for pattern-matching against graphs or tree-structures such as XML.
Egison is not famous at all now.
Please help us to make Egison popular.

### How to test (for developer)

```
% cabal install --enable-tests
% cabal test
```
### Acknowledgement

I thank Ryo Tanaka, Takahisa Watanabe, Takuya Kuwahara and Kentaro Honda for their help to implement the base of the interpreter.

### License

Copyright (c) 2011-2014, Satoshi Egi

Egison is released under the [MIT license](https://github.com/egison/egison/blob/master/LICENSE).

I used [husk-scheme](http://justinethier.github.io/husk-scheme/) by Justin Ethier as reference to implement the base part of the previous version of the interpreter.
