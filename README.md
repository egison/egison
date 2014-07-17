# The Egison Programming Language

Egison is the **pattern-matching-oriented**, purely functional programming language.
We can directly represent pattern-matching against lists, multisets, sets, trees, graphs and any kind of data types.
This is the repository of the interpreter of Egison.

For more information, visit <a target="_blank" href="http://www.egison.org">Egison website</a>.
You can try Egison online!

If you get interested in Egison, please contact <a target="_blank" href="http://www.egison.org/~egi/">Satoshi Egi</a> or tweet to <a target="_blank" href="https://twitter.com/Egison_Lang">@Egison_Lang</a>.

## Getting Started!

At first, you should install <a target="_blank" href="http://www.haskell.org/platform/">Haskell Platform</a>.

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

If you are a beginner of Egison, it would be better to install <a target="_blank" href="https://github.com/egisatoshi/egison-tutorial">`egison-tutorial`</a>.

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
Note that Egison really enumerates all pairs of two natural numbers in the following example.

Examples:
  (take 10 (match-all nats (set integer) [<cons $m <cons $n _>> [m n]]))
====================
>
```

Enjoy!

## Demonstrations

We can do non-linear pattern-matching against unfree data types in Egison.
An unfree data type is a data type whose data have no canonical form, a standard way to represent that object.
It enables us to write more elegant programs.

### Twin Primes

We can use pattern-matching for enumeration.
The following code enumerates all twin primes from the infinite list of prime numbers with pattern-matching!

```
(define $twin-primes
  (match-all primes (list integer)
    [<join _ <cons $p <cons ,(+ p 2) _>>>
     [p (+ p 2)]]))

;; Enumerate first 10 twin primes
(take 10 twin-primes)
;=>{[3 5] [5 7] [11 13] [17 19] [29 31] [41 43] [59 61] [71 73] [101 103] [107 109]}
```

### Poker Hands

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

(poker-hands {<Card <Club> 12>
              <Card <Club> 10>
              <Card <Club> 13>
              <Card <Club> 1>
              <Card <Club> 11>});=><Straight-Flush>

(poker-hands {<Card <Diamond> 1>
              <Card <Club> 2>
              <Card <Club> 1>
              <Card <Heart> 1>
              <Card <Diamond> 2>});=><Full-House>

(poker-hands {<Card <Diamond> 4>
              <Card <Club> 2>
              <Card <Club> 5>
              <Card <Heart> 1>
              <Card <Diamond> 3>});=><Straight>

(poker-hands {<Card <Diamond> 4>
              <Card <Club> 10>
              <Card <Club> 5>
              <Card <Heart> 1>
              <Card <Diamond> 3>});=><Nothing>
```

Isn't it exciting?
The pattern-matching of Egison is very powerful.
We can use it for pattern-matching against graphs or tree-structures such as XML.
Egison is not famous at all now.
Please help us to make Egison popular.

## Comparison with Related Work

There are <a target="_blank" href="https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns#Relatedwork">a lot of existing work</a> for pattern-matching.

The advantage of Egison is that it realized **all of the following features** at the same time.

* Modularization of the way of pattern-matching for each data type
* Pattern-matching with multiple results (backtracking)
* Non-linear pattern-matching

It enables us to express non-linear pattern-matching against unfree data types intuitively as above demonstrations.

Furthermore, Egison realized the following feature. We can even modularize patterns like functions keeping above features.

* Non-linear pattern-matching with **lexical scoping**

The <a target="_blank" href="http://www.egison.org/manual/mechanism.html">Pattern-Matching Mechanism</a> section in Egison developer's manual explains how we realized that.

Please read <a target="_blank" href="http://arxiv.org/abs/1407.0729">our paper on arXiv.org</a> for details.

## Note for Developers

### How to Run Test

```
% cabal install --enable-tests
% cabal test
```

### How to Profile the Interpreter

```
% sudo apt-get install haskell-platform-doc haskell-platform-prof
% cabal install --enable-library-profiling --enable-executable-profiling
% egison +RTS -p -RTS -l sample/sequence.egi
% cat egison.prof
```

## Acknowledgement

I thank Ryo Tanaka, Takahisa Watanabe, Takuya Kuwahara and Kentaro Honda for their help to implement the interpreter.

## License

Copyright (c) 2011-2014, Satoshi Egi

Egison is released under the [MIT license](https://github.com/egison/egison/blob/master/LICENSE).

I used [husk-scheme](http://justinethier.github.io/husk-scheme/) by Justin Ethier as reference to implement the base part of the previous version of the interpreter.
