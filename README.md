# The Egison Programming Language

Egison is the **pattern-matching-oriented**, purely functional programming language.
We can directly represent pattern-matching against lists, multisets, sets, trees, graphs and any kind of data types.
This is the repository of the interpreter of Egison.

For more information, visit <a target="_blank" href="http://www.egison.org">our website</a>.

## Non-Linear Pattern-Matching against Non-Free Data Types

We can do non-linear pattern-matching against non-free data types in Egison.
An non-free data type is a data type whose data have no canonical form, a standard way to represent that object.
It enables us to write elegant programs.

### Twin Primes

We can use pattern-matching for enumeration.
The following code enumerates all twin primes from the infinite list of prime numbers with pattern-matching!

<hr/>
<img width="100%" src="https://raw.githubusercontent.com/egison/egison/master/images/twin-primes.png" />
<hr/>

### Poker Hands

The following code is the program that determines poker-hands written in Egison.
All hands are expressed in a single pattern.

<hr/>
<img width="100%" src="https://raw.githubusercontent.com/egison/egison/master/images/poker-hands.png" />
<hr/>

### Mahjong

We can write a pattern even against mahjong tiles.
We modularize patterns to represent complex mahjong hands.

<hr/>
<img width="100%" src="https://raw.githubusercontent.com/egison/egison/master/images/mahjong.png" />
<hr/>

### Graphs

We can pattern-match against graphs.
We can write program to solve the travelling salesman problem in a single pattern-matching expression.

<hr/>
<img width="100%" src="https://raw.githubusercontent.com/egison/egison/master/images/salesman.png" />
<hr/>

Aren't these exciting?
The pattern-matching of Egison is very powerful.
We can use it for pattern-matching also against graphs and tree-structures such as XML.

## Comparison with Related Work

There are <a target="_blank" href="https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns#Relatedwork">a lot of existing work</a> for pattern-matching.

The advantage of Egison is that it achieves **all of the following features** at the same time.

* Modularization of the way of pattern-matching for each data type
* Pattern-matching with multiple results (backtracking)
* Non-linear pattern-matching with **lexical scoping**
* Parametric polymorphism of pattern-constructors

The <a target="_blank" href="http://www.egison.org/manual/mechanism.html">Pattern-Matching Mechanism</a> section in Egison developer's manual explains how we achieve that.

Please read <a target="_blank" href="http://arxiv.org/abs/1407.0729">our paper on arXiv.org</a> for details.

## Installation

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
Egison Tutorial Version 3.3.6 (C) 2013-2014 Satoshi Egi
Welcome to Egison Tutorial!
** Information **
We can use a 'Tab' key to complete keywords on the interpreter.
If we type a 'Tab' key after a closed parenthesis, the next closed parenthesis will be completed.
*****************
==============================
List of sections in the tutorial.
1: Calculate numbers                             (10 minutes)
2: Basics of functional programming              (10 minutes)
3: Basics of pattern-matching                    (10 minutes)
4: Pattern-matching against infinite collections (5 minutes)
==============================
Choose a section to learn.
(1-4): 1
====================
We can do arithmetic operations with '+', '-', '*', '/', 'modulo' and 'power'.

Examples:
  (+ 1 2)
  (- 30 15)
  (* 10 20)
  (/ 20 5)
  (modulo 17 4)
  (power 2 10)
====================
>
```

We can try it also <a target="_blank" href="http://try.egison.org">online</a>.
Enjoy!

## Note for Developers

### How to Run Test

```
% cabal test
```

### How to Profile the Interpreter

```
% sudo apt-get install haskell-platform-doc haskell-platform-prof
% cabal install --enable-library-profiling --enable-executable-profiling
% egison +RTS -p -RTS -l sample/sequence.egi
% cat egison.prof
```

## Community

We have <a target="_blank" href="http://www.egison.org/community.html">a mailing list</a>.
Please join us!

We are on <a target="_blank" href="https://twitter.com/Egison_Lang">Twitter</a>.
Please follow us.

## Acknowledgement

I thank Ryo Tanaka, Takahisa Watanabe, Takuya Kuwahara and Kentaro Honda for their help to implement the interpreter.

## License

Copyright (c) 2011-2015, Satoshi Egi

Egison is released under the [MIT license](https://github.com/egison/egison/blob/master/LICENSE).

I used [husk-scheme](http://justinethier.github.io/husk-scheme/) by Justin Ethier as reference to implement the base part of the previous version of the interpreter.

## Sponsors

Egison is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
