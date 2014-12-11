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
We can try it also <a target="_blank" href="http://try.egison.org">online</a>.
Enjoy!

## Demonstrations

We can do non-linear pattern-matching against unfree data types in Egison.
An unfree data type is a data type whose data have no canonical form, a standard way to represent that object.
It enables us to write more elegant programs.

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

<hr/>
<img width="100%" src="https://raw.githubusercontent.com/egison/egison/master/images/mahjong.png" />
<hr/>

Isn't it exciting?
The pattern-matching of Egison is very powerful.
We can use it for pattern-matching also against graphs and tree-structures such as XML.

## Comparison with Related Work

There are <a target="_blank" href="https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns#Relatedwork">a lot of existing work</a> for pattern-matching.

The advantage of Egison is that it realizes **all of the following features** at the same time.

* Modularization of the way of pattern-matching for each data type
* Pattern-matching with multiple results (backtracking)
* Non-linear pattern-matching

It enables us to express non-linear pattern-matching against unfree data types intuitively as above demonstrations.

Furthermore, Egison realizes the following feature. We can even modularize patterns like functions keeping above features.

* Non-linear pattern-matching with **lexical scoping**

The <a target="_blank" href="http://www.egison.org/manual/mechanism.html">Pattern-Matching Mechanism</a> section in Egison developer's manual explains how we realizes that.

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

## Sponsors

Egison is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
