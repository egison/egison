# The Egison Programming Language

Egison is the **pattern-matching-oriented**, purely functional programming language.
We can directly represent pattern-matching against lists, multisets, sets, trees, graphs and any kind of data types.
This is the repository of the interpreter of Egison.

For more information, visit <a target="_blank" href="https://www.egison.org">our website</a>.

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

## Egison as a Computer Algebra System

As an application of Egison, we implemented a computer algebra system on Egison.
The most part of this computer algebra system is written in Egison and extensible in Egison.

### Symbolic Algebra

Unbound variables are treated as symbols.

```
> x
x
> (** (+ x y) 2)
(+ x^2 (* 2 x y) y^2)
> (** (+ x y) 10)
(+ x^10 (* 10 x^9 y) (* 45 x^8 y^2) (* 120 x^7 y^3) (* 210 x^6 y^4) (* 252 x^5 y^5) (* 210 x^4 y^6) (* 120 x^3 y^7) (* 45 x^2 y^8) (* 10 x y^9) y^10)
```

We can handle algebraic numbers, too.

* [Definition of `sqrt` in `root.egi`](https://github.com/egison/egison/blob/master/lib/math/algebra/root.egi)

```
> (sqrt x)
(sqrt x)
> (sqrt 2)
(sqrt 2)
> (sqrt 4)
2
> (+ x (sqrt y))
(+ x (sqrt y))
```

### Complex Numbers

The symbol `i` is defined to rewrite `i^2` to `-1` in Egison library.

* [Rewriting rule for `i` in `normalize.egi`](https://github.com/egison/egison/blob/master/lib/math/normalize.egi)

```
> (* i i)
-1
> (* (+ 1 (* 1 i))  (+ 1 (* 1 i)))
(* 2 i)
> (** (+ 1 (* 1 i)) 10)
(* 32 i)
> (* (+ x (* y i))  (+ x (* y i)))
(+ x^2 (* 2 i x y) (* -1 y^2))
```

### Square Root

The rewriting rule for `sqrt` is also defined in Egison library.

* [Rewriting rule for `sqrt` in `normalize.egi`](https://github.com/egison/egison/blob/master/lib/math/normalize.egi)

```
> (* (sqrt 2) (sqrt 2))
2
> (* (sqrt 6) (sqrt 10))
(* 2 (sqrt 15))
> (sqrt x)
(sqrt x)
> (* (sqrt (* x y)) (sqrt (* 2 x)))
(* x (sqrt 2) (sqrt y))
```

### The 5th Roots of Unity

The following is a sample to calculate the 5th roots of unity.

* [Definition of `q-f'` in `equations.egi`](https://github.com/egison/egison/blob/master/lib/math/algebra/equations.egi)

```
> (q-f' 1 1 -1)
[(/ (+ -1 (sqrt 5)) 2) (/ (+ -1 (* -1 (sqrt 5))) 2)]
> (define $t (fst (q-f' 1 1 -1)))
> (q-f' 1 (* -1 t) 1)
[(/ (+ -1 (sqrt 5) (sqrt (+ -10 (* -2 (sqrt 5))))) 4) (/ (+ -1 (sqrt 5) (* -1 (sqrt (+ -10 (* -2 (sqrt 5)))))) 4)]
> (define $z (fst (q-f' 1 (* -1 t) 1)))
> z
(/ (+ -1 (sqrt 5) (sqrt (+ -10 (* -2 (sqrt 5))))) 4)
> (** z 5)
1
```

### Differentiation

We can implement differentiation easily in Egison.

* [Definition of `d/d` in `derivative.egi`](https://github.com/egison/egison/blob/master/lib/math/analysis/derivative.egi)

```
> (d/d (** x 3) x)
(* 3 x^2)
> (d/d (** e (* i x)) x)
(* i (** e (* i x)))
> (d/d (d/d (log x) x) x)
(/ -1 x^2)
> (d/d (* (cos x) (sin x)) x)
(+ (* -1 (sin x)^2) (cos x)^2)
```

### Taylor Expansion

The following sample executes Taylor expansion on Egison.
We verify [Euler's formula](https://en.wikipedia.org/wiki/Euler%27s_formula) in the following sample.

* [Definition of `taylor-expansion` in `derivative.egi`](https://github.com/egison/egison/blob/master/lib/math/analysis/derivative.egi)

```
> (take 8 (taylor-expansion (** e (* i x)) x 0))
{1 (* i x) (/ (* -1 x^2) 2) (/ (* -1 i x^3) 6) (/ x^4 24) (/ (* i x^5) 120) (/ (* -1 x^6) 720) (/ (* -1 i x^7) 5040)}
> (take 8 (taylor-expansion (cos x) x 0))
{1 0 (/ (* -1 x^2) 2) 0 (/ x^4 24) 0 (/ (* -1 x^6) 720) 0}
> (take 8 (taylor-expansion (* i (sin x)) x 0))
{0 (* i x) 0 (/ (* -1 i x^3) 6) 0 (/ (* i x^5) 120) 0 (/ (* -1 i x^7) 5040)}
> (take 8 (map2 + (taylor-expansion (cos x) x 0) (taylor-expansion (* i (sin x)) x 0)))
{1 (* i x) (/ (* -1 x^2) 2) (/ (* -1 i x^3) 6) (/ x^4 24) (/ (* i x^5) 120) (/ (* -1 x^6) 720) (/ (* -1 i x^7) 5040)}
```

### Vector and Matrix

We support tesnsor algebra.
We use [Einstein notation](https://en.wikipedia.org/wiki/Einstein_notation) to express arithmetic operations between tensors.

A tensor is expressed by enclosing its dimensions and elements with `(|` and `|)`.

```
(| <dimensions> <elements> |)
```


```
> (define $V1 (| {3} {x_1 x_2 x_3} |))
> (define $V2 (| {3} {y_1 y_2 y_3} |))
> (. V1~i V2_i)
(+ (* x_1 y_1) (* x_2 y_2) (* x_3 y_3))
> (. V1_i V2_j)
(| {3 3} {(* x_1 y_1) (* x_1 y_2) (* x_1 y_3) (* x_2 y_1) (* x_2 y_2) (* x_2 y_3) (* x_3 y_1) (* x_3 y_2) (* x_3 y_3)} |)_i_j
```

```
> (define $M1 (generate-tensor 2#x_%1_%2 {2 2}))
> (define $M2 (generate-tensor 2#y_%1_%2 {2 2}))
> M1
(| {2 2} {x_1_1 x_1_2 x_2_1 x_2_2} |)
> M2
(| {2 2} {y_1_1 y_1_2 y_2_1 y_2_2} |)
> M1_i_1
(| {2} {x_1_1 x_2_1} |)_i
> M1_1_j
(| {2} {x_1_1 x_1_2} |)_j
> (. M1_i_j M2_j_k)
(| {2 2} {(+ (* x_1_1 y_1_1) (* x_1_2 y_2_1)) (+ (* x_1_1 y_1_2) (* x_1_2 y_2_2)) (+ (* x_2_1 y_1_1) (* x_2_2 y_2_1)) (+ (* x_2_1 y_1_2) (* x_2_2 y_2_2))} |)_i_k
> (. M1_i_j M2_k_l)
(| {2 2 2 2} {(* x_1_1 y_1_1) (* x_1_1 y_1_2) (* x_1_1 y_2_1) (* x_1_1 y_2_2) (* x_1_2 y_1_1) (* x_1_2 y_1_2) (* x_1_2 y_2_1) (* x_1_2 y_2_2) (* x_2_1 y_1_1) (* x_2_1 y_1_2) (* x_2_1 y_2_1) (* x_2_1 y_2_2) (* x_2_2 y_1_1) (* x_2_2 y_1_2) (* x_2_2 y_2_1) (* x_2_2 y_2_2)} |)_i_j_k_l
```

Addition of tensors and arithmetic between a scalar and a tensor are expressed as follow.

* [tensor.egi](https://github.com/egison/egison/blob/master/lib/math/algebra/tensor.egi)

```
> (define $X (generate-tensor 2#x_%1_%2 {2 2}))
> (define $Y (generate-tensor 2#y_%1_%2 {2 2}))
> X
(| {2 2} {x_1_1 x_1_2 x_2_1 x_2_2} |)
> Y
(| {2 2} {y_1_1 y_1_2 y_2_1 y_2_2} |)
> (T.map2 + X_i_j  Y_j_i)
(| {2 2} {(+ x_1_1 y_1_1) (+ x_1_2 y_2_1) (+ x_2_1 y_1_2) (+ x_2_2 y_2_2)} |)_i_j
> (T.+ X 100)
(| {2 2} {(+ x_1_1 100) x_1_2 x_2_1 (+ x_2_2 100)} |)
```

### Egison Math Notebook

Here are more samples.

* [Egison Math Notebook](https://www.egison.org/math/)

## Comparison with Related Work

There are <a target="_blank" href="https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns#Relatedwork">a lot of existing work</a> for pattern-matching.

The advantage of Egison is that it achieves **all of the following features** at the same time.

* Modularization of the way of pattern-matching for each data type
* Pattern-matching with multiple results (backtracking)
* Non-linear pattern-matching with **lexical scoping**
* Parametric polymorphism of pattern-constructors

The <a target="_blank" href="https://www.egison.org/manual/mechanism.html">Pattern-Matching Mechanism</a> section in Egison developer's manual explains how we achieve that.

Please read <a target="_blank" href="http://arxiv.org/abs/1407.0729">our paper on arXiv.org</a> for details.

## Installation

If you are using Linux, please install `libncurses-dev` at first.

```
% sudo apt-get install libncurses-dev # on Debian
```

To compile Egison, you also need to install <a target="_blank" href="https://www.haskell.org/platform/">Haskell Platform</a>.

After you installed Haskell Platform, run the following commands on the terminal.

```
% cabal update
% cabal install egison
```

Now, you can try Egison.

```
% egison
Egison Version X.X.X(C) 2011-2014 Satoshi Egi
https://www.egison.org
Welcome to Egison Interpreter!
> ^D
Leaving Egison Interpreter.
```

If you are a beginner of Egison, it would be better to install <a target="_blank" href="https://github.com/egison/egison-tutorial">`egison-tutorial`</a>.

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

We have <a target="_blank" href="https://www.egison.org/community.html">a mailing list</a>.
Please join us!

We are on <a target="_blank" href="https://twitter.com/Egison_Lang">Twitter</a>.
Please follow us.

## Acknowledgement

I thank Ryo Tanaka, Takahisa Watanabe, Takuya Kuwahara and Kentaro Honda for their help to implement the interpreter.

## License

Copyright (c) 2011-2016, Satoshi Egi

Egison is released under the [MIT license](https://github.com/egison/egison/blob/master/LICENSE).

I used [husk-scheme](http://justinethier.github.io/husk-scheme/) by Justin Ethier as reference to implement the base part of the previous version of the interpreter.

## Sponsors

Egison is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
