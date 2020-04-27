# The Egison Programming Language
[![Build Status](https://travis-ci.org/egison/egison.svg?branch=master)](https://travis-ci.org/egison/egison)

Egison is a functional programming language featuring its expressive pattern-matching facility.
Egison allows users to define efficient and expressive pattern-matching methods for arbitrary user-defined data types including non-free data types such as lists, multisets, sets, trees, graphs, and mathematical expressions.
This is the repository of the interpreter of Egison.

For more information, visit <a target="_blank" href="https://www.egison.org">our website</a>.

## Refereed Papers

### Pattern Matching

* Satoshi Egi, Yuichi Nishiwaki: [Non-linear Pattern Matching with Backtracking for Non-free Data Types](https://arxiv.org/abs/1808.10603) (APLAS 2018)
* Satoshi Egi, Yuichi Nishiwaki: [Functional Programming in Pattern-Match-Oriented Programming Style](https://doi.org/10.22152/programming-journal.org/2020/4/7) (<programming> 2020)

### Tensor Index Notation

* Satoshi Egi: [Scalar and Tensor Parameters for Importing Tensor Index Notation including Einstein Summation Notation](https://arxiv.org/abs/1702.06343) (Scheme Workshop 2017)

## Non-Linear Pattern Matching for Non-Free Data Types

We can describe non-linear pattern matching for non-free data types in Egison.
A non-free data type is a data type whose data have no canonical form, a standard way to represent that object.
For example, multisets are non-free data types because the multiset {a,b,b} has two other equivalent but literally different forms {b,a,b} and {b,b,a}.
Expressive pattern matching for these data types enables us to write elegant programs.

### Twin Primes

We can use pattern matching for enumeration.
The following code enumerates all twin primes from the infinite list of prime numbers with pattern matching!

```hs
twinPrimes :=
  matchAll primes as list integer with
  | _ ++ $p :: #(p + 2) :: _ -> (p, p + 2)

take 8 twinPrimes
-- [(3, 5), (5, 7), (11, 13), (17, 19), (29, 31), (41, 43), (59, 61), (71, 73)]
```

### Poker Hands

The following code is the program that determines poker-hands written in Egison.
All hands are expressed in a single pattern.

```hs
poker cs :=
  match cs as multiset card with
  | card $s $n :: card #s #(n-1) :: card #s #(n-2) :: card #s #(n-3) :: card #s #(n-4) :: _
    -> "Straight flush"
  | card _ $n :: card _ #n :: card _ #n :: card _ #n :: _ :: []
    -> "Four of a kind"
  | card _ $m :: card _ #m :: card _ #m :: card _ $n :: card _ #n :: []
    -> "Full house"
  | card $s _ :: card #s _ :: card #s _ :: card #s _ :: card #s _ :: []
    -> "Flush"
  | card _ $n :: card _ #(n-1) :: card _ #(n-2) :: card _ #(n-3) :: card _ #(n-4) :: []
    -> "Straight"
  | card _ $n :: card _ #n :: card _ #n :: _ :: _ :: []
    -> "Three of a kind"
  | card _ $m :: card _ #m :: card _ $n :: card _ #n :: _ :: []
    -> "Two pair"
  | card _ $n :: card _ #n :: _ :: _ :: _ :: []
    -> "One pair"
  | _ :: _ :: _ :: _ :: _ :: [] -> "Nothing"
```

### Graphs

We can pattern-match against graphs.
We can write program to solve the travelling salesman problem in a single pattern-matching expression.

```hs
graph := multiset (string, multiset (string, integer))

graphData :=
  [("Berlin", [("New York", 14), ("London", 2), ("Tokyo", 14), ("Vancouver", 13)]),
   ("New York", [("Berlin", 14), ("London", 12), ("Tokyo", 18), ("Vancouver", 6)]),
   ("London", [("Berlin", 2), ("New York", 12), ("Tokyo", 15), ("Vancouver", 10)]),
   ("Tokyo", [("Berlin", 14), ("New York", 18), ("London", 15), ("Vancouver", 12)]),
   ("Vancouver", [("Berlin", 13), ("New York", 6), ("London", 10), ("Tokyo", 12)])]

trips :=
  let n := length graphData in
    matchAll graphData as graph with
    | (#"Berlin", (($s_1,$p_1) : _)) ::
        loop $i (2, n - 1)
          ((#s_(i - 1), ($s_i, $p_i) :: _) :: ...)
          ((#s_(n - 1), (#"Berlin" & $s_n, $p_n) :: _) :: [])
    -> sum (map (\i -> p_i) [1..n]), map (\i -> s_i) [1..n]

car (sortBy (\(_, x), (_, y) -> compare x y)) trips)
-- (["London", "New York", "Vancouver", "Tokyo"," Berlin"], 46)
```

## Egison as a Computer Algebra System

As an application of Egison pattern matching, we have implemented a computer algebra system on Egison.
The most part of this computer algebra system is written in Egison and extensible using Egison.

### Symbolic Algebra

Egison treats unbound variables as symbols.

```
> x
x
> (x + y)^2
x^2 + 2 * x * y + y^2
> (x + y)^4
x^4 + 4 * x^3 * y + 6 * x^2 * y^2 + 4 * x * y^3 + y^4
```

We can handle algebraic numbers, too.

* [Definition of `sqrt` in `root.egi`](https://github.com/egison/egison/blob/master/lib/math/algebra/root.egi)

```
> sqrt x
sqrt x
> sqrt 2
sqrt 2
> x + sqrt y
x + sqrt y
```

### Complex Numbers

The symbol `i` is defined to rewrite `i^2` to `-1` in Egison library.

* [Rewriting rule for `i` in `normalize.egi`](https://github.com/egison/egison/blob/master/lib/math/normalize.egi)

```
> i * i
-1
> (1 + i) * (1 + i)
2 * i
> (x + y * i) * (x + y * i)
x^2 + 2 * x * y * i - y^2
```

### Square Root

The rewriting rule for `sqrt` is also defined in Egison library.

* [Rewriting rule for `sqrt` in `normalize.egi`](https://github.com/egison/egison/blob/master/lib/math/normalize.egi)

```
> sqrt 2 * sqrt 2
2
> sqrt 6 * sqrt 10
2 * sqrt 15
> sqrt (x * y) * sqrt (2 * x)
x * sqrt 2 * sqrt y
```

### The 5th Roots of Unity

The following is a sample to calculate the 5th roots of unity.

* [Definition of `q-f'` in `equations.egi`](https://github.com/egison/egison/blob/master/lib/math/algebra/equations.egi)

```
> qF' 1 1 (-1)
((-1 + sqrt 5) / 2, (-1 - sqrt 5) / 2)
> t := fst (qF' 1 1 (-1))
> qF' 1 (-t) 1
((-1 + sqrt 5 + sqrt 2 * sqrt (-5 - sqrt 5)) / 4, (-1 + sqrt 5 - sqrt 2 * sqrt (-5 - sqrt 5)) / 4)
> z := fst (qF' 1 (-t) 1)
> z
(-1 + sqrt 5 + sqrt 2 * sqrt (-5 - sqrt 5)) / 4
> z ^ 5
1
```

### Differentiation

We can implement differentiation easily in Egison.

* [Definition of `d/d` in `derivative.egi`](https://github.com/egison/egison/blob/master/lib/math/analysis/derivative.egi)

```
> d/d (x ^ 3) x
3 * x^2
> d/d (e ^ (i * x)) x
exp (x * i) * i
> d/d (d/d (log x) x) x
-1 / x^2
> d/d (cos x * sin x) x
-2 * (sin x)^2 + 1
```

### Taylor Expansion

The following sample executes Taylor expansion on Egison.
We verify [Euler's formula](https://en.wikipedia.org/wiki/Euler%27s_formula) in the following sample.

* [Definition of `taylor-expansion` in `derivative.egi`](https://github.com/egison/egison/blob/master/lib/math/analysis/derivative.egi)

```
> take 8 (taylorExpansion (exp (i * x)) x 0)
[1, x * i, - x^2 / 2, - x^3 * i / 6, x^4 / 24, x^5 * i / 120, - x^6 / 720, - x^7 * i / 5040]
> take 8 (taylorExpansion (cos x) x 0)
[1, 0, - x^2 / 2, 0, x^4 / 24, 0, - x^6 / 720, 0]
> take 8 (taylorExpansion (i * sin x) x 0)
[0, x * i, 0, - x^3 * i / 6, 0, x^5 * i / 120, 0, - x^7 * i / 5040]
> take 8 (map2 (+) (taylorExpansion (cos x) x 0) (taylorExpansion (i * sin x) x 0))
[1, x * i, - x^2 / 2, - x^3 * i / 6, x^4 / 24, x^5 * i / 120, - x^6 / 720, - x^7 * i / 5040]
```

### Tensor Index Notation

Egison supports tesnsor index notation.
We can use [Einstein notation](https://en.wikipedia.org/wiki/Einstein_notation) to express arithmetic operations between tensors.

The method for importing tensor index notation into programming is discussed in [Egison tensor paper](https://arxiv.org/abs/1702.06343).

The following sample is from [Riemann Curvature Tensor of S2 - Egison Mathematics Notebook](https://www.egison.org/math/riemann-curvature-tensor-of-S2.html).


```hs
-- Parameters
x := [| θ, φ |]

X := [| r * (sin θ) * (cos φ) -- x
      , r * (sin θ) * (sin φ) -- y
      , r * (cos θ)           -- z
      |]

e_i_j := (∂/∂ X_j x~i)

-- Metric tensors
g_i_j := generateTensor (\x y -> V.* e_x_# e_y_#) [2, 2]
g~i~j := M.inverse g_#_#

g_#_# -- [| [| r^2, 0 |], [| 0, r^2 * (sin θ)^2 |] |]_#_#
g~#~# -- [| [| 1 / r^2, 0 |], [| 0, 1 / (r^2 * (sin θ)^2) |] |]~#~#

-- Christoffel symbols
Γ_i_j_k := (1 / 2) * (∂/∂ g_i_k x~j + ∂/∂ g_i_j x~k - ∂/∂ g_j_k x~i)

Γ_1_#_# -- [| [| 0, 0 |], [| 0, -1 * r^2 * (sin θ) * (cos θ) |] |]_#_#
Γ_2_#_# -- [| [| 0, r^2 * (sin θ) * (cos θ) |], [| r^2 * (sin θ) * (cos θ), 0 |] |]_#_#

Γ~i_j_k := withSymbols [m]
  g~i~m . Γ_m_j_k

Γ~1_#_# -- [| [| 0, 0 |], [| 0, -1 * (sin θ) * (cos θ) |] |]_#_#
Γ~2_#_# -- [| [| 0, (cos θ) / (sin θ) |], [| (cos θ) / (sin θ), 0 |] |]_#_#

-- Riemann curvature
R~i_j_k_l := withSymbols [m]
  ∂/∂ Γ~i_j_l x~k - ∂/∂ Γ~i_j_k x~l + Γ~m_j_l . Γ~i_m_k - Γ~m_j_k . Γ~i_m_l

R~#_#_1_1 -- [| [| 0, 0 |], [| 0, 0 |] |]~#_#
R~#_#_1_2 -- [| [| 0, (sin θ)^2 |], [| -1, 0 |] |]~#_#
R~#_#_2_1 -- [| [| 0, -1 * (sin θ)^2 |], [| 1, 0 |] |]~#_#
R~#_#_2_2 -- [| [| 0, 0 |], [| 0, 0 |] |]~#_#
```

### Differential Forms

By designing the index completion rules for omitted indices, we can use the above notation to express a calculation handling the differential forms.

The following sample is from [Curvature Form - Egison Mathematics Notebook](https://www.egison.org/math/curvature-form.html).

```hs
-- Parameters and metric tensor
x := [| θ, φ |]

g_i_j := [| [| r^2, 0 |], [| 0, r^2 * (sin θ)^2 |] |]_i_j
g~i~j := [| [| 1 / r^2, 0 |], [| 0, 1 / (r^2 * (sin θ)^2) |] |]~i~j

-- Christoffel symbols
Γ_j_l_k := (1 / 2) * (∂/∂ g_j_l x~k + ∂/∂ g_j_k x~l - ∂/∂ g_k_l x~j)

Γ~i_k_l := withSymbols [j] g~i~j . Γ_j_l_k

-- Exterior derivative
d %t := !(flip ∂/∂) x t

-- Wedge product
infixl expression 7 ∧

(∧) %x %y := x !. y

-- Connection form
ω~i_j := Γ~i_j_#

-- Curvature form
Ω~i_j := withSymbols [k]
  antisymmetrize (d ω~i_j + ω~i_k ∧ ω~k_j)

Ω~#_#_1_1 -- [| [| 0, 0 |], [| 0, 0 |] |]~#_#
Ω~#_#_1_2 -- [| [| 0, (sin θ)^2  / 2|], [| -1 / 2, 0 |] |]~#_#
Ω~#_#_2_1 -- [| [| 0, -1 * (sin θ)^2 / 2 |], [| 1 / 2, 0 |] |]~#_#
Ω~#_#_2_2 -- [| [| 0, 0 |], [| 0, 0 |] |]~#_#

```

### Egison Mathematics Notebook

Here are more samples.

* [Egison Mathematics Notebook](https://www.egison.org/math/)

## Comparison with Related Work

There are <a target="_blank" href="https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns#Relatedwork">a lot of existing work</a> for pattern matching.

The advantage of Egison is that it fulfills the following two requirements at the same time.

1. Efficient backtracking algorithm for non-linear pattern matching.
2. Extensibility of patterns.

Additionally, it fulfills the following requirements.

3. Polymorphism of patterns.
4. Pattern matching with infinitely many results.

Please read <a target="_blank" href="https://arxiv.org/abs/1808.10603">our paper</a> for details.

## Installation

Installation guide for [MacOS](https://www.egison.org/getting-started/getting-started-mac.html), [Linux](https://www.egison.org/getting-started/getting-started-linux.html) and [Windows](https://www.egison.org/getting-started/getting-started-windows.html) are available on our website.

If you are a beginner of Egison, it would be better to install <a target="_blank" href="https://github.com/egison/egison-tutorial">`egison-tutorial`</a>.

We also have [online interpreter](http://console.egison.org) and [online tutorial](http://try.egison.org/).
Enjoy!

## Notes for Developers

### How to Build
```
$ stack init
$ stack build --fast
```

### How to Run Test
```
$ stack init
$ stack test
```

## Community

We have <a target="_blank" href="https://www.egison.org/community.html">a mailing list</a>.
Please join us!

We are on <a target="_blank" href="https://twitter.com/Egison_Lang">Twitter</a>.
Please follow us.

## License

Egison is released under the [MIT license](https://github.com/egison/egison/blob/master/LICENSE).

We used [husk-scheme](http://justinethier.github.io/husk-scheme/) by Justin Ethier as reference to implement the base part of the previous version of the interpreter.

## Sponsors

Egison is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
