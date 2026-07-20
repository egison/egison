# Changelog

## 5.1.0

### New Features

* Added an extensible computer-algebra tower with CAS type aliases, subtype
  declarations, runtime dispatch, coercion, and canonical reshaping.
* Added quotient algebras and first-class finite fields, including polynomial
  reduction over declared quotient relations.
* Added multivariate polynomial GCD, Groebner bases, polynomial normal forms,
  and declarative ideals.
* Added user-defined automatic rewrite rules and expanded symbolic
  simplification for radicals, exponentials, and algebraic extensions.
* Added explicit function-symbol metadata, strict differentiation, and unified
  symbolic partial differentiation.
* Added tensor index metadata and higher-order tensor-map lifting.

### Type System and Pattern Matching

* Strengthened matcher consistency and exhaustiveness checking, promoting
  incomplete matcher arms to type errors.
* Improved type-class hierarchy handling, dictionary expansion, instance
  selection, runtime type dispatch, and diagnostics for shadowed class methods.
* Added typing for unannotated top-level recursion.
* Improved inference for tensor callbacks, matcher aliases, structured pattern
  holes, and nonlinear pattern targets.

### Mathematical Libraries

* Added Groebner-basis, ideal, finite-field, algebraic-extension, and interval
  functionality to the standard mathematical libraries.
* Expanded the geometry, algebra, and number-theory samples, including Kahler
  geometry, elliptic curves over finite fields, and CAS-tower examples.
* Improved normalization, matrix and tensor operations, differential forms,
  and root simplification.

### Breaking Changes

* Reorganized the exposed Haskell CAS modules around
  `Language.Egison.Math.CAS`; the former `Math.Arith`, `Math.Expr`, and
  `Math.Normalize` modules are no longer exposed.
* Changed parts of the CAS representation, normalization behavior, symbolic
  function representation, and tensor/matrix APIs. Code relying on Egison 5.0
  implementation details may require updates.

### Testing and Documentation

* Reorganized and expanded the Cabal test suite with CAS, type-error, matcher,
  tensor, quotient-field, and normalization regression tests.
* Added design documentation for the extensible CAS tower, quotient mechanism,
  runtime dispatch, matcher slots, and symbolic simplification.

## 5.0.0
### New Features
* **Static Type System**: Introduced a static type system for Egison.
  - Type annotations for function parameters and return types: `def f (x: Integer) : Integer := x + 1`
  - Polymorphic type parameters: `def id {a} (x: a) : a := x`
  - Type inference with unification
* **Type Classes**: Added Haskell-style type class system.
  - Type class declarations: `class Eq a where ...`
  - Instance declarations: `instance Eq Integer where ...`
  - Superclass constraints with `extends`: `class Ord a extends Eq a where ...`
  - Type class constraints in function signatures: `def f {Eq a} (x: a) (y: a) : Bool := x == y`
* **Inductive Data Types**: Added support for user-defined algebraic data types.
  - Data type declarations: `inductive Maybe a := | Nothing | Just a`
  - Pattern inductive types: `inductive pattern [a] := | [] | (::) a [a]`
* **Symbol Declarations**: Added `declare symbol` for declaring symbolic variables used in tensor calculations.
  - `declare symbol x, y, z : Integer`
* **Pattern Function Declarations**: Added typed pattern function syntax.
  - `def pattern twin {a} (p1 : a) (p2 : MyList a) : MyList a := ...`
* **New Built-in Types**: `MathExpr`, `IO`, `DiffForm`

### Previous Unreleased Changes
* Added binary function notation for arbitrary 2-ary functions. ([#260](https://github.com/egison/egison/pull/260))
```hs
> let mod x y := x % y in 103 `mod` 10
3
```
* Swapped the notation for `QuoteExpr` and `QuoteSymbolExpr`. ([#262](https://github.com/egison/egison/issues/262))
```hs
> `(a + b) + `(a + b) -- QuoteExpr, which prevents (a + b) from unpacking
2 * '(a + b)
> 'exp x              -- QuoteSymbolExpr
exp x
```
* Changed the specification of `generateTensor` so that the generation function takes indices as a collection. ([#267](https://github.com/egison/egison/pull/267))

## 4.1.2
* Nothing changed (bumped version of `sweet-egison` package)

## 4.1.1
* Fix compilation issue with GHC 8.10 ([#248](https://github.com/egison/egison/issues/248))

## 4.1.0
### New Features
* Enabled user-defined infixes for expressions and patterns: <https://egison.readthedocs.io/en/latest/reference/basic-syntax.html#infix-declaration>
* Allowed `let` expression to decompose data. Unlike `match` expressions (of Egison), this does not require matchers and the decomposition pattern is limited.
```
> let (x :: _) := [1, 2, 3] in x
1
> let (x :: _) := [] in x
Primitive data pattern match failed
  stack trace: <stdin>
```
* Enabled data decomposition at lambda arguments.
```
> (\(x, _) -> x) (1, 2)
1
```
* Implemented partial application.
```
> let add x y := x + y in map (add 1) [1, 2, 3]
[2, 3, 4]
```
* Huge speedup in mathematical programs:
    * Reimplemented math normalization, which was originally implemented in Egison, to the interpreter in Haskell.
    * Implemented lazy evaluation on tensor elements.
* Added new syntax for symmetric / anti-symmetric tensors.

### Backward-incompatible Changes

* Changed the syntax to start definitions with `def` keyword.
```
def x := 1
```

* `io` was previously defined as a syntastic constructs, but it is changed into a primitive function.
Namely, users will need to wrap the arguments to `io` in a parenthesis, or insert `$` after `io`.
```
-- Invalid
io isEof ()

-- OK
io (isEOF ())
io $ isEOF ()
```

### Miscellaneous
* Added a command line option `--no-normalize` to turn off math normalization implemented in the standard math library.
* Revived TSV input options: <https://egison.readthedocs.io/en/latest/reference/command-line-options.html#reading-tsv-input>
* Deprecated `redefine`.

## 4.0.3

* Renamed `f.pi` into `pi`.

## 4.0.1

* Fixed a bug of not-patterns inside sequential patterns.
* Deprecated `procedure` (replace them with anonymous function)

## 4.0.0

* Enabled the Haskell-like new syntax by default.
