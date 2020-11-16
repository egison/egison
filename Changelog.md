# Changelog

## Latest
### New Features
* Added binary function notation for arbitrary 2-ary functions. ([#260](https://github.com/egison/egison/pull/260))
```hs
> let mod x y := x % y in 103 `mod` 10
3
```

### Backward-incompatible Changes
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
