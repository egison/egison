# Changelog

## Latest

* Enabled user-defined infixes for expressions and patterns.
* Added a command line option `--no-normalize` to turn off math normalization implemented in the standard math library.
* Revived TSV input options: <https://egison.readthedocs.io/en/latest/reference/command-line-options.html#reading-tsv-input>
* Reimplemented math normalization, which was originally implemented in Egison, to the interpreter in Haskell. This led to a huge speedup of some of the mathematical programs.
* Deprecated `redefine`.
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

## 4.0.3

* Renamed `f.pi` into `pi`.

## 4.0.1

* Fixed a bug of not-patterns inside sequential patterns.
* Deprecated `procedure` (replace them with anonymous function)

## 4.0.0

* Enabled the Haskell-like new syntax by default.
