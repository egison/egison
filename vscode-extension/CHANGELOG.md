# Change Log

All notable changes to the Egison VS Code extension will be documented in this file.

## [0.3.0] - 2026-02-17

### Added
- Type system keywords: `class`, `instance`, `inductive`, `extends`, `declare`, `pattern`
- Dedicated highlighting for type class definitions and instances
- Highlighting for `def pattern` (pattern function) and `inductive pattern` (pattern inductive type) declarations
- Highlighting for `declare symbol` declarations
- Type class constraint highlighting in braces: `{Eq a}`, `{Eq a, Ord b}`
- Typed parameter highlighting: `(x: Integer)`
- Function name highlighting after `def` keyword
- New built-in types: `IO`, `DiffForm`, `MathExpr`
- New keywords from parser: `in`, `loop`, `forall`, `execute`, `memoizedLambda`, `cambda`, `capply`, `function`
- New tensor operations: `tensorMap2`, `transpose`, `flipIndices`, `subrefs`, `subrefs!`, `suprefs`, `suprefs!`, `userRefs`, `userRefs!`
- Prime arithmetic operators: `+'`, `-'`, `*'`, `/'`, `^'`
- Wedge operator: `∧`
- Indentation rules for Egison constructs
- Character literal support (single quotes)

### Removed
- `tensorShape`, `generateArray`, `arrayBounds`, `arrayRef` (not in parser)
- `assertEqualM` (not in primitives)
- `io` as keyword (not a reserved word; `IO` is the type)
- Redundant `constant.other.constructor.egison` pattern (was conflicting with type highlighting)

### Changed
- Reorganized grammar into semantic categories: definitions, type-system, keywords, etc.
- Improved operator precedence to avoid `:=` being matched as `:` + `=`
- More precise colon operator matching to avoid conflicts with type annotations

## [0.2.0] - 2026-01-04

### Added
- Initial release of Egison language support for VS Code
- Syntax highlighting for Egison language features:
  - Keywords (def, match, matchAll, if, then, else, etc.)
  - Pattern matching operators ($, #, &, |, !, etc.)
  - Type annotations and type variables
  - Tensor index notation (_i, ~j, etc.)
  - Mathematical symbols (Greek letters, ∂, ∇, etc.)
  - Comments (line and block)
  - String literals
  - Numbers (integer and float)
- Language configuration:
  - Auto-closing pairs for brackets, parentheses, and tensor notation
  - Comment toggling support
  - Bracket matching
  - Basic auto-indentation
- Documentation:
  - README with feature description and examples
  - Installation guide
  - Usage instructions

### Changed
- Updated from legacy Egison 3.x syntax to current Egison 4.x syntax
- Improved keyword recognition for current language version
- Enhanced type system support

## Future Plans

- Language Server Protocol (LSP) support
- Code completion
- Go to definition
- Find references
- Hover documentation
- Signature help
- Code formatting
- Error diagnostics
- Snippet support

