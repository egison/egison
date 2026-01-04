# Change Log

All notable changes to the Egison VS Code extension will be documented in this file.

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

