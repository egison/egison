# Egison Language Support for Visual Studio Code

Visual Studio Code extension for the [Egison programming language](https://www.egison.org/).

## Features

This extension provides syntax highlighting and basic language support for Egison (`.egi` files).

### Syntax Highlighting

- **Keywords**: `def`, `match`, `matchAll`, `if`, `then`, `else`, `with`, `as`, etc.
- **Pattern Matching Operators**: `$` (pattern variables), `#` (value patterns), `&`, `|`, `!`, etc.
- **Types**: Built-in types like `Bool`, `Integer`, `Float`, `String`, `List`, `Vector`, `Matrix`, `Tensor`
- **Tensor Notation**: Subscript and superscript indices (`_i`, `~j`, etc.)
- **Mathematical Symbols**: Greek letters (α, β, γ, ...), mathematical operators (∂, ∇, ∫, ∑, etc.)
- **Comments**: Line comments (`--`) and block comments (`{- ... -}`)
- **Strings**: Double-quoted string literals
- **Numbers**: Integer and floating-point literals

### Language Features

- **Auto-closing pairs**: Automatically closes brackets, parentheses, and special Egison syntax like `[| |]`
- **Comment toggling**: Quick comment/uncomment with standard VS Code shortcuts
- **Bracket matching**: Highlights matching brackets and parentheses
- **Auto-indentation**: Basic automatic indentation support

## Installation

### From VSIX file (Manual Installation)

1. Open VS Code
2. Go to Extensions view (⇧⌘X on macOS, Ctrl+Shift+X on Windows/Linux)
3. Click on the "..." menu at the top of the Extensions view
4. Select "Install from VSIX..."
5. Select the `.vsix` file

### From Source

1. Copy the `vscode-extension` directory to your VS Code extensions folder:
   - **macOS/Linux**: `~/.vscode/extensions/`
   - **Windows**: `%USERPROFILE%\.vscode\extensions\`

2. Rename the directory to `egison-language-support`

3. Restart VS Code

## Usage

Once installed, the extension will automatically activate when you open a file with the `.egi` extension.

### Auto-Indentation

By default, auto-indentation is **disabled** to preserve the original formatting when copying and pasting code. If you prefer automatic indentation, you can enable it in your VS Code/Cursor settings:

```json
{
  "[egison]": {
    "editor.autoIndent": "full"
  }
}
```

Available options:
- `"none"`: No auto-indentation (default behavior)
- `"keep"`: Keep the indentation from the previous line
- `"brackets"`: Auto-indent based on brackets
- `"advanced"`: Language-aware indentation
- `"full"`: Full auto-indentation

## Examples

Here's what Egison code looks like with syntax highlighting:

```egison
-- Define a pattern matcher for algebraic data types
def suit := algebraicDataMatcher
  | spade
  | heart
  | club
  | diamond

-- Pattern matching with typed definition
def poker (cs: [Card]) : String :=
  match cs as multiset card with
  | [card $s $n, card #s #(n-1), card #s #(n-2), card #s #(n-3), card #s #(n-4)]
    -> "Straight flush"
  | [card _ $n, card _ #n, card _ #n, card _ #n, _]
    -> "Four of a kind"
  | _ -> "Nothing"

-- Tensor computations with mathematical notation
def g : Matrix MathExpr_i_j := 
  generateTensor (\\[a, b] -> V.* e_a_# e_b_#) [2, 2]
```

## About Egison

Egison is a programming language featuring:

- **Pattern matching**: Non-linear pattern matching with backtracking and multiple results
- **Tensor index notation**: Built-in support for mathematical tensor notation
- **Functional programming**: First-class functions, algebraic data types, and more

Learn more at [egison.org](https://www.egison.org/)

## Contributing

Contributions are welcome! Please visit the [Egison repository](https://github.com/egison/egison) to report issues or submit pull requests.

## License

MIT License

Copyright (c) 2011-2026 Satoshi Egi

See the LICENSE file in the Egison repository for full license text.

## Release Notes

### 0.2.0

- Initial release of VS Code extension
- Syntax highlighting for Egison language
- Support for pattern matching operators
- Support for tensor index notation
- Support for mathematical symbols
- Auto-closing pairs and bracket matching
- Comment toggling support

