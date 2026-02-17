# Egison Mode for Emacs

A major mode for editing [Egison](https://www.egison.org) source code in Emacs.

## Features

- Syntax highlighting for all Egison keywords and constructs:
  - Type system: `class`, `instance`, `inductive`, `extends`, `declare`
  - Definitions: `def`, `let`, `in`, `where`
  - Pattern matching: `match`, `matchAll`, `matchDFS`, `matchAllDFS`, `as`, `with`, `loop`, `forall`
  - Matchers: `matcher`, `algebraicDataMatcher`
  - Tensor operations: `tensor`, `generateTensor`, `contract`, `tensorMap`, `tensorMap2`, `transpose`, `flipIndices`, etc.
  - Built-in types: `Integer`, `MathExpr`, `Float`, `Bool`, `Char`, `String`, `IO`, `Tensor`, `Vector`, `Matrix`, `DiffForm`, `Matcher`, `Pattern`, `List`
- Type annotation highlighting: `(x: Integer)`, return types after `:`
- Type class constraint highlighting: `{Eq a}`, `{Eq a, Ord b}`
- Pattern variable highlighting: `$x`, `$pat`
- Value pattern highlighting: `#x`, `#(expr)`
- Function name highlighting after `def`
- Data constructor highlighting in `inductive` declarations
- Index notation highlighting: `v~i`, `T_j`, `T~i~j_k`
- Comment support: line comments (`--`) and block comments (`{- -}`)
- Basic indentation support
- Greek letter and mathematical symbol support

## Installation

### Method 1: Manual Installation

1. Copy `egison-mode.el` to a directory in your Emacs `load-path`.

   For example, create a directory for Egison:

   ```bash
   mkdir -p ~/.emacs.d/site-lisp
   cp egison-mode.el ~/.emacs.d/site-lisp/
   ```

2. Add the following lines to your Emacs configuration file (`~/.emacs`, `~/.emacs.d/init.el`, etc.):

   ```elisp
   (add-to-list 'load-path "~/.emacs.d/site-lisp")
   (autoload 'egison-mode "egison-mode" "Major mode for editing Egison code." t)
   (add-to-list 'auto-mode-alist '("\\.egi\\'" . egison-mode))
   ```

3. Restart Emacs or evaluate the configuration with `M-x eval-buffer`.

### Method 2: Direct Load from Repository

If you have the Egison repository cloned, you can load the mode directly:

```elisp
(add-to-list 'load-path "/path/to/egison/elisp")
(autoload 'egison-mode "egison-mode" "Major mode for editing Egison code." t)
(add-to-list 'auto-mode-alist '("\\.egi\\'" . egison-mode))
```

Replace `/path/to/egison` with the actual path to your Egison repository.

### Method 3: use-package (Recommended for use-package users)

```elisp
(use-package egison-mode
  :load-path "/path/to/egison/elisp"
  :mode "\\.egi\\'")
```

## Configuration

### Enabling Level 2 Highlighting

By default, egison-mode uses basic (level 1) highlighting. To enable the more detailed level 2 highlighting, which includes type annotations, pattern variables, type constraints, and function name highlighting, add:

```elisp
(setq egison-font-lock-keywords egison-font-lock-keywords-2)
```

Or set it per buffer via a hook:

```elisp
(add-hook 'egison-mode-hook
  (lambda ()
    (setq font-lock-defaults
          '((egison-font-lock-keywords-2) nil t
            (("+*/=!?%:_~.'∂∇αβγδεζηθικλμνξοπρςστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ" . "w"))))))
```

### Custom Indentation

The basic indentation level can be customized by adjusting `egison-indent-line`. The default indentation uses 2 spaces.

## Usage

Once installed, any file with a `.egi` extension will automatically open in Egison mode. You can also manually activate it with:

```
M-x egison-mode
```

### Key Bindings

| Key       | Action               |
|-----------|----------------------|
| `C-j`     | Newline and indent   |
| `M-;`     | Comment/uncomment    |

## Verifying Installation

1. Open a `.egi` file:

   ```bash
   emacs sample/poker-hands.egi
   ```

2. Check that the mode line shows `Egison`.

3. Verify that keywords are highlighted in color.

## Example

Here is what the highlighting looks like for typical Egison code:

```egison
class Eq a where
  (==) (x: a) (y: a) : Bool

instance Eq Integer where
  (==) x y := x = y

inductive Maybe a :=
  | Nothing
  | Just a

def map {a, b} (fn: a -> b) (xs: [a]) : [b] :=
  match xs as list something with
    | [] -> []
    | $x :: $rest -> fn x :: map fn rest

declare symbol x, y, z : Integer

def v : Tensor Integer := [| x, y, z |]

v~i
```

## Compatibility

- Emacs 25.1 or later
- Supports Egison 5.x syntax with the static type system

## License

MIT License. See the header of `egison-mode.el` for details.
