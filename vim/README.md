# Egison Support for Vim / Neovim

Vim plugin providing syntax highlighting, indentation, and filetype settings for the [Egison](https://www.egison.org) programming language (version 5).

## Features

- Syntax highlighting for all Egison 5 keywords and constructs:
  - Type system: `class`, `instance`, `inductive`, `extends`, `declare`
  - Definitions: `def`, `let`, `in`, `where`
  - Pattern matching: `match`, `matchAll`, `matchDFS`, `matchAllDFS`, `as`, `with`, `loop`, `forall`
  - Matchers: `matcher`, `algebraicDataMatcher`
  - Tensor operations: `tensor`, `generateTensor`, `contract`, `tensorMap`, `tensorMap2`, `transpose`, `flipIndices`, etc.
  - Built-in types: `Integer`, `MathExpr`, `Float`, `Bool`, `Char`, `String`, `IO`, `Tensor`, `Vector`, `Matrix`, `DiffForm`, `Matcher`, `Pattern`, `List`
- Pattern variable highlighting (`$x`, `$pat`)
- Value pattern highlighting (`#x`, `#(expr)`)
- Type class constraint highlighting (`{Eq a}`)
- Function name highlighting after `def`
- Type name highlighting after `class`, `instance`, `inductive`
- Tensor index notation highlighting (`v~i`, `T_j`)
- Mathematical symbol and Greek letter support
- Comment support: line comments (`--`) and block comments (`{- -}`)
- Basic indentation support
- Automatic filetype detection for `.egi` files

## Installation

### Prerequisites

Make sure your `~/.vimrc` (or `~/.config/nvim/init.vim` for Neovim) contains the following lines. Without these, syntax highlighting and filetype detection will not work:

```vim
syntax on
filetype plugin indent on
```

If `~/.vimrc` does not exist, create it with the above content.

### Method 1: Symlink (Vim 8+ / Neovim) -- Recommended

Vim 8 and Neovim have a built-in package manager that loads plugins from specific directories. You can create a symbolic link from the Egison repository to that location. This way, the plugin stays up to date whenever you pull the repository.

**For Vim:**

```bash
mkdir -p ~/.vim/pack/egison/start
ln -s /path/to/egison/vim ~/.vim/pack/egison/start/egison
```

**For Neovim:**

```bash
mkdir -p ~/.local/share/nvim/site/pack/egison/start
ln -s /path/to/egison/vim ~/.local/share/nvim/site/pack/egison/start/egison
```

Replace `/path/to/egison` with the actual path to your Egison repository. No restart command or plugin install step is needed -- just restart Vim/Neovim.

### Method 2: Copy Files Manually

If you prefer not to use symlinks, copy the files directly into Vim's runtime directories:

**For Vim:**

```bash
mkdir -p ~/.vim/{syntax,ftdetect,ftplugin,indent}
cp /path/to/egison/vim/syntax/egison.vim   ~/.vim/syntax/
cp /path/to/egison/vim/ftdetect/egison.vim ~/.vim/ftdetect/
cp /path/to/egison/vim/ftplugin/egison.vim ~/.vim/ftplugin/
cp /path/to/egison/vim/indent/egison.vim   ~/.vim/indent/
```

**For Neovim:**

```bash
mkdir -p ~/.config/nvim/{syntax,ftdetect,ftplugin,indent}
cp /path/to/egison/vim/syntax/egison.vim   ~/.config/nvim/syntax/
cp /path/to/egison/vim/ftdetect/egison.vim ~/.config/nvim/ftdetect/
cp /path/to/egison/vim/ftplugin/egison.vim ~/.config/nvim/ftplugin/
cp /path/to/egison/vim/indent/egison.vim   ~/.config/nvim/indent/
```

### Method 3: lazy.nvim (Neovim)

If you use [lazy.nvim](https://github.com/folke/lazy.nvim) as your Neovim plugin manager, add the following to your plugin list:

```lua
{
  dir = "/path/to/egison/vim",
  ft = "egison",
}
```

## Verifying Installation

1. Open an `.egi` file:

   ```bash
   vim sample/poker-hands.egi
   ```

2. Check that the filetype is set correctly:

   ```vim
   :set filetype?
   ```

   Should output `filetype=egison`.

3. Verify syntax highlighting is active:

   ```vim
   :syntax
   ```

## Plugin Structure

```
vim/
├── ftdetect/
│   └── egison.vim    -- Filetype detection for .egi files
├── ftplugin/
│   └── egison.vim    -- Filetype settings (comments, indentation)
├── indent/
│   └── egison.vim    -- Indentation rules
├── syntax/
│   └── egison.vim    -- Syntax highlighting definitions
└── README.md
```

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
```

## Compatibility

- Vim 8.0 or later
- Neovim 0.5 or later
- Supports Egison 5.x syntax with the static type system

## License

MIT License
