# Function Symbol 仕様

## 概要

`function` キーワードで定義されるシンボリック関数。
具体的な実装を持たないが、微分・代入などのシンボリック操作が可能。

```egison
declare symbol x, y
def f := function (x)        -- 1変数関数 f(x)
def g := function (x, y)     -- 2変数関数 g(x, y)
```

## 内部表現

Haskell側: `FunctionData ScalarData [ScalarData]`

```
FunctionData name args
```

| フィールド | 型 | 例 (`def f := function (x, y)`) |
|---|---|---|
| `name` | `ScalarData` | `Symbol "" "f" []` |
| `args` | `[ScalarData]` | `[x, y]`（declare symbolで宣言されたシンボルへの参照） |

Egison側パターンマッチ:
```egison
match expr as mathExpr with
  | func $name $args -> ...
```

inductive pattern 宣言:
```egison
inductive pattern MathExpr :=
  | ...
  | func MathExpr [MathExpr]   -- 2フィールド（name, args）
```

## 動作

### 表示（show）

引数を**常に表示**する。

```
show f           -- "f x"
show g           -- "g x y"
show (f 0)       -- "f 0"
show (f a)       -- "f a"
show (g 0 y)     -- "g 0 y"
```

### 関数適用（f 0 構文）

`f 0` は args を `[0]` に置き換えた新しい `FunctionData` を返す。
引数の個数が合わない場合はエラー。

```egison
f 0        -- FunctionData("f", [0]) → show: "f 0"
f a        -- FunctionData("f", [a]) → show: "f a"
g 0 1      -- FunctionData("g", [0, 1]) → show: "g 0 1"
g 0        -- Error: function applied to wrong number of arguments: expected 2, got 1
```

### 微分（derivative.egi）

連鎖律で実装。偏微分の記号は**整数インデックス**（`|1`, `|2`, ...）:

```egison
-- ∂/∂' 内の func ケース
| func _ $args ->
   sum (map2 (\s r -> (userRefs f [s]) * ∂/∂' r x) (between 1 (length args)) args)
```

`def f := function (x, y)` に対して:
```
∂/∂ f x  = f|1 x y * (∂x/∂x) + f|2 x y * (∂y/∂x) = f|1 x y
∂/∂ f y  = f|2 x y
∂/∂ f z  = 0（f は z に依存しない）
∂/∂ (∂/∂ f x) y = f|1|2 x y
```

### 連鎖律の例

```egison
declare symbol r
def x := r^2
def f := function (x)   -- f(x) where x = r^2

show f                   -- "f r^2"
show (∂/∂ f r)           -- "2 * f|1 r^2 * r"
```

### 変数代入（V.substitute）

`mapSymbols` の `func` ケースで、`args` 内のシンボルを再帰的に置換:
```egison
| func _ $args ->
    let args' := map (mapSymbols fn) args
    in if args = args' then x ^' n
       else fn (updateFunctionArgs x args') ^' n
```

```
V.substitute [|x|] [|0|] f       -- f(x) → f(0)  内部: args=[0], show: "f 0"
V.substitute [|x|] [|a|] f       -- f(x) → f(a)  内部: args=[a], show: "f a"
V.substitute [|x,y|] [|0,0|] g   -- g(x,y) → g(0,0)
```

### 等値性

`name` と `args` の両方が等しい場合のみ等しい:
```
f(x) = f(x)   -- True
f(x) = f(0)   -- False
f(0) = f(a)   -- False
```

## 実装の変更点（argnames削除）

旧: `FunctionData ScalarData [String] [ScalarData]` (name, argnames, args)  
新: `FunctionData ScalarData [ScalarData]` (name, args)

### 変更ファイル

- `hs-src/Language/Egison/Math/Expr.hs` - 型定義・Eq・func pattern・pretty
- `hs-src/Language/Egison/Core.hs` - IFunctionExpr評価・userRefs・applyRef・PDFunctionPat
- `hs-src/Language/Egison/Data.hs` - Egison値との相互変換
- `hs-src/Language/Egison/Primitives.hs` - updateFunctionArgs
- `hs-src/Language/Egison/PrettyMath/AST.hs` - toMathExpr
- `hs-src/Language/Egison/AST.hs` - PDFunctionPat 2引数化
- `hs-src/Language/Egison/Parser/NonS.hs` - Function パターン引数数
- `hs-src/Language/Egison/Pretty.hs` - PDFunctionPat pretty
- `hs-src/Language/Egison/Type/IInfer.hs` - PDFunctionPat 型推論・MathExpr適用
- `hs-src/Language/Egison/Math/Rewrite.hs` - func pattern 2引数化
- `lib/math/expression.egi` - inductive pattern func 2引数化・mathExpr matcher
- `lib/math/analysis/derivative.egi` - func pattern 2引数化
