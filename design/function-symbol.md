# Function Symbol 仕様

> **2026-07-07 仕様確定**: 導関数索引は**位置正準形**に統一した。
> (1) `userRefs` はシンボル/値の索引を**構築時に位置へ解決**する
> (引数は名前でなく**値**なので、位置索引だけが常に well-defined —
> 合成引数 `function (rr)` (rr = r²) では名前索引は定義しようがない)。
> 非引数値・複数回現れる値 (`function (x, x)` への `[x]`)・範囲外整数は
> **エラー**。(2) 多重索引は**構築時に昇順ソート** — この機構が表す滑らかな
> 未知関数では混合偏導関数が可換 (Schwarz) なので、`f|2|1` は `f|1|2` と
> 同一原子として構築される (`d/d (d/d g x) y = d/d (d/d g y) x` が True)。
> (3) 適用形が冪の下に来るときは括弧付き表示 (`(g x y)^2`)。
> (4) 型統合: `function (…)` と導関数の型は `MathValue`。**値レベルは完全
> 統合済み** (GB エンジン・declare rule・トリガー収集が FunctionData を
> 扱うことを実測確認)。型レベルの原子集合 (`Poly … [f x]`) への出現禁止は
> 維持 — 必要になれば compound atom と同じ extendAtoms 経路で開放できる。
> 検収: mini-test/146・test/lib/math/analysis.egi (Taylor の混合項が
> Schwarz 正準形で `f|1|2` に併合される形)。実装: Core.hs IUserrefsExpr
> (FunctionData 分岐のみ; 裸シンボルへの user 索引は従来どおり)・
> Math/CAS.hs prettyPow。

> **2026-07-08 追補**: 第一級構築子 **`functionSymbol : String ->
> [MathValue] -> MathValue`** を追加(定義文脈不要; 名前を計算で作れる
> ので族を map で構築できる)。定義文脈由来のシンボルと名前・引数が
> 一致すれば等値。付随して `quoteScalar`(quote 原子の再構築; mapSymbols
> の quote 透過に使用)と `mathFunctionName`(apply1..4 頭の名前)も追加。
> 経緯と設計判断は [function-symbol-formurae.md](function-symbol-formurae.md)。

## 概要

`function` キーワードで定義されるシンボリック関数。
具体的な実装を持たないが、微分・代入などのシンボリック操作が可能。

```egison
declare symbol x, y
def f := function (x)        -- 1変数関数 f(x)
def g := function (x, y)     -- 2変数関数 g(x, y)
```

### テンソル成分の名前 (2026-07-11 仕様確定)

`generateTensor` の中で `function (...)` を評価すると、テンソルを束縛する
変数の基底名と、生成中の全成分位置から関数シンボル名を構成する。

```egison
def E := generateTensor (\[i] -> function (x, y, z)) [3]
-- [| E_1 x y z, E_2 x y z, E_3 x y z |]

def T := generateTensor (\[i, j] -> function (x, y, z)) [2, 3]
-- T_1_1, T_1_2, ..., T_2_3
```

定義左辺に明示された添字の上下は維持し、`generateTensor` のrankに対して
不足する位置は下添字で補完する。

```egison
def H~i := generateTensor (\[i, j] -> function (x, y, z)) [2, 2]
-- H~1_1, H~1_2, H~2_1, H~2_2
```

`generateTensor` がネストする場合、外側ですでに確定した位置を保持し、内側の
位置を後ろへ追加する。したがってrank-1生成器を2段ネストしたbare定義では、
成分名は `A_1_1`, `A_1_2`, ... のようになる。

この規則は、生成器の中で新しく評価される `function (...)` だけに適用する。
既存の関数シンボルを成分として参照した場合、その名前は変更しない。

```egison
def f := function (x, y, z)
def E := generateTensor (\_ -> f) [3]
-- [| f x y z, f x y z, f x y z |]
```

したがって、添字を省略したテンソル定義では `def E := E_#` のような別名は
不要である。テンソル値の省略軸と関数シンボル名の不足添字は、どちらも
共変を既定値として扱う。

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

`name`(索引込み)と `args` の両方が等しい場合のみ等しい:
```
f(x) = f(x)   -- True
f(x) = f(0)   -- False
f(0) = f(a)   -- False
```

導関数索引は構築時正規化により、次が構造的等価で成立する:
```
userRefs g [y]        = userRefs g [2]    -- True(位置へ解決)
d/d (d/d g x) y       = d/d (d/d g y) x   -- True(索引ソート = Schwarz)
userRefs g [2, 1]     = userRefs g [1, 2] -- True
userRefs h [r^2]      = userRefs h [1]    -- True(合成引数も値で解決)
```

エラー(構築時):
```
userRefs g [3]   -- index 3 is out of range for the 2-argument function symbol g
userRefs g [a]   -- a is not an argument of the function symbol g
userRefs k [x]   -- (k = function (x, x)) appears more than once; use a positional index
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
- `hs-src/Language/Egison/Type/Infer.hs` - PDFunctionPat 型推論・MathExpr適用
- `hs-src/Language/Egison/Math/Rewrite.hs` - func pattern 2引数化
- `lib/math/expression.egi` - inductive pattern func 2引数化・mathExpr matcher
- `lib/math/analysis/derivative.egi` - func pattern 2引数化
