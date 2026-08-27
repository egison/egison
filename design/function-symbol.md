# 関数シンボル

関数シンボルは、具体的な実装を持たない未知関数を CAS 値として表す。引数の置換、表示、
連鎖律による微分、テンソル成分名の生成を行える。

```egison
declare symbol x, y
def f := function (x)
def g := function (x, y)
```

`function (...)` の型と、その偏微分から作る値の型は `MathValue` である。

## 1. 構築

### 定義文脈からの構築

`function (...)` は、定義左辺の名前を関数シンボル名として使う。

```egison
def f := function (x, y)  -- f x y
```

### 名前を値として渡す構築

`functionSymbol` は定義文脈を使わず、名前と引数列から同じ値を作る。

```egison
functionSymbol : String -> [MathValue] -> MathValue

def fs := map (\n -> functionSymbol (S.append "f" (show n)) [x, y])
              (between 0 18)
```

名前と引数が同じなら、`function (...)` から作った値と `functionSymbol` から作った値は等しい。

## 2. 内部表現

Haskell の `SymbolExpr` に次の構成子を持つ。

```haskell
FunctionData CASValue [CASValue]
```

第1フィールドは索引を含む名前、第2フィールドは現在の引数列である。

```text
def f := function (x, y)

FunctionData
  (CASFactor (Symbol "" "f" []))
  [x, y]
```

Egison からは `mathValue` matcher の `func` view で分解する。

```egison
match expression as mathValue with
  | func $name $args -> ...
```

## 3. 表示と適用

関数シンボルは引数を常に表示する。

```text
show f       -> "f x"
show g       -> "g x y"
show (f 0)   -> "f 0"
```

`f 0` のような適用は、通常の関数クロージャを呼ぶのではなく、`FunctionData` の引数列を
置き換えた新しい関数シンボルを返す。引数の個数が違えばエラーになる。

```egison
f 0       -- f の引数列 [x] を [0] へ置換
g 0 1     -- [x, y] を [0, 1] へ置換
g 0       -- arity error
```

関数適用形が冪の底になる場合は、`(g x y)^2` のように括弧を付けて表示する。

## 4. テンソル成分の名前

`generateTensor` の中で新しく `function (...)` を評価すると、テンソルを束縛する変数の
基底名と、生成中の全成分位置から関数シンボル名を作る。

```egison
def E := generateTensor (\[i] -> function (x, y, z)) [3]
-- [| E_1 x y z, E_2 x y z, E_3 x y z |]

def T := generateTensor (\[i, j] -> function (x, y, z)) [2, 3]
-- T_1_1, T_1_2, ..., T_2_3
```

定義左辺に明示した添字の上下は維持する。`generateTensor` の階数に対して不足する位置は
下添字で補う。

```egison
def H~i := generateTensor (\[i, j] -> function (x, y, z)) [2, 2]
-- H~1_1, H~1_2, H~2_1, H~2_2
```

生成器を入れ子にした場合は、外側で決まった位置を保ち、内側の位置を後ろへ追加する。
生成器の中で既存の関数シンボルを参照しただけなら、その名前は変更しない。

```egison
def f := function (x, y, z)
def E := generateTensor (\_ -> f) [3]
-- [| f x y z, f x y z, f x y z |]
```

## 5. 偏微分索引

偏微分は位置索引 `|1`, `|2`, ... を使う。`userRefs` は、値で指定された引数を
関数シンボルの引数列における位置へ構築時に解決する。

```text
f = function (x, y)

partialDiff f x  = f|1 x y
partialDiff f y  = f|2 x y
```

引数は名前ではなく任意の CAS 値なので、位置索引だけが常に定義できる。例えば
`function (r^2)` の引数に名前はないが、位置 1 は一意である。

次の場合は構築時エラーになる。

- 指定した整数位置が引数範囲外である。
- 指定した値が引数列にない。
- 同じ値が引数列に複数回現れ、値から位置を一意に決められない。この場合は整数位置を使う。

この機構が表す未知関数は滑らかで混合偏微分が可換であると仮定し、多重索引を構築時に
昇順へ並べる。

```text
f|2|1  = f|1|2
partialDiff (partialDiff f x) y
  = partialDiff (partialDiff f y) x
```

## 6. 連鎖律

微分器は引数ごとの偏微分を内側の微分と掛けて足し合わせる。

```egison
declare symbol r
def rr := r^2
def f := function (rr)

show (∂/∂ f r)
-- "2 * f|1 r^2 * r"
```

`lib/math/analysis/derivative.egi` の `func` 分岐が、位置列と引数列を対応させてこの連鎖律を実装する。

## 7. 代入と等値性

`V.substitute` は `FunctionData` の引数列を再帰的に置換する。

```egison
V.substitute [|x|] [|0|] f
-- f x -> f 0

V.substitute [|x, y|] [|0, 0|] g
-- g x y -> g 0 0
```

名前の索引を含む構造と引数列が両方等しい場合だけ、二つの関数シンボルは等しい。
偏微分索引は構築時に整列済みなので、混合偏微分の等値性も通常の構造比較で決まる。

`mapSymbols` は `Quote` の内部も再帰し、必要なら `quoteScalar` でクオート原子を再構築する。
`mathFunctionName` は `Apply1`–`Apply4` の関数頭の名前を返す。これらは文字列表現を解析せず、
CAS の構造を使って代入や名前取得を行うための補助関数である。

## 8. 型レベル原子集合との境界

`FunctionData` は値レベルの CAS 原子として、簡約規則、グレブナー基底、トリガー収集、代入で扱う。
一方、`Poly Integer [f x]` のように関数シンボルを型レベルの閉じた原子集合へ入れる経路は
開放していない。関数シンボルを含む式は `MathValue` または開いた原子集合で扱う。

## 9. 実装と検証

- `Math/CAS.hs`: `FunctionData`, 等値性, 表示。
- `Core.hs`: `function`, `functionSymbol`, 引数置換, `userRefs`。
- `Primitives.hs`: `functionSymbol`, `updateFunctionArgs`, `quoteScalar`, `mathFunctionName`。
- `lib/math/expression.egi`: `func` view と `mapSymbols`。
- `lib/math/analysis/derivative.egi`: 連鎖律。

主な回帰は `test/lib/math/analysis.egi` で検証する。値レベルでは Taylor 展開の混合項が
`f|1|2` に正準化されることも確認する。
