# Egison CAS 型システム

この文書は、Egison の数式処理システム (computer algebra system; CAS) で現在使われる
型、内部表現、型の包含関係、型に基づく変換、宣言機構をまとめる総合仕様である。

関連文書:

- [type-cas-tower-implementation.md](./type-cas-tower-implementation.md): 型タワーと実装箇所の対応。
- [type-cas-quotient.md](./type-cas-quotient.md): 型タワーと独立した商型。
- [cas-simplification.md](./cas-simplification.md): 多項式 GCD、グレブナー基底、根号などの簡約。
- [function-symbol.md](./function-symbol.md): 関数シンボルと偏微分索引。
- [runtime-type-dispatch.md](./runtime-type-dispatch.md): CAS 値の浅い実行時型による辞書選択。

Matcher の形式仕様は `type-pm-mech3` を正本とする。Egison CAS の pattern view には
形式 core 外の互換境界が残るため、[matcher-capability.md](./matcher-capability.md) と
[type-pm-compatibility.md](./type-pm-compatibility.md) も参照する。

## 1. 基本原則

`MathValue` は CAS 計算の共通型である。整数、記号、関数適用、多項式、分数は同じ
`CASValue` 表現を共有し、算術と大域的な簡約規則はこの値の上で動く。

`Factor`, `Term`, `Poly`, `Frac` は、別の値表現を導入する型ではない。同じ CAS 値について、
利用できる代数的操作と、値をどの正規形で保持するかをより詳しく表す型である。正規形とは、
同じ数式を比較・表示しやすい一つの代表的な構造へそろえた形をいう。

型注釈は値の所属を動的に証明する cast ではなく、`reshape` による表現選択として扱う。

```egison
def p : Poly Integer [x] := x^2 + 2*x + 1
def q := (expression : Frac (Poly Integer [x]))
```

型に基づく変換は `IReshape target expression` を挿入し、評価器の `casReshapeAs` が
値を target の構造へ組み替える。注釈ごとに別の `coerce` 関数や埋め込み辞書は使わない。

## 2. CAS 型

| 型 | 意味 |
|---|---|
| `Integer` | CAS の整数。内部型は `TInt`、値は `CASInteger` |
| `Factor` | 一つの原子的な記号式。通常シンボル、数学関数適用、関数シンボル、クオートを含む |
| `Term a atoms` | 係数型 `a` と原子集合 `atoms` を持つ一つの単項式 |
| `Poly a atoms` | 係数型 `a`、原子集合 `atoms` のローラン多項式 |
| `Frac a` | `a` の値を分子・分母に持つ分数 |
| `MathValue` | 全ての CAS 型を含む共通型 |

ローラン多項式とは、通常の非負指数だけでなく `x^-1` のような負の整数指数も許す多項式である。
したがって `Poly Integer [x]` は `x`, `x^-1`, `x^2` を同じ単項式表現で扱う。

`Poly (Poly Integer [i]) [x]` と `Poly Integer [i, x]` は同じ値集合を表せるが、
前者は `x` について整理し係数を `i` の多項式として保持し、後者は `i` と `x` を平らに扱う。
型注釈がどちらの正規形を使うかを選ぶ。

### 原子集合

`Poly a [x, sqrt 2]` の角括弧部分は、型レベルの原子集合である。

```haskell
data TypeAtom
  = TANameAtom String
  | TAApplyAtom String [TypeAtom]
  | TAIntAtom Integer

data SymbolSet
  = SymbolSetClosed [TypeAtom]
  | SymbolSetOpen
  | SymbolSetVar TyVar
```

- `[x, y]` は列挙した原子だけを持つ閉じた集合である。
- `[..]` は具体的な原子を追跡しない開いた集合である。
- 閉じた集合どうしの包含は通常の集合包含として比較する。
- join では閉じた集合の和集合を取り、どちらかが開いていれば結果も開く。

入れ子の `Poly` の係数タワーでは、開いた集合 `[..]` は高々一箇所に限る。二箇所以上あると、
列挙されていない原子をどの係数段へ置くか一意に決まらない。

## 3. 内部表現

```haskell
data CASValue
  = CASInteger Integer
  | CASFactor SymbolExpr
  | CASPoly [CASTerm]
  | CASFrac CASValue CASValue

data CASTerm = CASTerm CASValue Monomial
type Monomial = [(SymbolExpr, Integer)]
```

`CASTerm` の係数も `CASValue` なので、分数係数や多項式係数を再帰的に表せる。
`Monomial` は原子と整数指数の平らなリストであり、型レベルの係数タワー分類は値へタグとして
重複保存しない。

```text
3 : Integer
  -> CASInteger 3

x + 1 : Poly Integer [x]
  -> CASPoly [CASTerm 1 [(x, 1)], CASTerm 1 []]

(1/2) * x : Poly (Frac Integer) [x]
  -> CASPoly [CASTerm (CASFrac 1 2) [(x, 1)]]
```

評価値は `EgisonValue` の `CASData CASValue` として保持する。整数を含めて別の
`ScalarData` 経路はない。

### `SymbolExpr`

```haskell
data SymbolExpr
  = Symbol Id String [Index CASValue]
  | Apply1 CASValue CASValue
  | Apply2 CASValue CASValue CASValue
  | Apply3 CASValue CASValue CASValue CASValue
  | Apply4 CASValue CASValue CASValue CASValue CASValue
  | Quote CASValue
  | QuoteFunction WHNFData
  | FunctionData CASValue [CASValue]
```

`Apply1`–`Apply4` は `declare mathfunc` 由来の数学関数適用、`FunctionData` は
`function (...)` 由来の未知関数を表す。5 引数以上を一般化する `ApplyN` は持たない。

## 4. 型の包含関係

`Type/Subtype.hs` は CAS 型の構造的な包含関係を定義する。主な規則は次のとおりである。

- 全ての CAS 型は `MathValue` に含まれる。
- `Integer` は `Factor` と、整数を受け入れる `Term`, `Poly`, `Frac` に含まれる。
- `Factor` は、整数係数の原子を受け入れる `Term` と `Poly` に含まれる。
- `Term a s` は、係数型と原子集合がともに包含される `Term b t` と `Poly b t` に含まれる。
- `Poly a s` は係数型と原子集合について共変である。
- `Frac a` は内側の型について共変である。
- 定数または分子として値を入れられる場合、`Poly` と `Frac` の間にも構造的な包含を導く。

ここで共変とは、内側の型をより広い型へ変えたとき外側の型も同じ向きに広がることをいう。

入れ子と平らな正規形のように、同じ値集合を別の構造で表す型は、自動的には順序づけない。
必要な関係は `declare cas-subtype` で明示する。

## 5. `reshape` と join

### `reshape`

`casReshapeAs` は、平らな原子を型注釈の係数タワーへ振り分けたり、入れ子の係数を平らに戻したりする。
値の意味は変えず、正規形だけを選ぶ。

通常の算術演算は入れ子の多項式係数を平らな既定形へ戻す。`reshape` の最終段階だけが、
注釈で指定した入れ子を保持する。したがって異なる表現から来た同類項も、算術の出口では
同じ平らな構造へ集められる。

### join

join は、二つの CAS 型を両方含む一意な最小の型である。関数適用で通常の単一化が失敗し、
二つの引数が CAS 型である場合だけ、推論器が `joinTypesWith` を使って共通型を求める。

```text
join(Poly a s, Poly b t) = Poly (join a b) (s union t)
join(Frac a, Frac b)     = Frac (join a b)
```

join が決まった場合、必要な実引数へ `reshape` を挿入して単一化を一度だけ再試行する。
join を一般の単一化そのものへ混ぜず、非 CAS 型の不一致や商型との混在を救済しない。

## 6. ユーザによる型タワーの拡張

```egison
declare cas-type GaussianInt := Poly Integer [i]
declare cas-type GaussianPoly := Poly GaussianInt [x]
declare cas-subtype Integer <: GaussianInt
```

`declare cas-type` は透明な別名であり、登録時に完全展開する。`declare cas-subtype` は
型レベルの順序だけを追加し、実行時の埋め込み関数を宣言しない。表現の変換は常に
`casReshapeAs` が担う。

新しい辺は、宣言に現れた有限な節点集合上で一意な最小上界を保つか検査する。循環と
複数の最小上界を生む辺は拒否する。検査の正確な範囲は
[type-cas-tower-implementation.md](./type-cas-tower-implementation.md) を参照する。

## 7. 商型

商型は、整数の剰余類や有限体のように、異なる基底値を同じ値とみなす型である。これは
値を保ったまま表現を変える型タワーと異なるため、独立した機構にする。

```egison
declare cas-quotient Mod7 := Integer by (\n -> modulo n 7)
```

宣言は不透明な型、`reduce`, `proj`, `repr`、加法・乗法・等値性のインスタンスへ展開される。
商型は CAS の部分型順序と join に参加しない。基底型との横断は `proj` と `repr` を明示する。

```egison
declare cas-quotient GF4 := MathValue
  by finiteFieldReduce 2 [alpha^2 + alpha + 1]
```

有限拡大体も、タワーの係数スロットを拡張せず、一つの商型として構成する。詳細は
[type-cas-quotient.md](./type-cas-quotient.md) を参照する。

## 8. 代数的型クラス

算術は一つの `Num` ではなく、`AddSemigroup`, `AddMonoid`, `AddGroup`,
`MulSemigroup`, `MulMonoid`, `MulGroup`, `Ring`, `Field` などに分ける。
関数は必要な演算だけを制約として要求する。

CAS のインスタンス選択は、通常の型一致に加えて CAS の部分型順序を使い、最も具体的な
候補を選ぶ。同じ値集合を別の正規形で表す順序同値の候補が残る場合は、対象型と外側の
型構成子が一致する辞書を優先する。

静的型だけで辞書を選べない `MathValue` 境界では、単一パラメータクラスの第1引数に限り、
値の浅い実行時型を調べる。詳細は [type-class.md](./type-class.md) と
[runtime-type-dispatch.md](./runtime-type-dispatch.md) を参照する。

## 9. 数学関数と微分

### `declare mathfunc` と `declare apply`

```egison
declare mathfunc sqrt
declare apply sqrt x :=
  if isRational x then ... else 'sqrt x
```

`declare mathfunc` は CAS の数学関数を登録する。`declare apply` があれば適用時にその本体で
簡約し、なければ `Apply1`–`Apply4` の原子的な関数適用を作る。

`declare apply` の本体で `'sqrt x` と書くと、同じ適用規則へ再帰せず、クオートした関数適用を
作る。クオートなしの `sqrt x` は適用規則を再び呼ぶ。

### 関数シンボル

```egison
declare symbol x, y
def f := function (x, y)
def fs := functionSymbol "f" [x, y]
```

関数シンボルは具体的な実装を持たない未知関数である。引数の置換、連鎖律による微分、
位置で正準化した偏微分索引を持つ。詳細は [function-symbol.md](./function-symbol.md) を参照する。

### 微分

`Differentiable` は `Factor`, `Term MathValue [..]`, `Poly MathValue [..]`,
`Frac MathValue` に構造ごとのインスタンスを持つ。明示的な `MathValue` インスタンスは置かず、
`partialDiffMV` の `MathValue` 呼び出しを実行時ディスパッチで適切な構造の辞書へ送る。

```egison
declare derivative sin = cos
declare derivative log = \z -> 1 / z
```

微分の前に `requireAnalyticDerivative` が値全体を確認し、未登録の解析関数適用を定数として
黙って 0 にしない。`Apply1` の登録済み関数、一般冪、関数シンボル、クオートには連鎖律を使う。
`∂/∂` は `Tensor MathValue` を受け、`tensorMap2` によりスカラーとテンソルの両方へ適用できる。

## 10. 簡約規則

### `declare rule`

```egison
declare rule auto term i^2 = -1
declare rule trig term ...
```

- `auto` 規則は算術の正規化時に不動点まで適用する。
- 名前つき規則は `simplify expression using trig` のように明示適用する。
- 規則は対象シンボルのトリガーを持ち、無関係な値では matcher を起動しない。

全ての自動規則は `MathValue` の大域理論に属する。型ごとの `in T` 規則は持たない。
係数領域ごとに等価性が変わる場合は商型の `reduce` を使う。

### 三種類のクオート

- バッククオートは式全体を一つの構造的な原子にする。
- `'f` は関数をクオートし、`declare apply` を通らない適用を作れる。
- `'(expression)` は `declare rule` の理論上の書き換えを抑えつつ、構造正規化を行う。

### `declare ideal`

```egison
declare ideal [w^2 + w + 1]
```

生成元をグレブナー基底へ完備化し、先頭単項式から軽い term 級規則を生成する。重い
グレブナー基底計算は最初に必要になったとき一度だけ行い、通常の算術経路には入れない。
明示的な正規形には `polyNF`, `idealNF`, `idealEquals` を使う。

高度な簡約の仕様は [cas-simplification.md](./cas-simplification.md) に集約する。

## 11. CAS のパターンマッチ

`lib/math/expression.egi` は `MathValue` の内部構造に対する pattern view を提供する。

- `plus` / `poly`: 多項式と項列。
- `term`: 係数と平らな単項式。
- `symbol`: 通常シンボル。
- `apply1`–`apply4`: 数学関数の適用。
- `quote`: クオートされた式。
- `func`: 関数シンボルの名前と引数。
- 分数 view: 分子と分母。

これらの view は CAS アルゴリズムを Egison 自身で書くために必要である。一方、
`Factor` や `Term` の view を異なる target 型の matcher として使う経路には、
`type-pm-mech3` の D5-CAS に相当する形式的な証拠がまだない。現行実装はこの箇所だけを
明示的な Egison 固有境界として扱い、一般の capability 等式へは広げない。

## 12. 観察型

観察型は、評価済みの `CASValue` の外形から計算する診断用の型である。静的型とは別に、
実際の値が整数、因子、多項式、分数のどの形になったかを表示する。

```text
CASInteger                -> Integer
通常の Symbol            -> Symbol
その他の CASFactor       -> Factor
CASPoly terms             -> Poly <係数の観察型> [実際の原子]
CASFrac numerator denom   -> Frac <内側の観察型>
```

`inspect` は値と観察型、`observedType` は観察型文字列を返す。観察型は値を実行した後の情報なので、
静的型推論、matcher capability、`reshape` の成功証拠には使わない。

係数の観察型 join は、`Integer` を下位として同じ非整数型をまとめる簡易な方式である。
宣言された CAS 部分型順序を完全には参照しないため、異なる非整数型が混ざると `MathValue` へ広げる。

## 13. 宣言の収集と有効範囲

`declare symbol`, `declare mathfunc`, `declare apply`, `declare rule`, `declare ideal`,
`declare derivative`, `declare cas-type`, `declare cas-subtype`, `declare cas-quotient` は、
評価前に収集・展開する。

- 型別名と部分型辺は `EvalState` に保存し、後から読み込む単位でも使う。
- `declare symbol` は型環境への登録に加え、評価環境へ `CASData (Symbol ...)` を事前束縛する。
- 同じ読み込み単位の自動規則とイデアル宣言は、再帰束縛された `mathNormalize` を共有する。
- `declare cas-quotient` は、通常の定義と型クラスインスタンスへ先に展開する。

自動規則を別の読み込み単位で追加しても、既に作られた演算子クロージャが古い
`mathNormalize` を捕捉している場合がある。規則を必要とするテストとプログラムは、規則宣言を
同じ初期読み込み単位へ含める。

## 14. 現在の制限

- 数学関数適用は `Apply1`–`Apply4` までで、5 引数以上の `ApplyN` はない。
- 関数シンボルを型レベルの閉じた原子集合へ入れる経路は開放していない。
- CAS の実行時型ディスパッチは、単一パラメータクラスの第1引数を浅く調べるだけである。
- 観察型の join は宣言された CAS 部分型順序と完全には同期していない。
- CAS 表現を選ぶ `reshape` の主な入口はトップレベル定義と式注釈であり、関数引数、match 節、
  typed `let` の全注釈へは一般化していない。
- 商型とタワー型の暗黙混在は許さない。
- CAS pattern view の D5-CAS は Egison 固有の互換境界である。
- 記号行列の逆行列は大きな行列向けに最適化されていない。

## 15. 実装と検証

主要な実装箇所:

- `Math/CAS.hs`: `CASValue`, 正規化、reshape、観察型、多項式 GCD。
- `Math/Rewrite.hs`: `sqrt` と `exp` の構造規則。
- `Type/Types.hs`: CAS 型、`TypeAtom`, `SymbolSet`。
- `Type/Subtype.hs`: 構造的部分型、宣言辺、join。
- `Type/Infer.hs`: CAS join と `IReshape` の挿入。
- `EnvBuilder.hs`, `Eval.hs`, `Desugar.hs`: `declare` 系の収集と展開。
- `lib/math/`: 型クラスインスタンス、数学関数、微分、簡約ライブラリ。

検証:

- `cabal test test`: 一般の型・評価回帰。
- `test/lib/math/`: 算術、微分、テンソル、GCD、グレブナー基底、商型。
- `design/cas-tower-usecases/`: 型別名、部分型、join、商型の実行可能な検収例。
- `sample/math/`: 代数・解析・幾何の公開例。
