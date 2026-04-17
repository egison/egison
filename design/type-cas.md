# Egison CAS 型システム設計

## 概要

Egisonの数式処理システム(CAS)のための型システムの設計方針をまとめる。

### 基本原則

**`MathValue` が第一級の計算型**である。`1`, `1 + i`, `x + x^2`, `x + sin x` はすべて `MathValue` として自由に計算できる。演算（`+`, `*`, `∂/∂` 等）も簡約規則（`i^2 = -1` 等）もすべて `MathValue` 上で動作する。

**`Poly a [cs] [ss] [fs]` 等は `MathValue` の subtype** であり、「この式はこの代数構造に属する」という型注釈 + 「その構造の正規形で表示・保持せよ」という指示として機能する。内部表現は `MathValue` と同じ `CASValue` で、subtype への `coerce` 時に所属検証と正規化を行う。

```egison
-- すべて MathValue として自由に計算
1 + i                    -- : MathValue
x + x^2                  -- : MathValue
x + sin x                -- : MathValue
∂/∂ (sin x) x           -- : MathValue (= cos x)

-- 型注釈で subtype に絞り込む → 正規化される
type GaussianInt := Poly Integer [] [i] []
def a : GaussianInt := 1 + i                           -- coerce + 正規化
def p : Poly (Frac Integer) [] [x] [] := (x + x^2) / 2 -- (1/2)x + (1/2)x^2 に正規化
```

### 型システムの基盤

HM型推論とtype classを基盤とする。基本的に依存型は採用しないが、`Poly` の3つの原子集合スロット（定数 Factor / 平のシンボル / 合成 Factor）のみ依存型的に扱う。

`Poly` の原子集合を3スロットに分けるのは、**微分演算子 `∂/∂` が型保存になる十分条件を型レベルで表現する** ためである。合成 Factor のスロット `fs` が空のとき、`Poly` は微分で閉じている。詳細は「微分演算子 `∂/∂`」のセクションを参照。

### 設計のレイヤー

| レイヤー | 責務 |
|---|---|
| **`MathValue` + `declare rule auto`** | 演算 + 簡約規則の適用（`i^2 = -1` 等すべて） |
| **subtype (`Poly a [cs] [ss] [fs]`, `Frac a` 等)** | 所属検証 + 正規形の保証。subtype 上の `Ring` は `MathValue` の演算をラップ + coerce |
| **`Differentiable` 型クラス** | `fs = []` の `Poly` で微分の型保存を静的に保証 |

---

## 型の構成要素

### 組み込み型


| 型                      | 意味                                      |
| ---------------------- | --------------------------------------- |
| `MathValue`            | **第一級の計算型**。全ての CAS 計算はこの型上で行う。ランタイム表現は `CASValue`。簡約規則もすべてこの型に紐づく |
| `Integer`              | 基本型。整数。`MathValue` の subtype                                  |
| `Frac a`                | `a` の分数体/分数環。`MathValue` の subtype          |
| `ConstantFactor`          | 内部に平のシンボルを含まない原子的要素（例: `sqrt 2`, `log 3`）。`Factor` の真の部分型 |
| `Symbol`               | `declare symbol` で宣言された平の不定元（例: `x`, `y`, `r`, `θ`）。`Factor` の真の部分型 |
| `AppliedFactor`           | 内部に平のシンボルを含む原子的要素（例: `sin x`, `sqrt x`, `log (x+1)`）。`Factor` の真の部分型 |
| `Factor`               | 原子的な数式要素全体（`ConstantFactor` ∪ `Symbol` ∪ `AppliedFactor` の和）。`MathValue` の subtype |
| `Term a [cs] [ss] [fs]` | `Poly a [cs] [ss] [fs]` の項。係数（型 `a`）とモノミアル（3スロットの組）を持つ補助型 |
| `Poly a [cs] [ss] [fs]` | `a` を係数とするローラン多項式環。`MathValue` の subtype。内部表現は `MathValue` と同じ `CASValue` で、型注釈による coerce 時に所属検証 + 正規化を行う。3スロットにより「微分で閉じている」性質を型レベルで表現できる |
| `Poly a [..] [..] [..]` | 3スロットそれぞれをフレッシュ型変数で開いたローラン多項式型（開いた多項式型）。`MathValue` の subtype |
| `Tensor a`             | `a` を成分とするテンソル                          |

### 型エイリアス

ユーザは `Poly` の特殊化に型エイリアスを定義できる:

```egison
type GaussianInt := Poly Integer [] [i] []              -- Z[i]
type Zsqrt2      := Poly Integer [sqrt 2] [] []        -- Z[√2]
type RatFunc     := Frac (Poly Integer [] [x] [])        -- Z[x] の分数体
```

### 糖衣構文

3スロットのうち1つだけ非空のケース（最も頻出）に対して糖衣構文を提供する:

| 糖衣構文 | 脱糖 | 例 |
|---|---|---|
| `ConstantPoly a [cs]` | `Poly a [cs] [] []` | `ConstantPoly Integer [sqrt 2]` = `Z[√2]` |
| `SymbolPoly a [ss]` | `Poly a [] [ss] []` | `SymbolPoly Integer [x, y]` = `Z[x,y]` |
| `AppliedPoly a [fs]` | `Poly a [] [] [fs]` | `AppliedPoly Integer [(sin x)]` |
| `ConstantTerm a [cs]` | `Term a [cs] [] []` | |
| `SymbolTerm a [ss]` | `Term a [] [ss] []` | |
| `AppliedTerm a [fs]` | `Term a [] [] [fs]` | |

2スロット以上が非空の場合は生の `Poly a [cs] [ss] [fs]` を使う:

```egison
-- 糖衣構文で書けるケース
def a : ConstantPoly Integer [sqrt 2] := 1 + sqrt 2
def p : SymbolPoly Integer [x, y] := x^2 + y

-- 混在ケースは生の Poly
def r : Poly Integer [sqrt 2] [x] [] := (sqrt 2) * x + 1
```

型エイリアスも糖衣構文を使って定義できる:

```egison
type GaussianInt := SymbolPoly Integer [i]     -- = Poly Integer [] [i] []
type Zsqrt2      := ConstantPoly Integer [sqrt 2]  -- = Poly Integer [sqrt 2] [] []
```

### Poly はローラン多項式環

`Poly a [cs] [ss] [fs]` は標準的な多項式環ではなく、ローラン多項式環を表す。
単項式（Monomial）の冪指数に負の整数を許可することで、`1/r` や `1/r^2` を `Frac` なしで直接表現できる。

```egison
declare symbol r, θ

-- 1/r, 1/r^2 は Poly Integer [] [r] [] でそのまま表現可能
r + 1/r + 1/r^2 : Poly Integer [] [r] []

-- 物理の式も Frac なしで書ける
∂/∂ (∂/∂ f r) r + ∂/∂ f r / r + ∂/∂ (∂/∂ f θ) θ / r^2 : Poly Integer [..] [..] [..]
```

`Frac` が必要になるのは、分母が非モノミアルの多項式の場合のみ：

```egison
1 / (r + 1)     : Frac (Poly Integer [] [r] [])    -- 分母 r+1 はモノミアルでない
r / (r^2 - 1)   : Frac (Poly Integer [] [r] [])    -- 分母 r^2-1 はモノミアルでない
```

### グレブナー基底との互換性

ローラン多項式に対してグレブナー基底を計算する場合、アルゴリズムの内部で標準多項式環に自動変換する。

```
-- 入力: ローラン多項式環 Z[r, r⁻¹]
r + 1/r + 1/r^2

-- 内部変換: r⁻¹ を新しい不定元 s に置換し、関係式 rs = 1 を追加
-- Z[r, s] 上で計算、イデアル (rs - 1) を含める
r + s + s^2   （ただし rs = 1）

-- Buchberger のアルゴリズムを標準多項式環 Z[r, s] 上で実行

-- 結果を逆変換: s → r⁻¹
```

この変換は数学的に `R[r, r⁻¹] ≅ R[r, s]/(rs - 1)` という同型に基づく。
ユーザーはこの変換を意識する必要はなく、アルゴリズムが内部で自動的に処理する。

### 閉じた Poly と開いた Poly

`Poly` の3つの原子集合スロットには、それぞれ2つの形式がある。

**閉じたスロット** `[s1, s2, ...]`: シンボル集合が具体的に固定されている。特定の多項式環で作業するときに使う。演算結果の型が厳密に制御される。

**開いたスロット** `[..]`: シンボル集合を固定しない。`sqrt 2 + sqrt 3` のように動的に原子が現れる探索的な計算に使う。

3スロットそれぞれを独立に閉じる/開くことができる。最も一般的な「全スロット閉じた」「全スロット開いた」型を以下で例示する。

```egison
declare symbol x, y

-- 全スロット閉じた Poly: Z[x, x⁻¹] で厳密に作業
def p : Poly Integer [] [x] [] := 1 + x
def q : Poly Integer [] [x] [] := 2 + 3 * x
p + q    -- OK: Poly Integer [] [x] []

-- 定数 Factor を含む閉じた Poly
def r : Poly Integer [sqrt 2] [x] [] := (sqrt 2) * x + 1

-- 全スロット開いた Poly: 自由に原子を導入
def s : Poly Integer [..] [..] [..] := sqrt 2 + sqrt 3    -- OK
def t : Poly Integer [..] [..] [..] := s + x              -- OK
```

混在するケースも書ける:

```egison
-- ss だけ閉じて他は開く
def u : Poly Integer [..] [x, y] [..] := x + y + sqrt 2 + sin x
```

#### 開いた Poly のランタイム表現と型具体化

各 `[..]` はフレッシュな型変数の糖衣構文として扱う。各出現ごとに独立したフレッシュ型変数を生成する。

```egison
-- ユーザーが書く型
polyAdd :: Poly a [..] [..] [..] -> Poly a [..] [..] [..] -> Poly a [..] [..] [..]

-- 型チェッカーが脱糖した内部表現（cs_i, ss_i, fs_i はフレッシュ型変数）
polyAdd :: Poly a [cs1] [ss1] [fs1] -> Poly a [cs2] [ss2] [fs2] -> Poly a [cs3] [ss3] [fs3]
```

入出力で同じ原子集合を共有したい場合は、名前付き型変数を明示する:

```egison
polyAdd :: {cs, ss, fs} Poly a [cs] [ss] [fs] -> Poly a [cs] [ss] [fs] -> Poly a [cs] [ss] [fs]
```

具体化の流れ：

1. 適用時に引数の型からフレッシュ型変数がユニフィケーションで具体化される
2. 原子集合が異なる場合は、Embed（coercive subtyping）で各スロット独立に拡大して揃える
3. ランタイム表現は閉じた `Poly` と同一（`CASPoly [CASTerm]`）で、型レベルのみの区別

```egison
declare symbol x y

polyAdd (x^2 + 1) (y + 3)
-- 第1引数: Poly Integer [] [x] [],  第2引数: Poly Integer [] [y] []
-- ss スロットがユニファイ: [x] と [y]
-- Embed で [x] → [x, y], [y] → [x, y] に拡大（ss スロットのみ変化）
-- 結果: Poly Integer [] [x, y] []
```

#### 型構文の実装

Type ADT に `TPoly Type SymbolSet SymbolSet SymbolSet` を追加し、`SymbolSet` は `Closed [SymbolExpr]` / `Open` の2構成子。3つのスロットがそれぞれ独立に閉じる/開くことができる。パーサーでの `[a]`（コレクション型）との曖昧性解消が必要。

### 型の構成例

```
Integer                                      -- Z（整数）
Frac Integer                                  -- Q（有理数）
Poly Integer [] [x] []                       -- Z[x, x⁻¹]（整数係数ローラン多項式、閉じた型）
Poly Integer [] [x, y] []                    -- Z[x, x⁻¹, y, y⁻¹]（多変数、閉じた型）
Poly Integer [] [i] []                       -- Z[i]（ガウス整数、i^2 = -1）
Poly Integer [sqrt 2] [] []                 -- Z[√2]（定数 Factor のみ）
Poly Integer [sqrt 2] [x] []                -- Z[√2, x, x⁻¹]
Poly Integer [] [] [(sin x)]                -- Z[sin x]（合成 Factor を含む、微分で閉じない）
Poly (Frac Integer) [] [x] []                 -- Q[x, x⁻¹]
Poly (Frac Integer) [] [i] []                 -- Q(i)
Frac (Poly Integer [] [x] [])                 -- Z[x, x⁻¹] の分数体（非モノミアル除算）
Poly (Poly Integer [] [x] []) [] [y] []      -- Z[x, x⁻¹][y, y⁻¹]
Poly Integer [..] [..] [..]                  -- 整数係数、原子集合自由なローラン多項式
Poly (Frac Integer) [..] [..] [..]            -- 有理数係数、原子集合自由なローラン多項式
Tensor (Poly (Frac Integer) [] [x] [])        -- 有理数係数ローラン多項式を成分とするテンソル
```

### 入れ子の Poly と多変数の Poly

```egison
Poly (Poly Integer [] [x] []) [] [y] []   -- Z[x, x⁻¹][y, y⁻¹]: y について整理、係数が x のローラン多項式
Poly Integer [] [x, y] []                 -- Z[x, x⁻¹, y, y⁻¹]: x と y を対等に扱う
```

数学的に同型だが、正規形が異なる。型システムの設計上、両者は異なる型・異なる内部表現・異なるマッチャー（`poly (poly integer)` vs `poly integer`）として自然に区別される。ユーザーが型注釈で選択する。入れ子の場合の具体的な内部表現は「構成的な内部表現 > 表現の対応」の `Poly (Poly Integer [] [x] []) [] [y] []` の例を参照。

---

## 構成的な内部表現

型構造がそのままランタイムの内部表現を決定する。現在の一枚岩な `ScalarData = Div PolyExpr PolyExpr` とは異なり、型の組み合わせごとに異なる内部表現を持つ。

### 表現の対応

`Monomial` は 3 スロット（定数 Factor / 平のシンボル / 合成 Factor）に分かれ、それぞれが `(原子, 冪指数)` のリストを持つ。表記は `<cs | ss | fs>` を用いる。

```
Integer                      → CASInteger 3
Frac Integer                  → CASFrac (CASInteger 2) (CASInteger 3)        -- 2/3
Poly Integer [] [x] []       → CASPoly [CASTerm (CASInteger 1) <[]|[(x,1)]|[]>,
                                         CASTerm (CASInteger 1) <[]|[]|[]>]  -- x + 1
Poly Integer [] [r] []       → CASPoly [CASTerm (CASInteger 1) <[]|[(r,1)]|[]>,
                                         CASTerm (CASInteger 1) <[]|[(r,-1)]|[]>,
                                         CASTerm (CASInteger 1) <[]|[(r,-2)]|[]>] -- r + 1/r + 1/r^2
Poly Integer [sqrt 2] [x] []
                              → CASPoly [CASTerm (CASInteger 1) <[(sqrt 2,1)]|[(x,1)]|[]>,
                                         CASTerm (CASInteger 1) <[]|[]|[]>]  -- (sqrt 2)*x + 1
Poly Integer [] [] [(sin x)]
                              → CASPoly [CASTerm (CASInteger 1) <[]|[]|[(sin x,1)]>] -- sin x
Poly (Frac Integer) [] [x] [] → CASPoly [CASTerm (CASFrac 1 2) <[]|[(x,1)]|[]>]  -- (1/2)x
Poly (Poly Integer [] [x] []) [] [y] []
                              → CASPoly [CASTerm (CASPoly [CASTerm (CASInteger 1) <[]|[(x,1)]|[]>,
                                                            CASTerm (CASInteger 1) <[]|[]|[]>])
                                                           <[]|[(y,2)]|[]>,
                                         CASTerm (CASPoly [CASTerm (CASInteger 2) <[]|[(x,1)]|[]>])
                                                           <[]|[(y,1)]|[]>,
                                         CASTerm (CASPoly [CASTerm (CASInteger 3) <[]|[]|[]>])
                                                           <[]|[]|[]>]   -- (x+1)y^2 + 2x·y + 3
Frac (Poly Integer [] [x] []) → CASFrac (CASPoly ...) (CASPoly ...)        -- 非モノミアル分数
```

### Haskell での表現（概念）

```haskell
data CASValue
  = CASInteger Integer
  | CASFactor SymbolExpr          -- declare mathfunc で宣言された関数が生成する原子的要素
  | CASPoly [CASTerm]             -- 項のリスト
  | CASFrac CASValue CASValue     -- 分子 / 分母（非モノミアル除算のみ）

data CASTerm = CASTerm CASValue Monomial  -- 係数 × 単項式
-- 係数は CASValue（再帰的）: CASInteger, CASFactor, CASFrac, 別の CASPoly 等

-- Monomial は3スロットに分かれる
data Monomial = Monomial
  { monoConsts  :: [(SymbolExpr, Integer)]  -- ConstantFactor の冪の積（自由シンボルなしの Apply1/Quote）
  , monoSymbols :: [(SymbolExpr, Integer)]  -- Symbol の冪の積（平のシンボルのみ）
  , monoComps   :: [(SymbolExpr, Integer)]  -- AppliedFactor の冪の積（自由シンボルを含む Apply1/Quote）
  }
-- 冪指数は負も可（ローラン多項式のため）
```

型の入れ子構造が内部表現の入れ子構造に直接対応する。例えば `Poly (Frac Integer) [] [x] []` の各項の係数は `CASFrac (CASInteger _) (CASInteger _)` になる。

`Monomial` の冪指数が `Integer` であることにより、`r⁻¹` は `monoSymbols = [(r, -1)]`、`r⁻²` は `monoSymbols = [(r, -2)]` として自然に表現される。

`Monomial` を 3 スロットに分ける設計は、内部のソート・GCD 計算・連鎖律の実装をスロットごとに独立に行えるようにする。特に `monoComps` の各要素にだけ連鎖律を適用すればよいので、`∂/∂` の実装が直接的になる。

### EgisonValue との統合

現在の `ScalarData` を `CASValue` に一括置換する。

```haskell
-- 現在
data EgisonValue = ... | ScalarData ScalarData | ...
-- Integer は ScalarData (SingleTerm i []) として表現されている

-- 移行後: ScalarData を CASValue に一括置換
data EgisonValue = ... | CASData CASValue | ...
-- Integer は CASData (CASInteger i) に
```

段階的に並存させても最終的に全箇所を書き換える必要があり、並存期間中は `ScalarData` と `CASValue` の両方のコードパスと変換層を維持する負担が増えるだけで、総作業量は減らない。既存コード（`EgisonData Integer` インスタンス、プリミティブ関数、パターンマッチ）への影響は大きいが、一度に移行する。

### SymbolExpr の内部参照

現在の `SymbolExpr` は内部で `ScalarData` を参照しているが、新しい設計では `CASValue` を参照する。

```haskell
data SymbolExpr
  = Symbol Id String [Index CASValue]
  | Apply1 CASValue CASValue
  | Quote CASValue
  | QuoteFunction WHNFData
  | FunctionData CASValue [CASValue]
  ...
```

`CASValue` → `CASTerm` → `Monomial` → `SymbolExpr` → `CASValue` という相互再帰になるが、Haskell では問題ない。`sin(x + 1)` のような式で `Apply1` の引数が `CASValue`（多項式 `x + 1`）になるケースを自然に表現できる。

### 演算の構成性

各層の演算は内側の型の演算を使って定義される。

- `Poly a [cs] [ss] [fs]` の `(+)`: 同じ単項式の項をまとめ、係数の加算は `a` の `(+)` で行う
- `Poly a [cs] [ss] [fs]` の `(*)`: 係数の乗算は `a` の `(*)` で行い、単項式を結合する（冪指数の加算、負も可）
- `Frac a` の `(+)`: 通分して `a` の演算で計算し、`a` の `gcd` で簡約する
- `Frac a` の `(*)`: 分子同士・分母同士を `a` の `(*)` で掛け、`gcd` で簡約する

演算のディスパッチは、型チェッカーが正しい組み合わせを保証するため、ランタイムでは `CASValue` のコンストラクタによるパターンマッチで再帰的に処理する。

```haskell
casPlus :: CASValue -> CASValue -> CASValue
casPlus (CASInteger a) (CASInteger b) = CASInteger (a + b)
casPlus (CASPoly ts1) (CASPoly ts2) = casNormalizePoly (ts1 ++ ts2)
casPlus (CASFrac n1 d1) (CASFrac n2 d2) =
  casNormalizeFrac (casPlus (casMult n1 d2) (casMult n2 d1))
                  (casMult d1 d2)
```

### 正規化規則

#### 項の順序

降冪順（正の冪 → 定数項 → 負の冪）で現在の `ScalarData` 実装を踏襲する。多変数の場合は辞書式順序。

#### 零の正規表現

各型の零の正規表現:

| 型 | 零の正規表現 |
|---|---|
| `Integer` | `CASInteger 0` |
| `Factor` | 零なし（`Factor` は原子的な非零要素） |
| `Poly a [cs] [ss] [fs]` | `CASPoly []`（空の項リスト） |
| `Frac a` | 分子が `a` の零 / 分母が `a` の1。例: `Frac Integer` → `CASFrac (CASInteger 0) (CASInteger 1)` |
| `Frac (Poly a [cs] [ss] [fs])` | `CASFrac (CASPoly []) (CASPoly [CASTerm <one> empty3])` （`empty3 = Monomial [] [] []`） |

正規化時に零を簡約する。具体的な規則：

- `CASPoly` の正規化で零係数の項（`CASTerm` の係数が零）を除去する。結果として項が空になれば `CASPoly []` が零の正規形
- `CASFrac` の正規化で分子が零なら、`CASFrac <zero> <one>` に簡約する（分子を `a` の零、分母を `a` の1にする）。例: `CASFrac (CASInteger 0) (CASInteger 5)` → `CASFrac (CASInteger 0) (CASInteger 1)`。型構造を維持し、`Frac a` の値は常に `CASFrac` コンストラクタを持つ
- 各演算（`casPlus`, `casMult`）の出口で必ず正規化を通し、零の項が残らないようにする

これにより等価比較が構造的な比較だけで済み、現在の `mathNormalize` の方針とも一致する。

#### GCD 簡約

初期実装では現在の `Math/Normalize.hs` と同じ**モノミアルGCD**のみを移植する：

1. 分子・分母の全項から `casTermsGcd` で共通因子（係数の整数GCD + 各変数の最小冪）を抽出
2. 各項をそのGCDで割る（係数を `div`、冪を減算）
3. 分母が単項で負なら符号を反転

```haskell
casTermsGcd :: [CASTerm] -> CASTerm
casTermsGcd = foldl1 (\(CASTerm a xs) (CASTerm b ys) -> CASTerm (casGcd a b) (monoGcd xs ys))
-- casGcd は係数の型に応じて再帰的に定義（CASInteger なら gcd、CASPoly なら将来拡張）

monoGcd :: Monomial -> Monomial -> Monomial
monoGcd (Monomial c1 s1 f1) (Monomial c2 s2 f2) =
  Monomial (slotGcd c1 c2) (slotGcd s1 s2) (slotGcd f1 f2)
-- 各スロットは独立に GCD を計算
```

将来的に多項式GCD（ユークリッドの互除法等）へ拡張できるよう、`casGcd` を型クラスまたはパターンマッチで分離し、モノミアルGCDと多項式GCDを差し替え可能にしておく。

---

## シンボルと不定元

不定元（`x`, `y`）とシンボル（`i` 等）は型システム上の区別がない。どちらも `declare symbol` で宣言し、`Poly a [cs] [ss] [fs]` の `ss` スロットに入る。簡約規則は `declare rule` で別途付与する（例: `declare rule auto i^2 = -1`）。

```egison
declare symbol x, y           -- 不定元
declare symbol sin x, sqrt 2   -- 合成原子（簡約系に登録するため）
```

---

## Factor 型と数学関数

### Factor の3分類

`Factor` は原子的な数式要素全体を指す型で、**内部に自由シンボルを含むかどうか** で 3 つの真部分型に分かれる:

| 分類 | 例 | 内部表現 | 性質 |
|----|----|----|----|
| **`ConstantFactor`**（定数 Factor） | `sqrt 2`, `log 3`, `sin (1+1)` | 自由シンボルなしの `Apply1`/`Quote` | 任意のシンボル `s` で微分すると 0（連鎖律対象が存在しない） |
| **`Symbol`**（平のシンボル） | `x`, `y`, `r`, `θ` | `CASFactor (Symbol _)` | `s` 自身で微分すると 1、それ以外は 0 |
| **`AppliedFactor`**（合成 Factor） | `sin x`, `sqrt x`, `log (x+1)` | 自由シンボルを含む `Apply1`/`Quote` | 連鎖律で新しい Factor が生成されうる（微分で型構造が変わる可能性） |

包含関係:

```
ConstantFactor  ⊂  Factor
Symbol       ⊂  Factor
AppliedFactor   ⊂  Factor
```

`ConstantFactor`, `Symbol`, `AppliedFactor` の3つは互いに disjoint で、`Factor` はこれらの和に等しい。

### 3分類の動機

`Poly` の原子集合スロットを 3 つに分けるのは、**`fs = []` のとき `Poly c [cs] [ss] []` が微分で閉じる** ことを型レベルで表現するため。詳細は「微分演算子 `∂/∂`」セクションを参照。

### 静的な分類

各 Factor 値の分類は **コンパイル時に決定** される:

```egison
declare symbol x, y

x               : Symbol      -- 平のシンボル
y               : Symbol      -- 平のシンボル
sqrt 2         : ConstantFactor -- 引数 2 に自由シンボルなし
log 3          : ConstantFactor -- 引数 3 に自由シンボルなし
sin (1 + 1)    : ConstantFactor -- 引数 1+1 に自由シンボルなし
sin x          : AppliedFactor  -- 引数 x に自由シンボル x が現れる
sqrt x         : AppliedFactor  -- 引数 x に自由シンボル x が現れる
log (x + 1)    : AppliedFactor  -- 引数 x+1 に自由シンボル x が現れる
```

分類のアルゴリズム:

1. `'f a1 a2 ...` の形の Factor について、各引数 `ai` の自由シンボル集合 `freeSymbols(ai)` を再帰的に計算
2. 全引数が `freeSymbols = ∅` ならば `ConstantFactor`、それ以外は `AppliedFactor`
3. `declare symbol x` で宣言された平の名前は `Symbol`

`freeSymbols` の計算は `CASValue` を再帰的に走査して `CASFactor (Symbol _)` を集める処理で、既存の正規化と同程度のコストで実装可能。

### 内部表現

内部表現上は 3 分類とも `CASFactor SymbolExpr` だが、`SymbolExpr` の構成子と内部の自由シンボルの有無で実質的に区別される:

```
ConstantFactor:
  CASFactor (Apply1 _ a)    where  freeSymbols(a) = ∅
  CASFactor (Quote v)       where  freeSymbols(v) = ∅

Symbol:
  CASFactor (Symbol _ _ _)

AppliedFactor:
  CASFactor (Apply1 _ a)    where  freeSymbols(a) ≠ ∅
  CASFactor (Quote v)       where  freeSymbols(v) ≠ ∅
```

### Factor の各分類から Poly への埋め込み

各分類の Factor を `Poly` に埋め込むときは、対応するスロットに入る:

```
ConstantFactor → Poly Integer [cs] [] []     -- cs に入る
Symbol      → Poly Integer [] [ss] []     -- ss に入る
AppliedFactor  → Poly Integer [] [] [fs]     -- fs に入る
```

なお `declare symbol sin x` のような合成原子の宣言構文は、簡約系に登録するためのものであり、結果の値は通常通り `AppliedFactor` 型である（`declare symbol` 構文の対象と `Symbol` 型の外延は一致しない）。

### 数学関数の宣言 (`declare mathfunc`)

`sin`, `cos` 等の数学関数を「`Factor` を生成する関数」として明示的に宣言する。`declare mathfunc` で宣言された関数は、引数に自由シンボルがあれば `AppliedFactor`、なければ `ConstantFactor` を返す。

```egison
declare mathfunc sin : MathValue -> Factor
declare mathfunc cos : MathValue -> Factor
declare mathfunc log : MathValue -> Factor
declare mathfunc exp : MathValue -> Factor
declare mathfunc sqrt : MathValue -> Factor
```

`declare mathfunc` で宣言された関数を呼ぶと、関数適用が自動的に `CASFactor (Apply1 ...)` を生成する:

```egison
sin x           -- : AppliedFactor（x は自由シンボル）
sin 1           -- : ConstantFactor（1 に自由シンボルなし）
sqrt 2          -- : ConstantFactor
sqrt x          -- : AppliedFactor
sin (sqrt 2)    -- : ConstantFactor（引数にも自由シンボルなし）
```

内部表現は `CASFactor (Apply1 ...)` である。

### 数学関数の適用規則 (`declare apply`)

`declare mathfunc` で宣言した関数の適用時の評価ロジックを `declare apply` で定義する。引数が具体的な値の場合に簡約を行う。RHS にはユーザー定義関数を含む任意の Egison 式を書ける。

```egison
-- sqrt の適用規則
declare apply sqrt x :=
  if isInteger x then
    if isPerfectSquare x then isqrt x                    -- sqrt 4 → 2
    else if hasPerfectSquareFactor x then
      let (k, m) = extractPerfectSquare x in k * sqrt m  -- sqrt 8 → 2 * sqrt 2
    else 'sqrt x                                          -- sqrt 2 → そのまま Factor
  else 'sqrt x                                            -- sqrt (x+1) → そのまま Factor
```

`declare apply` の RHS でクォート `'sqrt x` と書くと、`declare apply` を再帰呼び出しせずに `CASFactor (Apply1 ...)` をそのまま生成する。クォートなしの `sqrt x` は `declare apply` を再帰呼び出しするため、フォールバックには必ずクォートを使う。

```egison
-- sin/cos の適用規則（特定の値に対する簡約）
declare apply sin x :=
  if x == 0 then 0
  else 'sin x                                             -- それ以外はそのまま Factor

declare apply cos x :=
  if x == 0 then 1
  else 'cos x
```

`declare apply` が定義されていない `declare mathfunc` は、常にデフォルトの挙動（`CASFactor (Apply1 ...)` を生成）になる。

**まとめ**: `declare apply` の RHS 内で同じ関数名を使う場合:
- `sqrt m` — `declare apply` が再適用される（`m` が更に簡約できる場合に有用）
- `'sqrt x` — `CASFactor` をそのまま生成（フォールバック、無限ループ回避）

パターン書き換え規則（`(sqrt $x)^2 = x` 等）は引き続き `declare rule` で定義する:

```egison
declare rule auto (sqrt $x)^2 = x
```

**`declare apply` と `declare rule` の使い分け**:

| | `declare apply` | `declare rule` |
|---|---|---|
| 対象 | `declare mathfunc` で宣言された関数 | 任意の数式パターン |
| 適用タイミング | 関数適用時（1回） | 正規化時（収束まで繰り返し） |
| RHS | 任意の Egison 式 | 任意の Egison 式 |
| 用途 | `sqrt 8 → 2 * sqrt 2`（アルゴリズム的簡約） | `i^2 = -1`、`(sqrt x)^2 = x`（パターン書き換え） |

### Factor の自動昇格

Factor 型の値が `Poly` の原子集合スロット以外の位置で使われたとき、自動的に多項式型に昇格する。
型注釈が閉じた `Poly` を指定していればそれに従い、指定がなければ開いた `Poly` に昇格する。

```egison
def s := sqrt 2                -- s : ConstantFactor

-- 開いた Poly への昇格
s + 1                           -- Poly Integer [..] [..] [..]

-- 閉じた Poly への昇格（型注釈あり）
def t : Poly Integer [sqrt 2] [] [] := s + 1    -- ConstantFactor は cs スロットに入る
```

`Factor`（および3分類）には `Ring` インスタンスを与えない。演算が必要な文脈では、型チェッカーが coercive subtyping（`Embed` 型クラス）により自動的に `Poly` へ `embed` を挿入する。これにより `casPlus` / `casMult` に `CASFactor` のケースは不要で、演算関数の実装がシンプルになる。

### CASFactor コンストラクタ

内部表現には `CASFactor` コンストラクタを持つ。`declare mathfunc` で宣言された関数の適用結果を表現する。`Poly` に昇格が必要になった時点で変換する。

```
-- sqrtSym = Apply1 (CASFactor (QuoteFunction sqrtFunc)) (CASInteger 2) :: SymbolExpr
-- ※ sqrtFunc は sqrt 関数の WHNFData

'(sqrt 2) : ConstantFactor
  → CASFactor sqrtSym
'(sqrt 2) : Poly Integer [sqrt 2] [] []
  → CASPoly [CASTerm (CASInteger 1) <[(sqrtSym, 1)] | [] | []>]
```

### sqrt の定義全体像

`sqrt` は `declare mathfunc` + `declare apply` + `declare rule` の3つの組み合わせで定義される:

```egison
-- 1. 数学関数の宣言
declare mathfunc sqrt : MathValue -> Factor

-- 2. 適用規則（アルゴリズム的簡約）
declare apply sqrt x :=
  if isInteger x then
    if isPerfectSquare x then isqrt x
    else if hasPerfectSquareFactor x then
      let (k, m) = extractPerfectSquare x in k * sqrt m
    else sqrt x
  else sqrt x

-- 3. パターン書き換え規則
declare rule auto (sqrt $x)^2 = x
```

呼び出し側で型注釈を付けることで `coerce` が自動挿入され、具体的な型に絞り込める（詳細は「coerce（型の絞り込み）」を参照）。

---

## 型の包含関係

### 基本的な包含

```
ConstantFactor  ⊂  Factor       -- 定数 Factor は Factor の真の部分型
Symbol       ⊂  Factor       -- 平のシンボルは Factor の真の部分型
AppliedFactor   ⊂  Factor       -- 合成 Factor は Factor の真の部分型

-- 各分類は対応するスロットに入る形で Poly に埋め込まれる
ConstantFactor  ⊂  Poly Integer [..] [] []
Symbol       ⊂  Poly Integer [] [..] []
AppliedFactor   ⊂  Poly Integer [] [] [..]
Factor       ⊂  Poly Integer [..] [..] [..]

Integer  ⊂  Frac Integer
Integer  ⊂  Poly Integer [cs] [ss] [fs]    -- 任意のスロット構成

Frac Integer  ⊂  Poly (Frac Integer) [cs] [ss] [fs]
Poly Integer [cs] [ss] [fs]  ⊂  Poly (Frac Integer) [cs] [ss] [fs]

Term a [cs] [ss] [fs]  ⊂  Poly a [cs] [ss] [fs]    -- 単一項の多項式として埋め込み

-- MathValue は全 CAS 型の上位型
Integer            ⊂  MathValue
ConstantFactor        ⊂  MathValue
Symbol             ⊂  MathValue
AppliedFactor         ⊂  MathValue
Factor             ⊂  MathValue
Poly a [cs] [ss] [fs]  ⊂  MathValue   -- 任意の a, cs, ss, fs に対して
Frac a             ⊂  MathValue       -- 任意の a に対して
```

**embed の内部表現:**
```
Term a [cs] [ss] [fs] → Poly a [cs] [ss] [fs]:
  CASTerm coeff mono → CASPoly [CASTerm coeff mono]
```

### 原子集合の包含

3 スロットそれぞれが独立に包含関係を持ち、全スロットで包含関係が成り立つときのみ全体の包含が成立する。

```
-- ss スロットだけ拡大
Poly a [] [x] []  ⊂  Poly a [] [x, y] []        -- ss: [x] ⊆ [x, y]

-- cs スロットだけ拡大
Poly a [] [] []  ⊂  Poly a [sqrt 2] [] []      -- cs: [] ⊆ [sqrt 2]

-- 一般則: 全スロット同方向の拡大
Poly a [cs₁] [ss₁] [fs₁]  ⊂  Poly a [cs₂] [ss₂] [fs₂]
   when  cs₁ ⊆ cs₂  ∧  ss₁ ⊆ ss₂  ∧  fs₁ ⊆ fs₂

-- 開いた型への昇格（各スロット独立）
Poly a [cs] [ss] [fs]  ⊂  Poly a [..] [ss] [fs]
Poly a [cs] [ss] [fs]  ⊂  Poly a [cs] [..] [fs]
Poly a [cs] [ss] [fs]  ⊂  Poly a [cs] [ss] [..]
Poly a [cs] [ss] [fs]  ⊂  Poly a [..] [..] [..]
```

### 包含の伝播規則

```
a ⊂ b  ならば  Poly a [cs] [ss] [fs] ⊂ Poly b [cs] [ss] [fs]    -- 係数の埋め込み
a ⊂ b  ならば  Frac a ⊂ Frac b                                  -- 分数の埋め込み
a ⊂ b  ならば  Tensor a ⊂ Tensor b                              -- テンソルの埋め込み
```

---

## 自動変換の3つの仕組み

### 1. embed（包含関係による自動埋め込み）

型の包含関係がある場合、ターゲット型が文脈から既知のときに自動的に `embed` を挿入する。

```egison
def f (x : Poly Integer [] [x, y] []) : Poly Integer [] [x, y] [] := x + x
def p : Poly Integer [] [x] [] := 1 + x

f p  ⇝  f (embed p)    -- Poly Integer [] [x] [] → Poly Integer [] [x, y] []
                       -- ss スロットだけ拡大
```

```egison
def n : Integer := 3
def q : Poly Integer [] [i] [] := n + i    -- Integer → Poly Integer [] [i] [] に自動 embed
```

#### embed の実装

`Embed` 型クラスと型チェッカーによる AST 変換の組み合わせで実装する。

**`Embed` 型クラス**で変換規則を宣言的に定義する：

```egison
class Embed a b where
  embed :: a -> b

instance Embed Integer (Poly Integer [..] [..] [..]) where
  embed n = ...  -- CASInteger n → CASPoly [CASTerm (CASInteger n) <[] | [] | []>]

instance Embed Integer (Frac Integer) where
  embed n = ...  -- CASInteger n → CASFrac (CASInteger n) (CASInteger 1)

-- 各分類の Factor は対応するスロットに入る
instance Embed ConstantFactor (Poly Integer [..] [] []) where
  embed f = ...  -- CASFactor sym → CASPoly [CASTerm (CASInteger 1) <[(sym, 1)] | [] | []>]

instance Embed Symbol (Poly Integer [] [..] []) where
  embed s = ...  -- CASFactor sym → CASPoly [CASTerm (CASInteger 1) <[] | [(sym, 1)] | []>]

instance Embed AppliedFactor (Poly Integer [] [] [..]) where
  embed f = ...  -- CASFactor sym → CASPoly [CASTerm (CASInteger 1) <[] | [] | [(sym, 1)]>]

instance {Embed a b} Embed (Poly a [..] [..] [..]) (Poly b [..] [..] [..]) where
  embed p = ...  -- 各項の係数を再帰的に embed

-- 原子集合の拡大（内部表現は変わらない、3スロット独立）
instance {cs₁ ⊆ cs₂, ss₁ ⊆ ss₂, fs₁ ⊆ fs₂}
         Embed (Poly a [cs₁] [ss₁] [fs₁]) (Poly a [cs₂] [ss₂] [fs₂]) where
  embed p = p
```

**型チェッカー**が型不一致を検出したとき、包含関係があれば `Embed` 制約を解決し、AST に `embed` 呼び出しを挿入する（elaboration）：

```
-- 型チェック前
x + 1        -- x : Poly Integer [] [x] [], 1 : Integer

-- 型チェック後（elaborated）
x + embed 1  -- embed 1 : Poly Integer [] [x] []
```

**推移的な変換**（例: `Integer → Poly Integer [] [s] [] → Poly (Frac Integer) [] [s] []`）は、型チェッカーが包含関係の推移閉包を探索し、必要に応じて `embed . embed` を合成する。

各変換の内部表現は `Embed` インスタンスのコメントに記載。原子集合の拡大は内部表現を変えない（型レベルのみの操作）。

#### 実装方式

Coq の Coercion mechanism に倣い、型チェッカーに coercive subtyping を組み込む：

- `Embed` インスタンスの宣言時に、型チェッカーが包含関係のグラフを構築する
- 型不一致の検出時に、グラフ上の最短経路（深さ制限付きBFS）で推移的な `embed` の合成を探索する
- **Coherence（一貫性）** は、CAS型の包含関係が数学的に明確な半順序であることから自然に保証される（どの経路でも同じ数学的な値に変換される）
- 参考文献: Luo (1999) *Coercive subtyping*, Breazu-Tannen et al. (1991) *Inheritance as implicit coercion*

#### coerce（型の絞り込み）

`embed` は安全な widening（具体 → 一般）だが、逆方向の narrowing（一般 → 具体）が必要な場面がある。典型的には、関数の戻り値が開いた型 `[..] [..] [..]` で推論されるが、ユーザは結果が特定の原子集合に属することを知っている場合。

```egison
-- sqrt は開いた型しか返せない
sqrt : Integer -> Poly Integer [..] [..] [..]

-- ユーザは結果の型を知っている
def x : Poly Integer [sqrt 2] [] [] := sqrt 2    -- coerce が自動挿入される
def n : Integer := sqrt 4                          -- coerce が自動挿入される
```

`coerce` は `embed` の逆方向の操作で、ランタイム検証を伴う：

```egison
class Coerce a b where
  coerce :: a -> b    -- ランタイムで型の適合性を検証し、失敗時はエラー
```

型注釈がトリガーとなり、型チェッカーが `coerce` を自動挿入する（elaboration）。

**ランタイム検証の内容**:

| 変換 | 検証内容 |
|------|----------|
| `Poly a [..] [..] [..] → Poly a [cs] [ss] [fs]` | 全項のモノミアルの各スロットの原子が、対応する閉じた集合の部分集合か |
| `Poly a [..] [..] [..] → Integer` | 全項のモノミアルが3スロット全て空で、係数が `Integer` に変換可能か |
| `Poly a [..] [..] [..] → ConstantFactor` | 単一項で係数が1、`monoConsts` が単一原子の1乗、他スロットが空 |
| `Poly a [..] [..] [..] → Symbol` | 単一項で係数が1、`monoSymbols` が単一原子の1乗、他スロットが空 |
| `Poly a [..] [..] [..] → AppliedFactor` | 単一項で係数が1、`monoComps` が単一原子の1乗、他スロットが空 |
| `Frac a → a` | 分母が1か |

**embed との対称性**:

```
embed  : 具体 → 一般（常に安全、コンパイル時保証、型チェッカーが自動挿入）
coerce : 一般 → 具体（ランタイム検証、失敗時エラー、型注釈による自動挿入）
```

### 2. join（二項演算時の最小上界の計算）

二項演算 `a + b` で `a : τ₁`, `b : τ₂` のとき、最小上界 `join(τ₁, τ₂)` を求め、双方を embed する。

#### join の計算規則

```
join(a, a) = a

-- 係数の昇格
join(Integer, Frac Integer) = Frac Integer
join(Integer, Poly Integer [cs] [ss] [fs]) = Poly Integer [cs] [ss] [fs]

-- 閉じた Poly 同士: 各スロット独立に和集合を自動計算
join(Poly a [cs₁] [ss₁] [fs₁], Poly b [cs₂] [ss₂] [fs₂])
  = Poly (join(a, b)) [cs₁ ∪ cs₂] [ss₁ ∪ ss₂] [fs₁ ∪ fs₂]

-- 閉じた Poly と他の型
join(Poly a [cs] [ss] [fs], b) = Poly (join(a, b)) [cs] [ss] [fs]
join(a, Poly b [cs] [ss] [fs]) = Poly (join(a, b)) [cs] [ss] [fs]

-- 開いた Poly（任意のスロットが [..] のとき結果も対応スロットが [..]）
-- 一般則: 各スロットが独立に join される。あるスロットが片方でも [..] なら結果も [..]
join(Poly a [..] [..] [..], Poly b [..] [..] [..]) = Poly (join(a, b)) [..] [..] [..]
join(Poly a [cs] [..] [fs], Poly b [cs'] [ss'] [fs']) = Poly (join(a, b)) [cs ∪ cs'] [..] [fs ∪ fs']
-- 等

-- Frac
join(Frac a, Frac b) = Frac (join(a, b))
join(a, Frac b) = Frac (join(a, b))
```

> 各スロットの和集合は **スロット内の原子集合に対して** 計算する。スロット間で原子は混ざらない（`Symbol` と `ConstantFactor` を同じスロットに入れることはない）。

> あるスロットが `[..]` のとき、そのスロットだけ開いた型として吸収される。他のスロットは閉じたままでよい。これにより部分的な絞り込みが可能になる。

#### join の具体例

```
-- ss スロットだけ拡大、他は空のまま
join(Poly Integer [] [x] [], Poly Integer [] [x, y] []) = Poly Integer [] [x, y] []

-- 包含関係なし → 和集合で自動合流（ss スロット）
join(Poly Integer [] [x] [], Poly Integer [] [y] []) = Poly Integer [] [x, y] []

-- cs と ss の両方が拡大
join(Poly Integer [] [x] [], Poly Integer [sqrt 2] [y] [])
  = Poly Integer [sqrt 2] [x, y] []

-- 係数の昇格（原子集合は同一）
join(Poly Integer [] [x] [], Poly (Frac Integer) [] [x] []) = Poly (Frac Integer) [] [x] []

-- 開いた型との合流（ss スロットだけ）
join(Poly Integer [] [x] [], Poly Integer [] [..] []) = Poly Integer [] [..] []
```

#### 設計の意図

型レベルの原子集合操作は以下の3つを **各スロット独立に** 適用する。


| 操作        | 用途                 |
| --------- | ------------------ |
| `S₁ = S₂` | 同型の演算判定            |
| `S₁ ⊆ S₂` | embed 判定            |
| `S₁ ∪ S₂` | join 時の結果原子集合計算   |

#### 型推論との統合

通常の HM 型推論の単一化を拡張し、等式制約 `τ₁ = τ₂` の代わりに包含制約 `τ₁ ⊂ τ`, `τ₂ ⊂ τ` を生成して最小の `τ` を求める。

```egison
declare symbol x, y

def p : Poly Integer [] [x] [] := 1 + x
def q : Poly Integer [] [x, y] [] := 1 + x + y

p + q
-- join(Poly Integer [] [x] [], Poly Integer [] [x, y] []) = Poly Integer [] [x, y] []
-- （ss スロット: [x] ⊆ [x, y] なので OK）
⇝ (embed p) + q : Poly Integer [] [x, y] []
```

```egison
def p : Poly Integer [] [x] [] := 1 + x
def q : Poly Integer [] [y] [] := 2 + y

p + q
-- join(Poly Integer [] [x] [], Poly Integer [] [y] []) = Poly Integer [] [x, y] []
-- ss スロットの和集合 [x] ∪ [y] = [x, y] を自動計算し、双方を embed
⇝ (embed p) + (embed q) : Poly Integer [] [x, y] []
-- 結果: x + y + 3
```

### 3. tensorMap（スカラー関数のテンソルへの自動持ち上げ）

スカラー関数がテンソルに適用されたとき、自動的に `tensorMap` を挿入する。

```egison
def f (x : Poly Integer [] [i] []) : Poly Integer [] [i] [] := x + x
def t : Tensor Integer := [| 1, 2, 3 |]

f t  ⇝  tensorMap (f . embed) t
-- 結果 : Tensor (Poly Integer [] [i] [])
```

#### 3つの仕組みの複合例

```egison
def t : Tensor (Poly Integer [] [x] []) := [| 1 + x, x^2 |]
def q : Poly Integer [] [x] [] := 1 + x

t + q
-- tensorMap 挿入: q はスカラー、t はテンソル
-- 成分と q は同じ Poly Integer [] [x] [] なのでそのまま加算
⇝ tensorMap (λe -> e + q) t
-- 結果 : Tensor (Poly Integer [] [x] [])
```

---

## 代数的型クラス階層

法則（結合律、交換律、分配律など）は型クラスには含めず、ドキュメントで記述する。
型クラスは操作ベースで定義する。

### 加法的構造

```egison
class AddSemigroup a where
  (+) (x: a) (y: a) : a

class AddMonoid a extends AddSemigroup a where
  zero : a

class AddGroup a extends AddMonoid a where
  neg (x: a) : a
```

### 乗法的構造

```egison
class MulSemigroup a where
  (*) (x: a) (y: a) : a

class MulMonoid a extends MulSemigroup a where
  one : a

class MulGroup a extends MulMonoid a where
  inv (x: a) : a
```

### 複合構造

```egison
class Ring a extends AddGroup a, MulMonoid a
class Field a extends Ring a, MulGroup a
```

### CAS 用の操作

```egison
class GCDDomain a extends Ring a where
  gcd (x: a) (y: a) : a

class EuclideanDomain a extends GCDDomain a where
  divMod (x: a) (y: a) : (a, a)
```

### インスタンスの2つのパターン

型クラスのインスタンスには2つのパターンがある:

**パターン1: MathValue に委譲する型クラス（`Ring` 等）**

`Ring` の演算（`+`, `*`）はすべて `MathValue` レベルの `casPlus`/`casMult` + `casNormalize` で実装される。subtype のインスタンスは MathValue の演算をラップ + coerce するだけで、**独自の簡約ロジックを持たない**。簡約規則は `declare rule auto` で `MathValue` レベルに統一されるため、`Ring` の特化インスタンスは不要。

**パターン2: subtype ごとに独自実装する型クラス（`GCDDomain` 等）**

`gcd` や素因数分解は型ごとに異なるアルゴリズムが必要であり、`MathValue` には定義できない。各 subtype が**独自のアルゴリズムを直接実装**する。汎用インスタンスと特化インスタンスが共存する場合は、**より具体的なインスタンスを優先**する。

| | パターン1 (`Ring`) | パターン2 (`GCDDomain`) |
|---|---|---|
| `MathValue` インスタンス | あり（全計算の基盤） | **なし**（型エラー） |
| subtype の実装方式 | MathValue に委譲 + coerce | 各 subtype が独自実装 |
| 特化インスタンス | 不要 | 必要（`Z[i]` 等に専用アルゴリズム） |

#### Ring インスタンス（パターン1）

```egison
-- MathValue: 全 CAS 計算の基盤
instance Ring MathValue where
  (+) a b := casNormalize (casPlus a b)
  (*) a b := casNormalize (casMult a b)
  zero := CASInteger 0
  one := CASInteger 1
  neg := casNeg

-- subtype: MathValue の演算をラップ + coerce
instance Ring Integer
instance {Ring a} Ring (Poly a [..] [..] [..]) where
  (+) p q := coerce (unwrap p + unwrap q)
  (*) p q := coerce (unwrap p * unwrap q)
instance {GCDDomain a} Ring (Frac a) where
  (+) (p/q) (r/s) := coerce (unwrap (p/q) + unwrap (r/s))
  (*) (p/q) (r/s) := coerce (unwrap (p/q) * unwrap (r/s))
instance {GCDDomain a} Field (Frac a)
```

#### GCDDomain / EuclideanDomain インスタンス（パターン2）

```egison
-- 整数: ユークリッドの互除法
instance EuclideanDomain Integer where
  gcd a b := ...
  divMod a b := ...

-- Z[i]（ガウス整数）: ノルム N(a+bi) = a²+b² を使うユークリッド算法
type GaussianInt := Poly Integer [] [i] []
instance EuclideanDomain GaussianInt where
  gcd a b := ...  -- ガウス整数専用のアルゴリズム
  divMod a b := ...

-- 係数が体の多項式: 汎用の多項式GCD
instance {Field a} EuclideanDomain (Poly a [..] [..] [..]) where
  gcd a b := ...  -- 一般の多項式ユークリッド算法
  divMod a b := ...

-- 係数が GCDDomain の多項式: 内容と原始部分に分解する GCD
instance {GCDDomain a} GCDDomain (Poly a [..] [..] [..]) where
  gcd a b := ...  -- 内容 GCD × 原始部分 GCD
```

#### インスタンス解決の優先順位

汎用インスタンスと特化インスタンスが共存する場合、**より具体的なインスタンスを優先**する:

```egison
-- GaussianInt (= Poly Integer [] [i] []) に gcd を適用
gcd (1 + i) (2 + i)
-- 候補1: instance EuclideanDomain GaussianInt             -- 具体的 ✓ 選ばれる
-- 候補2: instance {GCDDomain a} GCDDomain (Poly a [..] [..] [..])  -- 汎用

-- Poly (Frac Integer) [] [x] [] に gcd を適用
gcd (x^2 - 1) (x - 1)
-- 候補: instance {Field a} EuclideanDomain (Poly a [..] [..] [..])  -- a = Frac Integer, Field ✓
```

具体性の判定: 型引数がすべて具体的（型変数なし）なインスタンスは、型変数を含むインスタンスより具体的とみなす。CAS 型の特化インスタンスは有限個（`Z[i]`, `Z[ω]` 等）なので、この規則で十分。

#### 型推論の挙動

```egison
declare symbol x, i
declare rule auto i^2 = -1

type GaussianInt := Poly Integer [] [i] []

def a : GaussianInt := 1 + i
def b : GaussianInt := 2 + 3*i
a + b         -- Ring GaussianInt が選ばれる → : GaussianInt (= 3 + 4*i)
a * b         -- Ring GaussianInt が選ばれる → : GaussianInt (= -1 + 5*i)
gcd a b       -- EuclideanDomain GaussianInt（特化）が選ばれる

def p : Poly Integer [] [x] [] := 1 + x
def q : Poly Integer [] [x] [] := 2 + 3 * x
p + q         -- Ring (Poly Integer [] [x] []) が選ばれる → : Poly Integer [] [x] []

-- subtype が異なる場合は join で和集合を計算
a + p         -- GaussianInt と Poly Integer [] [x] [] の混合
              -- → join: ss スロット [i] ∪ [x] = [i, x]
              -- → 双方を embed → Ring (Poly Integer [] [i, x] []) → : Poly Integer [] [i, x] []

-- MathValue には GCDDomain インスタンスがないので型エラー
-- gcd (unwrap a) (unwrap p)   -- 型エラー: GCDDomain MathValue は存在しない
```

### 微分演算子 `∂/∂`

偏微分演算子 `∂/∂` は **型クラス `Differentiable` のメソッド** として定義する。各 CAS 型ごとにインスタンスを定義し、それらを合成することで `MathValue` 全体の微分が定義される。返り値は常に `MathValue`。

#### 型クラス定義

```egison
class Differentiable a where
  ∂/∂ : a -> Symbol -> MathValue
```

#### 基本型のインスタンス

```egison
-- 整数の微分は常に 0
instance Differentiable Integer where
  ∂/∂ n s = 0

-- 定数 Factor: 自由シンボルなし → 常に 0
instance Differentiable ConstantFactor where
  ∂/∂ f s = 0

-- 平のシンボル: 自分自身なら 1、それ以外は 0
instance Differentiable Symbol where
  ∂/∂ x s = if x == s then 1 else 0

-- 合成 Factor: 連鎖律（ユーザーが関数ごとの導関数を定義）
instance Differentiable AppliedFactor where
  ∂/∂ f s = ...  -- 連鎖律: f の外側関数の導関数 × 内部引数の微分
                 -- 導関数の定義は declare derivative で登録（後述）
```

#### 複合型のインスタンス

```egison
-- Term: 積の微分法則（係数 × 各モノミアル因子の product rule）
instance {Differentiable a} Differentiable (Term a [cs] [ss] [fs]) where
  ∂/∂ t s = ...  -- 係数の微分 × モノミアル + 係数 × モノミアルの微分（product rule）
                 -- モノミアルの微分は3スロットの各因子に product rule を適用

-- Poly: 線形性（各項の微分の和）
instance {Differentiable a} Differentiable (Poly a [cs] [ss] [fs]) where
  ∂/∂ p s = sum [∂/∂ t s | t <- terms p]

-- Frac: 商の微分法則
instance {Differentiable a} Differentiable (Frac a) where
  ∂/∂ (n/d) s = (∂/∂ n s * d - n * ∂/∂ d s) / d^2

-- MathValue: 内部の CASValue コンストラクタに応じて上記インスタンスにディスパッチ
instance Differentiable MathValue where
  ∂/∂ v s = ...  -- CASInteger → Differentiable Integer
                 -- CASFactor → Differentiable ConstantFactor / Symbol / AppliedFactor
                 -- CASPoly → Differentiable (Poly ...)
                 -- CASFrac → Differentiable (Frac ...)
```

**設計の利点**:
- `Haskell にハードコードされた `casDeriv` ではなく、各型ごとの微分規則をユーザーが定義・拡張できる
- `AppliedFactor` の導関数（`sin` → `cos` 等）をユーザーが `declare derivative` で登録する
- `Poly`, `Frac`, `Term` の微分法則は数学的な構造（線形性、積の法則、商の法則）に直接対応

#### `AppliedFactor` の導関数定義

合成 Factor の外側関数の導関数は `declare derivative` で登録する:

```egison
declare derivative sin = cos
declare derivative cos = (- sin)    -- マイナスをつける
declare derivative log = \x -> 1/x
declare derivative exp = exp
declare derivative sqrt = \x -> 1 / (2 * sqrt x)
```

`∂/∂ (sin x) s` の計算:
1. `sin` の導関数 `cos` を `declare derivative` から取得
2. 連鎖律: `cos x * ∂/∂ x s`

これにより `∂/∂ (sin x) x = cos x * 1 = cos x` となる。

#### 微分で閉じる条件

> `Poly c [cs] [ss] []`（`fs` スロットが空）は、任意の `Symbol` での `∂/∂` で型保存になる。

理由:
- `cs` の各定数 Factor は微分で 0 になる
- `ss` の各平のシンボルは冪則で微分され、`Poly c [cs] [ss] []` 内に閉じる
- `fs = []` なので連鎖律対象が存在せず、新しい原子が生成されない

`fs ≠ []` の場合は連鎖律で新しい原子（例: `cos x`）が生成されうるため、型保存ではない。

返り値が `MathValue` なので型保存は自動的には保証されないが、`fs = []` の `Poly` に対して `∂/∂` の結果を `coerce` で同じ型に戻すことが常に成功することを、ユーザーは型注釈で表明できる:

```egison
def f : Poly Integer [] [x, y] [] := x^2 * y + 3 * x * y^2
def df : Poly Integer [] [x, y] [] := coerce (∂/∂ f x)   -- 常に成功
```

#### `Symbol` を第2引数に取る根拠

1. **意味的明確性**: 数学的に微分は変数（シンボル）に対して定義される
2. **`Embed` 階層との整合**: `Symbol ⊂ Factor` なので、ユーザコードでは `∂/∂ f x` のように自然に書ける

#### 利用例

```egison
declare symbol x, y

def f : Poly Integer [] [x, y] [] := x^2 * y + 3 * x * y^2

∂/∂ f x   -- => 2 * x * y + 3 * y^2 : MathValue
∂/∂ f y   -- => x^2 + 6 * x * y     : MathValue

-- 型保存が分かっている場合は coerce で絞り込み
def df : Poly Integer [] [x, y] [] := coerce (∂/∂ f x)

-- 定数 Factor を含むケース
def g : Poly Integer [sqrt 2] [x] [] := (sqrt 2) * x^2 + 3*x
∂/∂ g x   -- => 2 * (sqrt 2) * x + 3 : MathValue

-- 合成 Factor を含むケース（連鎖律）
∂/∂ (sin x) x          -- => cos x : MathValue
∂/∂ (x^2 + sin x) x    -- => 2*x + cos x : MathValue

-- 型エラー
-- ∂/∂ f (log x)   -- 型エラー: log x は Symbol でない
```

#### 微分演算子の合成

```egison
-- MathValue を返すので、合成は自然に行える
def laplacian2D (f : MathValue) : MathValue :=
  ∂/∂ (∂/∂ f x) x + ∂/∂ (∂/∂ f y) y
```

### 積分演算子 `Sd`

不定積分 `Sd` も `MathValue` 上の関数として定義する。積分は結果の型が入力から予測困難（係数に分数が出る、対数が出る等）なため、`MathValue` で統一する。第1引数は微分と同様に **`Symbol` 型** に制限し、合成原子で積分しようとする呼び出しを静的に弾く。

```egison
Sd : Symbol -> MathValue -> MathValue    -- ∫ f dx
```

将来的に `∂/∂` と同様に `Integrable` 型クラス化することも考えられるが、積分は閉じた形で結果が得られるとは限らず、多項式に対しても係数に分数が出るため `Poly Integer [xs]` → `Poly (Frac Integer) [xs]` のように型が変わる。型保存型の型クラスでの整理が難しいので、当面は `MathValue` 上の単一関数として実装する。

#### 実装

内部では `CASValue` のコンストラクタをパターンマッチし、各層に応じた積分を行う。

```egison
Sd x f = ...
  -- CASPoly の場合: 各項の冪を1上げて係数を割る
  -- CASFrac の場合: 部分分数分解等
```

注: 積分は微分と異なり、常に閉じた形で結果が得られるとは限らない。

#### 利用例

```egison
declare symbol x

def f : Poly Integer [] [x] [] := x^2 + 3 * x

Sd x f   -- => (1/3) * x^3 + (3/2) * x^2 : MathValue
-- 精密な型に絞り込みたい場合は coerce を使う
def result : Poly (Frac Integer) [] [x] [] := coerce (Sd x f)
```

### `substitute` と `expandAll`

`substitute`（代入）や `expandAll`（展開）は `MathValue` 上の関数として定義する。内部で `CASValue` のコンストラクタをパターンマッチし、`Poly` の場合は項を分解して処理、`Frac` の場合は分子・分母に再帰的に適用する。

```egison
substitute : List (Factor, MathValue) -> MathValue -> MathValue
expandAll : MathValue -> MathValue
```

### type class が不要な演算子

以下の演算子は `MathValue` 上の関数、既存の型クラス制約、または関数合成で定義でき、新たな型クラスは不要。

| 演算子 | 定義方法 |
|--------|----------|
| `∇` / `grad` / `nabla` | `MathValue` 上で `∂/∂` を合成 |
| `div`（発散）, `rot`（回転） | `∂/∂` + `Ring` でテンソル演算と合成 |
| `taylorExpansion` | `∂/∂` を繰り返し適用 |
| `wedge` (`∧`), `ι` | `Field` 制約でテンソル上の演算として定義 |
| `dotProduct`, `crossProduct`, `trace` | `Ring` 制約で定義 |
| `M.inverse`, `M.determinant` | `Field` / `Ring` 上のテンソル演算 |

---

## 簡約規則の宣言 (`declare rule`)

簡約規則はシンボル宣言から独立した専用構文 `declare rule` で宣言する。これにより単一シンボルの規則も複数シンボル間の関係式も統一的に扱える。`declare symbol` と `declare rule` は常に分けて定義し、`declare symbol ... with ...` の糖衣構文は採用しない。

**すべての簡約規則は `MathValue` レベルに紐づく**。`i^2 = -1` は `GaussianInt` (`Poly Integer [] [i] []`) 固有の規則ではなく、`i` を含む任意の `MathValue` の演算で `casNormalize` により適用される。subtype の `Ring` インスタンスは `MathValue` の演算をラップして coerce するだけなので、簡約規則は自動的に全ての subtype に波及する。

```egison
-- シンボル宣言と簡約規則は常に別々に書く
declare symbol i
declare rule auto i^2 = -1
```

### 自動規則と手動規則

```egison
-- 自動規則 (auto): 正規化時に常に適用される。正規形を定義する。
declare rule auto i^2 = -1
declare rule auto (sqrt $x)^2 = x

-- 手動規則: 名前付きで登録し、ユーザーが明示的に適用する。
-- $x で束縛、#x で同じ値を要求（Egison の非線形パターンマッチ）
declare rule trig_pythagorean (sin $x)^2 + (cos #x)^2 = 1
declare rule trig_addition sin ($a + $b) = sin a * cos b + cos a * sin b
```

- **自動規則 (`auto`)**: `casNormalize` 実行時に常に適用される。`i^2 = -1` のように正規形を一意に定める規則に使う。Maxima の `tellsimp` に相当。
- **手動規則**: 名前付きで規則環境に登録され、`simplify expr using rule_name` で明示的に適用する。`sin(x)^2 + cos(x)^2 = 1` のように、どちらの方向に簡約すべきか自明でない規則に使う。Maxima の `defrule` に相当。

`declare rule` はトップレベル宣言のみを許可する。パターン変数 `$x` を使うことで汎用的な規則をトップレベルで定義できるため、式レベル構文や `IO` モナドは不要。

`declare rule` の右辺（RHS）は**パターン変数を参照できる任意の Egison 式**である。ユーザー定義関数の呼び出しも可能。

なお、`sqrt 8 → 2 * sqrt 2` のような関数適用時のアルゴリズム的簡約は `declare rule` ではなく `declare apply`（「数学関数の適用規則」セクション参照）で定義する。`declare rule` はあくまで数式パターンの書き換えに使う。

### 手動規則の適用

```egison
declare rule trig_pythagorean (sin $x)^2 + (cos #x)^2 = 1

def expr := (sin x)^2 + (cos x)^2 + 1

simplify expr using trig_pythagorean
-- 結果: 2
```

### 設計の根拠

自動/手動の区別が必要な理由:

- `i^2 = -1` は常に適用しても正規形が一意に定まる（次数を下げる方向に収束する）
- `sin(x)^2 + cos(x)^2 = 1` を常に自動適用すると、`sin(x)^2` を見るたびに `1 - cos(x)^2` に置換するか `cos(x)^2` を `1 - sin(x)^2` に置換するかの選択が生じ、正規形が一意に定まらない
- Mathematica も同様の問題に対して「ユーザーが `TrigReduce` 等を明示的に呼ぶ」方針を取っている

### 規則環境の内部表現

```haskell
-- 簡約規則
data ReductionRule = ReductionRule
  { ruleName :: Maybe String           -- 手動規則の名前（auto 規則は Nothing）
  , ruleFunc :: CASValue -> CASValue   -- LHS → RHS の書き換え関数
  }

-- 規則環境: シンボルから独立した専用の環境
type ReductionEnv = [ReductionRule]
```

`ruleFunc` は `declare rule auto <lhs> | <guard> = <rhs>` の LHS パターン、ガード条件、RHS 式から動的に生成される書き換え関数。LHS のパターンマッチで変数を束縛し、ガードが真なら RHS の Egison 式を評価して結果を返す。RHS ではユーザー定義関数を含む任意の式が使える。

正規化関数は規則環境を参照する:

```haskell
casNormalizeWithRules :: ReductionEnv -> CASValue -> CASValue
```

### 関数引数内のパターン分解

関数引数のパターンマッチを含む規則（例: `sin ($a + $b) = sin a * cos b + cos a * sin b`）では、規則の左辺 `sin ($a + $b)` が `SymbolExpr` の `Apply1` コンストラクタにマッチし、その引数（`CASValue`）を `poly` マッチャーで分解して `$a + $b` を束縛する。つまり、`declare rule` の規則適用エンジンは、CAS マッチャー（`poly`, `frac` 等）を内部的に利用して関数引数内のパターンを分解する。

パターン変数の型は LHS の構造から推論される。`+` で分解される位置のパターン変数は `Poly a [..] [..] [..]` 型、モノミアル内のパターン変数は `Factor` 型として推論される。例えば `sin ($a + $b) = ...` では、`$a + $b` が `poly` マッチャーで分解されるため、`$a` と `$b` は `Poly a [..] [..] [..]` 型となる。

---

## CASValue のパターンマッチ

`multiset` が `Matcher a → Matcher [a]` であるのと同様に、CAS の各層をパラメトリックなマッチャーとして設計する。マッチャーの合成により、入れ子の数式構造に対するパターンマッチを統一的に扱える。

### マッチャーの定義

```egison
-- 基本マッチャー（パラメータなし）
integer     : Matcher Integer
constantFactor : Matcher ConstantFactor
symbol      : Matcher Symbol
appliedFactor  : Matcher AppliedFactor
factor      : Matcher Factor

-- パラメトリックマッチャー（係数マッチャーを受け取る）
poly {a} (m : Matcher a) : Matcher (Poly a .. .. ..)
term {a} (m : Matcher a) : Matcher (Term a .. .. ..)
frac {a} (m : Matcher a) : Matcher (Frac a)
```

`poly`, `term` の原子集合スロットはランタイムのマッチャーには不要（CASValue の内部構造が型ごとに異なるため、マッチャーが区別する必要がない）。型レベルでのみ `Poly a [] [x] []` / `Poly a [] [x, y] []` 等を区別する。

`factor` は `Factor` 型全体のマッチャーで、`constantFactor`, `symbol`, `appliedFactor` の和集合として振る舞う。各分類専用のマッチャー（`constantFactor`, `symbol`, `appliedFactor`）も提供し、`term m` のモノミアル分解では各スロット用に使い分ける。

### `:+` パターンの意味論

`poly m` の `:+` パターンは、多項式を**項**と**残りの多項式**に分解する。項は `Term a` 型であり、係数（型 `a`）と3スロットのモノミアルの組である。`multiset m` の `::` と同じ意味論（順序不問の分解）。

```
poly m       の  $a :+ $rest  →  a : Term a [cs] [ss] [fs], rest : Poly a [cs] [ss] [fs]
multiset m   の  $a :: $rest  →  a : a,                     rest : [a]
```

`a : Term a [cs] [ss] [fs]` はそのまま `Poly a [cs] [ss] [fs]` を期待する関数に渡せる（`Term a [cs] [ss] [fs] ⊂ Poly a [cs] [ss] [fs]` の embed による）。

項をさらに分解するには `term m` マッチャーを使う。3スロットの構造に対応して、戻り値は係数と3つのモノミアルスロットの組:

```
term m  の  ($c, ($mc, $ms, $mf))
  → c  : a
    mc : [(ConstantFactor, Integer)]
    ms : [(Symbol, Integer)]
    mf : [(AppliedFactor, Integer)]
```

### 利用例

```egison
declare symbol x, y

-- Poly Integer [] [x] [] のパターンマッチ
-- poly: 項と残りに分解
match expr as poly integer with
  | $a :+ $rest -> ...   -- a : Term Integer [] [x] [], rest : Poly Integer [] [x] []

-- term: 係数と3スロットモノミアルに分解
match a as term integer with
  | ($c, ($mc, $ms, $mf)) -> ...
  -- c : Integer, mc : [(ConstantFactor, Integer)], ms : [(Symbol, Integer)], mf : [(AppliedFactor, Integer)]

-- 入れ子: poly (poly integer) で Z[x][y] を分解
match expr as poly (poly integer) with
  | $a :+ $rest -> ...   -- 係数が Poly Integer [] [x] []

-- frac: 分子/分母に分解
match expr as frac (poly integer) with
  | $n / $d -> ...       -- n, d : Poly Integer [] [x] []

-- モノミアルの3スロットを assocMultiset で分解
match (mc, ms, mf) as
  (assocMultiset constantFactor, assocMultiset symbol, assocMultiset appliedFactor) with
  | (([], [(x, $n), (y, $m)], [])) -> ...  -- coeff * x^n * y^m

-- factor マッチャーでシンボルの内部構造を分解
match (sin x) as appliedFactor with
  | apply1 $f $arg -> ...   -- f : sin 関数, arg : x
```

### 安全なダウンキャスト

型の絞り込みには **パターンマッチ**（`frac` の `$n / #1` で分母が1のときだけ抽出 — 常に安全）と **`coerce`**（型注釈による自動挿入 — ランタイム検証付き、「coerce（型の絞り込み）」参照）の2つがある。

---

## Axiom/FriCAS との比較

Axiom は「ドメインタワー」（例: `Polynomial(Fraction(Integer))`）で同様の正規形制御を実現している。
Egison の設計は以下の点で異なる。


|           | Axiom        | Egison                             |
| --------- | ------------ | ---------------------------------- |
| 型システム     | 独自（SPAD言語）   | HM型推論 + type class                 |
| 正規形の制御    | ドメインタワー      | 型注釈による `Poly`, `Frac` の組み合わせ        |
| 多項式の表現    | 標準多項式        | ローラン多項式（負の冪を許可）                    |
| 型変換       | `::` 演算子で明示的 | embed の自動挿入                        |
| 内部表現      | ドメインごとに固定    | 型構造から構成的に決定                        |
| シンボル集合の制御 | なし（全シンボル対等）  | 3スロット (`[cs] [ss] [fs]`) の閉じた/開いた集合を選択可能 |
| 微分の型保存    | なし           | `fs = []` で型保存になることを型レベルで表現          |
| テンソルとの統合  | なし           | tensorMap の自動挿入（論文で証明済み）           |
| 新しい数の導入   | ドメイン定義（SPAD） | Factor 3分類 + `declare mathfunc`           |


特に tensorMap 挿入と embed 挿入が単一の型推論パスで統一的に処理される点が新しい。

---

## 実装 TODO

### Phase 1: CASValue の基盤実装（完了）

`CASValue` データ型（`CASInteger`, `CASFactor`, `CASPoly`, `CASFrac`, `CASTerm`）を `Math/CAS.hs` に定義し、演算関数（`casPlus`, `casMult`）とローラン多項式の正規化（降冪順、零の除去、モノミアルGCD簡約）を実装した。`SymbolExpr` も `CASValue` を参照するよう移行済み。

> **3スロット化に伴う更新**: 現在の `Monomial = [(SymbolExpr, Integer)]` を 3 スロット構造 `Monomial { monoConsts, monoSymbols, monoComps }` に再リファクタリングする必要がある。Phase 5 着手前に Phase 1.5 として実施する（後述）。

### Phase 1.5: Monomial の3スロット化リファクタリング（新規）

3スロット設計（`ConstantFactor` / `Symbol` / `AppliedFactor` の分離）を内部表現に反映する。

- [ ] `Monomial` を `data Monomial = Monomial { monoConsts, monoSymbols, monoComps :: [(SymbolExpr, Integer)] }` に変更
- [ ] `freeSymbols :: SymbolExpr -> Set Symbol` を実装（`CASValue` を再帰走査）
- [ ] Factor の3分類判定関数 `factorKind :: SymbolExpr -> FactorKind` を実装
- [ ] `casNormalizePoly`, `casTermsGcd`, `monoGcd` 等を3スロットに対応させる
- [ ] `casPlus`, `casMult` のモノミアル比較・結合ロジックを3スロット独立に
- [ ] 既存の単一リスト前提のコードを全て修正
- [ ] mini-test: 3スロット混在の Poly の演算 (`(sqrt 2) * x + sin x` 等)

### Phase 2: 型システムへの統合（1スロット版は完了、3スロット化は未実施）

- `Type` ADT に `TPoly Type SymbolSet`、`TFrac Type`、`TFactor` を追加（`Types.hs`）— **1スロット版**
- `SymbolSet` の定義（`SymbolSetClosed [String]` / `SymbolSetOpen` / `SymbolSetVar TyVar`）
- パーサーで `Poly Integer [x, y]` / `Poly Integer [..]` / `Frac a` / `Factor` をパース — **1スロット版**
- 型推論での `Poly` 型の単一化とシンボル集合の包含判定（`S₁ ⊆ S₂`）
- `join` の実装（`Join.hs`: `joinTypes`, `isSubtype`, `symbolSetSubset`）
  - **TODO**: `joinSymbolSets` の `otherwise` 分岐を和集合 `S₁ ∪ S₂` に変更する（現状は `Nothing` を返して型エラーになる）
- 開いた `[..]` のフレッシュ型変数への脱糖（`freshenOpenSymbolSets`）
- `Embed` 型クラスと coercive subtyping は Phase 5.5 として後続実装予定

> **3スロット化に伴う更新**: 以下の変更が必要:
> - `TPoly Type SymbolSet` を `TPoly Type SymbolSet SymbolSet SymbolSet`（3スロット）に拡張
> - `TFactor` を `TConstantFactor`, `TSymbol`, `TAppliedFactor`, `TFactor` の4種類に分割
> - パーサーを `Poly a [cs] [ss] [fs]` の3スロット記法に対応
> - `joinTypes` を3スロット独立の和集合計算に拡張
> - `isSubtype` を3スロット同方向包含チェックに拡張
> - `freshenOpenSymbolSets` を3スロット独立のフレッシュ化に拡張

### Phase 3: ScalarData の CASValue 置換（完了）

`ScalarData` を完全に削除し、`CASData CASValue` に統一した。`Math/Expr.hs`（旧 `ScalarData`, `PolyExpr`, `TermExpr`, 旧 `SymbolExpr`）を削除し、全モジュール（`Primitives/Arith.hs`, `Core.hs`, `Data.hs`, `Tensor.hs` 等）を `CASValue` ベースに移行完了。`cabal test` 全21テストパス。

### Phase 4: CASValue のプリミティブパターンマッチ（完了）

`CASData CASValue` を直接パターンマッチできるプリミティブパターンを実装し、`fromMathExpr`/`toMathExpr` 変換関数を不要にした。

**実装したプリミティブパターン**:


| パターン         | 対象                    | 抽出内容                |
| ------------ | --------------------- | ------------------- |
| `div $ $`    | `CASFrac num den`      | 分子、分母（各 `CASData`）  |
| `poly $`     | `CASPoly terms`       | 項リスト（各 `CASData`）   |
| `term $ $`   | `CASTerm coeff mono`  | 係数（`CASData`）、モノミアル |
| `symbol $ $` | `Symbol name indices` | 名前、インデックスリスト        |
| `apply1 $ $` | `Apply1 fn arg`       | 関数、引数（各 `CASData`）  |
| `quote $`    | `Quote expr`          | 引用式（`CASData`）      |


**現状の制約**: これらのプリミティブパターンは `mathExpr` マッチャー内でのみ使われ、係数やシンボル集合を型に応じて区別できない。次の Phase 5 でパラメトリックマッチャーに拡張する。

**保持するもの**: `inductive pattern MathExpr` / `IndexExpr` 宣言、`CASIndexData`。

### Phase 5: パラメトリックマッチャー（poly, div, term）

**目標**: 現在の固定的な `mathExpr` マッチャーを、`list` や `multiset` と同様のパラメトリックなマッチャーに拡張する。型の入れ子構造に対応したマッチャーをユーザーが自由に合成できるようにする。

#### 背景

`mathExpr` マッチャーは固定的なモノリシック構造で、以下ができない:

- `Poly (Frac Integer) [] [x] []` と `Poly Integer [] [x] []` で異なるマッチャーを使い分ける
- 入れ子構造（`Poly (Poly Integer [] [x] []) [] [y] []`）の各層に適切なマッチャーを指定する

目標は型とマッチャーの構造を一致させること:

```
型                                              マッチャー
Integer                                         integer
ConstantFactor                                     constantFactor
Symbol                                          symbol
AppliedFactor                                      appliedFactor
Factor                                          factor
Frac Integer                                     frac integer
Poly Integer [] [x] []                          poly integer
Poly (Frac Integer) [] [x] []                   poly (frac integer)
Frac (Poly Integer [] [x] [])                   frac (poly integer)
Poly (Poly Integer [] [x] []) [] [y] []         poly (poly integer)
Tensor (Poly (Frac Integer) [] [x] [])           tensor (poly (frac integer))
```

マッチャーは係数マッチャーのみを引数に取る。原子集合の3スロットはランタイムのマッチングに影響しないため、マッチャー引数には含めない（型レベルでのみ区別）。

#### 実装方針

`poly`, `frac`, `term` を純粋な Egison のマッチャー定義（`matcher` 式）として実装し、プリミティブは `casToTerms` 等の補助関数のみとする。既存の `PDPlusPat`, `PDFracPat`, `PDTermPat` への変更は最小限で済む。

パターン環境と値環境は分離されているため、`inductive pattern MathExpr` のパターンコンストラクタ `poly`, `div`, `term` と同名のマッチャー関数 `def poly ...` は衝突しない。`frac` はパターンコンストラクタ `div` と名前が異なるため衝突の問題は生じない。

#### Step 5.0: 基本マッチャーの定義

CAS 型に対応する基本マッチャーを定義する。Factor の3分類に対応して `constantFactor`, `symbol`, `appliedFactor`, `factor` の4種類を用意する。

```egison
def integer : Matcher Integer := something

-- symbol: symbolName パターンで名前とインデックスを抽出
def symbol : Matcher Symbol := matcher | symbolName $ $ as ... | ...

-- constantFactor / appliedFactor: apply1, quote パターンで関数と引数を抽出
-- 内部的に同じ構造だが、自由シンボル有無で区別される
def constantFactor : Matcher ConstantFactor := matcher | apply1 $ $ as ... | quote $ as ... | ...
def appliedFactor : Matcher AppliedFactor := matcher | apply1 $ $ as ... | quote $ as ... | ...

-- factor: 3分類の和（symbol, apply1, quote パターンをすべて持つ）
def factor : Matcher Factor := matcher | symbol $ $ as ... | apply1 $ $ as ... | quote $ as ... | ...
```

`constantFactor` と `appliedFactor` は自由シンボルの有無で区別。既存の `PDSymbolPat`, `PDApply1Pat`, `PDQuotePat` に対応するプリミティブ関数を使う。

- [ ] `integer`, `symbol`, `constantFactor`, `appliedFactor`, `factor` マッチャーを `lib/math/expression.egi` に定義
- [ ] `extractSymbolName`, `extractApply1`, `extractQuote` 等のプリミティブ関数追加
- [ ] `freeSymbolsCount`（Factor の自由シンボル有無判定）プリミティブを追加

#### Step 5.1: `poly` パラメトリックマッチャーの実装

`:+` パターンは多項式を**項**と**残りの多項式**に分解する。項は `Term a` 型（係数 + 3スロットモノミアルの組）。

```egison
def poly {a} (m : Matcher a) : Matcher (Poly a .. .. ..) :=
  matcher
    | [] as () with
      | #0 -> [()]
      | _  -> []
    | $ :+ $ as (term m, poly m) with
      | $tgt -> matchAll (casToTerms tgt) as multiset something with
                  | $t :: $rest -> (t, casFromTerms rest)
    | #$val as () with
      | $tgt -> if tgt == val then [()] else []
    | $ as (something) with
      | $tgt -> [tgt]
```

- `:+` の左側は `term m` マッチャーでマッチ（項の構造を分解可能にする）
- `:+` の右側は `poly m` で再帰
- `casToTerms` / `casFromTerms` はプリミティブ関数として提供
- [ ] `poly` マッチャー関数を `lib/math/expression.egi` に定義（`mini-test/62-poly-div-term.egi` にテストあり、未通過）
- [ ] `casToTerms`, `casFromTerms` のプリミティブ関数追加（`mini-test/60-parametric-matcher.egi` にテストあり、未通過）
- [ ] mini-test: `poly integer` での基本的なマッチ

#### Step 5.2: `frac` パラメトリックマッチャーの実装

`Frac a` 型に対応するマッチャー。マッチャー名は `frac` とする（`div` はパターンコンストラクタ名や Haskell の組み込み関数と衝突するため）。

```egison
def frac {a} (m : Matcher a) : Matcher (Frac a) :=
  matcher
    | $ / $ as (m, m) with
      | $tgt -> [(getCASNumerator tgt, getCASDenominator tgt)]
    | #$val as () with
      | $tgt -> if tgt == val then [()] else []
    | $ as (something) with
      | $tgt -> [tgt]
```

- [ ] `frac` マッチャー関数を `lib/math/expression.egi` に定義（`mini-test/62-poly-div-term.egi` にテストあり、未通過）
- [ ] mini-test: `frac (poly integer)` でのマッチ

#### Step 5.3: `term` パラメトリックマッチャーの実装

`term m` の戻り値は係数と3スロットのモノミアルのタプルになる。各スロットは対応する Factor 分類のマッチャーで分解する。

```egison
def term {a} (m : Matcher a) : Matcher (Term a .. .. ..) :=
  matcher
    | ($, ($, $, $)) as (m,
                         assocMultiset constantFactor,
                         assocMultiset symbol,
                         assocMultiset appliedFactor) with
      | $tgt -> [(termCoeff tgt,
                  (termMonoConsts tgt, termMonoSymbols tgt, termMonoComps tgt))]
    | $ as (something) with
      | $tgt -> [tgt]
```

- 項を（係数, (定数Factor のスロット, シンボルのスロット, 合成Factor のスロット)）に分解
- 係数は `m` でマッチ、各モノミアルスロットは対応する `assocMultiset` でマッチ
- `termCoeff` / `termMonoConsts` / `termMonoSymbols` / `termMonoComps` はプリミティブ関数として提供
- [ ] `term` マッチャー関数を `lib/math/expression.egi` に定義
- [ ] `termCoeff`, `termMonoConsts`, `termMonoSymbols`, `termMonoComps` のプリミティブ関数追加
- [ ] mini-test: `term integer` での3スロット分解

#### Step 5.4: `mathExpr` マッチャーとの互換性

既存の `mathExpr` マッチャーは後方互換性のために維持する。

- `mathExpr` を `poly`, `frac`, `term` の合成として再定義
- 既存テスト（`cabal test`）と `mini-test/50-primitive-pattern.egi` が通ることを確認

#### Step 5.5: 型推論との統合

- `Type/Infer.hs` でマッチャー式 `poly m` の型推論
  - `m : Matcher a` のとき `poly m : Matcher (Poly a .. .. ..)`
  - `frac m : Matcher (Frac a)`
  - `term m : Matcher (Term a .. .. ..)`
- マッチャー引数の型からパターン変数の型を推論
  - `match expr as poly integer with | $a :+ _ -> ...` で `a : Term Integer [..] [..] [..]` を推論
  - `match a as term integer with | ($c, ($mc, $ms, $mf)) -> ...` で
    `c : Integer`, `mc : [(ConstantFactor, Integer)]`, `ms : [(Symbol, Integer)]`, `mf : [(AppliedFactor, Integer)]` を推論

#### Step 5.6: テストと検証

- [ ] 基本テスト: `integer`, `symbol`, `constantFactor`, `appliedFactor`, `factor`, `poly integer`, `frac integer`, `term integer`
- [ ] symbol/constantFactor/appliedFactor テスト: 各分類専用マッチャーが対応する Factor のみにマッチすること
- [ ] 入れ子テスト: `poly (poly integer)`, `frac (poly integer)`
- [ ] 複合テスト: `frac (poly (frac integer))`
- [ ] 3スロットモノミアル分解テスト: `term integer` で `($c, ($mc, $ms, $mf))` のパターン
- [ ] 後方互換テスト: 既存の `mathExpr` マッチャーを使うコードが動作すること
- [ ] sample/ の数学サンプルが正しく動作すること

### Phase 5.5: Embed 型クラスと Coercive Subtyping

CAS 型間の自動変換を実現するための型クラスと、型チェッカーへの統合を行う。Phase 5（パラメトリックマッチャー）の完了後、Phase 6（ライブラリ関数の再実装）の前に着手する。

**Phase 6 の前に行う理由**: `∂/∂` を `Differentiable` 型クラスのメソッドとして、`expandAll` の `Frac` への持ち上げを `CASMap` 型クラスで最初から実装するため。型クラスの基盤なしにライブラリ関数を実装すると、Phase 7 で二度手間のリファクタリングが必要になる。

#### 概要

型の包含関係（例: `Integer ⊂ Poly Integer [] [x] []`）がある場合に、型チェッカーが自動的に `embed` 関数の呼び出しを挿入する（elaboration）。これにより、ユーザーは明示的な型変換を書かずに、自然な数式表記で計算できる。

```egison
-- ユーザーが書くコード
x + 1    -- x : Poly Integer [] [x] [], 1 : Integer

-- 型チェッカーが変換後（elaborated）
x + embed 1  -- embed 1 : Poly Integer [] [x] []
```

#### Step 5.5.1: Embed 型クラスの定義

- `Embed` 型クラスを `lib/core/cas.egi` に定義
  ```egison
  class Embed a b where
    embed :: a -> b
  ```
- 基本インスタンスの実装
  - `Embed Integer (Poly Integer [..] [..] [..])`
  - `Embed Integer (Frac Integer)`
  - `Embed ConstantFactor Factor` — 型レベルのみの埋め込み
  - `Embed Symbol Factor` — 型レベルのみの埋め込み
  - `Embed AppliedFactor Factor` — 型レベルのみの埋め込み
  - `Embed ConstantFactor (Poly Integer [..] [] [])` — cs スロットに入る
  - `Embed Symbol (Poly Integer [] [..] [])` — ss スロットに入る
  - `Embed AppliedFactor (Poly Integer [] [] [..])` — fs スロットに入る
  - `Embed Factor (Poly Integer [..] [..] [..])` — 任意のスロットに入りうる
  - `Embed (Poly a [..] [..] [..]) (Poly b [..] [..] [..])` where `Embed a b`
  - `Embed (Poly a [cs₁] [ss₁] [fs₁]) (Poly a [cs₂] [ss₂] [fs₂])` where `cs₁ ⊆ cs₂ ∧ ss₁ ⊆ ss₂ ∧ fs₁ ⊆ fs₂`
  - `Embed (Term a [..] [..] [..]) (Poly a [..] [..] [..])` — `CASTerm` → `CASPoly`（1要素リストで包む）

各 `[..]` はフレッシュ型変数の糖衣構文なので、これらのインスタンスは任意のスロット構成に対して機能する。特化インスタンスは不要（簡約規則は `MathValue` の `casNormalize` で処理されるため）。

#### Step 5.5.2: 型チェッカーでの包含関係グラフの構築

- `Type/Join.hs`（既存）に包含関係グラフの機能を追加
  - `isSubtype`, `symbolSetSubset` は実装済み
  - `Embed` インスタンス宣言時にグラフにエッジを追加
  - 推移閉包の計算（深さ制限付きBFSで探索）

#### Step 5.5.3: 型推論での embed 自動挿入

- `Type/Infer.hs` の `unify` で型不一致検出時に包含関係をチェック
  - 型 `τ₁` と `τ₂` が不一致の場合:
    1. `isSubtype τ₁ τ₂` なら `embed` で `τ₁ → τ₂` に変換
    2. `isSubtype τ₂ τ₁` なら `embed` で `τ₂ → τ₁` に変換
    3. 閉じた Poly 同士でいずれかのスロットが異なる場合は `join` で各スロット独立に和集合を計算し双方を embed
    4. ターゲット型が既知で逆方向（一般 → 具体）なら `coerce` を挿入
    5. いずれでもなければ型エラー
- 推移的な変換（`embed . embed`）の合成
  - 例: `Integer → Poly Integer [] [x] [] → Poly (Frac Integer) [] [x] []`
  - グラフ上の最短経路で `embed` を連鎖
- `Embed` 制約の解決と辞書渡し
  - 型クラス解決機構と連携

#### Step 5.5.3a: coerce の自動挿入

- 型注釈で具体型が指定されており、推論された型が包含関係の逆方向にある場合に `coerce` を挿入
  - 例: `def x : Poly Integer [sqrt 2] [] [] := sqrt 2` で `sqrt 2 : Poly Integer [..] [..] [..]` → `coerce (sqrt 2) : Poly Integer [sqrt 2] [] []`
  - 例: `def n : Integer := sqrt 4` で `sqrt 4 : Poly Integer [..] [..] [..]` → `coerce (sqrt 4) : Integer`
- `Coerce` 型クラスの定義
  ```egison
  class Coerce a b where
    coerce :: a -> b    -- ランタイム検証付き
  ```
- 基本インスタンスの実装（`Math/CAS.hs` にランタイム検証関数）
  - `Coerce (Poly a [..] [..] [..]) (Poly a [cs] [ss] [fs])` — 各スロットの原子集合の検証
  - `Coerce (Poly a [..] [..] [..]) Integer` — 全モノミアルが3スロット空で定数項のみかを検証
  - `Coerce (Poly a [..] [..] [..]) ConstantFactor` — 単一項で `monoConsts` のみが単一原子1乗かを検証
  - `Coerce (Poly a [..] [..] [..]) Symbol` — 単一項で `monoSymbols` のみが単一原子1乗かを検証
  - `Coerce (Poly a [..] [..] [..]) AppliedFactor` — 単一項で `monoComps` のみが単一原子1乗かを検証
  - `Coerce (Frac a) a` — 分母が1かを検証
- ランタイム検証失敗時は `CoerceError` を送出（エラーメッセージに期待される型と実際の値を表示）

#### Step 5.5.4: 二項演算での join と embed の連携

- 二項演算 `a + b` での処理フロー
  1. `a : τ₁`, `b : τ₂` を推論
  2. `joinTypes τ₁ τ₂` で最小上界 `τ` を計算（`Join.hs`）
  3. `τ₁ ≠ τ` なら `embed` で `τ₁ → τ`
  4. `τ₂ ≠ τ` なら `embed` で `τ₂ → τ`
  5. 結果の型は `τ`
- 条件式 `if c then a else b` でも同様の join を実行
- リストリテラル `[a, b, c]` での要素型の join

#### Step 5.5.5: CAS 用の代数的型クラスと MathValue の定義

「代数的型クラス階層」セクションと「微分演算子 `∂/∂`」セクションの設計に基づき、`Ring`, `Field`, `GCDDomain`, `Differentiable` 等の型クラス階層と `MathValue` の `Ring` インスタンスを実装する。内部表現変換関数（`embedIntToPoly` 等）は `Embed` インスタンスのコメントに基づき `Math/CAS.hs` に実装する。

#### Step 5.5.6: 基本テストの作成と検証

- embed の基本テスト（`x + 1` の自動 embed）
- 原子集合拡大テスト（ss スロットの拡大）
- join テスト（異なる ss スロットの和集合）
- 推移的 embed テスト（`Integer → Frac Integer → Poly (Frac Integer) [..] [..] [..]`）
- 3スロット混在テスト
- 型クラス `Ring` の解決テスト

### Phase 6: ライブラリ関数の再実装

Phase 5.5 の基盤（`Embed`, `MathValue`, `Ring MathValue`）が完成した後、既存のライブラリ関数を新しいマッチャー（`poly m`, `frac m`, `term m`）と `MathValue` 上の関数として再実装する。

#### Step 6.1: `expandAll` の再実装

`expandAll : MathValue -> MathValue` を再実装。`CASValue` のコンストラクタをパターンマッチし、`CASPoly` は展開、`CASFrac` は分子・分母に再帰適用。

#### Step 6.2: `substitute` の再実装

`substitute : List (Factor, MathValue) -> MathValue -> MathValue` を再実装。正規化は Phase 7.3 まで `mathNormalize` を使用。

#### Step 6.3: 偏微分演算子 `∂/∂` の再実装

「微分演算子 `∂/∂`」セクションの設計に基づき、`Differentiable` 型クラスの各インスタンス（`Integer`, `ConstantFactor`, `Symbol`, `AppliedFactor`, `Term`, `Poly`, `Frac`, `MathValue`）を実装する。`declare derivative` の仕組みも合わせて実装。テンソル対応は `tensorMap2` の既存ラッパーを維持。

#### Step 6.4: `coefficients` / `coefficient` の再実装

```egison
def coefficients {a} (f : Poly a [..] [..] [..]) (x : Symbol) : [a] :=
  ...  -- poly m マッチャーで各項から x の冪ごとに係数を収集
       -- x : Symbol なので ms スロットだけを見れば良い
```

- `coefficients`, `coefficient` を再実装
- mini-test: `coefficients ((x^2 + 3*x + 1) : Poly Integer [] [x] []) x` → `[1, 3, 1]`

#### Step 6.5: テイラー展開と積分演算子

- `taylorExpansion`, `maclaurinExpansion` の再実装（`∂/∂` と `substitute` に依存）
- `Sd`（不定積分）を `MathValue` 上の関数として再実装
  ```egison
  Sd : Symbol -> MathValue -> MathValue
  -- CASPoly の場合: 各項の冪を1上げて係数を割る
  -- CASFrac の場合: 部分分数分解等
  ```
- これらは `∂/∂` と `substitute` の完成後に着手
- mini-test: `taylorExpansion (sin x) x 0` の最初の数項を検証

#### 依存関係

```
Phase 5 完了（パラメトリックマッチャー）
    ↓
Phase 5.5 完了（Embed, MathValue, Ring MathValue 基盤）
    ↓
Step 6.1 expandAll（MathValue 上の関数として再実装）
Step 6.2 substitute（MathValue 上の関数として再実装、mathNormalize 使用）
    ↓
Step 6.3 ∂/∂（MathValue 上の関数として実装）
Step 6.4 coefficients
    ↓
Step 6.5 taylorExpansion, Sd（∂/∂, substitute に依存、MathValue 上の関数）
    ↓
Phase 7.3 で mathNormalize 廃止時に substitute 等を casNormalizeWithRules に移行
```

### Phase 7: 簡約規則

#### 現状のアーキテクチャ

正規化ロジックが3層に分散している:

```
[Layer 1] casNormalize (CAS.hs)
  - 構造的正規化のみ: 項の結合、ゼロ除去、降冪順ソート、GCD簡約
  - casPlus/casMult/casDivide の内部で自動呼び出し
  - 簡約規則なし

[Layer 2] casRewriteSymbol (Rewrite.hs)
  - Haskell レベルのハードコードルール: i^2=-1, w^3=1, log, exp, sqrt, rt, dd
  - プリミティブ関数 symbolNormalize として公開

[Layer 3] mathNormalize (lib/math/normalize.egi)
  - Egison レベルの書き換え: rtu, sin/cos のまとめ
  - symbolNormalize を呼んだ後に追加の簡約を実行
  - --no-normalize オプションで id に差し替え可能
  - arithmetic.egi の (+), (-), (*) が各演算後に mathNormalize を呼ぶ
```

問題点:

- 簡約規則が Haskell (Layer 2) と Egison (Layer 3) に分散
- ユーザーが新しいシンボルの簡約規則を追加できない
- `mathNormalize` は `--no-normalize` フラグに依存しており、ロード時に切り替わる設計

#### 目標のアーキテクチャ

`mathNormalize` を廃止し、`casNormalize` が構造的正規化と簡約規則の適用を一体で担う:

```
[統合] casNormalize (CAS.hs)
  - 構造的正規化（現在と同じ）
  - 自動簡約規則の適用（declare rule auto で登録されたもの）
  - 手動規則は simplify ... using ... で明示的に適用
```

`declare rule auto` で登録されたルールは `casNormalize` の中で適用される。
これにより:

- `casRewriteSymbol` (Rewrite.hs) のハードコードルール → `declare rule auto` に移行
- `mathNormalize` (normalize.egi) の Egison レベルルール → `declare rule auto` または `declare rule <name>` に移行
- `--no-normalize` オプション → `declare rule` を読み込まないモードとして再実装するか廃止
- `arithmetic.egi` の `plusForMathExpr` 等 → 単に `x +' y` を呼ぶだけに簡略化（`casNormalize` が自動適用するため）

#### 簡約規則のマッチ対象とツリー走査

CASValue はネストする再帰的な構造を持つ。型によって構造が異なる:

```
Frac (Poly Integer [] [x] [])           → CASFrac { num: CASPoly [...], den: CASPoly [...] }
Poly (Frac Integer) [] [x] []            → CASPoly [CASTerm (CASFrac ...) monomial, ...]
Poly (Poly Integer [] [x] []) [] [y] [] → CASPoly [CASTerm (CASPoly [...]) monomial, ...]
```

型によって入れ子の順序が変わるため、ルールを「Termレベル」「Polyレベル」のように固定的な階層で分類することはできない。ルールは LHS のトップレベル構造によって、**何にマッチするか**で分類する:

**和パターン（LHS に `+` を含む）**: CASPoly の項リストに対して multiset マッチ

```egison
declare rule trig_pythagorean (sin $x)^2 + (cos #x)^2 = 1
```

**積パターン（LHS が単一の積/冪）**: CASTerm のモノミアル内に対して multiset マッチ

```egison
declare rule auto i^2 = -1
declare rule auto (sqrt $x)^2 = x
```

**商パターン（LHS に `/` を含む）**: CASFrac の分子・分母に対してマッチ

```egison
-- 分母の有理化: 2 / sqrt(2) => sqrt(2)
declare rule rationalize_sqrt $x / (sqrt $y) = x * sqrt y / y
```

ルールは CASValue ツリーを**再帰的に走査**して、マッチする全ノードに適用する。入れ子構造のどの深さにあってもルールが適用される:

- `Poly (Poly Integer [] [x] []) [] [y] []` では、外側の CASPoly にも内側の CASPoly（係数）にも和パターンルールが適用される
- `Poly (Frac Integer) [] [x] []` では、係数の CASFrac に商パターンルールが適用される
- ユーザーは入れ子の深さを意識する必要がない

既存の `mapCASTerms` / `mapCASPolys` も実質的にツリー走査を行っている（CASFrac の中身にも再帰的に適用）。統合後のツリー走査はこれを一般化したもの。

#### 設計方針

- `casNormalize` に規則環境（`ReductionEnv`）を渡せるようにする
- `declare rule auto` は `casNormalize` 内部で**固定小数点（収束するまで繰り返し）**適用
- `declare rule <name>` は `simplify ... using ...` で**一回だけ**適用
- 既存の `Rewrite.hs` → 最終的に `declare rule auto` で置き換え、`Rewrite.hs` は削除
- ルールのマッチ対象（和/積/商パターン）は LHS の形から自動判定

#### Step 7.1: sin/cos, rtu ルールの `Rewrite.hs` への移植

`normalize.egi` の Egison レベルルールを `Rewrite.hs` に移植し、`mapCASTerms` + `mapCASPolys` の統一的な仕組みで適用する。これにより `mathNormalize` の廃止に向けた準備が整う。

現状:


| ルール                       | 実装場所                     | 適用方法                                     |
| ------------------------- | ------------------------ | ---------------------------------------- |
| `rewriteRuleForSinAndCos` | `normalize.egi` (Egison) | `mapPolys` + Egison matchAll             |
| `rewriteRuleForRtu`       | `normalize.egi` (Egison) | `mapPolys` + Egison matchAll (loop パターン) |
| `casRewriteRtu`           | `Rewrite.hs` (Haskell)   | `mapCASTerms` のみ（積パターン）                  |


`casRewriteRtu` は積パターン（`rtu(n)^k` で `k >= n` なら `k mod n` に縮小）だけで、`normalize.egi` の `rewriteRuleForRtu` は和パターン（`rtu(n)^1 + rtu(n)^2 + ... + rtu(n)^(n-1) = -1`）も行っている。

- `casRewriteSinCos` を `Rewrite.hs` に新規実装
  - `mapCASPolys` で項リストを multiset マッチ
  - ルール1: `a*mr + (-a)*cos(x)^2*mr + pr → a*sin(x)^2*mr + pr`
  - ルール2: `a*cos(x)^2*mr + b*sin(x)^2*mr + pr → a*mr + (b-a)*sin(x)^2*mr + pr`
  - `casRewriteW` の `g` 関数と同じパターンで実装
- `casRewriteRtu` に和パターン部分を追加
  - 現在の積パターン（`mapCASTerms`）に加え、`mapCASPolys` で和パターンを追加
  - `rtu(n)^(n-1)` を `-(1 + rtu(n) + ... + rtu(n)^(n-2))` に展開
- `casRewriteSymbol` のパイプラインに `casRewriteSinCos` を追加
- mini-test: sin/cos, rtu の簡約テスト

備考: `normalize.egi` の `containFunction1` による条件分岐（sin/cos が含まれるときだけルール適用）は、パフォーマンス最適化であり、正しさには影響しない。移植段階では省略してよい。

#### Step 7.2: `casNormalize` への規則環境の導入

`casNormalize` が規則環境を参照できるようにし、既存の `casRewriteSymbol` を `casNormalize` の中に統合する。

- 規則の内部表現を定義 (`Math/CAS.hs` または新規 `Math/Rule.hs`)
  ```haskell
  data ReductionRule = ReductionRule
    { ruleName :: Maybe String  -- auto 規則は Nothing
    , ruleFunc :: CASValue -> CASValue  -- 書き換え関数
    }
  type ReductionEnv = [ReductionRule]
  ```
- `casNormalize` のシグネチャ拡張
  - 現在: `casNormalize :: CASValue -> CASValue`
  - 変更後: `casNormalizeWithRules :: ReductionEnv -> CASValue -> CASValue`
  - `casNormalize` は `casNormalizeWithRules []` のラッパーとして残す（後方互換性）
- 既存 `casRewriteSymbol` を `casNormalizeWithRules` に統合
  - `Rewrite.hs` の各ルールを `ReductionRule` として表現
  - `casNormalizeWithRules` = 構造的正規化 + 規則適用を収束まで繰り返し（ツリー走査）
- `symbolNormalize` プリミティブを `casNormalizeWithRules` 経由に変更
- mini-test: 既存テストが通ることを確認

規則環境はトップレベルの専用環境で管理する。`casPlus`/`casMult` 等の算術関数内部では構造的正規化（`casNormalize`、規則なし）のみを行い、自動規則の適用は算術演算の最終結果に対して `casNormalizeWithRules` で行う。これにより算術関数は規則環境に依存せず、規則適用は明確に分離される。

#### Step 7.3: `mathNormalize` の廃止

Step 7.1 で sin/cos, rtu が `Rewrite.hs` に移植され、Step 7.2 で `casRewriteSymbol` が `casNormalizeWithRules` に統合されたことにより、`mathNormalize` は不要になる。

- `arithmetic.egi` の簡略化（`mathNormalize` 呼び出しを `symbolNormalize` に置換）
- `expression.egi` の `substitute` 関数の修正
- `normalize.egi` / `no-normalize.egi` の削除
- `--no-normalize` オプションの処理変更
- `egison.hs` の mathLib ロード処理の除去
- mini-test / sample の回帰テスト

#### Step 7.4: AST とパーサー — `declare rule` 構文の追加

ユーザーが Egison レベルで簡約規則を定義できるようにする。

- `TopExpr` に `DeclareRule (Maybe String) Expr Expr` コンストラクタを追加
- `declare rule auto <lhs> = <rhs>` / `declare rule <name> <lhs> = <rhs>` のパーサー追加
- mini-test: パースだけのテスト（実行はまだしない）

#### Step 7.5: 自動規則のデシュガーと実行

`declare rule auto` をパースし、`ReductionEnv` に登録し、`casNormalizeWithRules` で適用する。

- LHS パターンの解析（和/積/商パターンの自動判定）
- 書き換え関数の生成（LHS → RHS の `CASValue -> CASValue` 関数を動的に生成）
- `EnvBuilder.hs`: `DeclareRule Nothing` を規則環境に追加
- `EvalState` への規則環境の追加と伝搬
- mini-test: 自動規則の動作テスト
  ```egison
  declare symbol j
  declare rule auto j^2 = -1
  j * j      -- => -1
  (1 + j)^2  -- => 2 * j
  ```

#### Step 7.6: 手動規則と `simplify ... using ...`

名前付きの手動規則を登録し、ユーザーが明示的に適用できるようにする。

- 手動規則の環境登録（`Map String ReductionRule`）
- `simplify <expr> using <rule_name>` のパーサー・AST・評価
- 規則適用エンジン（非線形パターン `#x` サポート）
- mini-test
  ```egison
  declare symbol x
  declare rule trig_pythagorean (sin $x)^2 + (cos #x)^2 = 1
  def expr := (sin x)^2 + (cos x)^2 + 1
  simplify expr using trig_pythagorean  -- => 2
  ```

#### Step 7.7: 既存ハードコードルールの移行と `Rewrite.hs` 削除

`Rewrite.hs` の全ハードコードルールを `declare rule auto` に移行し、`Rewrite.hs` を削除する。

- 単純な積パターンルールの移行（`i^2=-1`, `(sqrt $x)^2=x` 等）
- 関数引数内パターンを含むルールの移行（`log`, `exp`, `power` 等）
- 和パターンルールの移行（`w^2+w+1=0`, sin/cos, rtu 等）
- 回帰テスト: sample/ 以下の数学サンプルの出力が一致すること
- `Rewrite.hs` の削除

#### 依存関係

```
Step 7.1 (sin/cos, rtu を Rewrite.hs に移植)
    ↓
Step 7.2 (casNormalize に規則環境を導入 + casRewriteSymbol 統合)
    ↓
Step 7.3 (mathNormalize 廃止 + normalize.egi 削除)
    ↓
Step 7.4 (declare rule パーサー/AST)
    ↓
Step 7.5 (declare rule auto の実行) ← ユーザー定義規則の最初の動作確認ポイント
    ↓
Step 7.6 (declare rule <name> + simplify)
    ↓
Step 7.7 (Rewrite.hs のハードコードルール移行・削除)
```

Step 7.1 → 7.3 は既存コードのリファクタリング（新構文不要）。
Step 7.4 → 7.5 で初めて `declare rule` がユーザーに見える。
Step 7.6 → 7.7 で全ルールが統一的に管理される。


