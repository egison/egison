# Egison CAS 型システム設計

## 概要

Egisonの数式処理システム(CAS)のための型システムの設計方針をまとめる。
型によって数式の正規形（内部表現）を制御し、HM型推論とtype classを基盤とする。
基本的に依存型は採用しないが、`Poly` の第2引数（シンボル集合）のみ依存型的に扱う。
型推論は開いた型 `[..]` を推論し、閉じた型 `[S]` への絞り込みはユーザが型注釈で指定する。
絞り込み時にはランタイムでシンボル集合の検証を行う（`coerce`）。

---

## 型の構成要素

### 組み込み型


| 型                      | 意味                                      |
| ---------------------- | --------------------------------------- |
| `Integer`              | 基本型。整数                                  |
| `Factor`               | クォート演算子 `'` で生成される原子的な数式要素              |
| `Frac a`                | `a` の分数体/分数環。分母が非モノミアルの場合にのみ必要          |
| `Poly a [s1, s2, ...]` | `a` を係数とするローラン多項式環（閉じた多項式型）。冪指数は負も許可    |
| `Poly a [..]`          | `a` を係数とし、シンボル集合を固定しないローラン多項式型（開いた多項式型） |
| `Term a [s1, s2, ...]` | `Poly a [s1, s2, ...]` の項。係数（型 `a`）とモノミアルの組。パターンマッチの中間結果として主に使われる補助型 |
| `Tensor a`             | `a` を成分とするテンソル                          |


### Poly はローラン多項式環

`Poly a [s]` は標準的な多項式環 `a[s]` ではなく、ローラン多項式環 `a[s, s⁻¹]` を表す。
単項式（Monomial）の冪指数に負の整数を許可することで、`1/r` や `1/r^2` を `Frac` なしで直接表現できる。

```egison
declare symbol r, θ

-- 1/r, 1/r^2 は Poly Integer [r] でそのまま表現可能
r + 1/r + 1/r^2 : Poly Integer [r]

-- 物理の式も Frac なしで書ける
∂/∂ (∂/∂ f r) r + ∂/∂ f r / r + ∂/∂ (∂/∂ f θ) θ / r^2 : Poly Integer [..]
```

`Frac` が必要になるのは、分母が非モノミアルの多項式の場合のみ：

```egison
1 / (r + 1)     : Frac (Poly Integer [r])    -- 分母 r+1 はモノミアルでない
r / (r^2 - 1)   : Frac (Poly Integer [r])    -- 分母 r^2-1 はモノミアルでない
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

`Poly` の第2引数には2つの形式がある。

**閉じた Poly** `Poly a [s1, s2, ...]`: シンボル集合が具体的に固定されている。特定の多項式環で作業するときに使う。演算結果の型が厳密に制御される。

**開いた Poly** `Poly a [..]`: シンボル集合を固定しない。`sqrt 2 + sqrt 3` のように動的にシンボルが現れる探索的な計算に使う。

```egison
declare symbol x, y

-- 閉じた Poly: Z[x, x⁻¹] で厳密に作業
def p : Poly Integer [x] := 1 + x
def q : Poly Integer [x] := 2 + 3 * x
p + q    -- OK: Poly Integer [x]

-- 開いた Poly: 自由にシンボルを導入
def r : Poly Integer [..] := sqrt 2 + sqrt 3    -- OK
def s : Poly Integer [..] := r + x              -- OK
```

#### 開いた Poly のランタイム表現と型具体化

`[..]` はフレッシュな型変数の糖衣構文として扱う。各出現ごとに独立したフレッシュ型変数を生成する。

```egison
-- ユーザーが書く型
polyAdd :: Poly a [..] -> Poly a [..] -> Poly a [..]

-- 型チェッカーが脱糖した内部表現（s1, s2, s3 はフレッシュ型変数）
polyAdd :: Poly a [s1] -> Poly a [s2] -> Poly a [s3]
```

具体化の流れ：

1. 適用時に引数の型からフレッシュ型変数がユニフィケーションで具体化される
2. シンボル集合が異なる場合は、Embed（coercive subtyping）で片方を拡大して揃える
3. ランタイム表現は閉じた `Poly` と同一（`CASPoly [CASTerm]`）で、型レベルのみの区別

```egison
declare symbol x y

polyAdd (x^2 + 1) (y + 3)
-- 第1引数: Poly Integer [x],  第2引数: Poly Integer [y]
-- s1 = [x], s2 = [y] にユニファイ
-- Embed で [x] → [x, y], [y] → [x, y] に拡大
-- 結果: Poly Integer [x, y]
```

#### 型構文の実装

Type ADT に `TPoly Type SymbolSet` を追加し、`SymbolSet` は `Closed [SymbolExpr]` / `Open` の2構成子。パーサーでの `[a]`（コレクション型）との曖昧性解消が必要。

### 型の構成例

```
Integer                              -- Z（整数）
Frac Integer                          -- Q（有理数）
Poly Integer [x]                     -- Z[x, x⁻¹]（整数係数ローラン多項式、閉じた型）
Poly Integer [x, y]                  -- Z[x, x⁻¹, y, y⁻¹]（多変数、閉じた型）
Poly Integer [i]                     -- Z[i]（ガウス整数、i^2 = -1）
Poly Integer ['sqrt 2]               -- Z[√2]
Poly (Frac Integer) [x]               -- Q[x, x⁻¹]（有理数係数ローラン多項式）
Poly (Frac Integer) [i]               -- Q(i)
Frac (Poly Integer [x])               -- Z[x, x⁻¹] の分数体（非モノミアル除算）
Poly (Poly Integer [x]) [y]         -- Z[x, x⁻¹][y, y⁻¹]（y について整理、係数が x のローラン多項式）
Poly Integer [..]                    -- 整数係数、シンボル自由なローラン多項式
Poly (Frac Integer) [..]              -- 有理数係数、シンボル自由なローラン多項式
Tensor (Poly (Frac Integer) [x])      -- 有理数係数ローラン多項式を成分とするテンソル
```

### 入れ子の Poly と多変数の Poly

```egison
Poly (Poly Integer [x]) [y]     -- Z[x, x⁻¹][y, y⁻¹]: y について整理、係数が x のローラン多項式
Poly Integer [x, y]             -- Z[x, x⁻¹, y, y⁻¹]: x と y を対等に扱う
```

数学的に同型だが、正規形が異なる。型システムの設計上、両者は異なる型・異なる内部表現・異なるマッチャー（`poly (poly integer)` vs `poly integer`）として自然に区別される。ユーザーが型注釈で選択する。入れ子の場合の具体的な内部表現は「構成的な内部表現 > 表現の対応」の `Poly (Poly Integer [x]) [y]` の例を参照。

---

## 構成的な内部表現

型構造がそのままランタイムの内部表現を決定する。現在の一枚岩な `ScalarData = Div PolyExpr PolyExpr` とは異なり、型の組み合わせごとに異なる内部表現を持つ。

### 表現の対応

```
Integer               → CASInteger 3
Frac Integer           → CASFrac (CASInteger 2) (CASInteger 3)         -- 2/3
Poly Integer [x]      → CASPoly [CASTerm (CASInteger 1) [(x,1)],
                                  CASTerm (CASInteger 1) []]         -- x + 1
Poly Integer [r]      → CASPoly [CASTerm (CASInteger 1) [(r,1)],
                                  CASTerm (CASInteger 1) [(r,-1)],
                                  CASTerm (CASInteger 1) [(r,-2)]]   -- r + 1/r + 1/r^2
Poly (Frac Integer) [x]→ CASPoly [CASTerm (CASFrac 1 2) [(x,1)]]     -- (1/2)x
Poly (Poly Integer [x]) [y]
                       → CASPoly [CASTerm (CASPoly [CASTerm (CASInteger 1) [(x,1)],
                                                     CASTerm (CASInteger 1) []]) [(y,2)],
                                  CASTerm (CASPoly [CASTerm (CASInteger 2) [(x,1)]]) [(y,1)],
                                  CASTerm (CASPoly [CASTerm (CASInteger 3) []]) []]
                                                    -- (x+1)y^2 + 2x·y + 3
Frac (Poly Integer [x])→ CASFrac (CASPoly ...) (CASPoly ...)          -- 非モノミアル分数
```

### Haskell での表現（概念）

```haskell
data CASValue
  = CASInteger Integer
  | CASFactor SymbolExpr          -- クォート演算子で生成された原子的要素
  | CASPoly [CASTerm]             -- 項のリスト
  | CASFrac CASValue CASValue     -- 分子 / 分母（非モノミアル除算のみ）

data CASTerm = CASTerm CASValue Monomial  -- 係数 × 単項式
-- 係数は CASValue（再帰的）: CASInteger, CASFactor, CASFrac, 別の CASPoly 等

type Monomial = [(SymbolExpr, Integer)]   -- シンボルの冪の積（冪指数は負も可）
```

型の入れ子構造が内部表現の入れ子構造に直接対応する。例えば `Poly (Frac Integer) [x]` の各項の係数は `CASFrac (CASInteger _) (CASInteger _)` になる。

`Monomial` の冪指数が `Integer` であることにより、`r⁻¹` は `[(r, -1)]`、`r⁻²` は `[(r, -2)]` として自然に表現される。

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

`CASValue` → `CASTerm` → `Monomial` → `SymbolExpr` → `CASValue` という相互再帰になるが、Haskell では問題ない。`'sin(x + 1)` のような式で `Apply1` の引数が `CASValue`（多項式 `x + 1`）になるケースを自然に表現できる。

### 演算の構成性

各層の演算は内側の型の演算を使って定義される。

- `Poly a [s]` の `(+)`: 同じ単項式の項をまとめ、係数の加算は `a` の `(+)` で行う
- `Poly a [s]` の `(*)`: 係数の乗算は `a` の `(*)` で行い、単項式を結合する（冪指数の加算、負も可）
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
| `Poly a [S]` | `CASPoly []`（空の項リスト） |
| `Frac a` | 分子が `a` の零 / 分母が `a` の1。例: `Frac Integer` → `CASFrac (CASInteger 0) (CASInteger 1)` |
| `Frac (Poly a [S])` | `CASFrac (CASPoly []) (CASPoly [CASTerm <one> []])` |

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
```

将来的に多項式GCD（ユークリッドの互除法等）へ拡張できるよう、`casGcd` を型クラスまたはパターンマッチで分離し、モノミアルGCDと多項式GCDを差し替え可能にしておく。

---

## シンボルと不定元

### 宣言

```egison
-- 不定元（簡約規則なし。正規形の次数に制限なし）
declare symbol x, y

-- シンボル（簡約規則なし。declare symbol で宣言するだけ）
declare symbol 'sin x, 'cos x, 'sqrt 2
```

`declare symbol` は現在のEgisonに既に存在するキーワードであり、新しいキーワード `indeterminate` を導入せずに統一的に扱える。
シンボルに対する簡約規則は `declare rule` で別途宣言する。

### 違い


|        | 不定元                          | シンボル                                               |
| ------ | ---------------------------- | -------------------------------------------------- |
| 簡約規則   | なし（ただし `declare rule` で付与可能） | `declare rule` で付与可能                               |
| 正規形の次数 | 規則がなければ無制限                   | 規則がなければ無制限                                         |
| 例      | `x^2` はそのまま                  | `i^2` は `declare rule auto i^2 = -1` があれば `-1` に簡約 |


どちらも `Poly a [...]` の第2引数として同じ構文で使用される。

---

## Factor 型とクォート演算子 `'`

### クォート演算子 `'`

`'` は関数適用を停止して Factor 型の値を生成する組み込み演算子。

```egison
'sqrt 2         : Factor     -- √2 を表す原子
'sin x          : Factor     -- sin(x) を表す原子
'sin ('sqrt 2)  : Factor     -- sin(√2) を表す原子
```

### Factor の自動昇格

Factor 型の値が `Poly` の第2引数以外の位置で使われたとき、自動的に多項式型に昇格する。
型注釈が閉じた `Poly` を指定していればそれに従い、指定がなければ開いた `Poly` に昇格する。

```egison
def s := 'sqrt 2                -- s : Factor

-- 開いた Poly への昇格
s + 1                           -- Poly Integer [..]

-- 閉じた Poly への昇格（型注釈あり）
def t : Poly Integer ['sqrt 2] := s + 1    -- Poly Integer ['sqrt 2]
```

`Factor` 型には `Ring` インスタンスを与えない。演算が必要な文脈では、型チェッカーが coercive subtyping（`Embed` 型クラス）により自動的に `Poly` へ `embed` を挿入する。これにより `casPlus` / `casMult` に `CASFactor` のケースは不要で、演算関数の実装がシンプルになる。

### CASFactor コンストラクタ

内部表現には `CASFactor` コンストラクタを持つ。クォート直後の値を自然に表現できる。`Poly` に昇格が必要になった時点で変換する。

```
-- sqrtSym = Apply1 (CASFactor (QuoteFunction sqrtFunc)) (CASInteger 2) :: SymbolExpr
-- ※ sqrtFunc は sqrt 関数の WHNFData

'(sqrt 2) : Factor
  → CASFactor sqrtSym
'(sqrt 2) : Poly Integer [..]
  → CASPoly [CASTerm (CASInteger 1) [(sqrtSym, 1)]]
```

### sqrt 関数の定義例

```egison
-- トップレベルでパターン変数を使った汎用規則を宣言
declare rule auto ('sqrt $x)^2 = x

sqrt : Integer -> Poly Integer [..]
sqrt n =
  if (isPerfectSquare n) then embed (isqrt n)    -- Integer → Poly Integer [..] の embed
  else embed ('sqrt n)                            -- Factor → Poly Integer [..] の embed
```

- `declare rule auto ('sqrt $x)^2 = x` はトップレベル宣言で、任意の `x` に対して `('sqrt x)^2` を `x` に簡約する
- 完全平方数のときは整数を `Poly Integer [..]` に embed して返す
- そうでないとき、Factor `'sqrt n` を生成し `Poly Integer [..]` に embed して返す。簡約規則は既にトップレベルで宣言済み
- `sin`, `cos` などの超越関数も同様にユーザーが定義できる

#### 呼び出し側での型の絞り込み

`sqrt` は `Poly Integer [..]` を返すが、呼び出し側で型注釈を付けることで具体的な型に絞り込める。型チェッカーが `coerce` を自動挿入し、ランタイムで検証される。

```egison
-- 開いた型のまま使う（型注釈不要）
sqrt 2 + sqrt 3         -- : Poly Integer [..]

-- 閉じた型に絞り込む（型注釈で coerce が自動挿入）
def s : Poly Integer ['sqrt 2] := sqrt 2

-- Integer に絞り込む（sqrt 4 = 2 なのでランタイム検証成功）
def n : Integer := sqrt 4

-- Integer に絞り込む（sqrt 2 は整数でないのでランタイムエラー）
-- def m : Integer := sqrt 2    -- エラー: coerce 失敗
```

---

## 型の包含関係

### 基本的な包含

```
Factor  ⊂  Poly Integer [s]
Integer  ⊂  Frac Integer
Integer  ⊂  Poly Integer [s]

Frac Integer  ⊂  Poly (Frac Integer) [s]
Poly Integer [s]  ⊂  Poly (Frac Integer) [s]

Term a [S]  ⊂  Poly a [S]    -- 単一項の多項式として埋め込み
```

**embed の内部表現:**
```
Term a [S] → Poly a [S]:
  CASTerm coeff mono → CASPoly [CASTerm coeff mono]
```

### シンボル集合の包含

```
Poly a [x]  ⊂  Poly a [x, y]     -- [x] ⊆ [x, y]
Poly a [S₁]  ⊂  Poly a [S₂]      -- S₁ ⊆ S₂ のとき
Poly a [S]  ⊂  Poly a [..]        -- 閉じた型 → 開いた型
```

### 包含の伝播規則

```
a ⊂ b  ならば  Poly a [S] ⊂ Poly b [S]     -- 係数の埋め込み
a ⊂ b  ならば  Frac a ⊂ Frac b               -- 分数の埋め込み
a ⊂ b  ならば  Tensor a ⊂ Tensor b          -- テンソルの埋め込み
```

---

## 自動変換の3つの仕組み

### 1. embed（包含関係による自動埋め込み）

型の包含関係がある場合、ターゲット型が文脈から既知のときに自動的に `embed` を挿入する。

```egison
def f (x : Poly Integer [x, y]) : Poly Integer [x, y] := x + x
def p : Poly Integer [x] := 1 + x

f p  ⇝  f (embed p)    -- Poly Integer [x] → Poly Integer [x, y]
```

```egison
def n : Integer := 3
def q : Poly Integer [i] := n + i    -- Integer → Poly Integer [i] に自動 embed
```

#### embed の実装

`Embed` 型クラスと型チェッカーによる AST 変換の組み合わせで実装する。

**`Embed` 型クラス**で変換規則を宣言的に定義する：

```egison
class Embed a b where
  embed :: a -> b

instance Embed Integer (Poly Integer [..]) where
  embed n = ...  -- CASInteger n → CASPoly [CASTerm (CASInteger n) []]

instance Embed Integer (Frac Integer) where
  embed n = ...  -- CASInteger n → CASFrac (CASInteger n) (CASInteger 1)

instance Embed Factor (Poly Integer [..]) where
  embed f = ...  -- CASFactor sym → CASPoly [CASTerm (CASInteger 1) [(sym, 1)]]

instance {Embed a b} Embed (Poly a [..]) (Poly b [..]) where
  embed p = ...  -- 各項の係数を再帰的に embed

-- シンボル集合の拡大（内部表現は変わらない）
instance {S₁ ⊆ S₂} Embed (Poly a [S₁]) (Poly a [S₂]) where
  embed p = p
```

**型チェッカー**が型不一致を検出したとき、包含関係があれば `Embed` 制約を解決し、AST に `embed` 呼び出しを挿入する（elaboration）：

```
-- 型チェック前
x + 1        -- x : Poly Integer [x], 1 : Integer

-- 型チェック後（elaborated）
x + embed 1  -- embed 1 : Poly Integer [x]
```

**推移的な変換**（例: `Integer → Poly Integer [s] → Poly (Frac Integer) [s]`）は、型チェッカーが包含関係の推移閉包を探索し、必要に応じて `embed . embed` を合成する。

#### 各変換の内部表現

```
Integer → Poly Integer [..]:
  CASInteger n → CASPoly [CASTerm (CASInteger n) []]

Integer → Frac Integer:
  CASInteger n → CASFrac (CASInteger n) (CASInteger 1)

Factor → Poly Integer [..]:
  CASFactor sym → CASPoly [CASTerm (CASInteger 1) [(sym, 1)]]

Poly a [..] → Poly b [..]  (Embed a b):
  CASPoly terms → CASPoly (map liftCoeff terms)
  where liftCoeff (CASTerm coeff m) = CASTerm (embed coeff) m

Poly a [S₁] → Poly a [S₂]  (S₁ ⊆ S₂):
  内部表現は変わらない（Monomial のシンボルは S₂ の部分集合として有効）
```

#### 実装方式

Coq の Coercion mechanism に倣い、型チェッカーに coercive subtyping を組み込む：

- `Embed` インスタンスの宣言時に、型チェッカーが包含関係のグラフを構築する
- 型不一致の検出時に、グラフ上の最短経路（深さ制限付きBFS）で推移的な `embed` の合成を探索する
- **Coherence（一貫性）** は、CAS型の包含関係が数学的に明確な半順序であることから自然に保証される（どの経路でも同じ数学的な値に変換される）
- 参考文献: Luo (1999) *Coercive subtyping*, Breazu-Tannen et al. (1991) *Inheritance as implicit coercion*

#### coerce（型の絞り込み）

`embed` は安全な widening（具体 → 一般）だが、逆方向の narrowing（一般 → 具体）が必要な場面がある。典型的には、関数の戻り値が開いた型 `[..]` で推論されるが、ユーザは結果が特定のシンボル集合に属することを知っている場合。

```egison
-- sqrt は開いた型しか返せない
sqrt : Integer -> Poly Integer [..]

-- ユーザは結果の型を知っている
def x : Poly Integer ['sqrt 2] := sqrt 2    -- coerce が自動挿入される
def n : Integer := sqrt 4                   -- coerce が自動挿入される
```

`coerce` は `embed` の逆方向の操作で、ランタイム検証を伴う：

```egison
class Coerce a b where
  coerce :: a -> b    -- ランタイムで型の適合性を検証し、失敗時はエラー
```

**型チェッカーによる自動挿入**: ターゲット型が文脈から既知で、ソース型が包含関係の逆方向にある場合、型チェッカーが `coerce` を自動挿入する。ユーザが明示的に書く必要はなく、型注釈がトリガーとなる。

```egison
-- ユーザが書くコード
def x : Poly Integer ['sqrt 2] := sqrt 2

-- 型チェッカーが変換後（elaborated）
def x : Poly Integer ['sqrt 2] := coerce (sqrt 2)
```

**ランタイム検証の内容**:

| 変換 | 検証内容 |
|------|----------|
| `Poly a [..] → Poly a [S]` | 全項のモノミアルに含まれるシンボルが `S` の部分集合か |
| `Poly a [..] → Integer` | 全項のモノミアルが空（定数項のみ）で、係数が `Integer` に変換可能か |
| `Poly a [..] → Factor` | 単一項で係数が1、モノミアルが単一シンボルの1乗か |
| `Frac a → a` | 分母が1か |

**embed との対称性**:

```
embed  : 具体 → 一般（常に安全、コンパイル時保証、型チェッカーが自動挿入）
coerce : 一般 → 具体（ランタイム検証、失敗時エラー、型注釈による自動挿入）
```

**設計の根拠**: `sqrt` のような関数は、入力値によって戻り値の具体的な型が変わる（`sqrt 4 → Integer`, `sqrt 2 → Poly Integer ['sqrt 2]`）。依存型を導入すれば戻り値の型を精密に表現できるが、型推論の複雑さが大幅に増す。代わりに、関数の型シグネチャは `Poly Integer [..]` のような上限型で統一し、呼び出し側でユーザが型注釈で具体型を指定する方式を採る。これにより型推論は HM の枠内に収まり、narrowing のコストはランタイム検証で支払う。

### 2. join（二項演算時の最小上界の計算）

二項演算 `a + b` で `a : τ₁`, `b : τ₂` のとき、最小上界 `join(τ₁, τ₂)` を求め、双方を embed する。

#### join の計算規則

```
join(a, a) = a

-- 係数の昇格
join(Integer, Frac Integer) = Frac Integer
join(Integer, Poly Integer [S]) = Poly Integer [S]

-- 閉じた Poly 同士: シンボル集合の和集合を自動計算
join(Poly a [S₁], Poly b [S₂]) = Poly (join(a, b)) [S₁ ∪ S₂]

-- 閉じた Poly と他の型
join(Poly a [S], b) = Poly (join(a, b)) [S]
join(a, Poly b [S]) = Poly (join(a, b)) [S]

-- 開いた Poly
join(Poly a [..], Poly b [..]) = Poly (join(a, b)) [..]
join(Poly a [S], Poly b [..]) = Poly (join(a, b)) [..]
join(Poly a [..], Poly b [S]) = Poly (join(a, b)) [..]
join(Poly a [..], b) = Poly (join(a, b)) [..]
join(a, Poly b [..]) = Poly (join(a, b)) [..]

-- Frac
join(Frac a, Frac b) = Frac (join(a, b))
join(a, Frac b) = Frac (join(a, b))
```

> シンボル集合が具体的に定まっている閉じた `Poly` 同士の `join` では、和集合 `S₁ ∪ S₂` を自動計算する。包含関係がある場合（`[x] ⊆ [x, y]`）は和集合が大きい方と一致するだけなので、包含関係の有無に関わらず統一的に扱える。

> `join(Poly a [S], Poly b [..])` では閉じたシンボル集合 `[S]` の情報が失われ、開いた型 `[..]` に吸収される。これは暗黙に許可する。ユーザーが閉じた型で厳密に作業したい場合は、両辺を閉じた型に揃える必要がある。

#### join の具体例

```
-- 包含関係あり → OK
join(Poly Integer [x], Poly Integer [x, y]) = Poly Integer [x, y]
join(Poly Integer [x, y], Poly Integer [x]) = Poly Integer [x, y]

-- 包含関係なし → 和集合で自動合流
join(Poly Integer [x], Poly Integer [y]) = Poly Integer [x, y]

-- 係数の昇格（シンボル集合は同一）
join(Poly Integer [x], Poly (Frac Integer) [x]) = Poly (Frac Integer) [x]

-- 開いた型との合流
join(Poly Integer [x], Poly Integer [..]) = Poly Integer [..]
```

#### 設計の意図

型レベルのシンボル集合操作は以下の3つ。


| 操作        | 用途                 |
| --------- | ------------------ |
| `S₁ = S₂` | 同型の演算判定            |
| `S₁ ⊆ S₂` | embed 判定            |
| `S₁ ∪ S₂` | join 時の結果シンボル集合計算   |

#### 型推論との統合

通常の HM 型推論の単一化を拡張し、等式制約 `τ₁ = τ₂` の代わりに包含制約 `τ₁ ⊂ τ`, `τ₂ ⊂ τ` を生成して最小の `τ` を求める。

```egison
declare symbol x, y

def p : Poly Integer [x] := 1 + x
def q : Poly Integer [x, y] := 1 + x + y

p + q
-- join(Poly Integer [x], Poly Integer [x, y]) = Poly Integer [x, y]
-- （[x] ⊆ [x, y] なので OK）
⇝ (embed p) + q : Poly Integer [x, y]
```

```egison
def p : Poly Integer [x] := 1 + x
def q : Poly Integer [y] := 2 + y

p + q
-- join(Poly Integer [x], Poly Integer [y]) = Poly Integer [x, y]
-- 和集合 [x] ∪ [y] = [x, y] を自動計算し、双方を embed
⇝ (embed p) + (embed q) : Poly Integer [x, y]
-- 結果: x + y + 3
```

### 3. tensorMap（スカラー関数のテンソルへの自動持ち上げ）

スカラー関数がテンソルに適用されたとき、自動的に `tensorMap` を挿入する。

```egison
def f (x : Poly Integer [i]) : Poly Integer [i] := x + x
def t : Tensor Integer := [| 1, 2, 3 |]

f t  ⇝  tensorMap (f . embed) t
-- 結果 : Tensor (Poly Integer [i])
```

#### 3つの仕組みの複合例

```egison
def t : Tensor (Poly Integer [x]) := [| 1 + x, x^2 |]
def q : Poly Integer [x] := 1 + x

t + q
-- tensorMap 挿入: q はスカラー、t はテンソル
-- 成分と q は同じ Poly Integer [x] なのでそのまま加算
⇝ tensorMap (λe -> e + q) t
-- 結果 : Tensor (Poly Integer [x])
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

### インスタンス例

```egison
instance Ring Integer
instance EuclideanDomain Integer where
  divMod := ...
  gcd := ...

-- 汎用: 任意のシンボル集合に対するインスタンス（[..] はフレッシュ型変数に脱糖）
instance {Ring a} Ring (Poly a [..]) where
  (+) := ...
  (*) := ...

-- 特化: 特定のシンボル集合に対するインスタンス（例: i^2 = -1 の簡約）
-- 汎用 [..] より優先される（overlapping instances の優先順位）
instance Ring (Poly Integer [i]) where
  ...

instance {Field a} EuclideanDomain (Poly a [..]) where
  divMod := ...
  gcd := ...

instance {GCDDomain a} GCDDomain (Poly a [..]) where
  gcd := ...

instance {GCDDomain a} Ring (Frac a) where
  (+) (p/q) (r/s) := simplify ((p*s + r*q) / (q*s))
  (*) (p/q) (r/s) := simplify ((p*r) / (q*s))

instance {GCDDomain a} Field (Frac a)
```

### 型クラス制約による安全性

```egison
-- gcd を多項式に使うには係数が体である必要がある
def gcd {EuclideanDomain a} (x: a) (y: a) : a := ...

-- Poly Integer [x] で gcd を使うには Field Integer が必要 → エラー
-- Poly (Frac Integer) [x] で gcd を使うには Field (Frac Integer) ✓ → OK
```

### 微分演算子と Differentiable 型クラス

偏微分演算子 `∂/∂` は `Poly`、`Frac`、`Tensor` など複数の型構造に対して定義する必要があるため、型クラスとして定義する。

```egison
class Differentiable a where
  ∂/∂ : a -> Factor -> a
```

第2引数は `Factor` 型であり、`declare symbol x` で宣言されたシンボルを直接渡す。シンボルは `Factor` として扱われ、`Poly` に昇格する前の段階で微分変数として使われる。呼び出し側では `∂/∂ f x` のように書き、`x` は `Factor` への coercive subtyping（`Embed` 経由の昇格ではなく、シンボル自体が `Factor` 型を持つ）で渡される。

#### インスタンス

```egison
instance {Ring a} Differentiable (Poly a [..]) where
  ∂/∂ p s = ...  -- 各項のモノミアルから該当シンボルの冪を取り出し、冪を1下げて係数に掛ける

instance {Differentiable a, GCDDomain a} Differentiable (Frac a) where
  ∂/∂ (n/d) s = (∂/∂ n s * d - n * ∂/∂ d s) / d^2  -- 商の微分法則
```

#### 利用例

```egison
declare symbol x, y

def f : Poly Integer [x, y] := x^2 * y + 3 * x * y^2

∂/∂ f x   -- => 2 * x * y + 3 * y^2 : Poly Integer [x, y]
∂/∂ f y   -- => x^2 + 6 * x * y     : Poly Integer [x, y]
```

#### 微分演算子の合成

`∂/∂` を組み合わせた高階の微分演算子は、`Differentiable` 制約を使った通常の関数として定義する。演算子ごとに新たな型クラスを用意する必要はない。

```egison
-- ラプラシアン（2次元）
def laplacian2D {Differentiable a} (f : a) : a :=
  ∂/∂ (∂/∂ f x) x + ∂/∂ (∂/∂ f y) y

-- ラプラシアン（極座標）
def laplacianPolar {Differentiable a} (f : a) : a :=
  ∂/∂ (∂/∂ f r) r + ∂/∂ f r / r + ∂/∂ (∂/∂ f θ) θ / r^2
```

### 積分演算子と Integrable 型クラス

不定積分 `Sd` も微分と同様に `Poly` と `Frac` で異なるロジックが必要であるため、型クラスとして定義する。

```egison
class Integrable a where
  Sd : Factor -> a -> a    -- ∫ f dx
```

#### インスタンス

```egison
instance {Ring a} Integrable (Poly a [..]) where
  Sd x p = ...  -- 各項の冪を1上げて係数を割る

instance {Integrable a, GCDDomain a} Integrable (Frac a) where
  Sd x (n/d) = ...  -- 部分分数分解等
```

注: 積分は微分と異なり、常に閉じた形で結果が得られるとは限らない。

#### 利用例

```egison
declare symbol x

def f : Poly Integer [x] := x^2 + 3 * x

Sd x f   -- => (1/3) * x^3 + (3/2) * x^2 : Poly (Frac Integer) [x]
```

### CASMap 型クラス

`substitute`（代入）や `expandAll`（展開）のような操作は、`Poly` レベルの変換を `Frac` の分子・分母に持ち上げるだけで済む。この共通パターンを `CASMap` 型クラスで抽象化する。`Tensor` への持ち上げは `tensorMap` の自動挿入で行われるため、`CASMap` は主に `Frac` のためのものである。

```egison
class CASMap f where
  casMap : (a -> b) -> f a -> f b

instance {GCDDomain b} CASMap Frac where
  casMap f (n/d) = f n / f d
```

> **実装上の注意**: `CASMap` は型変数 `f` が高カインド（`* -> *`）である。Egison の型システムで高カインド多相をサポートしない場合は、`CASMap` を型クラスとして実装する代わりに、`casMapFrac : (a -> b) -> Frac a -> Frac b` のような具体的な関数として定義する。`CASMap` のインスタンスは実質的に `Frac` のみであるため、高カインド多相なしでも機能に影響はない。

`substitute` と `expandAll` は `Poly` 上の関数として定義し、`Frac` への適用は `casMap` で持ち上げる。

```egison
-- Poly 上の関数
substitute : List (Factor, Poly a [..]) -> Poly a [..] -> Poly a [..]
expandAll : Poly a [..] -> Poly a [..]

-- Frac への適用は casMap で持ち上げ
-- casMap (substitute ls) : Frac (Poly a [..]) -> Frac (Poly a [..])
-- casMap expandAll       : Frac (Poly a [..]) -> Frac (Poly a [..])
```

### type class が不要な演算子

以下の演算子は既存の型クラス制約と関数合成で定義でき、新たな型クラスは不要。

| 演算子 | 定義方法 |
|--------|----------|
| `∇` / `grad` / `nabla` | `Differentiable` 制約で `∂/∂` を合成 |
| `div`（発散）, `rot`（回転） | `Differentiable` + `Ring` でテンソル演算と合成 |
| `taylorExpansion` | `Differentiable` 制約で `∂/∂` を繰り返し適用 |
| `wedge` (`∧`), `ι` | `Field` 制約でテンソル上の演算として定義 |
| `dotProduct`, `crossProduct`, `trace` | `Ring` 制約で定義 |
| `M.inverse`, `M.determinant` | `Field` / `Ring` 上のテンソル演算 |

---

## 簡約規則の宣言 (`declare rule`)

簡約規則はシンボル宣言から独立した専用構文 `declare rule` で宣言する。これにより単一シンボルの規則も複数シンボル間の関係式も統一的に扱える。`declare symbol` と `declare rule` は常に分けて定義し、`declare symbol ... with ...` の糖衣構文は採用しない。

```egison
-- シンボル宣言と簡約規則は常に別々に書く
declare symbol i
declare rule auto i^2 = -1
```

### 自動規則と手動規則

```egison
-- 自動規則 (auto): 正規化時に常に適用される。正規形を定義する。
declare rule auto i^2 = -1
declare rule auto ('sqrt $x)^2 = x

-- 手動規則: 名前付きで登録し、ユーザーが明示的に適用する。
-- $x で束縛、#x で同じ値を要求（Egison の非線形パターンマッチ）
declare rule trig_pythagorean ('sin $x)^2 + ('cos #x)^2 = 1
declare rule trig_addition 'sin ($a + $b) = 'sin a * 'cos b + 'cos a * 'sin b
```

- **自動規則 (`auto`)**: `casNormalize` 実行時に常に適用される。`i^2 = -1` のように正規形を一意に定める規則に使う。Maxima の `tellsimp` に相当。
- **手動規則**: 名前付きで規則環境に登録され、`simplify expr using rule_name` で明示的に適用する。`sin(x)^2 + cos(x)^2 = 1` のように、どちらの方向に簡約すべきか自明でない規則に使う。Maxima の `defrule` に相当。

`declare rule` はトップレベル宣言のみを許可する。パターン変数 `$x` を使うことで汎用的な規則をトップレベルで定義できるため、式レベル構文や `IO` モナドは不要。

### 手動規則の適用

```egison
declare rule trig_pythagorean ('sin $x)^2 + ('cos #x)^2 = 1

def expr := ('sin x)^2 + ('cos x)^2 + 1

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

`ruleFunc` は `declare rule auto <lhs> = <rhs>` の LHS パターンと RHS テンプレートから動的に生成される書き換え関数。LHS のパターン変数の束縛と RHS への代入を1つの関数に閉じ込める。

正規化関数は規則環境を参照する:

```haskell
casNormalizeWithRules :: ReductionEnv -> CASValue -> CASValue
```

### 関数引数内のパターン分解

関数引数のパターンマッチを含む規則（例: `'sin ($a + $b) = 'sin a * 'cos b + 'cos a * 'sin b`）では、規則の左辺 `'sin ($a + $b)` が `SymbolExpr` の `Apply1` コンストラクタにマッチし、その引数（`CASValue`）を `poly` マッチャーで分解して `$a + $b` を束縛する。つまり、`declare rule` の規則適用エンジンは、CAS マッチャー（`poly`, `frac` 等）を内部的に利用して関数引数内のパターンを分解する。

パターン変数の型は LHS の構造から推論される。`+` で分解される位置のパターン変数は `Poly a [..]` 型、モノミアル内のパターン変数は `Factor` 型として推論される。例えば `'sin ($a + $b) = ...` では、`$a + $b` が `poly` マッチャーで分解されるため、`$a` と `$b` は `Poly a [..]` 型となる。

---

## CASValue のパターンマッチ

`multiset` が `Matcher a → Matcher [a]` であるのと同様に、CAS の各層をパラメトリックなマッチャーとして設計する。マッチャーの合成により、入れ子の数式構造に対するパターンマッチを統一的に扱える。

### マッチャーの定義

```egison
-- 基本マッチャー（パラメータなし）
integer : Matcher Integer
factor  : Matcher Factor

-- パラメトリックマッチャー（係数マッチャーを受け取る）
poly {a} (m : Matcher a) : Matcher (Poly a ..)
term {a} (m : Matcher a) : Matcher (Term a ..)
frac {a} (m : Matcher a) : Matcher (Frac a)
```

`poly`, `term` のシンボルリストはランタイムのマッチャーには不要（CASValue の内部構造が型ごとに異なるため、マッチャーが区別する必要がない）。型レベルでのみ `Poly a [x]` / `Poly a [x, y]` を区別する。

`factor` は `Factor` 型（クォート演算子 `'` で生成される原子的な数式要素）のマッチャーで、`symbol`, `apply1`, `quote` 等のパターンを持つ。`term m` のモノミアル分解で `assocMultiset factor` として使われる。

### `:+` パターンの意味論

`poly m` の `:+` パターンは、多項式を**項**と**残りの多項式**に分解する。項は `Term a` 型であり、係数（型 `a`）とモノミアル（`[(Factor, Integer)]`）の組である。`multiset m` の `::` と同じ意味論（順序不問の分解）。

```
poly m       の  $a :+ $rest  →  a : Term a [S],      rest : Poly a [S]
multiset m   の  $a :: $rest  →  a : a,               rest : [a]
```

`a : Term a [S]` はそのまま `Poly a [S]` を期待する関数に渡せる（`Term a [S] ⊂ Poly a [S]` の embed による）。

項をさらに分解するには `term m` マッチャーを使う:

```
term m       の  ($c, $mono)  →  c : a,               mono : [(Factor, Integer)]
```

### 利用例

```egison
declare symbol x, y

-- Poly Integer [x] のパターンマッチ
-- :+ は項（Term Integer）と残り（Poly Integer [x]）に分解
match expr as poly integer with
  | $a :+ $rest -> ...   -- a : Term Integer, rest : Poly Integer [x]

-- 項をさらに係数とモノミアルに分解
match expr as poly integer with
  | $a :+ _ ->
    match a as term integer with
      | ($c, $mono) -> ...  -- c : Integer, mono : [(Factor, Integer)]

-- Poly (Poly Integer [x]) [y] のパターンマッチ（入れ子）
-- 各項の係数は Poly Integer [x] 型
match expr as poly (poly integer) with
  | $a :+ $rest ->
    match a as term (poly integer) with
      | ($c, $mono) -> ...  -- c : Poly Integer [x], mono : [(Factor, Integer)]

-- Poly Integer [x, y] のパターンマッチ（多変数）
match expr as poly integer with
  | $a :+ $rest -> ...   -- a : Term Integer, rest : Poly Integer [x, y]

-- Frac (Poly Integer [x]) のパターンマッチ
match expr as frac (poly integer) with
  | $n / $d -> ...       -- n, d : Poly Integer [x]
```

### モノミアルと Factor の分解

`term` マッチャーで取り出したモノミアル（`[(Factor, Integer)]`）は `assocMultiset factor` でさらに分解できる。`factor` マッチャーは `symbol`, `apply1`, `quote` 等のパターンを持ち、シンボルの内部構造を分解する。

```egison
-- モノミアルの各要素を factor マッチャーで分解
match expr as poly integer with
  | $a :+ _ ->
    match a as term integer with
      | ($coeff, $mono) ->
        match mono as assocMultiset factor with
          | (x, $n) :: (y, $m) :: [] -> ...  -- coeff * x^n * y^m

-- factor マッチャーでシンボルの構造を分解
match ('sin x) as factor with
  | apply1 $f $arg -> ...   -- f : sin 関数, arg : x

-- factor マッチャーでシンボル名とインデックスを取得
match x as factor with
  | symbol $name $indices -> ...   -- name : "x", indices : []
```

### 安全なダウンキャスト

型の絞り込みには2つの方法がある。

**1. パターンマッチによる条件付き抽出**（`coerce` 不要、常に安全）:

```egison
-- Frac (Poly Integer [x]) → Poly Integer [x] への安全な抽出
match expr as frac (poly integer) with
  | $n / #1 -> n    -- 分母が1のときだけ安全に取り出す
```

**2. 型注釈による `coerce`**（ランタイム検証付き）:

```egison
-- Poly Integer [..] → Poly Integer ['sqrt 2] への絞り込み
def s : Poly Integer ['sqrt 2] := sqrt 2    -- coerce が自動挿入

-- Poly Integer [..] → Integer への絞り込み
def n : Integer := sqrt 4                   -- coerce が自動挿入（成功）
-- def m : Integer := sqrt 2               -- coerce が自動挿入（ランタイムエラー）
```

パターンマッチはプログラマが条件分岐を書くため常に安全。`coerce` はランタイム検証を伴うが、型注釈だけで簡潔に書ける。用途に応じて使い分ける。

### 設計の利点

- **`multiset` と同じ仕組み**: `poly m` の `:+` は `multiset m` の `::` と同じ意味論（順序不問の分解）
- **`term` による段階的分解**: 項の構造（係数 + モノミアル）を `term m` で明示的に分解。係数マッチャー `m` がここで活きる
- **`factor` でシンボルの内部構造を分解**: `symbol`, `apply1`, `quote` 等のパターンでシンボルの種類に応じた分解が可能
- **合成が統一的**: `poly`, `frac`, `term`, `factor`, `integer`, `multiset`, `assocMultiset` がすべて同じマッチャー合成の仕組みで組み合わさる

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
| シンボル集合の制御 | なし（全シンボル対等）  | 閉じた `[s1, ...]` / 開いた `[..]` を選択可能 |
| テンソルとの統合  | なし           | tensorMap の自動挿入（論文で証明済み）           |
| 新しい数の導入   | ドメイン定義（SPAD） | Factor + クォート演算子 `'`               |


特に tensorMap 挿入と embed 挿入が単一の型推論パスで統一的に処理される点が新しい。

---

## 実装 TODO

### Phase 1: CASValue の基盤実装（完了）

`CASValue` データ型（`CASInteger`, `CASFactor`, `CASPoly`, `CASFrac`, `CASTerm`）を `Math/CAS.hs` に定義し、演算関数（`casPlus`, `casMult`）とローラン多項式の正規化（降冪順、零の除去、モノミアルGCD簡約）を実装した。`SymbolExpr` も `CASValue` を参照するよう移行済み。

### Phase 2: 型システムへの統合（完了）

- `Type` ADT に `TPoly Type SymbolSet`、`TFrac Type`、`TFactor` を追加（`Types.hs`）
- `SymbolSet` の定義（`SymbolSetClosed [String]` / `SymbolSetOpen` / `SymbolSetVar TyVar`）
- パーサーで `Poly Integer [x, y]` / `Poly Integer [..]` / `Frac a` / `Factor` をパース
- 型推論での `Poly` 型の単一化とシンボル集合の包含判定（`S₁ ⊆ S₂`）
- `join` の実装（`Join.hs`: `joinTypes`, `isSubtype`, `symbolSetSubset`）
  - **TODO**: `joinSymbolSets` の `otherwise` 分岐を和集合 `S₁ ∪ S₂` に変更する（現状は `Nothing` を返して型エラーになる）
- 開いた `[..]` のフレッシュ型変数への脱糖（`freshenOpenSymbolSets`）
- `Embed` 型クラスと coercive subtyping は Phase 5.5 として後続実装予定

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

- `Poly (Frac Integer) [x]` と `Poly Integer [x]` で異なるマッチャーを使い分ける
- 入れ子構造（`Poly (Poly Integer [x]) [y]`）の各層に適切なマッチャーを指定する

目標は型とマッチャーの構造を一致させること:

```
型                                   マッチャー
Integer                              integer
Factor                               factor
Frac Integer                          frac integer
Poly Integer [x]                     poly integer
Poly (Frac Integer) [x]              poly (frac integer)
Frac (Poly Integer [x])              frac (poly integer)
Poly (Poly Integer [x]) [y]         poly (poly integer)
Tensor (Poly (Frac Integer) [x])     tensor (poly (frac integer))
```

マッチャーは係数マッチャーのみを引数に取る。シンボルリストはランタイムのマッチングに影響しないため、マッチャー引数には含めない（型レベルでのみ区別）。

#### 実装方針

`poly`, `frac`, `term` を純粋な Egison のマッチャー定義（`matcher` 式）として実装し、プリミティブは `casToTerms` 等の補助関数のみとする。既存の `PDPlusPat`, `PDFracPat`, `PDTermPat` への変更は最小限で済む。

パターン環境と値環境は分離されているため、`inductive pattern MathExpr` のパターンコンストラクタ `poly`, `div`, `term` と同名のマッチャー関数 `def poly ...` は衝突しない。`frac` はパターンコンストラクタ `div` と名前が異なるため衝突の問題は生じない。

#### Step 5.0: 基本マッチャーの定義

CAS 型に対応する基本マッチャーを定義する。

```egison
def integer : Matcher Integer := something

def factor : Matcher Factor :=
  matcher
    | symbol $ $ as (something, list indexExpr) with
      | $tgt -> ...  -- extractSymbol tgt で (name, indices) を抽出
    | apply1 $ $ as (something, factor) with
      | $tgt -> ...  -- extractApply1 tgt で (fn, arg) を抽出
    | apply2 $ $ $ as (something, factor, factor) with
      | $tgt -> ...
    | apply3 $ $ $ $ as (something, factor, factor, factor) with
      | $tgt -> ...
    | apply4 $ $ $ $ $ as (something, factor, factor, factor, factor) with
      | $tgt -> ...
    | quote $ as (something) with
      | $tgt -> ...  -- extractQuote tgt で expr を抽出
    | #$val as () with
      | $tgt -> if tgt == val then [()] else []
    | $ as (something) with
      | $tgt -> [tgt]
```

`integer` は `something` のエイリアス（型チェッカー完成後に制約を追加可能）。`factor` は `SymbolExpr` の構造を分解するマッチャーで、既存の `PDSymbolPat`, `PDApply1Pat`, `PDQuotePat` 等に対応するプリミティブ関数を使う。

- [ ] `integer` マッチャーを `lib/math/expression.egi` に定義（`mini-test/60-parametric-matcher.egi` にテストあり、未通過）
- [ ] `factor` マッチャーを `lib/math/expression.egi` に定義（`mini-test/60-parametric-matcher.egi` にテストあり、未通過）
- [ ] `extractSymbol`, `extractApply1`, `extractQuote` 等のプリミティブ関数追加（既存の `PDSymbolPat` 等の Haskell コードを公開）

#### Step 5.1: `poly` パラメトリックマッチャーの実装

`:+` パターンは多項式を**項**と**残りの多項式**に分解する。項は `Term a` 型（係数 + モノミアルの組）。

```egison
def poly {a} (m : Matcher a) : Matcher (Poly a ..) :=
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

```egison
def term {a} (m : Matcher a) : Matcher (Term a ..) :=
  matcher
    | ($, $) as (m, assocMultiset factor) with
      | $tgt -> [(termCoeff tgt, termMonomial tgt)]
    | $ as (something) with
      | $tgt -> [tgt]
```

- 項を（係数, モノミアル）のペアに分解
- 係数は `m` でマッチ、モノミアルは `assocMultiset factor` でマッチ
- `termCoeff` / `termMonomial` はプリミティブ関数として提供
- [ ] `term` マッチャー関数を `lib/math/expression.egi` に定義（`mini-test/62-poly-div-term.egi` にテストあり、未通過）
- [ ] `termCoeff`, `termMonomial` のプリミティブ関数追加（`mini-test/60-parametric-matcher.egi` にテストあり、未通過）
- [ ] mini-test: `term integer` でのマッチ

#### Step 5.4: `mathExpr` マッチャーとの互換性

既存の `mathExpr` マッチャーは後方互換性のために維持する。

- `mathExpr` を `poly`, `frac`, `term` の合成として再定義
- 既存テスト（`cabal test`）と `mini-test/50-primitive-pattern.egi` が通ることを確認

#### Step 5.5: 型推論との統合

- `Type/Infer.hs` でマッチャー式 `poly m` の型推論
  - `m : Matcher a` のとき `poly m : Matcher (Poly a ..)`
  - `frac m : Matcher (Frac a)`
  - `term m : Matcher (Term a ..)`
- マッチャー引数の型からパターン変数の型を推論
  - `match expr as poly integer with | $a :+ _ -> ...` で `a : Term Integer` を推論
  - `match a as term integer with | ($c, $mono) -> ...` で `c : Integer` を推論

#### Step 5.6: テストと検証

- [ ] 基本テスト: `integer`, `factor`, `poly integer`, `frac integer`, `term integer`（`mini-test/60-parametric-matcher.egi`, `mini-test/62-poly-div-term.egi` にテストあり、未通過）
- [ ] factor テスト: `symbol`, `apply1`, `quote` パターン（`mini-test/60-parametric-matcher.egi` にテストあり、未通過）
- [ ] 入れ子テスト: `poly (poly integer)`, `frac (poly integer)`
- [ ] 複合テスト: `frac (poly (frac integer))`
- [ ] モノミアル分解テスト: `term integer` + `assocMultiset factor`
- [ ] 後方互換テスト: 既存の `mathExpr` マッチャーを使うコードが動作すること
- [ ] sample/ の数学サンプルが正しく動作すること

### Phase 5.5: Embed 型クラスと Coercive Subtyping

CAS 型間の自動変換を実現するための型クラスと、型チェッカーへの統合を行う。Phase 5（パラメトリックマッチャー）の完了後、Phase 6（ライブラリ関数の再実装）の前に着手する。

**Phase 6 の前に行う理由**: `∂/∂` を `Differentiable` 型クラスのメソッドとして、`expandAll` の `Frac` への持ち上げを `CASMap` 型クラスで最初から実装するため。型クラスの基盤なしにライブラリ関数を実装すると、Phase 7 で二度手間のリファクタリングが必要になる。

#### 概要

型の包含関係（例: `Integer ⊂ Poly Integer [x]`）がある場合に、型チェッカーが自動的に `embed` 関数の呼び出しを挿入する（elaboration）。これにより、ユーザーは明示的な型変換を書かずに、自然な数式表記で計算できる。

```egison
-- ユーザーが書くコード
x + 1    -- x : Poly Integer [x], 1 : Integer

-- 型チェッカーが変換後（elaborated）
x + embed 1  -- embed 1 : Poly Integer [x]
```

#### Step 5.5.1: Embed 型クラスの定義

- `Embed` 型クラスを `lib/core/cas.egi` に定義
  ```egison
  class Embed a b where
    embed :: a -> b
  ```
- 基本インスタンスの実装
  - `Embed Integer (Poly Integer [..])`
  - `Embed Integer (Frac Integer)`
  - `Embed Factor (Poly Integer [..])`
  - `Embed (Poly a [..]) (Poly b [..])` where `Embed a b`
  - `Embed (Poly a [S₁]) (Poly a [S₂])` where `S₁ ⊆ S₂`
  - `Embed (Term a [..]) (Poly a [..])` — `CASTerm` → `CASPoly`（1要素リストで包む）

#### `[..]` インスタンスと閉じた Poly の関係

`[..]` はフレッシュ型変数の糖衣構文であるため、`Embed Integer (Poly Integer [..])` は「任意のシンボル集合の `Poly Integer` への embed」として機能する。閉じた `Poly Integer [x]` や `Poly Integer [x, y]` に対して個別のインスタンスを定義する必要はない。

```
-- インスタンス宣言
instance Embed Integer (Poly Integer [..]) where ...
-- 脱糖後の内部表現（s はフレッシュ型変数）
instance Embed Integer (Poly Integer [s]) where ...

-- x + 1 の型チェック（x : Poly Integer [x], 1 : Integer）
-- 1. Embed Integer (Poly Integer [s]) にマッチ
-- 2. s が [x] にユニファイされる
-- 3. 結果: embed 1 : Poly Integer [x]
```

同様に `Embed (Poly a [..]) (Poly b [..])` も、各 `[..]` が独立したフレッシュ型変数に脱糖されるため、任意のシンボル集合の組み合わせに対して機能する。

**インスタンス解決の優先順位**:

特定のシンボル集合に対する特化インスタンス（例: `instance Ring (Poly Integer [i])` で `i^2 = -1`）は、汎用の `[..]` インスタンスより優先される（overlapping instances の標準的な「より具体的な方が優先」規則）。

```
instance {Ring a} Ring (Poly a [..])      -- 汎用（優先度: 低）
instance Ring (Poly Integer [i])          -- 特化（優先度: 高、i^2 = -1 の簡約を含む）

-- Poly Integer [i] に対しては特化インスタンスが選ばれる
-- Poly Integer [x] に対しては汎用インスタンスが選ばれ、[..] が [x] にユニファイ
```

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
    3. 閉じた Poly 同士でシンボル集合が異なる場合は `join` で `S₁ ∪ S₂` を計算し双方を embed
    4. ターゲット型が既知で逆方向（一般 → 具体）なら `coerce` を挿入
    5. いずれでもなければ型エラー
- 推移的な変換（`embed . embed`）の合成
  - 例: `Integer → Poly Integer [x] → Poly (Frac Integer) [x]`
  - グラフ上の最短経路で `embed` を連鎖
- `Embed` 制約の解決と辞書渡し
  - 型クラス解決機構と連携

#### Step 5.5.3a: coerce の自動挿入

- 型注釈で具体型が指定されており、推論された型が包含関係の逆方向にある場合に `coerce` を挿入
  - 例: `def x : Poly Integer ['sqrt 2] := sqrt 2` で `sqrt 2 : Poly Integer [..]` → `coerce (sqrt 2) : Poly Integer ['sqrt 2]`
  - 例: `def n : Integer := sqrt 4` で `sqrt 4 : Poly Integer [..]` → `coerce (sqrt 4) : Integer`
- `Coerce` 型クラスの定義
  ```egison
  class Coerce a b where
    coerce :: a -> b    -- ランタイム検証付き
  ```
- 基本インスタンスの実装（`Math/CAS.hs` にランタイム検証関数）
  - `Coerce (Poly a [..]) (Poly a [S])` — シンボル集合の検証
  - `Coerce (Poly a [..]) Integer` — 定数項のみかを検証
  - `Coerce (Poly a [..]) Factor` — 単一シンボル項かを検証
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

#### Step 5.5.5: 内部表現変換関数の実装

- `Math/CAS.hs` に変換関数を追加
  ```haskell
  -- Integer → Poly Integer [..]
  embedIntToPoly :: CASValue -> CASValue
  embedIntToPoly (CASInteger n) = CASPoly [CASTerm (CASInteger n) []]

  -- Integer → Frac Integer
  embedIntToDiv :: CASValue -> CASValue
  embedIntToDiv (CASInteger n) = CASFrac (CASInteger n) (CASInteger 1)

  -- Factor → Poly Integer [..]
  embedFactorToPoly :: CASValue -> CASValue
  embedFactorToPoly (CASFactor sym) = CASPoly [CASTerm (CASInteger 1) [(sym, 1)]]

  -- Poly a [S₁] → Poly a [S₂] (S₁ ⊆ S₂): 内部表現は変わらない
  -- Poly a [..] → Poly b [..] (Embed a b): 各項の係数を再帰的に embed
  embedPolyCoeff :: (CASValue -> CASValue) -> CASValue -> CASValue
  ```

#### Step 5.5.6: CAS 用の代数的型クラスと演算子型クラスの定義

Phase 6 でライブラリ関数を型クラスのメソッドとして実装するための基盤を定義する。

- **代数的型クラス階層**の実装（`Ring`, `Field`, `GCDDomain` 等）
  ```egison
  class Ring a extends AddGroup a, MulMonoid a
  instance Ring Integer
  instance {Ring a} Ring (Poly a [..])
  instance {GCDDomain a} Ring (Frac a)
  ```
- **`Differentiable` 型クラス**の定義
  ```egison
  class Differentiable a where
    ∂/∂ : a -> Factor -> a
  ```
- **`CASMap` 型クラス**の定義（高カインド多相が必要。代替案は `casMapDiv` 関数として定義）
  ```egison
  class CASMap f where
    casMap : (a -> b) -> f a -> f b
  instance {GCDDomain b} CASMap Frac where
    casMap f (n/d) = f n / f d
  ```
- **`Integrable` 型クラス**の定義
  ```egison
  class Integrable a where
    Sd : Factor -> a -> a
  ```

#### Step 5.5.7: 基本テストの作成と検証

Phase 5.5 の各ステップの動作確認のために、型クラスの基盤がなくてもテストできる基本的な式と、型クラス統合後にテストすべき式を段階的に作成する。

**embed の基本テスト** (`mini-test/60-embed-basic.egi`):
```egison
-- Integer → Poly Integer [x] の自動 embed
declare symbol x
x + 1               -- => x + 1 : Poly Integer [x]（1 が自動 embed）
x * 3               -- => 3 * x : Poly Integer [x]
2 * x + 3           -- => 2 * x + 3 : Poly Integer [x]
```

**embed のシンボル集合拡大テスト** (`mini-test/61-embed-symbolset.egi`):
```egison
declare symbol x, y
def p : Poly Integer [x] := 1 + x
def q : Poly Integer [x, y] := p + y    -- Poly Integer [x] → Poly Integer [x, y] の自動 embed
```

**join のテスト** (`mini-test/62-join.egi`):
```egison
declare symbol x, y
def p : Poly Integer [x] := 1 + x
def q : Poly Integer [y] := 2 + y
-- join(Poly Integer [x], Poly Integer [y]) = Poly Integer [x, y]
p + q               -- => x + y + 3 : Poly Integer [x, y]
```

**推移的 embed のテスト** (`mini-test/63-embed-transitive.egi`):
```egison
declare symbol x
-- Integer → Frac Integer → Poly (Frac Integer) [x]
def r : Poly (Frac Integer) [x] := x + 1    -- Integer 1 が推移的に embed
(1 / 2) * x + 1    -- => (1/2) * x + 1 : Poly (Frac Integer) [x]
```

**型クラスの基本テスト** (`mini-test/64-typeclass-ring.egi`):
```egison
-- Ring のインスタンスが正しく解決されることを確認
declare symbol x
def double {Ring a} (v : a) : a := v + v
double 3             -- => 6 : Integer
double (1 + x)       -- => 2 + 2 * x : Poly Integer [x]
```

**型エラーのテスト** (`mini-test/65-type-error.egi`):
```egison
-- 互換性のない型の演算でエラーが出ることを確認
-- "hello" + 1       -- 型エラー: String と Integer に Embed 関係なし
```

#### 設計上の考慮点

- **Coherence（一貫性）**: CAS型の包含関係は数学的に明確な半順序であり、どの経路で embed しても同じ数学的な値になる
- **パフォーマンス**: embed は実行時に内部表現を変換するため、頻繁な変換はコストがかかる。型推論時に最適な変換経路を選択することで最小化
- **エラーメッセージ**: 包含関係がない場合の型エラーで、利用可能な embed 候補を提示

### Phase 6: ライブラリ関数の再実装

Phase 5.5 の型クラス基盤（`Embed`, `Differentiable`, `CASMap`, `Integrable`）が完成した後、既存のライブラリ関数を新しいマッチャー（`poly m`, `frac m`, `term m`）と型クラスを用いて再実装する。

#### Step 6.1: `expandAll` の再実装

`lib/math/expression.egi` の `expandAll` を `poly m` / `frac m` / `term m` マッチャーで書き直す。

```egison
-- Poly 上の関数として定義
def expandAll (mexpr : Poly a [..]) : Poly a [..] :=
  ...  -- poly m の :+ パターンで項を分解し、各項のモノミアルを展開

-- Frac への適用は CASMap で持ち上げ
-- casMap expandAll : Frac (Poly a [..]) -> Frac (Poly a [..])
```

- `expandAll` を `poly m` マッチャーで再実装
- `expandAll'`（正規化版）も同様に再実装
- `Frac` への適用は `casMap expandAll` で統一的に持ち上げ
- mini-test: `expandAll ((1 + x) * (1 + y))` 等の展開テスト

#### Step 6.2: `substitute` の再実装

`lib/math/expression.egi` の `substitute` を再実装する。

```egison
-- Poly 上の関数として定義
substitute : List (Factor, Poly a [..]) -> Poly a [..] -> Poly a [..]

-- Frac への適用は casMap で持ち上げ
-- casMap (substitute ls) : Frac (Poly a [..]) -> Frac (Poly a [..])
```

- `substitute`, `substitute'`, `rewriteSymbol` を再実装
- `V.substitute`（ベクトル版）も再実装
- 正規化は現時点では `mathNormalize` を呼ぶ（Phase 7.3 で `casNormalizeWithRules` に移行）
- mini-test: `substitute [('x, 1 + y)] (x^2 + x)` 等の代入テスト

#### Step 6.3: 偏微分演算子 `∂/∂` の再実装

`lib/math/analysis/derivative.egi` の `∂/∂` を `Differentiable` 型クラスのメソッドとして再実装する。

```egison
instance {Ring a} Differentiable (Poly a [..]) where
  ∂/∂ p s = matchAll p as poly (something) with
    | ($c, $mono) :+ _ ->
      -- mono 内の s の冪 n を取り出し、n * c * s^(n-1) * (残りのモノミアル) を生成
      ...

instance {Differentiable a, GCDDomain a} Differentiable (Frac a) where
  ∂/∂ (n/d) s = (∂/∂ n s * d - n * ∂/∂ d s) / d^2  -- 商の微分法則
```

- `Poly` インスタンス: `poly m` + `term m` + `assocMultiset factor` で微分を実装
- `Frac` インスタンス: 商の微分法則をそのまま表現
- テンソル対応は `tensorMap2` を使う既存のラッパーを維持
- 連鎖律（`Apply1` 等の合成関数の微分）の再実装
- mini-test: `∂/∂ (x^2 + 3*x) x` → `2*x + 3` 等のテスト

#### Step 6.4: `coefficients` / `coefficient` の再実装

```egison
def coefficients (f : Poly a [..]) (x : Factor) : [a] :=
  ...  -- poly m マッチャーで各項から x の冪ごとに係数を収集
```

- `coefficients`, `coefficient` を再実装
- mini-test: `coefficients (x^2 + 3*x + 1) x` → `[1, 3, 1]`

#### Step 6.5: テイラー展開と積分演算子

- `taylorExpansion`, `maclaurinExpansion` の再実装（`∂/∂` と `substitute` に依存）
- `Sd`（不定積分）を `Integrable` 型クラスのメソッドとして再実装
  ```egison
  instance {Ring a} Integrable (Poly a [..]) where
    Sd x p = ...  -- 各項の冪を1上げて係数を割る
  ```
- これらは `∂/∂` と `substitute` の完成後に着手
- mini-test: `taylorExpansion ('sin x) x 0` の最初の数項を検証

#### 依存関係

```
Phase 5 完了（パラメトリックマッチャー）
    ↓
Phase 5.5 完了（Embed, Differentiable, CASMap, Integrable 型クラス基盤）
    ↓
Step 6.1 expandAll（poly m + CASMap で再実装）
Step 6.2 substitute（poly m + CASMap で再実装、mathNormalize 使用）
    ↓
Step 6.3 ∂/∂（Differentiable 型クラスメソッドとして実装）
Step 6.4 coefficients
    ↓
Step 6.5 taylorExpansion, Sd（∂/∂, substitute, Integrable に依存）
    ↓
Phase 7.3 で mathNormalize 廃止時に substitute 等を casNormalizeWithRules に移行
```

### テスト開始時期のまとめ

| Phase | テスト可能な内容 |
|-------|----------------|
| Phase 1-4（完了） | `cabal test` 全21テストパス。CASValue の基本演算、プリミティブパターンマッチ |
| Phase 5 完了後 | パラメトリックマッチャー（`poly integer`, `frac (poly integer)` 等）のテスト。`mini-test/` でマッチャーの動作確認 |
| Phase 5.5 完了後 | embed の基本テスト（`x + 1`, シンボル集合拡大, join, 推移的 embed）。型クラス `Ring` の基本テスト（`double 3`, `double (1 + x)`）。**ここから型安全な数式計算が動作し始める** |
| Phase 6 完了後 | `expandAll`, `substitute`, `∂/∂`, `coefficients`, `taylorExpansion` のテスト。**ここから `sample/` 以下の数学サンプルの一部が動作し始める** |
| Phase 7.1-7.3 完了後 | `mathNormalize` 廃止後の回帰テスト。`sample/` 以下の全サンプルの動作確認 |
| Phase 7.5 完了後 | `declare rule auto` のユーザー定義規則テスト（例: `j^2 = -1`） |
| Phase 7.7 完了後 | `Rewrite.hs` 削除後の全回帰テスト。**ここで既存テスト + sample が全て通ることを目標** |

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
Frac (Poly Integer [x])      → CASFrac { num: CASPoly [...], den: CASPoly [...] }
Poly (Frac Integer) [x]      → CASPoly [CASTerm (CASFrac ...) monomial, ...]
Poly (Poly Integer [x]) [y] → CASPoly [CASTerm (CASPoly [...]) monomial, ...]
```

型によって入れ子の順序が変わるため、ルールを「Termレベル」「Polyレベル」のように固定的な階層で分類することはできない。ルールは LHS のトップレベル構造によって、**何にマッチするか**で分類する:

**和パターン（LHS に `+` を含む）**: CASPoly の項リストに対して multiset マッチ

```egison
declare rule trig_pythagorean ('sin $x)^2 + ('cos #x)^2 = 1
```

**積パターン（LHS が単一の積/冪）**: CASTerm のモノミアル内に対して multiset マッチ

```egison
declare rule auto i^2 = -1
declare rule auto ('sqrt $x)^2 = x
```

**商パターン（LHS に `/` を含む）**: CASFrac の分子・分母に対してマッチ

```egison
-- 分母の有理化: 2 / sqrt(2) => sqrt(2)
declare rule rationalize_sqrt $x / ('sqrt $y) = x * 'sqrt y / y
```

ルールは CASValue ツリーを**再帰的に走査**して、マッチする全ノードに適用する。入れ子構造のどの深さにあってもルールが適用される:

- `Poly (Poly Integer [x]) [y]` では、外側の CASPoly にも内側の CASPoly（係数）にも和パターンルールが適用される
- `Poly (Frac Integer) [x]` では、係数の CASFrac に商パターンルールが適用される
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
  declare rule trig_pythagorean ('sin $x)^2 + ('cos #x)^2 = 1
  def expr := ('sin x)^2 + ('cos x)^2 + 1
  simplify expr using trig_pythagorean  -- => 2
  ```

#### Step 7.7: 既存ハードコードルールの移行と `Rewrite.hs` 削除

`Rewrite.hs` の全ハードコードルールを `declare rule auto` に移行し、`Rewrite.hs` を削除する。

- 単純な積パターンルールの移行（`i^2=-1`, `('sqrt $x)^2=x` 等）
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


