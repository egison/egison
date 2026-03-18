# Egison CAS 型システム設計

## 概要

Egisonの数式処理システム(CAS)のための型システムの設計方針をまとめる。
型によって数式の正規形（内部表現）を制御し、HM型推論とtype classを基盤とする。
基本的に依存型は採用しないが、`Poly` の第2引数（シンボル集合）のみ依存型的に扱う。
ただし自動型推論できる範囲に制限する。

---

## 型の構成要素

### 組み込み型

| 型 | 意味 |
|---|---|
| `Integer` | 基本型。整数 |
| `Factor` | クォート演算子 `'` で生成される原子的な数式要素 |
| `Div a` | `a` の分数体/分数環。分母が非モノミアルの場合にのみ必要 |
| `Poly a [s1, s2, ...]` | `a` を係数とするローラン多項式環（閉じた多項式型）。冪指数は負も許可 |
| `Poly a [..]` | `a` を係数とし、シンボル集合を固定しないローラン多項式型（開いた多項式型） |
| `Tensor a` | `a` を成分とするテンソル |

### Poly はローラン多項式環

`Poly a [s]` は標準的な多項式環 `a[s]` ではなく、ローラン多項式環 `a[s, s⁻¹]` を表す。
単項式（Monomial）の冪指数に負の整数を許可することで、`1/r` や `1/r^2` を `Div` なしで直接表現できる。

```egison
declare symbol r, θ

-- 1/r, 1/r^2 は Poly Integer [r] でそのまま表現可能
r + 1/r + 1/r^2 : Poly Integer [r]

-- 物理の式も Div なしで書ける
∂/∂ (∂/∂ f r) r + ∂/∂ f r / r + ∂/∂ (∂/∂ f θ) θ / r^2 : Poly Integer [..]
```

`Div` が必要になるのは、分母が非モノミアルの多項式の場合のみ：

```egison
1 / (r + 1)     : Div (Poly Integer [r])    -- 分母 r+1 はモノミアルでない
r / (r^2 - 1)   : Div (Poly Integer [r])    -- 分母 r^2-1 はモノミアルでない
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
Div Integer                          -- Q（有理数）
Poly Integer [x]                     -- Z[x, x⁻¹]（整数係数ローラン多項式、閉じた型）
Poly Integer [x, y]                  -- Z[x, x⁻¹, y, y⁻¹]（多変数、閉じた型）
Poly Integer [i]                     -- Z[i]（ガウス整数、i^2 = -1）
Poly Integer ['sqrt 2]               -- Z[√2]
Poly (Div Integer) [x]               -- Q[x, x⁻¹]（有理数係数ローラン多項式）
Poly (Div Integer) [i]               -- Q(i)
Div (Poly Integer [x])               -- Z[x, x⁻¹] の分数体（非モノミアル除算）
Poly (Poly Integer [x]) [y]         -- Z[x, x⁻¹][y, y⁻¹]（y について整理、係数が x のローラン多項式）
Poly Integer [..]                    -- 整数係数、シンボル自由なローラン多項式
Poly (Div Integer) [..]              -- 有理数係数、シンボル自由なローラン多項式
Tensor (Poly (Div Integer) [x])      -- 有理数係数ローラン多項式を成分とするテンソル
```

### 入れ子の Poly と多変数の Poly

```egison
Poly (Poly Integer [x]) [y]     -- Z[x, x⁻¹][y, y⁻¹]: y について整理、係数が x のローラン多項式
Poly Integer [x, y]             -- Z[x, x⁻¹, y, y⁻¹]: x と y を対等に扱う
```

数学的に同型だが、正規形が異なる。型システムの設計上、両者は異なる型・異なる内部表現・異なるマッチャー（`poly (poly integer ['x]) ['y]` vs `poly integer ['x, 'y]`）として自然に区別される。ユーザーが型注釈で選択する。入れ子の場合の具体的な内部表現は「構成的な内部表現 > 表現の対応」の `Poly (Poly Integer [x]) [y]` の例を参照。

---

## 構成的な内部表現

型構造がそのままランタイムの内部表現を決定する。現在の一枚岩な `ScalarData = Div PolyExpr PolyExpr` とは異なり、型の組み合わせごとに異なる内部表現を持つ。

### 表現の対応

```
Integer               → CASInteger 3
Div Integer           → CASDiv (CASInteger 2) (CASInteger 3)         -- 2/3
Poly Integer [x]      → CASPoly [CASTerm (CASInteger 1) [(x,1)],
                                  CASTerm (CASInteger 1) []]         -- x + 1
Poly Integer [r]      → CASPoly [CASTerm (CASInteger 1) [(r,1)],
                                  CASTerm (CASInteger 1) [(r,-1)],
                                  CASTerm (CASInteger 1) [(r,-2)]]   -- r + 1/r + 1/r^2
Poly (Div Integer) [x]→ CASPoly [CASTerm (CASDiv 1 2) [(x,1)]]     -- (1/2)x
Poly (Poly Integer [x]) [y]
                       → CASPoly [CASTerm (CASPoly [CASTerm (CASInteger 1) [(x,1)],
                                                     CASTerm (CASInteger 1) []]) [(y,2)],
                                  CASTerm (CASPoly [CASTerm (CASInteger 2) [(x,1)]]) [(y,1)],
                                  CASTerm (CASPoly [CASTerm (CASInteger 3) []]) []]
                                                    -- (x+1)y^2 + 2x·y + 3
Div (Poly Integer [x])→ CASDiv (CASPoly ...) (CASPoly ...)          -- 非モノミアル分数
```

### Haskell での表現（概念）

```haskell
data CASValue
  = CASInteger Integer
  | CASFactor SymbolExpr          -- クォート演算子で生成された原子的要素
  | CASPoly [CASTerm]             -- 項のリスト
  | CASDiv CASValue CASValue     -- 分子 / 分母（非モノミアル除算のみ）

data CASTerm = CASTerm CASValue Monomial  -- 係数 × 単項式
-- 係数は CASValue（再帰的）: CASInteger, CASFactor, CASDiv, 別の CASPoly 等

type Monomial = [(SymbolExpr, Integer)]   -- シンボルの冪の積（冪指数は負も可）
```

型の入れ子構造が内部表現の入れ子構造に直接対応する。例えば `Poly (Div Integer) [x]` の各項の係数は `CASDiv (CASInteger _) (CASInteger _)` になる。

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
- `Div a` の `(+)`: 通分して `a` の演算で計算し、`a` の `gcd` で簡約する
- `Div a` の `(*)`: 分子同士・分母同士を `a` の `(*)` で掛け、`gcd` で簡約する

演算のディスパッチは、型チェッカーが正しい組み合わせを保証するため、ランタイムでは `CASValue` のコンストラクタによるパターンマッチで再帰的に処理する。

```haskell
casPlus :: CASValue -> CASValue -> CASValue
casPlus (CASInteger a) (CASInteger b) = CASInteger (a + b)
casPlus (CASPoly ts1) (CASPoly ts2) = casNormalizePoly (ts1 ++ ts2)
casPlus (CASDiv n1 d1) (CASDiv n2 d2) =
  casNormalizeDiv (casPlus (casMult n1 d2) (casMult n2 d1))
                  (casMult d1 d2)
```

### 正規化規則

#### 項の順序

降冪順（正の冪 → 定数項 → 負の冪）で現在の `ScalarData` 実装を踏襲する。多変数の場合は辞書式順序。

#### 零の正規表現

正規化時に零を簡約する。具体的な規則：

- `CASPoly` の正規化で零係数の項（`CASTerm` の係数が零）を除去する。結果として項が空になれば `CASPoly []` が零の正規形
- `CASDiv` の正規化で分子が零なら、その型レベルの零に簡約する（例: `CASDiv (CASInteger 0) x` → `CASInteger 0`）
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

| | 不定元 | シンボル |
|---|---|---|
| 簡約規則 | なし（ただし `declare rule` で付与可能） | `declare rule` で付与可能 |
| 正規形の次数 | 規則がなければ無制限 | 規則がなければ無制限 |
| 例 | `x^2` はそのまま | `i^2` は `declare rule auto i^2 = -1` があれば `-1` に簡約 |

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

sqrt : Integer -> Factor
sqrt n =
  if (isPerfectSquare n) then embed (isqrt n)
  else 'sqrt n
```

- `declare rule auto ('sqrt $x)^2 = x` はトップレベル宣言で、任意の `x` に対して `('sqrt x)^2` を `x` に簡約する
- 完全平方数のときは整数を返す（Integer ⊂ Factor で embed）
- そうでないとき、新しい Factor `'sqrt n` を生成する。簡約規則は既にトップレベルで宣言済み
- `sin`, `cos` などの超越関数も同様にユーザーが定義できる

---

## 型の包含関係

### 基本的な包含

```
Integer  ⊂  Factor  ⊂  Poly Integer [s]
Integer  ⊂  Div Integer
Integer  ⊂  Poly Integer [s]

Div Integer  ⊂  Poly (Div Integer) [s]
Poly Integer [s]  ⊂  Poly (Div Integer) [s]
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
a ⊂ b  ならば  Div a ⊂ Div b                -- 分数の埋め込み
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

instance Embed Integer (Div Integer) where
  embed n = ...  -- CASInteger n → CASDiv (CASInteger n) (CASInteger 1)

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

**推移的な変換**（例: `Integer → Poly Integer [s] → Poly (Div Integer) [s]`）は、型チェッカーが包含関係の推移閉包を探索し、必要に応じて `embed . embed` を合成する。

#### 各変換の内部表現

```
Integer → Poly Integer [..]:
  CASInteger n → CASPoly [CASTerm (CASInteger n) []]

Integer → Div Integer:
  CASInteger n → CASDiv (CASInteger n) (CASInteger 1)

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

### 2. join（二項演算時の最小上界の計算）

二項演算 `a + b` で `a : τ₁`, `b : τ₂` のとき、最小上界 `join(τ₁, τ₂)` を求め、双方を embed する。

#### join の計算規則

```
join(a, a) = a

-- 係数の昇格
join(Integer, Div Integer) = Div Integer
join(Integer, Poly Integer [S]) = Poly Integer [S]

-- 閉じた Poly 同士: 包含関係がある場合のみ許可
join(Poly a [S₁], Poly b [S₂]) = Poly (join(a, b)) [S₂]    （S₁ ⊆ S₂ のとき）
join(Poly a [S₁], Poly b [S₂]) = Poly (join(a, b)) [S₁]    （S₂ ⊆ S₁ のとき）
join(Poly a [S₁], Poly b [S₂]) = 型エラー                    （S₁ ⊄ S₂ かつ S₂ ⊄ S₁）

-- 閉じた Poly と他の型
join(Poly a [S], b) = Poly (join(a, b)) [S]
join(a, Poly b [S]) = Poly (join(a, b)) [S]

-- 開いた Poly
join(Poly a [..], Poly b [..]) = Poly (join(a, b)) [..]
join(Poly a [S], Poly b [..]) = Poly (join(a, b)) [..]
join(Poly a [..], Poly b [S]) = Poly (join(a, b)) [..]
join(Poly a [..], b) = Poly (join(a, b)) [..]
join(a, Poly b [..]) = Poly (join(a, b)) [..]

-- Div
join(Div a, Div b) = Div (join(a, b))
join(a, Div b) = Div (join(a, b))
```

#### join の具体例

```
-- 包含関係あり → OK
join(Poly Integer [x], Poly Integer [x, y]) = Poly Integer [x, y]
join(Poly Integer [x, y], Poly Integer [x]) = Poly Integer [x, y]

-- 包含関係なし → 型エラー
join(Poly Integer [x], Poly Integer [y]) = 型エラー
-- ユーザーが明示的に Poly Integer [x, y] に合わせる必要がある

-- 係数の昇格（シンボル集合は同一）
join(Poly Integer [x], Poly (Div Integer) [x]) = Poly (Div Integer) [x]

-- 開いた型との合流
join(Poly Integer [x], Poly Integer [..]) = Poly Integer [..]
```

#### 設計の意図

閉じた `Poly` のシンボル集合に対して、`join` が新しいシンボル集合を自動計算（和集合 `∪`）することはしない。どちらに合わせるべきか一意に決まる（包含関係がある）場合のみ自動で行い、合流先を新しく作る必要がある場合はユーザーに委ねる。

これにより型レベルのシンボル集合操作は以下の2つだけで済む。

| 操作 | 用途 |
|---|---|
| `S₁ = S₂` | 同型の演算判定 |
| `S₁ ⊆ S₂` | embed 判定 / join 判定 |

型レベルの集合和 `S₁ ∪ S₂` は不要。

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
-- join(Poly Integer [x], Poly Integer [y]) = 型エラー
-- ユーザーは以下のように明示的に合わせる必要がある:
def p' : Poly Integer [x, y] := 1 + x
def q' : Poly Integer [x, y] := 2 + y
p' + q' -- OK: Poly Integer [x, y]
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

instance {GCDDomain a} Ring (Div a) where
  (+) (p/q) (r/s) := simplify ((p*s + r*q) / (q*s))
  (*) (p/q) (r/s) := simplify ((p*r) / (q*s))

instance {GCDDomain a} Field (Div a)
```

### 型クラス制約による安全性

```egison
-- gcd を多項式に使うには係数が体である必要がある
def gcd {EuclideanDomain a} (x: a) (y: a) : a := ...

-- Poly Integer [x] で gcd を使うには Field Integer が必要 → エラー
-- Poly (Div Integer) [x] で gcd を使うには Field (Div Integer) ✓ → OK
```

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
  { ruleName    :: Maybe String     -- 手動規則の名前（auto 規則は Nothing）
  , ruleLHS     :: CASValue         -- 左辺のパターン
  , ruleRHS     :: CASValue         -- 右辺
  , ruleSymbols :: [SymbolExpr]     -- 関与するシンボル群
  , ruleAuto    :: Bool             -- True = 自動適用、False = 手動適用
  }

-- 規則環境: シンボルから独立した専用の環境
type ReductionEnv = [ReductionRule]
```

正規化関数は規則環境を参照する:

```haskell
casNormalizePoly :: ReductionEnv -> [CASTerm] -> CASValue
```

### 関数引数内のパターン分解

関数引数のパターンマッチを含む規則（例: `'sin ($a + $b) = 'sin a * 'cos b + 'cos a * 'sin b`）では、規則の左辺 `'sin ($a + $b)` が `SymbolExpr` の `Apply1` コンストラクタにマッチし、その引数（`CASValue`）を `poly` マッチャーで分解して `$a + $b` を束縛する。つまり、`declare rule` の規則適用エンジンは、CAS マッチャー（`poly`, `div` 等）を内部的に利用して関数引数内のパターンを分解する。

---

## CASValue のパターンマッチ

`multiset` が `Matcher a → Matcher [a]` であるのと同様に、CAS の各層をパラメトリックなマッチャーとして設計する。マッチャーの合成により、入れ子の数式構造に対するパターンマッチを統一的に扱える。

### マッチャーの定義

```egison
-- poly: 係数マッチャーとファクターのリストを受け取り、多項式のマッチャーを返す
poly {a} (m : Matcher a) (fs : [Factor]) : Matcher (Poly a fs)

-- div: 被除数のマッチャーを受け取り、分数のマッチャーを返す
div {a} (m : Matcher a) : Matcher (Div a)
```

### 利用例

```egison
-- Poly Integer [x] のパターンマッチ
match expr as poly integer ['x] with
  | $a :+ $rest -> ...   -- a : Integer, rest : Poly Integer [x]

-- Poly (Poly Integer [x]) [y] のパターンマッチ（入れ子）
match expr as poly (poly integer ['x]) ['y] with
  | $a :+ $rest -> ...   -- a : Poly Integer [x]

-- Poly Integer [x, y] のパターンマッチ（多変数）
match expr as poly integer ['x, 'y] with
  | $a :+ $rest -> ...   -- a : Integer

-- Div (Poly Integer [x]) のパターンマッチ
match expr as div (poly integer ['x]) with
  | $n / $d -> ...       -- n, d : Poly Integer [x]
```

### シンボル集合への multiset factor によるマッチ

`poly` の第2引数は `[Factor]` であり、`multiset factor` でマッチできる。これにより、シンボル集合自体にもパターンマッチが使える。

```egison
-- 項の分解時、モノミアル部分も multiset でマッチ
-- モノミアルは [(Factor, Integer)] なので：
match term as termOf (poly integer ['x, 'y]) with
  | ($coeff, $mono) ->
    match mono as multiset (factor, integer) with
      | ('x, $n) :: ('y, $m) :: [] -> ...  -- x^n * y^m
```

### 安全なダウンキャスト

パターンマッチにより、`coerce` を使わずに安全に値を抽出できる。

```egison
-- Div (Poly Integer [x]) → Poly Integer [x] への安全な抽出
match expr as div (poly integer ['x]) with
  | $n / #1 -> n    -- 分母が1のときだけ安全に取り出す
```

### 設計の利点

- **`multiset` と同じ仕組み**: `poly m fs` の `:+` パターンは `multiset m` の `::` パターンと同じ意味論（順序不問の分解）
- **`Factor` は CAS の既存概念**: 新しい型を導入せず、`multiset factor` でシンボル集合をマッチ
- **合成が統一的**: `poly`, `div`, `multiset`, `factor` がすべて同じマッチャー合成の仕組みで組み合わさる

---

## Axiom/FriCAS との比較

Axiom は「ドメインタワー」（例: `Polynomial(Fraction(Integer))`）で同様の正規形制御を実現している。
Egison の設計は以下の点で異なる。

| | Axiom | Egison |
|---|---|---|
| 型システム | 独自（SPAD言語） | HM型推論 + type class |
| 正規形の制御 | ドメインタワー | 型注釈による `Poly`, `Div` の組み合わせ |
| 多項式の表現 | 標準多項式 | ローラン多項式（負の冪を許可） |
| 型変換 | `::` 演算子で明示的 | embed の自動挿入 |
| 内部表現 | ドメインごとに固定 | 型構造から構成的に決定 |
| シンボル集合の制御 | なし（全シンボル対等） | 閉じた `[s1, ...]` / 開いた `[..]` を選択可能 |
| テンソルとの統合 | なし | tensorMap の自動挿入（論文で証明済み） |
| 新しい数の導入 | ドメイン定義（SPAD） | Factor + クォート演算子 `'` |

特に tensorMap 挿入と embed 挿入が単一の型推論パスで統一的に処理される点が新しい。

---

## 実装 TODO

### Phase 1: CASValue の基盤実装（設計確定済み、着手可能）

#### Step 1-2: 新モジュールの作成（既存コードを壊さずに進行）

- [x] `CASValue` データ型を新ファイル `Math/CAS.hs` に定義（`CASInteger`, `CASFactor`, `CASPoly`, `CASDiv`, `CASTerm`）
  - 不正な入れ子構造を防ぐためスマートコンストラクタも用意する
- [x] `casPlus`, `casMult` の実装（`CASInteger`, `CASPoly`, `CASDiv` の3ケース + `CASFactor` のlift分岐）
  - Phase 2 の型チェッカーによる自動 embed が完成するまでは、ランタイムに `CASFactor` が演算に渡される可能性がある。`CASFactor sym` を `CASPoly [CASTerm (CASInteger 1) [(sym, 1)]]` にliftして処理する防御的な分岐を入れる。Phase 2 完成後も害はない
- [x] ローラン多項式の正規化（降冪順、零の除去、モノミアルGCD簡約）
  - 再帰的 `casGcd` は初期実装では `CASInteger` のみ対応し、他の係数型は GCD = 1 にフォールバック
- [x] `Math/CAS.hs` の単体テスト（`casPlus`, `casMult`, `casNormalize` が正しく動作することを確認）

#### Step 3: `ScalarData` → `CASData` の一括置換

- [x] `Data.hs` で `ScalarData` を `CASData CASValue` に置換（ここでコンパイルエラーが大量発生する）
  - まず機械的に `ScalarData (Div p q)` → `CASData (CASDiv (CASPoly ...) (CASPoly ...))` に変換してコンパイルを通す
  - その後、整数リテラル等の明らかな箇所を `CASData (CASInteger n)` に段階的に精密化
  - **実装メモ**: 一括置換ではなく、`CASData` を `ScalarData` と並存させる形で追加。変換関数で橋渡し。

#### Step 4-7: 既存モジュールの移行（Step 3 のコンパイルエラー修正と合わせて進行）

- [x] `Math/Arith.hs` を `CASValue` ベースに書き換え
- [x] `Math/Normalize.hs` の正規化ロジックを `CASValue` に移植
- [x] `Math/Rewrite.hs` のrewrite ruleを `CASValue` に移植
- [x] `Primitives/Arith.hs` のプリミティブ関数を書き換え

#### Step 8: 残りの参照箇所の修正

- [x] その他の `ScalarData` 参照箇所を修正しコンパイルを通す
  - **現状**: `CASData` と `ScalarData` が並存。変換関数で相互運用可能。完全な置換は Phase 2 以降で段階的に実施予定。

#### Step 9: SymbolExpr の CASValue 参照への移行（完了）

- [x] `SymbolExpr` を `CASValue` 参照に移行
  - `hs-src/Language/Egison/Math/CAS.hs` に新しい `SymbolExpr` を定義（`CASValue` を参照）
  - `hs-src/Language/Egison/Math/Expr.hs` には旧 `SymbolExpr`（`ScalarData` を参照）を維持
  - 変換関数を追加:
    - `oldSymbolExprToNew`: 旧 `SymbolExpr` → 新 `SymbolExpr`
    - `newSymbolExprToOld`: 新 `SymbolExpr` → 旧 `SymbolExpr`
    - `oldMonomialToNew`, `newMonomialToOld`: `Monomial` の変換
  - `{-# SOURCE #-}` インポートで `WHNFData` の循環依存を解決
  - 既存コードとの互換性を維持しながら、新しい CAS システムでは `CASValue` ベースの `SymbolExpr` を使用可能

### Phase 2: 型システムへの統合（基盤実装完了）

- [x] `Type` ADT に `TPoly Type SymbolSet`、`TDiv Type`、`TFactor` を追加
  - `hs-src/Language/Egison/Type/Types.hs` に追加
- [x] `SymbolSet` の定義（`SymbolSetClosed [String]` / `SymbolSetOpen` / `SymbolSetVar TyVar`）
  - `hs-src/Language/Egison/Type/Types.hs` に追加
- [x] パーサーで `Poly Integer [x, y]` / `Poly Integer [..]` / `Div a` / `Factor` をパース
  - `hs-src/Language/Egison/Parser/NonS.hs` に `factorTypeExpr`, `divTypeExpr`, `polyTypeExpr`, `symbolSetExpr` を追加
  - `hs-src/Language/Egison/AST.hs` に `TEFactor`, `TEDiv`, `TEPoly`, `SymbolSetExpr` を追加
- [x] 型推論での `Poly` 型の単一化とシンボル集合の包含判定（`S₁ ⊆ S₂`）
  - `hs-src/Language/Egison/Type/Unify.hs` に `TFactor`, `TDiv`, `TPoly` の単一化規則を追加
  - `unifySymbolSets` 関数でシンボル集合の統合を実装
- [x] `join` の実装（二項演算時の最小上界計算）
  - `hs-src/Language/Egison/Type/Join.hs` を新規作成
  - `joinTypes`, `isSubtype`, `symbolSetSubset` 関数を実装
- [ ] `Embed` 型クラスと coercive subtyping（型チェッカーでの `embed` 自動挿入）
  - 基盤は整備済み。型チェッカーへの統合は Phase 2.5 として後続実装予定
- [x] 開いた `[..]` のフレッシュ型変数への脱糖
  - `hs-src/Language/Egison/Type/Infer.hs` に `freshenOpenSymbolSets` 関数を追加
  - `SymbolSetOpen` を `SymbolSetVar` に変換し、各出現ごとにフレッシュな型変数を生成
  - `unifySymbolSets` でサブセットチェックを実装（`[x] ⊆ [x, y]` の判定）

### Phase 2.1: ScalarData の CASValue 置換（機械的移行）

**重要な洞察**: `ScalarData = Div PolyExpr PolyExpr` は `CASDiv (CASPoly [..]) (CASPoly [..])` と1対1対応する。この対応を利用して機械的に置換できる。

```
ScalarData (Div p q)  ≡  CASDiv (CASPoly [CASTerm ...]) (CASPoly [CASTerm ...])
```

この段階では `CASInteger`, `CASFactor` を単体で使わず、常に `CASDiv (CASPoly ...) (CASPoly ...)` 形式を維持する。型の精緻化（`Integer` を `CASInteger` で表現など）は Phase 2.5 以降で行う。

#### Step 1: EgisonValue の置換（完了）

- [x] `Data.hs` の `ScalarData ScalarData` コンストラクタを削除
- [x] `CASData CASValue` コンストラクタのみ残す
- [x] ヘルパー関数 `toScalarVal`, `fromScalarVal` を追加（`CASValue` と `ScalarData` 間の変換）
- [x] `Show`, `Eq`, `isAtomic` インスタンスを `CASData` ベースに更新

#### Step 2: 参照箇所の置換（完了）

以下のファイルを `CASData` / `toScalarVal` / `fromScalarVal` を使用するよう更新:
- [x] `Primitives/Arith.hs` - `CASValue` を直接使用（`casPlus`, `casMinus`, `casMult`, `casDivide`）
- [x] `Primitives/Types.hs` - `fromScalarVal` でパターンマッチ
- [x] `PrettyMath/AST.hs` - `CASData` と `casValueToScalarData` を使用
- [x] `Tensor.hs` - `fromScalarVal` / `toScalarVal` を使用
- [x] `Core.hs` - `fromScalarVal` / `toScalarVal` を使用（パターンマッチ、構築の両方）
- [x] `Primitives.hs` - `fromScalarVal` / `toScalarVal` を使用

#### Step 3: CAS ベース API の公開（完了）

`Math.hs` を更新して CAS ベースの公開 API を提供:
- [x] `casNormalize'` - `mathNormalize'` の CAS 版
- [x] `casRewriteSymbol` - `rewriteSymbol` の CAS 版
- [x] `casPlus`, `casMinus`, `casMult`, `casDivide`, `casPower` をエクスポート
- [x] `Primitives/Arith.hs` を CAS API を使用するよう更新
- [x] `Data.hs` の `EgisonData Rational` インスタンスを CAS API を使用するよう更新

#### Step 4: 変換関数と旧型の削除（進行中）

**現状（2024年）:**
- `CASData` と `ScalarData` が並存
- 外部 API は `CASValue` ベースに移行済み
- `Math/` 内部モジュールは引き続き `ScalarData` を使用

**完了した作業:**
- [x] `Primitives/Arith.hs` - `CASData` パターンで直接マッチング
- [x] `Primitives/Types.hs` - `CASData` パターンで直接マッチング（`isRationalCAS`, `extractRational` ヘルパー追加）
- [x] `Data.hs` の `EgisonData Integer` - `CASData (CASInteger n)` を直接使用
- [x] `Data.hs` の `Eq EgisonValue` - 正規化してから比較
- [x] `casNormalizeDiv` の改善 - ネスト除算 `a/(b/c)`, `(a/b)/c` の処理、負の分母の正規化、`Poly/Integer` の簡約

**残りの作業:**
- [x] `Core.hs` の `ScalarData` パターン使用を CAS ベースに移行（完了）
  - `ScalarData` の参照を全て削除
  - `PolyExprData`/`TermExprData`/`SymbolExprData`/`IndexExprData` を `CASPolyData`/`CASTermData`/`CASSymbolData`/`CASIndexData` に置換
  - `casValueToScalarData` / `scalarDataToCASValue` 変換を排除し、`CASValue` を直接使用
- [x] `Data.hs` から旧中間型 `PolyExprData`/`TermExprData`/`SymbolExprData`/`IndexExprData` を削除
- [ ] `Math/Arith.hs` を `CASValue` ベースに書き換え（21箇所）
- [ ] `Math/Normalize.hs` を `CASValue` ベースに書き換え（8箇所）
- [ ] `Math/Rewrite.hs` を `CASValue` ベースに書き換え（21箇所）
- [ ] `Math/Expr.hs` から `ScalarData`, `PolyExpr`, `TermExpr` を削除
- [ ] 旧 `SymbolExpr`（`Math/Expr.hs`）と変換関数を削除
- [ ] `toScalarVal`, `fromScalarVal` を削除（現在は後方互換性のため維持）

#### Step 5: テスト（完了）

- [x] 既存テストがすべて通過することを確認
- [x] `mini-test/40-casvalue-basic.egi` に CASValue 固有のテストを追加
- [x] `casNormalizePoly` で空多項式を `CASInteger 0` に正規化（等価性比較の問題を修正）
- [x] `cabal test` の全21テストがエラーなしでパス
- [x] ネスト除算 `1/(5/2) = 2/5` の正規化が正しく動作
- [x] 連分数テスト（`regularContinuedFractionOfSqrt`）がすべてパス

### Phase 2.5: Embed 型クラスと Coercive Subtyping

CAS 型間の自動変換を実現するための型クラスと、型チェッカーへの統合を行う。

#### 概要

型の包含関係（例: `Integer ⊂ Poly Integer [x]`）がある場合に、型チェッカーが自動的に `embed` 関数の呼び出しを挿入する（elaboration）。これにより、ユーザーは明示的な型変換を書かずに、自然な数式表記で計算できる。

```egison
-- ユーザーが書くコード
x + 1    -- x : Poly Integer [x], 1 : Integer

-- 型チェッカーが変換後（elaborated）
x + embed 1  -- embed 1 : Poly Integer [x]
```

#### Step 1: Embed 型クラスの定義

- [ ] `Embed` 型クラスを `lib/core/cas.egi` に定義
  ```egison
  class Embed a b where
    embed :: a -> b
  ```
- [ ] 基本インスタンスの実装
  - `Embed Integer (Poly Integer [..])`
  - `Embed Integer (Div Integer)`
  - `Embed Factor (Poly Integer [..])`
  - `Embed (Poly a [..]) (Poly b [..])` where `Embed a b`
  - `Embed (Poly a [S₁]) (Poly a [S₂])` where `S₁ ⊆ S₂`

#### Step 2: 型チェッカーでの包含関係グラフの構築

- [ ] `Type/Subtype.hs` を新規作成
  - 包含関係のグラフ構造を定義
  - `Embed` インスタンス宣言時にグラフにエッジを追加
  - 推移閉包の計算（深さ制限付きBFSで探索）
- [ ] `isSubtype` 関数（`Join.hs` に既存）と連携
  - 包含関係の判定に使用
  - `symbolSetSubset` でシンボル集合の包含も判定

#### Step 3: 型推論での embed 自動挿入

- [ ] `Type/Infer.hs` の `unify` で型不一致検出時に包含関係をチェック
  - 型 `τ₁` と `τ₂` が不一致の場合:
    1. `isSubtype τ₁ τ₂` なら `embed` で `τ₁ → τ₂` に変換
    2. `isSubtype τ₂ τ₁` なら `embed` で `τ₂ → τ₁` に変換
    3. どちらでもなければ型エラー
- [ ] 推移的な変換（`embed . embed`）の合成
  - 例: `Integer → Poly Integer [x] → Poly (Div Integer) [x]`
  - グラフ上の最短経路で `embed` を連鎖
- [ ] `Embed` 制約の解決と辞書渡し
  - 型クラス解決機構と連携

#### Step 4: 二項演算での join と embed の連携

- [ ] 二項演算 `a + b` での処理フロー
  1. `a : τ₁`, `b : τ₂` を推論
  2. `joinTypes τ₁ τ₂` で最小上界 `τ` を計算（`Join.hs`）
  3. `τ₁ ≠ τ` なら `embed` で `τ₁ → τ`
  4. `τ₂ ≠ τ` なら `embed` で `τ₂ → τ`
  5. 結果の型は `τ`
- [ ] 条件式 `if c then a else b` でも同様の join を実行
- [ ] リストリテラル `[a, b, c]` での要素型の join

#### Step 5: 内部表現変換関数の実装

- [ ] `Math/CAS.hs` に変換関数を追加
  ```haskell
  -- Integer → Poly Integer [..]
  embedIntToPoly :: CASValue -> CASValue
  embedIntToPoly (CASInteger n) = CASPoly [CASTerm (CASInteger n) []]

  -- Integer → Div Integer
  embedIntToDiv :: CASValue -> CASValue
  embedIntToDiv (CASInteger n) = CASDiv (CASInteger n) (CASInteger 1)

  -- Factor → Poly Integer [..]
  embedFactorToPoly :: CASValue -> CASValue
  embedFactorToPoly (CASFactor sym) = CASPoly [CASTerm (CASInteger 1) [(sym, 1)]]

  -- Poly a [S₁] → Poly a [S₂] (S₁ ⊆ S₂): 内部表現は変わらない
  -- Poly a [..] → Poly b [..] (Embed a b): 各項の係数を再帰的に embed
  embedPolyCoeff :: (CASValue -> CASValue) -> CASValue -> CASValue
  ```

#### Step 6: テストと検証

- [ ] 自動 embed 挿入のテストケース
  - `x + 1` → `x + embed 1`
  - `f p` where `f : Poly Integer [x, y] -> ...`, `p : Poly Integer [x]`
- [ ] 推移的な embed のテスト
  - `Integer → Div Integer → Poly (Div Integer) [x]`
- [ ] join との連携テスト
  - `(1/2) * x` → `Poly (Div Integer) [x]`
- [ ] エラーケースのテスト
  - 互換性のない型の演算（例: `String + Integer`）で適切なエラーメッセージ

#### 設計上の考慮点

- **Coherence（一貫性）**: CAS型の包含関係は数学的に明確な半順序であり、どの経路で embed しても同じ数学的な値になる
- **パフォーマンス**: embed は実行時に内部表現を変換するため、頻繁な変換はコストがかかる。型推論時に最適な変換経路を選択することで最小化
- **エラーメッセージ**: 包含関係がない場合の型エラーで、利用可能な embed 候補を提示

### Phase 3: パターンマッチ

- [ ] `poly` マッチャーの実装 — `poly {a} (m : Matcher a) (fs : [Factor]) : Matcher (Poly a fs)`。`:+` パターン（順序不問の項分解）、モノミアルの `multiset (factor, integer)` によるマッチを提供
- [ ] `div` マッチャーの実装 — `div {a} (m : Matcher a) : Matcher (Div a)`。`$n / $d` パターンで分子・分母を分解
- [ ] `declare rule` の規則適用エンジンで `poly` / `div` マッチャーを利用し、関数引数内のパターン分解を実装

### Phase 4: 簡約規則（設計確定済み、着手可能）

- [ ] `declare rule` のパーサー実装（`declare rule auto ...` / `declare rule name ...`）
  - パターン変数 `$x` と非線形パターン `#x` のサポート
    - Egisonプログラムに変換したい
- [ ] 規則環境（`ReductionEnv`）の構築（プログラムロード時に収集）
- [ ] `casNormalize` での自動規則の適用
- [ ] `simplify expr using rule_name` の実装

