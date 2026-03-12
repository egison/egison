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

---

## シンボルと不定元

### 宣言

```egison
-- 不定元（簡約規則なし。正規形の次数に制限なし）
declare symbol x, y

-- シンボル（簡約規則あり。正規形の次数が有限になる）
declare symbol i with i^2 = -1
declare symbol √2 with √2^2 = 2
```

`with` 句の有無で不定元とシンボルを区別する。
`declare symbol` は現在のEgisonに既に存在するキーワードであり、新しいキーワード `indeterminate` を導入せずに統一的に扱える。

### 違い

| | 不定元 | シンボル |
|---|---|---|
| 簡約規則 | なし | あり |
| 正規形の次数 | 無制限 | 簡約規則で決まる有限次数 |
| 例 | `x^2` はそのまま | `i^2` は `-1` に簡約 |

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

### sqrt 関数の定義例

```egison
sqrt : Integer -> Factor
sqrt n =
  if (完全平方数 n) then embed (isqrt n)
  else introduce 'sqrt n with ('sqrt n)^2 = n
       return 'sqrt n
```

- 完全平方数のときは整数を返す（Integer ⊂ Factor で embed）
- そうでないとき、新しい Factor `'sqrt n` を生成し、簡約規則 `('sqrt n)^2 = n` を登録する
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

instance {Ring a} Ring (Poly a [s]) where
  (+) := ...
  (*) := ...

instance {Field a} EuclideanDomain (Poly a [s]) where
  divMod := ...
  gcd := ...

instance {GCDDomain a} GCDDomain (Poly a [s]) where
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

## オープンな問題

### CASValue の残設計課題

#### CASFactor コンストラクタ

**方針: CASFactor を入れる。**

```haskell
data CASValue = CASInteger Integer | CASFactor SymbolExpr | CASPoly [CASTerm] | CASDiv CASValue CASValue
```

クォート直後の値を自然に表現できる。`Poly` に昇格が必要になった時点で変換する。

```
-- sqrtSym = Apply1 (CASFactor (QuoteFunction sqrtFunc)) (CASInteger 2) :: SymbolExpr
-- ※ sqrtFunc は sqrt 関数の WHNFData

'(sqrt 2) : Factor
  → CASFactor sqrtSym
'(sqrt 2) : Poly Integer [..]
  → CASPoly [CASTerm (CASInteger 1) [(sqrtSym, 1)]]
```

#### EgisonValue との統合方針

```haskell
-- 現在
data EgisonValue = ... | ScalarData ScalarData | ...
-- Integer は ScalarData (SingleTerm i []) として表現されている

-- 移行後: ScalarData を CASValue に一括置換
data EgisonValue = ... | CASData CASValue | ...
-- Integer は CASData (CASInteger i) に
```

**方針: 一括置換（旧案A）を採用。** 段階的に並存させても最終的に全箇所を書き換える必要があり、並存期間中は `ScalarData` と `CASValue` の両方のコードパスと変換層を維持する負担が増えるだけで、総作業量は減らない。既存コード（`EgisonData Integer` インスタンス、プリミティブ関数、パターンマッチ）への影響は大きいが、一度に移行する。

#### ローラン多項式の正規化

- **項の順序**: 負の冪を含む場合の単項式の標準順序（例: `r² + r + 1 + r⁻¹ + r⁻²` の並び順）
- **零の表現**: `CASInteger 0`、`CASPoly []`、`CASDiv (CASInteger 0) (CASInteger 1)` が各レベルに存在。正規化で統一的に扱う方針が必要
- **GCD 簡約**: `CASDiv` の分子・分母からモノミアルの共通因子を抽出して `CASPoly` の負冪に戻す処理

#### パターンマッチの提供

現在 `PolyExprData`, `TermExprData`, `SymbolExprData` で数式の内部構造にパターンマッチできる。新しい `CASValue` で同等の機能をどう提供するか。構成的な表現になるため、各層（`CASInteger`, `CASPoly`, `CASDiv`）に対するパターンマッチの設計が必要。

#### 既存コードの移行

以下のモジュールが現在の `ScalarData` に密結合しており、`CASValue` への移行が必要。

- `Math/Arith.hs`: 算術演算（`mathPlus`, `mathMult` 等）→ `casPlus`, `casMult` へ
- `Math/Normalize.hs`: 正規化（`mathNormalize'` 等）→ 各層の正規化関数へ
- `Math/Rewrite.hs`: 簡約規則の適用
- `Data.hs`: `EgisonValue` との相互変換、`EgisonData` インスタンス
- `Primitives/Arith.hs`: プリミティブ算術関数

### Poly の入れ子 vs 多変数

```egison
Poly (Poly Integer [x]) [y]     -- Z[x, x⁻¹][y, y⁻¹]: y について整理、係数が x のローラン多項式
Poly Integer [x, y]             -- Z[x, x⁻¹, y, y⁻¹]: x と y を対等に扱う
```

数学的に同型だが、正規形が異なる。
区別するかどうかはグレブナー基底などの実装時に判断する。
入れ子の場合の具体的な内部表現は「構成的な内部表現 > 表現の対応」の `Poly (Poly Integer [x]) [y]` の例を参照。

### Factor 間の関係式

```egison
-- 超越関数間の関係式
declare factor 'sin x, 'cos x
  with ('sin x)^2 + ('cos x)^2 = 1
```

関係式が多項式的でない場合（例: `sin(x+y) = sin(x)cos(y) + cos(x)sin(y)`）の扱いは今後の検討課題。

### coerce の設計

Factor 型からの Integer 等への coerce はユーザーの責任とするが、具体的な構文や安全性の保証については今後検討が必要。

```egison
-- 概念的な例
def x : Factor := 'sqrt 2
def n : Integer := coerce x    -- ユーザーの責任
```

---

## 現在のEgisonとの統合に向けたTODO

現在のEgisonの構文・型システムとこの設計ドキュメントを自然に統合するための作業項目。

| # | 優先度 | 項目 | 状態 | 内容 |
|---|---|---|---|---|
| 1 | 高 | 型クラス制約構文の統一 | **完了** | `(C a) =>` を現在のEgisonの `extends` / `{C a}` 構文に修正済み |
| 2 | 高 | `declare indeterminate` の廃止 | **完了** | `declare symbol` に統合し、`with` 句の有無で不定元とシンボルを区別する形に修正済み |
| 3 | 中 | `Poly a [...]` / `Poly a [..]` の型構文 | 設計済み | 閉じた `[s1, ...]` と開いた `[..]` の2形式。Type ADT に `TPoly Type SymbolSet` を追加し、`SymbolSet` は `Closed [SymbolExpr]` / `Open` の2構成子。パーサーでの `[a]`（コレクション型）との曖昧性解消が必要 |
| 4 | 中 | 構成的な内部表現 (`CASValue`) | 設計中 | 基本構造（`CASInteger`, `CASPoly`, `CASDiv` の再帰的データ型、ローラン多項式）は決定。`EgisonValue` との統合は一括置換方針に決定。`CASFactor` コンストラクタを採用。正規化の詳細、パターンマッチの提供、既存コードの移行方針が未決定。詳細は「オープンな問題 > CASValue の残設計課題」を参照 |
| 5 | 中 | `Num` クラスの分割 | **完了** | `AddSemigroup` → `AddMonoid` → `AddGroup`、`MulSemigroup` → `MulMonoid` → `MulGroup`、`Ring`、`Field`、`GCDDomain`、`EuclideanDomain` の階層を定義・実装済み。前提の基盤強化（複数スーパークラスのパース、制約伝播、0-arity メソッド）も完了。詳細は `type-class.md` を参照 |
| 6 | 中 | `introduce ... with ...` 構文の修正 | 未着手 | `sqrt` 関数の定義例で使われている `introduce ... with ... return ...` はEgisonに存在しない構文。簡約規則の動的登録を宣言的構文で表現するか、`IO` 型に包むかを決める必要がある |
| 7 | 低 | `coerce` の安全性設計 | 未着手 | `Factor` → `Integer` 等のダウンキャストの安全なAPIを設計する。Egisonの強みであるパターンマッチを活用した安全な抽出方法を検討する |
| 8 | 低 | マーカークラスのサポート確認 | **確認済み** | メソッドなしクラス定義は動作する。`where` なしインスタンスは不可だが `where` + 空メソッドで代用可能。詳細は `type-class.md` を参照 |

---

## 実装前に決めるべき未決定事項

### 1. ローラン多項式の正規化規則（優先度: 高）

`casPlus` / `casMult` の実装に直結する。現在の `Math/Normalize.hs` のロジックを確認し、新設計に移植する方針を決める必要がある。

#### 1a. 項の順序

負の冪を含む場合の単項式の標準順序を決める。降冪順（`r², r, 1, r⁻¹, r⁻²`）が自然だが、多変数の場合の辞書式順序の具体的な定義が必要。現在の `ScalarData` での順序を踏襲するか、新たに定義するか。

#### 1b. 零の正規表現

各レベルで `0` をどう正規化するか：

- `CASInteger 0` は `CASInteger` レベルの零
- `CASPoly []` は `CASPoly` レベルの零
- `CASDiv (CASInteger 0) (CASInteger 1)` は `CASDiv` レベルの零

方針の選択肢：
- 各レベルの零をそのまま保持し、比較時に考慮する
- 正規化時に最も内側の表現に統一する（例: `CASPoly []` → `CASPoly []` のまま、`CASDiv` の分子が零なら `CASDiv` ごと除去）

#### 1c. GCD 簡約の詳細

`CASDiv` の分子・分母からモノミアルの共通因子を抽出して `CASPoly` の負冪に戻す処理のアルゴリズム。

### 2. CASFactor の演算の扱い（優先度: 高）

`casPlus` の定義（178行目）に `CASFactor` のケースがない。以下を決める必要がある：

- `CASFactor` 同士や `CASFactor` と他の `CASValue` の演算は型レベルで禁止するか？（`Factor` 型には `Ring` インスタンスを与えず、`Poly` に昇格してからでないと演算できない設計にするか）
- それとも `CASFactor` を自動的に `CASPoly` に昇格してから演算するか？

型レベルで禁止する場合、`casPlus` に `CASFactor` ケースは不要だが、その方針を明示する必要がある。

### 3. embed 関数の実装詳細（優先度: 高）

`embed` が自動挿入されることは記述されているが、各変換の具体的な実装が体系的に列挙されていない。主な変換：

```
Integer → Poly Integer [s]:
  CASInteger n → CASPoly [CASTerm (CASInteger n) []]

Integer → Div Integer:
  CASInteger n → CASDiv (CASInteger n) (CASInteger 1)

Factor → Poly Integer [s]:
  CASFactor sym → CASPoly [CASTerm (CASInteger 1) [(sym, 1)]]

Poly Integer [s] → Poly (Div Integer) [s]:
  CASPoly terms → CASPoly (map liftCoeff terms)
  where liftCoeff (CASTerm (CASInteger n) m) = CASTerm (CASDiv (CASInteger n) (CASInteger 1)) m

Poly a [S₁] → Poly a [S₂]  (S₁ ⊆ S₂):
  内部表現は変わらない（Monomial のシンボルは S₂ の部分集合として有効）
```

### 4. introduce ... with ... 構文の代替設計（優先度: 中）

`sqrt` 関数の定義例で使われている `introduce ... with ... return ...` はEgisonに存在しない構文。簡約規則を動的に登録する仕組みがないと `Factor` 型の動的生成が実装できない。選択肢：

- `IO` モナドに包み、副作用として簡約規則を登録する
- `declare symbol ... with ...` を実行時にも使えるよう拡張する
- 宣言的構文を新設する

### 5. 開いた Poly [..] のランタイム表現と型具体化（優先度: 中）

`CASPoly [CASTerm]` のデータ構造は閉じた `Poly` と同じで問題ないが、以下が未定：

- 型推論時に `Poly a [..]` から `Poly a [x, y]` のような閉じた型に具体化する規則
- 開いた `Poly` の値を閉じた `Poly` の関数に渡す際の型チェック

### 6. Poly a [s] のインスタンス定義における s の扱い（優先度: 中）

```egison
instance {Ring a} Ring (Poly a [s]) where
```

この `[s]` は任意のシンボル集合に対するインスタンスを意味するが、Haskell側の型クラス解決でシンボル集合をどう扱うか（型レベルリストとして？パラメトリックに？）が具体化されていない。

### 7. パターンマッチの提供（優先度: 低）

現在の `PolyExprData` / `TermExprData` / `SymbolExprData` に相当するものを `CASValue` でどう提供するか。算術演算の実装後に対応可能。
