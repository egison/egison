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
| `Div a` | `a` の分数体/分数環 |
| `Poly a [s1, s2, ...]` | `a` を係数、`s1, s2, ...` (Factor) による多項式環/拡大環（閉じた多項式型） |
| `Poly a [..]` | `a` を係数とし、シンボル集合を固定しない多項式型（開いた多項式型） |
| `Tensor a` | `a` を成分とするテンソル |

### 閉じた Poly と開いた Poly

`Poly` の第2引数には2つの形式がある。

**閉じた Poly** `Poly a [s1, s2, ...]`: シンボル集合が具体的に固定されている。特定の多項式環で作業するときに使う。演算結果の型が厳密に制御される。

**開いた Poly** `Poly a [..]`: シンボル集合を固定しない。`sqrt 2 + sqrt 3` のように動的にシンボルが現れる探索的な計算に使う。

```egison
declare symbol x, y

-- 閉じた Poly: Z[x] で厳密に作業
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
Poly Integer [x]                     -- Z[x]（整数係数多項式、閉じた型）
Poly Integer [x, y]                  -- Z[x,y]（多変数多項式、閉じた型）
Poly Integer [i]                     -- Z[i]（ガウス整数、i^2 = -1）
Poly Integer ['sqrt 2]               -- Z[√2]
Poly (Div Integer) [x]               -- Q[x]（有理数係数多項式）
Poly (Div Integer) [i]               -- Q(i)
Div (Poly Integer [x])               -- Z(x)（有理関数）
Poly Integer [..]                    -- 整数係数、シンボル自由な多項式
Poly (Div Integer) [..]              -- 有理数係数、シンボル自由な多項式
Tensor (Poly (Div Integer) [x])      -- 有理数係数多項式を成分とするテンソル
```

---

## 構成的な内部表現

型構造がそのままランタイムの内部表現を決定する。現在の一枚岩な `ScalarData = Div PolyExpr PolyExpr` とは異なり、型の組み合わせごとに異なる内部表現を持つ。

### 表現の対応

```
Integer               → IntegerVal 3
Div Integer           → DivVal (IntegerVal 2) (IntegerVal 3)         -- 2/3
Poly Integer [x]      → PolyVal [TermVal (IntegerVal 1) [(x,1)],
                                  TermVal (IntegerVal 1) []]         -- x + 1
Poly (Div Integer) [x]→ PolyVal [TermVal (DivVal 1 2) [(x,1)]]     -- (1/2)x
Div (Poly Integer [x])→ DivVal (PolyVal ...) (PolyVal ...)          -- 多項式の分数
```

### Haskell での表現（概念）

```haskell
data CASValue
  = CASInteger Integer
  | CASPoly [CASTerm]             -- 項のリスト
  | CASDiv CASValue CASValue     -- 分子 / 分母

data CASTerm = CASTerm CASValue Monomial  -- 係数 × 単項式
-- 係数は CASValue（再帰的）: IntegerVal, DivVal, 別の PolyVal 等

type Monomial = [(SymbolExpr, Integer)]   -- シンボルの冪の積
```

型の入れ子構造が内部表現の入れ子構造に直接対応する。例えば `Poly (Div Integer) [x]` の各項の係数は `CASDiv (CASInteger _) (CASInteger _)` になる。

### 演算の構成性

各層の演算は内側の型の演算を使って定義される。

- `Poly a [s]` の `(+)`: 同じ単項式の項をまとめ、係数の加算は `a` の `(+)` で行う
- `Poly a [s]` の `(*)`: 係数の乗算は `a` の `(*)` で行い、単項式を結合する
- `Div a` の `(+)`: 通分して `a` の演算で計算し、`a` の `gcd` で簡約する
- `Div a` の `(*)`: 分子同士・分母同士を `a` の `(*)` で掛け、`gcd` で簡約する

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
| 型変換 | `::` 演算子で明示的 | embed の自動挿入 |
| 内部表現 | ドメインごとに固定 | 型構造から構成的に決定 |
| シンボル集合の制御 | なし（全シンボル対等） | 閉じた `[s1, ...]` / 開いた `[..]` を選択可能 |
| テンソルとの統合 | なし | tensorMap の自動挿入（論文で証明済み） |
| 新しい数の導入 | ドメイン定義（SPAD） | Factor + クォート演算子 `'` |

特に tensorMap 挿入と embed 挿入が単一の型推論パスで統一的に処理される点が新しい。

---

## オープンな問題

### Poly の入れ子 vs 多変数

```egison
Poly (Poly Integer [x]) [y]     -- Z[x][y]: y について整理、係数が x の多項式
Poly Integer [x, y]             -- Z[x,y]: x と y を対等に扱う
```

数学的に同型だが、正規形が異なる。
区別するかどうかはグレブナー基底などの実装時に判断する。

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
| 4 | 中 | 構成的な内部表現の実装 | 設計済み | 現在の `ScalarData = Div PolyExpr PolyExpr` に代わる再帰的な `CASValue` データ型の実装。型の組み合わせごとに異なる内部表現を持たせる |
| 5 | 中 | `Num` クラスの分割 | 設計中 | 詳細は `type-class.md` を参照。前提として型クラス基盤の強化（複数スーパークラスのパース、スーパークラス制約伝播）が必要 |
| 6 | 中 | `introduce ... with ...` 構文の修正 | 未着手 | `sqrt` 関数の定義例で使われている `introduce ... with ... return ...` はEgisonに存在しない構文。簡約規則の動的登録を宣言的構文で表現するか、`IO` 型に包むかを決める必要がある |
| 7 | 低 | `coerce` の安全性設計 | 未着手 | `Factor` → `Integer` 等のダウンキャストの安全なAPIを設計する。Egisonの強みであるパターンマッチを活用した安全な抽出方法を検討する |
| 8 | 低 | マーカークラスのサポート確認 | **確認済み** | メソッドなしクラス定義は動作する。`where` なしインスタンスは不可だが `where` + 空メソッドで代用可能。詳細は `type-class.md` を参照 |
