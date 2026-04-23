# Egison CAS 型システム設計

## 概要

Egisonの数式処理システム(CAS)のための型システムの設計方針をまとめる。

### 基本原則

**`MathValue` が第一級の計算型**である。`1`, `1 + i`, `x + x^2`, `x + sin x` はすべて `MathValue` として自由に計算できる。演算（`+`, `*`, `∂/∂` 等）も簡約規則（`i^2 = -1` 等）もすべて `MathValue` 上で動作する。

**`Poly a [atoms]` 等は `MathValue` の subtype** であり、「この式はこの代数構造に属する」という型注釈 + 「その構造の正規形で表示・保持せよ」という指示として機能する。内部表現は `MathValue` と同じ `CASValue` で、subtype への `coerce` 時に所属検証と正規化を行う。

```egison
-- すべて MathValue として自由に計算
1 + i                    -- : MathValue
x + x^2                  -- : MathValue
x + sin x                -- : MathValue
∂/∂ (sin x) x           -- : MathValue (= cos x)

-- 型注釈で subtype に絞り込む → 正規化される
type GaussianInt := Poly Integer [i]
def a : GaussianInt := 1 + i                           -- coerce + 正規化
def p : Poly (Frac Integer) [x] := (x + x^2) / 2       -- (1/2)x + (1/2)x^2 に正規化
```

**観察型 (observed type) の導入**: 静的型は最小限に保ち、評価後に得られる `CASValue` から最も具体的な型を逆算して報告する。これをこの設計書では **観察型 (observed type)** と呼ぶ。インタラクティブな数式計算で、ユーザーが計算を始める前に型を決めない workflow を一級サポートするための機構である。Success typing (Lindahl & Sagonas, 2006) の系列に位置づけられる。詳細は「観察型」セクション参照。

### 型システムの基盤

HM型推論とtype classを基盤とする。基本的に依存型は採用しないが、`Poly` の原子集合スロットのみ依存型的に扱う。

差分閉性(微分演算子 `∂/∂` で型が閉じる性質)は **実行後に観察型として報告される**(型レベルでは静的保証しない)。静的型システムは原子集合の追跡のみに簡素化し、微分の型保存は観察型機構に委ねる。

### 設計のレイヤー

| レイヤー | 責務 |
|---|---|
| **`MathValue` + `declare rule auto`** | 演算 + 簡約規則の適用（`i^2 = -1` 等すべて） |
| **subtype (`Poly a [atoms]`, `Frac a` 等)** | 所属検証 + 正規形の保証。subtype 上の `Ring` は `MathValue` の演算をラップ + coerce |
| **`Differentiable` 型クラス** | 微分演算の提供 |
| **観察型機構** | `typeOf :: CASValue → Type` による評価後の具体型報告 |

### 宣言環境 (Declaration Environment)

`declare symbol`, `declare mathfunc`, `declare apply`, `declare rule`, `declare derivative` などの `declare` 系宣言、および `class` / `instance` 宣言は、**プログラム読み込み時の専用のプリパスで一括収集され、大域的な宣言環境 (Declaration Environment) に登録される**。

型推論・評価・正規化・マッチングの各フェーズは、この環境を自由に参照できる。

#### 構成要素

| 環境 | 内容 | 参照箇所 |
|---|---|---|
| SymbolEnv | `declare symbol` で宣言されたシンボル | パース(注釈内の原子の正規化)、`Symbol` 型の値の識別 |
| MathFuncEnv | `declare mathfunc` で宣言された関数(シグネチャ) | パース(原子式の認識)、型推論 |
| ApplyRuleEnv | `declare apply` の適用規則 | 関数適用時の簡約 |
| ReductionEnv | `declare rule` の書き換え規則 | `casNormalizeWithRules` |
| DerivativeEnv | `declare derivative` の導関数登録 | `Differentiable` インスタンスの連鎖律 |
| ClassEnv | `class` 定義と `instance` 宣言 | 型クラス解決 |

#### 重要な帰結

**宣言の順序制約はない**: プリパスで全宣言を先に集めるので、`declare mathfunc sqrt` より前の位置に `def f : Poly Integer [sqrt 2] := ...` と書いても有効。Haskell の型クラス宣言と同じ扱い。

**相互参照が可能**: `declare apply sqrt x := ... sqrt ... ` のような再帰参照や、`declare rule` の RHS が他の `declare mathfunc` 関数を呼ぶことも許される。

**`casNormalize` は環境依存**: 正規化関数は `ReductionEnv` を参照する。同じ `CASValue` でも環境が異なれば(例: ルールを差し替えた `--no-normalize` 相当モード)結果が変わる。

**ロード時のエラー検出**: 宣言の整合性(例: `declare derivative f = g` の `f` が `declare mathfunc` 済みか、`declare rule` の LHS が有効か)はプリパスで検証する。

#### 実装上の構造

```haskell
data DeclarationEnv = DeclarationEnv
  { symbolEnv     :: Map String SymbolExpr           -- declare symbol
  , mathFuncEnv   :: Map String MathFuncSig          -- declare mathfunc
  , applyRuleEnv  :: Map String ApplyRule            -- declare apply
  , reductionEnv  :: [ReductionRule]                 -- declare rule
  , derivativeEnv :: Map String CASValue             -- declare derivative
  , classEnv      :: ClassEnv                        -- class/instance
  }

-- 2 パス構造:
-- Pass 1: プログラム全体を走査、declare を DeclarationEnv に登録
-- Pass 2: DeclarationEnv を参照しながら型推論・評価
```

---

## 型の構成要素

### 組み込み型


| 型                      | 意味                                      |
| ---------------------- | --------------------------------------- |
| `MathValue`            | **第一級の計算型**。全ての CAS 計算はこの型上で行う。ランタイム表現は `CASValue`。簡約規則もすべてこの型に紐づく |
| `Integer`              | 基本型。整数。`MathValue` の subtype                                  |
| `Frac a`                | `a` の分数体/分数環。`MathValue` の subtype          |
| `Symbol`               | `declare symbol` で宣言された平の不定元（例: `x`, `y`, `r`, `θ`）。`Factor` の真の部分型 |
| `Factor`               | 原子的な数式要素全体(`Symbol` と `Apply1`/`Quote` ベースの合成原子の和)。`MathValue` の subtype |
| `Term a [atoms]`        | `Poly a [atoms]` の項。係数（型 `a`）とモノミアルを持つ補助型 |
| `Poly a [atoms]`        | `a` を係数とするローラン多項式環。`MathValue` の subtype。内部表現は `MathValue` と同じ `CASValue` で、型注釈による coerce 時に所属検証 + 正規化を行う |
| `Poly a [..]`            | 原子集合をフレッシュ型変数で開いたローラン多項式型（開いた多項式型）。`MathValue` の subtype |
| `Tensor a`             | `a` を成分とするテンソル                          |

**Symbol 型と Factor 型の扱いについての注記**: `Symbol` は型として残す(`∂/∂` の第2引数の型制約として機能するため)。`Factor` 型も維持する。ただし `ConstantFactor` / `AppliedFactor` のような Factor 3分類は型システムには持たない。理由: 誤注釈の foot-gun を避けるため、かつ `Differentiable MathValue` が always-AF 規則 + `Apply1`/`Symbol` のランタイム構成子で振り分けるので、型レベルでの分類は冗長。

### 型エイリアス

ユーザは `Poly` の特殊化に型エイリアスを定義できる:

```egison
type GaussianInt := Poly Integer [i]              -- Z[i]
type Zsqrt2      := Poly Integer [sqrt 2]         -- Z[√2]
type RatFunc     := Frac (Poly Integer [x])        -- Z[x] の分数体
```

### Poly はローラン多項式環

`Poly a [atoms]` は標準的な多項式環ではなく、ローラン多項式環を表す。
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

`Poly` の原子集合スロットには、2つの形式がある。

**閉じたスロット** `[s1, s2, ...]`: シンボル集合が具体的に固定されている。特定の多項式環で作業するときに使う。演算結果の型が厳密に制御される。

**開いたスロット** `[..]`: シンボル集合を固定しない。`sqrt 2 + sqrt 3` のように動的に原子が現れる探索的な計算に使う。

```egison
declare symbol x, y

-- 閉じた Poly: Z[x, x⁻¹] で厳密に作業
def p : Poly Integer [x] := 1 + x
def q : Poly Integer [x] := 2 + 3 * x
p + q    -- OK: Poly Integer [x]

-- 定数原子を含む閉じた Poly
def r : Poly Integer [sqrt 2, x] := (sqrt 2) * x + 1

-- 開いた Poly: 自由に原子を導入
def s : Poly Integer [..] := sqrt 2 + sqrt 3    -- OK
def t : Poly Integer [..] := s + x              -- OK
```

#### 開いた Poly のランタイム表現と型具体化

`[..]` は **TypeScript の `any` に相当する「未指定の上位型」** として扱う。各出現ごとに独立したフレッシュ型変数を生成する「行多相」的な設計ではなく、**単純なワイルドカード**。

- 単一化時: `[..]` は任意の閉じたスロット・他の `[..]` と常にマッチする（制約を生まない）
- 部分型関係: `Poly a [atoms] ⊂ Poly a [..]` が常に成立
- `join` で一方が `[..]` の場合、結果も `[..]`

```egison
-- ユーザーが書く型
polyAdd :: Poly a [..] -> Poly a [..] -> Poly a [..]

-- 各 [..] は独立のワイルドカードで、引数間・引数と結果の間で原子集合を共有しない
```

入出力で同じ原子集合を共有したい場合は、名前付き型変数を明示する:

```egison
polyAdd :: {atoms} Poly a [atoms] -> Poly a [atoms] -> Poly a [atoms]
```

具体化の流れ：

1. 適用時に `[..]` のスロットは任意の閉じたスロットを受け入れる（単一化はスキップ）
2. 閉じたスロット同士で原子集合が異なる場合は、Embed（coercive subtyping）で拡大して揃える
3. ランタイム表現は閉じた `Poly` と同一（`CASPoly [CASTerm]`）で、型レベルのみの区別

```egison
declare symbol x y

polyAdd (x^2 + 1) (y + 3)
-- 第1引数: Poly Integer [x], 第2引数: Poly Integer [y]
-- polyAdd の型は Poly a [..] なので、スロットは any 扱い → そのまま受け入れる
-- 結果型は Poly Integer [..]（静的には原子は追跡されない）
```

閉じたスロット同士の合流（例: 2つの引数が `Poly Integer [x]` と `Poly Integer [y]` で、出力も対応する閉じた型が欲しい）の場合は、名前付き型変数と `join` で書く:

```egison
polyJoinAdd :: {atoms₁, atoms₂} Poly a [atoms₁] -> Poly a [atoms₂] -> Poly a [atoms₁ ∪ atoms₂]
```

##### `[..]` 採用の理由

HM 互換で `[atoms₁ ⊆ atoms₂]` のような集合包含制約付き単一化を導入せずに済む。`[..]` を any にすることで:
- 単一化アルゴリズムへの変更が最小（既存の等価判定 + `[..]` とのマッチ追加のみ）
- `join` の結果計算が単純（一方が `[..]` なら `[..]` 吸収）
- 閉じたスロット同士の部分集合チェックは `embed`（coercive subtyping）の責務として分離

#### 型構文の実装

Type ADT に `TPoly Type SymbolSet` を追加し、`SymbolSet` は `Closed [SymbolExpr]` / `Open` の2構成子。

**文法設計**: `Poly T [S]` はコレクション型 `[T]` との曖昧性を避けるため、型コンストラクタ `Poly` に必ず型引数と原子集合スロットを続ける構文を要求する:

```
<poly-type> ::= 'Poly' <type-atom> <slot-set>
<slot-set>  ::= '[' '..' ']'           -- open slot
              | '[' <atom-list> ']'    -- closed slot
<atom-list> ::= <empty>
              | <atom-expr> (',' <atom-expr>)*
<atom-expr> ::= <symbol>              -- x, i, r
              | <func-app>            -- sin x, sqrt (x+1), log 3
```

スロット内の `<atom-expr>` は関数適用を許容する。複数トークンで構成される原子(例: `sin x`, `sqrt (x+1)`)もそのまま書ける。

`[..]` はレキサ段階で専用トークン化する(あるいはパーサーが `[` の直後の `..` を特別扱いする)。

型コンストラクタ `Poly` は型引数と原子集合スロットの両方を常に伴うため、`Poly T [x]` と `[T]` の曖昧性はパースの局所性によって回避される。

### 型の構成例

```
Integer                                  -- Z（整数）
Frac Integer                              -- Q（有理数）
Poly Integer [x]                         -- Z[x, x⁻¹]（整数係数ローラン多項式、閉じた型）
Poly Integer [x, y]                      -- Z[x, x⁻¹, y, y⁻¹]（多変数、閉じた型）
Poly Integer [i]                         -- Z[i]（ガウス整数、i^2 = -1）
Poly Integer [sqrt 2]                    -- Z[√2]（定数原子のみ）
Poly Integer [sqrt 2, x]                 -- Z[√2, x, x⁻¹]
Poly Integer [sin x]                     -- Z[sin x]（合成原子を含む）
Poly (Frac Integer) [x]                   -- Q[x, x⁻¹]
Poly (Frac Integer) [i]                   -- Q(i)
Frac (Poly Integer [x])                   -- Z[x, x⁻¹] の分数体（非モノミアル除算）
Poly (Poly Integer [x]) [y]              -- Z[x, x⁻¹][y, y⁻¹]
Poly Integer [..]                        -- 整数係数、原子集合自由なローラン多項式
Poly (Frac Integer) [..]                  -- 有理数係数、原子集合自由なローラン多項式
Tensor (Poly (Frac Integer) [x])          -- 有理数係数ローラン多項式を成分とするテンソル
```

### 入れ子の Poly と多変数の Poly

```egison
Poly (Poly Integer [x]) [y]   -- Z[x, x⁻¹][y, y⁻¹]: y について整理、係数が x のローラン多項式
Poly Integer [x, y]            -- Z[x, x⁻¹, y, y⁻¹]: x と y を対等に扱う
```

数学的に同型だが、正規形が異なる。型システムの設計上、両者は異なる型・異なる内部表現・異なるマッチャー（`poly (poly integer)` vs `poly integer`）として自然に区別される。ユーザーが型注釈で選択する。入れ子の場合の具体的な内部表現は「構成的な内部表現 > 表現の対応」の `Poly (Poly Integer [x]) [y]` の例を参照。

---

## 構成的な内部表現

型構造がそのままランタイムの内部表現を決定する。現在の一枚岩な `ScalarData = Div PolyExpr PolyExpr` とは異なり、型の組み合わせごとに異なる内部表現を持つ。

### 表現の対応

**ランタイムの `Monomial` は flat な `(原子, 冪指数)` のリスト**として保持する。型の `[atoms]` は「この原子集合がスロットに属することを型レベルで表明する」契約であり、ランタイム値には分類タグを保持しない。

原子のメンバーシップ判定は、以下の境界操作でのみ必要に応じて行う：

- `coerce : MathValue → Poly a [atoms]`（所属検証: 原子が `atoms` に含まれるかを検証）
- `Differentiable MathValue` のディスパッチ（always-AF 規則で分類なしに動作）

`term m` マッチャーはランタイム Monomial が flat であることを反映し、**1 スロット(flat)で分解**する(`($c, $m)` で `$m : [(Factor, Integer)]`)。

`casNormalize` の内部ループでは分類判定を**行わない**。詳細は「境界での名前集合照合」セクションを参照。

以下の内部表現例では、各原子がどの意味で使われているかをコメントで示すが、ランタイムの `Monomial` は flat な単一リストである：

```
Integer                      → CASInteger 3
Frac Integer                  → CASFrac (CASInteger 2) (CASInteger 3)        -- 2/3
Poly Integer [x]             → CASPoly [CASTerm (CASInteger 1) [(x,1)],
                                         CASTerm (CASInteger 1) []]           -- x + 1
Poly Integer [r]             → CASPoly [CASTerm (CASInteger 1) [(r,1)],
                                         CASTerm (CASInteger 1) [(r,-1)],
                                         CASTerm (CASInteger 1) [(r,-2)]]     -- r + 1/r + 1/r^2
Poly Integer [sqrt 2, x]
                              → CASPoly [CASTerm (CASInteger 1) [(sqrt 2,1),(x,1)],
                                         CASTerm (CASInteger 1) []]           -- (sqrt 2)*x + 1
                                 -- 型レベルで sqrt 2 と x は同じ atoms に属し、
                                 -- ランタイムは flat リストに並ぶ
Poly Integer [sin x]
                              → CASPoly [CASTerm (CASInteger 1) [(sin x,1)]]  -- sin x
Poly (Frac Integer) [x]       → CASPoly [CASTerm (CASFrac 1 2) [(x,1)]]       -- (1/2)x
Poly (Poly Integer [x]) [y]
                              → CASPoly [CASTerm (CASPoly [CASTerm (CASInteger 1) [(x,1)],
                                                            CASTerm (CASInteger 1) []])
                                                           [(y,2)],
                                         CASTerm (CASPoly [CASTerm (CASInteger 2) [(x,1)]])
                                                           [(y,1)],
                                         CASTerm (CASPoly [CASTerm (CASInteger 3) []])
                                                           []]                -- (x+1)y^2 + 2x·y + 3
Frac (Poly Integer [x])       → CASFrac (CASPoly ...) (CASPoly ...)           -- 非モノミアル分数
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

-- Monomial はランタイムでは flat な (原子, 冪指数) のリスト
-- 冪指数は負も可（ローラン多項式のため）
type Monomial = [(SymbolExpr, Integer)]
```

型の入れ子構造が内部表現の入れ子構造に直接対応する。例えば `Poly (Frac Integer) [x]` の各項の係数は `CASFrac (CASInteger _) (CASInteger _)` になる。

`Monomial` の冪指数が `Integer` であることにより、`r⁻¹` は `[(r, -1)]`、`r⁻²` は `[(r, -2)]` として自然に表現される。

**1スロット化の利点**:

- 型とランタイムの構造が自然に一致する(ランタイムが最初から flat なので型も flat)
- `casPlus`/`casMult` のモノミアル結合が一意に決まる（同じ原子は常に同じ表現）
- `casNormalize` のループ内でスロット分類を判定する必要がない
- embed（`Factor → Poly`, 原子集合拡大 `Poly [atoms] → Poly [atoms']` 等）の多くがランタイム no-op になり、coherence が自明に成立

分類が必要なのは以下の**境界操作**のみ（次節で詳述）:

1. `coerce : MathValue → Poly a [atoms]` の所属検証(原子が `atoms` に含まれるかの検証)
2. `Differentiable MathValue` のディスパッチ(always-AF 規則)

どちらもワンショット操作であり、正規化ループの内側で走らないので、「`sin(x-x)` の正規化で分類が変わる」問題は起きない。

`term m` マッチャーは flat に分解するので、ランタイムに型情報を渡す必要がない。

### 境界での名前集合照合

ランタイム分類関数（`classifyFactor` や `hasFreeSymbol` 等の自由シンボル解析）は**持たない**。代わりに、`Poly a [atoms]` の閉じた型が指定された境界で、**型注釈に現れる原子の名前集合との照合**を行う。

#### 設計原則

- 原子のメンバーシップは**ユーザーの型注釈が決定**する。ランタイム解析はしない
- 境界操作では「この原子は `atoms` に含まれるか」を注釈集合のルックアップで決める

#### SymbolExpr の正規形比較

注釈 `[sqrt 2]` とランタイム値中の `Apply1 sqrt (CASInteger 2)` を照合するため、パーサが注釈を**正規形の `SymbolExpr`** に変換し、照合は `SymbolExpr` の構造的 `Eq` で行う。Pretty-print ベースの文字列比較は脆弱なので避ける。

型レベルのシンボル集合 `SymbolSet` の定義:

```haskell
-- 現状: SymbolSetClosed [String]
-- 変更後:
data SymbolSet
  = SymbolSetClosed [SymbolExpr]   -- 正規化済み SymbolExpr の集合
  | SymbolSetOpen                  -- [..]
  | SymbolSetVar TyVar
```

注釈 `[sqrt 2, sin x]` をパースする際、`sqrt 2` は `declare mathfunc sqrt` の適用として、`sin x` も同様に正規化し、`SymbolExpr` として集合に格納する。

#### 利用箇所

**1. `coerce` の検証**:

```haskell
-- Poly a [..] → Poly a [atoms] の検証
coerceToClosed :: [SymbolExpr] -> CASValue -> Either CoerceError CASValue
coerceToClosed atoms (CASPoly terms) = do
  forM_ terms $ \(CASTerm _ mono) ->
    forM_ mono $ \(sym, _) ->
      require (sym `elem` atoms)  -- 注釈された原子のみ許可
```

システムは原子が **`atoms` に含まれていること**だけを検証する。

**2. `Differentiable MathValue` のディスパッチ**:

ランタイム分類はせず、**always-AF 規則**で動作する:

```haskell
diffFactor :: SymbolExpr -> Symbol -> CASValue
diffFactor (Symbol _ _ _) s sym = if sym == s then casOne else casZero
diffFactor (Apply1 f arg) s     = applyChainRule f arg s     -- 常に連鎖律
diffFactor (Quote expr)   s     = applyChainRuleToQuote expr s
```

引数が定数（自由シンボルなし）の場合、連鎖律の内側微分 `∂/∂ arg s` が自然に 0 を返すので、結果も 0 になる（`sqrt 2`, `log 3` 等の「実質的に定数的な」ケース）。従ってランタイム分類なしで数学的に正しい微分が得られる。

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

- `Poly a [atoms]` の `(+)`: 同じ単項式の項をまとめ、係数の加算は `a` の `(+)` で行う
- `Poly a [atoms]` の `(*)`: 係数の乗算は `a` の `(*)` で行い、単項式を結合する（冪指数の加算、負も可）
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
| `Poly a [atoms]` | `CASPoly []`（空の項リスト） |
| `Frac a` | 分子が `a` の零 / 分母が `a` の1。例: `Frac Integer` → `CASFrac (CASInteger 0) (CASInteger 1)` |
| `Frac (Poly a [atoms])` | `CASFrac (CASPoly []) (CASPoly [CASTerm <one> []])` （空 Monomial = 空リスト） |

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

-- Monomial は flat なリストなので、2 つのリストから共通原子の最小冪を取り出す
monoGcd :: Monomial -> Monomial -> Monomial
monoGcd m1 m2 = [ (s, min e1 e2)
                | (s, e1) <- m1, Just e2 <- [lookup s m2] ]
```

将来的に多項式GCD（ユークリッドの互除法等）へ拡張できるよう、`casGcd` を型クラスまたはパターンマッチで分離し、モノミアルGCDと多項式GCDを差し替え可能にしておく。

---

## シンボルと不定元

不定元（`x`, `y`）とシンボル（`i` 等）は型システム上の区別がない。どちらも `declare symbol` で宣言し、`Poly a [atoms]` の `atoms` スロットに入る。簡約規則は `declare rule` で別途付与する（例: `declare rule auto term i^2 = -1`）。

```egison
declare symbol x, y           -- 不定元
declare symbol i              -- シンボル(簡約規則で意味を付与)
```

**合成原子(`sin x`, `sqrt 2` 等)は `declare symbol` の対象にしない**。合成原子は `declare mathfunc` で関数を宣言し、型注釈 `Poly Integer [sqrt 2, sin x]` 等に出現した時点で自動的に登録される(「数学関数の宣言」セクション参照)。

---

## Factor 型と数学関数

### Factor の扱い

`Factor` は原子的な数式要素全体を指す型で、`Symbol` と `Apply1`/`Quote` ベースの合成原子の和として定義される。

分類はランタイムの `SymbolExpr` コンストラクタで識別する。型レベルには `ConstantFactor` / `AppliedFactor` のような真部分型は**持たない**。型として残るのは `Symbol` のみ(`∂/∂` の第2引数の型制約として必要)。

`∂/∂` は `Differentiable MathValue` の **always-AF 規則**で動作する: `Apply1 f a` を常に連鎖律で扱うと、`a` が定数（自由シンボルなし）の場合も `∂/∂ a s` が再帰的に 0 を返すため、数学的に正しい微分が得られる。

包含関係:

```
Symbol  ⊂  Factor
Factor  ⊂  MathValue
```

### 内部表現

Factor のランタイム表現は `CASFactor SymbolExpr`。`SymbolExpr` の3つのコンストラクタ (`Symbol`, `Apply1`, `Quote`) で実行時のケース分けを行う。

```
Symbol:   CASFactor (Symbol _ _ _)
Apply1:   CASFactor (Apply1 _ _)
Quote:    CASFactor (Quote _)
```

**利点**:

- `casNormalize` の内部ループでは分類判定を行わない
- `Poly` への embed は Monomial に `(sym, 1)` を追加するだけ
- ランタイム分類関数を持たない
- `coerce` は原子が `atoms` に含まれるかだけを検証
- `Differentiable MathValue` は always-AF 規則で動作

### Factor から Poly への埋め込み

Factor 値が `Poly` に埋め込まれるとき、Monomial の末尾に `(sym, 1)` を追加する。型レベルでは `atoms` スロットに原子が加わったことを表明する。

合成原子(`sin x`, `sqrt 2` 等)は `declare symbol` の対象にせず、`declare mathfunc` で関数を宣言するだけで登録される。型注釈 `Poly Integer [sqrt 2, sin x]` に原子として出現したときに宣言環境(`MathFuncEnv`)を参照して SymbolExpr として正規化・登録される。

### 数学関数の宣言 (`declare mathfunc`)

`sin`, `cos` 等の数学関数を「CAS の数学関数」として明示的に宣言する。**型は常に `MathValue -> MathValue` で固定**し、引数型による dispatch や返り値型の多態は行わない。

```egison
declare mathfunc sin : MathValue -> MathValue
declare mathfunc cos : MathValue -> MathValue
declare mathfunc log : MathValue -> MathValue
declare mathfunc exp : MathValue -> MathValue
declare mathfunc sqrt : MathValue -> MathValue
```

返り値型を `MathValue` に固定する理由:

- `sqrt n`（`n : Integer`）の結果は `isPerfectSquare n` の実行時判定で `Integer`（`sqrt 4 = 2`）にも `Factor`（`sqrt 2`）にもなり得る。実行時値に依存する返り値型を静的に決めるには依存型が必要
- 同様に、`sqrt m` のように `m` が実行時値の場合、`Poly Integer [sqrt m]` のような「特定の原子を含む型」は HM で表現できない（`m` が型レベルに現れてしまう）
- 返り値型を `MathValue` に固定することで、型推論を単純化し、`declare mathfunc` を普通のEgison関数と同じ型規則で扱える

`declare apply` が定義されていない場合のデフォルトの挙動は、`CASFactor (Apply1 ...)` を生成する。`declare apply` が定義されている場合は、その評価結果が返る（`Integer` / `Poly` / `Factor` 等のいずれか、ただし**静的型は常に `MathValue`**）。

呼び出し側で具体的な型が必要な場合は、**式レベル型注釈**（後述）で `coerce` を挿入する:

```egison
-- 注釈なし(MathValue のまま)
sqrt 2                                          -- : MathValue

-- 原子集合まで指定(推奨、後続計算で型情報を活用できる)
(sqrt 4 : Integer)                              -- : Integer（sqrt 4 = 2、成功）
(sqrt 8 : Poly Integer [sqrt 2])               -- : Poly Integer [sqrt 2]（2*sqrt 2、成功）
(sqrt 2 : Poly Integer [sqrt 2])               -- : Poly Integer [sqrt 2]（単項、原子 sqrt 2）

-- 実行時値を含む場合は [..] で退行
def m := extractPerfectSquareFactor x
(sqrt m : Poly Integer [..])                   -- atoms は any(具体原子は静的に不明)
```

### 数学関数の適用規則 (`declare apply`)

`declare mathfunc` で宣言した関数の適用時の評価ロジックを `declare apply` で定義する。引数が具体的な値の場合に簡約を行う。RHS にはユーザー定義関数を含む任意の Egison 式を書ける。**返り値は常に `MathValue`** として扱われ、`declare apply` の RHS にブランチ別の型注釈を書く必要はない(書いても呼び出し側の型推論には影響しない)。呼び出し側で具体的な型が必要な場合は、式レベル型注釈 `(sqrt 4 : Integer)` 等で coerce を挿入する。

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
declare rule auto term (sqrt $x)^2 = x
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
def s := (sqrt 2 : Factor)  -- s : Factor（式レベル型注釈で coerce）

-- 開いた Poly への昇格
s + 1                           -- Poly Integer [..]

-- 閉じた Poly への昇格（型注釈あり）
def t : Poly Integer [sqrt 2] := s + 1    -- sqrt 2 は atoms スロットに入る
```

`Factor` には `Ring` インスタンスを与えない。演算が必要な文脈では、型チェッカーが coercive subtyping（`Embed` 型クラス）により自動的に `Poly` へ `embed` を挿入する。これにより `casPlus` / `casMult` に `CASFactor` のケースは不要で、演算関数の実装がシンプルになる。

### CASFactor コンストラクタ

内部表現には `CASFactor` コンストラクタを持つ。`declare mathfunc` で宣言された関数の適用結果を表現する。`Poly` に昇格が必要になった時点で変換する。

```
-- sqrtSym = Apply1 (CASFactor (QuoteFunction sqrtFunc)) (CASInteger 2) :: SymbolExpr
-- ※ sqrtFunc は sqrt 関数の WHNFData

'(sqrt 2) : Factor
  → CASFactor sqrtSym
'(sqrt 2) : Poly Integer [sqrt 2]
  → CASPoly [CASTerm (CASInteger 1) [(sqrtSym, 1)]]
```

### sqrt の定義全体像

`sqrt` は `declare mathfunc` + `declare apply` + `declare rule` の3つの組み合わせで定義される:

```egison
-- 1. 数学関数の宣言（返り値は MathValue）
declare mathfunc sqrt : MathValue -> MathValue

-- 2. 適用規則（アルゴリズム的簡約。RHS は MathValue として評価される）
declare apply sqrt x :=
  if isInteger x then
    if isPerfectSquare x then isqrt x                    -- sqrt 4 → 2
    else if hasPerfectSquareFactor x then
      let (k, m) = extractPerfectSquareFactor x in
      k * sqrt m                                          -- sqrt 8 → 2 * sqrt 2
    else 'sqrt x                                          -- sqrt 2 → そのまま Factor
  else 'sqrt x                                            -- sqrt (x+1) → そのまま Factor

-- 3. パターン書き換え規則
declare rule auto term (sqrt $x)^2 = x
```

**`declare apply` RHS の型**: 返り値は常に `MathValue`。RHS 内のブランチに型注釈を書く必要はなく、書いても呼び出し側の型推論には影響しない(実装検証のための内部的な注釈として使うことは可能だが、関数インタフェースには漏れない)。

呼び出し側で具体的な型が必要な場合は、**式レベル型注釈** `(expr : Type)` で `coerce` を挿入する:

```egison
(sqrt 4 : Integer)                      -- 2 : Integer
(sqrt 8 : Poly Integer [sqrt 2])        -- 2 * sqrt 2 : Poly Integer [sqrt 2]
(sqrt 2 : Factor)                       -- sqrt 2 : Factor（原子不明）
def s : Poly Integer [sqrt 2] := sqrt 2 + 1   -- def の型注釈でも coerce が挿入される
```

---

## 型の包含関係

### 基本的な包含

```
Symbol  ⊂  Factor       -- 平のシンボルは Factor の真の部分型

Factor  ⊂  Poly Integer [..]

Integer  ⊂  Frac Integer
Integer  ⊂  Poly Integer [atoms]    -- 任意の原子集合

Frac Integer  ⊂  Poly (Frac Integer) [atoms]
Poly Integer [atoms]  ⊂  Poly (Frac Integer) [atoms]

Term a [atoms]  ⊂  Poly a [atoms]    -- 単一項の多項式として埋め込み

-- MathValue は全 CAS 型の上位型
Integer            ⊂  MathValue
Symbol             ⊂  MathValue
Factor             ⊂  MathValue
Poly a [atoms]     ⊂  MathValue       -- 任意の a, atoms に対して
Frac a             ⊂  MathValue       -- 任意の a に対して
```

**embed の内部表現:**
```
Term a [atoms] → Poly a [atoms]:
  CASTerm coeff mono → CASPoly [CASTerm coeff mono]
```

### 原子集合の包含

```
-- 原子集合の拡大
Poly a [atoms₁]  ⊂  Poly a [atoms₂]   when  atoms₁ ⊆ atoms₂

-- 開いた型への昇格
Poly a [atoms]  ⊂  Poly a [..]
```

### 包含の伝播規則

```
a ⊂ b  ならば  Poly a [atoms] ⊂ Poly b [atoms]    -- 係数の埋め込み
a ⊂ b  ならば  Frac a ⊂ Frac b                      -- 分数の埋め込み
a ⊂ b  ならば  Tensor a ⊂ Tensor b                  -- テンソルの埋め込み
```

---

## 実行時の型昇格タワー

`MathValue` 上の演算で、異なる部分型の値が混在したときに内部表現を一意化するためのタワー（priority ordering）を定める。後述の `join` は、このタワーに沿って結果の外側構造を決定する。

### タワーの定義

| レベル | 型 | 意味 | 内部表現 |
|---|---|---|---|
| 1 | `Integer` | Z | `CASInteger n` |
| 2 | `Frac Integer` | Q | `CASFrac (CASInteger _) (CASInteger _)` |
| 3 | `Poly Integer [..]` | Z のローラン多項式 | `CASPoly [CASTerm (CASInteger _) _]` |
| 4 | `Poly (Frac Integer) [..]` | Q のローラン多項式 | `CASPoly [CASTerm (CASFrac _ _) _]` |
| 5 | `Frac (Poly Integer [..])` | Q(x₁,...) の分数体 | `CASFrac (CASPoly _) (CASPoly _)` |

`Tensor` は **このタワーと直交** しており、`Tensor MathValue` として扱う（成分ごとに独立にタワー中の位置を持つ）。

### 演算規則

二項演算 `a op b` の結果レベルは `max(level(a), level(b))`。両方を max レベルまで `embed` してから演算する。

- **embed は自動**（演算の前処理として挿入）
- **demote は明示のみ**（`coerce` および表示時のみ）— 通常演算の出口で走査コストを毎回払うのを避ける方針（選択肢 B）

```egison
-- level 1 + level 2 → level 2
def a : Integer := 3
def b : Frac Integer := 1/2
a + b
-- a を level 2 に embed → 3/1
-- Frac Integer での加算 → 7/2

-- level 3 + level 2 → level 4
def p : Poly Integer [x] := x + 1
def q : Frac Integer := 1/2
p + q
-- 双方を level 4 に embed → Poly (Frac Integer) [x]
-- 結果: (1)x + (3/2)

-- level 3 + level 5 → level 5
def p : Poly Integer [x] := x + 1
def r : Frac (Poly Integer [x]) := 1 / (x + 1)
p + r
-- 双方を level 5 に embed
-- 結果: ((x+1)^2 + 1) / (x+1) : Frac (Poly Integer [x])
```

### スロット情報との合成

タワーによる **外側の構造**（Integer/Frac/Poly/...）の決定と、`Poly` の原子集合の和集合計算は **直交** しており、合成される。

1. まずタワーで外側の構造レベルを決定
2. `Poly` 相当のレベルに着地したら、原子集合は [join セクション](#2-join二項演算時の最小上界の計算) の和集合ルールで計算

```
join(Poly Integer [sqrt 2, x], Frac Integer)
  → タワー: max(level 3, level 2) = level 4
  → 原子集合: [sqrt 2, x]  (Frac Integer は空として合成)
  → 結果: Poly (Frac Integer) [sqrt 2, x]
```

### ローラン多項式との整合性

タワーの level 3/4（`Poly`）はローラン多項式環を表すため、**モノミアル分母は level 3/4 に吸収** される。level 5（`Frac (Poly ...)`）は **分母が非モノミアルの多項式** のときのみ用いる。

```egison
-- モノミアル分母 → level 3 に留まる（負冪で表現）
x / r^2 : Poly Integer [x, r]    -- CASTerm の Monomial = [(x,1), (r,-2)]

-- 非モノミアル分母 → level 5 に上がる
1 / (x + 1) : Frac (Poly Integer [x])
```

割り算 `a / b` のレベル判定は `b` の形で分岐する:

- `b` が定数 → 係数除算（係数側の level に委ねる）
- `b` が単項式（`CASPoly [CASTerm c m]` が1項のみ） → `m` の負冪として吸収、level 3/4 に留まる
- `b` が非単項式 → level 5 に上げる

### 不変条件と `declare rule` への影響

`MathValue` 内部の `CASValue` は、演算の出口で **タワー中の適切なレベルに embed された形** で保持される。demote は明示のみ（選択肢 B）なので:

- 同じ数学的値が同じ `CASValue` 構造を持つとは限らない（例: `(x+1)/1` と `x+1` は別表現になりうる）
- 等価比較や `declare rule` の適用は、必要に応じて明示的に `normalize` / `coerce` を通して demote してから行う
- 通常演算の出口で走査コストを払わずに済む

Axiom のドメインタワーとの関係については [Axiom/FriCAS との比較](#axiomfricas-との比較) を参照。

---

## 自動変換の3つの仕組み

### 1. embed（包含関係による自動埋め込み）

型の包含関係がある場合、ターゲット型が文脈から既知のときに自動的に `embed` を挿入する。

```egison
def f (x : Poly Integer [x, y]) : Poly Integer [x, y] := x + x
def p : Poly Integer [x] := 1 + x

f p  ⇝  f (embed p)    -- Poly Integer [x] → Poly Integer [x, y]
                       -- atoms スロットの拡大
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

-- Factor は Poly Integer [..] に embed される
-- ランタイムでは Monomial に (sym, 1) が入るだけ
instance Embed Factor (Poly Integer [..]) where
  embed f = ...  -- CASFactor sym → CASPoly [CASTerm (CASInteger 1) [(sym, 1)]]

instance {Embed a b} Embed (Poly a [..]) (Poly b [..]) where
  embed p = ...  -- 各項の係数を再帰的に embed

-- 原子集合の拡大（ランタイム no-op、型レベルのみの操作）
instance {atoms₁ ⊆ atoms₂} Embed (Poly a [atoms₁]) (Poly a [atoms₂]) where
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

各変換の内部表現は `Embed` インスタンスのコメントに記載。原子集合の拡大は内部表現を変えない（型レベルのみの操作）。

#### 実装方式

Coq の Coercion mechanism に倣い、型チェッカーに coercive subtyping を組み込む：

- `Embed` インスタンスの宣言時に、型チェッカーが包含関係のグラフを構築する
- 型不一致の検出時に、グラフ上の最短経路（深さ制限付きBFS）で推移的な `embed` の合成を探索する
- **Coherence（一貫性）** は、CAS型の包含関係が数学的に明確な半順序であることから自然に保証される（どの経路でも同じ数学的な値に変換される）
- 参考文献: Luo (1999) *Coercive subtyping*, Breazu-Tannen et al. (1991) *Inheritance as implicit coercion*

#### 式レベル型注釈

任意の式に型注釈を付けることで `coerce` を挿入できる。構文は `(expr : Type)`:

```egison
(sqrt 4 : Integer)                          -- sqrt 4 の結果を Integer に coerce
(sqrt 8 : Poly Integer [sqrt 2])           -- sqrt 8 の結果を Poly に coerce
(sqrt 2 : Factor)                           -- sqrt 2 の結果を Factor に coerce
(sin x + cos x : MathValue)                 -- 型注釈が推論型と一致すれば coerce 不要
```

式レベル型注釈は `declare mathfunc` の戻り値（`MathValue`）を具体型に絞り込む主要な手段である。`def` の型注釈と同様に、ターゲット型が推論型より具体的な場合に `coerce` が自動挿入される。

##### 注釈粒度の選び方

同じ `sqrt 2` でも注釈型によって得られる静的情報量が異なる。原子集合まで指定した **閉じた `Poly` / `Term` 型** が最も情報量が多く、後続の計算で型推論が精密になるため推奨:

| 注釈型 | 静的に分かること | 主な用途 |
|---|---|---|
| `MathValue` | 何でも（最汎） | 型注釈なしと同等 |
| `Poly Integer [..]` | 原子集合は非固定 | 実行時値を含む場合の退行形 |
| `Poly Integer [sqrt 2]` | 原子が `sqrt 2` 限定、`(sqrt 2)^2 = 2` 等のルールが型上で追跡可 | **推奨**（引数が静的に既知の場合） |
| `Term Integer [sqrt 2]` | 単項であることも表明 | さらに精密に絞り込みたい場合 |

**REPL / inspect での観察型の表示**: 注釈を省いた場合、REPL は評価後に観察型を表示する。ユーザーはこれをコピーして `def` の注釈にできる。詳細は「観察型」セクション参照。

#### coerce（型の絞り込み）

`embed` は安全な widening（具体 → 一般）だが、逆方向の narrowing（一般 → 具体）が必要な場面がある。典型的には `declare mathfunc` の関数の戻り値が `MathValue` で推論されるが、ユーザは結果が特定の型に属することを知っている場合。

```egison
-- sqrt : MathValue -> MathValue（declare mathfunc で宣言）

-- def の型注釈で coerce が挿入される
def x : Poly Integer [sqrt 2] := sqrt 2
def n : Integer := sqrt 4

-- 式レベル型注釈でも coerce が挿入される
(sqrt 4 : Integer) + 1    -- 2 + 1 = 3
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
| `Poly a [..] → Poly a [atoms]` | 全項のモノミアルの各原子が、`atoms` の名前集合に含まれるか |
| `Poly a [..] → Integer` | 全項のモノミアルが空で、係数が `Integer` に変換可能か |
| `Poly a [..] → Symbol` | 単一項で係数が1、Monomial が単一原子の1乗で、その原子が `Symbol` コンストラクタ |
| `Frac a → a` | 分母が1か |

**embed との対称性**:

```
embed  : 具体 → 一般（常に安全、コンパイル時保証、型チェッカーが自動挿入）
coerce : 一般 → 具体（ランタイム検証、失敗時エラー、型注釈による自動挿入）
```

観察型との関係: 観察型と宣言型が一致すれば `coerce` は成功する。観察型の計算については「観察型」セクション参照。

### 2. join（二項演算時の最小上界の計算）

二項演算 `a + b` で `a : τ₁`, `b : τ₂` のとき、型チェッカーが以下の手順で処理する:

1. `joinTypes τ₁ τ₂` を試みる
2. 成功 → 結果 `τ` に対して、`τ₁ ≠ τ` なら `embed` で `τ₁ → τ`、`τ₂ ≠ τ` なら `embed` で `τ₂ → τ`
3. 失敗（タワー外でかつ共通の上位 subtype がない）→ `τ = MathValue` として双方を `embed`

外側の構造（Integer/Frac/Poly/...）の合流は [実行時の型昇格タワー](#実行時の型昇格タワー) が規定する。以下の規則は、タワーによる外側決定と `Poly` の原子集合和集合を合成した形になっている。

#### join の計算規則

```
join(a, a) = a

-- 係数の昇格（タワー: level 1 → 2、1 → 3）
join(Integer, Frac Integer) = Frac Integer
join(Integer, Poly Integer [atoms]) = Poly Integer [atoms]

-- 閉じた Poly 同士: 原子集合の和集合を自動計算
join(Poly a [atoms₁], Poly b [atoms₂]) = Poly (join(a, b)) [atoms₁ ∪ atoms₂]

-- 閉じた Poly と埋め込み可能な型
join(Poly a [atoms], b) = Poly (join(a, b)) [atoms]    -- b が a に embed 可能
join(a, Poly b [atoms]) = Poly (join(a, b)) [atoms]    -- a が b に embed 可能

-- 開いた Poly
join(Poly a [..], Poly b [..]) = Poly (join(a, b)) [..]
join(Poly a [atoms], Poly b [..]) = Poly (join(a, b)) [..]
join(Poly a [..], Poly b [atoms]) = Poly (join(a, b)) [..]

-- Poly と Frac Integer（タワー: level 3 + level 2 → level 4）
join(Poly Integer [atoms], Frac Integer) = Poly (Frac Integer) [atoms]
join(Frac Integer, Poly Integer [atoms]) = Poly (Frac Integer) [atoms]

-- Frac 同士（同レベルでの再帰）
join(Frac a, Frac b) = Frac (join(a, b))

-- Frac (Poly ...) が絡むケース（タワー: level 5 へ）
join(Poly a [atoms₁], Frac (Poly b [atoms₂]))
  = Frac (Poly (join(a, b)) [atoms₁ ∪ atoms₂])
join(Frac (Poly a [atoms₁]), Frac (Poly b [atoms₂]))
  = Frac (Poly (join(a, b)) [atoms₁ ∪ atoms₂])

-- 上記のどれにもマッチしない場合 → MathValue にフォールバック
join(_, _) = MathValue
```

> あるスロットが `[..]` のとき、結果の原子集合は `[..]` として吸収される。

#### フォールバックの例

タワー内に収まる型同士の join は必ずタワーによって解消されるため、`MathValue` フォールバックは **タワー外の型**（将来拡張型、`Tensor` 要素の不一致など）でのみ発生する。

```
-- タワー内で解消される例（フォールバックしない）
join(Integer, Factor) = Poly Integer [..]
  -- Factor ⊂ Poly Integer [..]、Integer は Poly に embed 可
join(Poly Integer [x], Frac Integer) = Poly (Frac Integer) [x]
  -- タワー level 3 + level 2 → level 4

-- タワー外で共通の上位 subtype がない → MathValue
-- 例: Tensor a と Tensor b で join(a, b) が MathValue に落ちる場合
join(Tensor (Poly Integer [sqrt 2]), Tensor Factor) = Tensor MathValue
```

#### join の具体例

```
-- 原子集合の拡大
join(Poly Integer [x], Poly Integer [x, y]) = Poly Integer [x, y]

-- 包含関係なし → 和集合で自動合流
join(Poly Integer [x], Poly Integer [y]) = Poly Integer [x, y]

-- 異なる種類の原子の合流
join(Poly Integer [x], Poly Integer [sqrt 2, y])
  = Poly Integer [sqrt 2, x, y]

-- 係数の昇格（原子集合は同一）
join(Poly Integer [x], Poly (Frac Integer) [x]) = Poly (Frac Integer) [x]

-- 開いた型との合流
join(Poly Integer [x], Poly Integer [..]) = Poly Integer [..]
```

#### 型推論との統合

```egison
declare symbol x, y

def p : Poly Integer [x] := 1 + x
def q : Poly Integer [x, y] := 1 + x + y

p + q
-- joinTypes で Poly Integer [x, y] を計算
-- p を embed: Poly Integer [x] → Poly Integer [x, y]
⇝ (embed p) + q : Poly Integer [x, y]
```

```egison
def p : Poly Integer [x] := 1 + x
def q : Poly Integer [y] := 2 + y

p + q
-- joinTypes で Poly Integer [x, y] を計算（atoms の和集合）
-- 双方を embed
⇝ (embed p) + (embed q) : Poly Integer [x, y]
-- 結果: x + y + 3
```

```egison
-- タワーで解消される例: Integer + Factor → Poly Integer [..]
def n : Integer := 3
def s := (sqrt 2 : Factor)

n + s
-- joinTypes Integer Factor
--   Factor ⊂ Poly Integer [..]、Integer は任意の Poly に embed 可
-- → Poly Integer [..]
⇝ (embed n) + (embed s) : Poly Integer [..]
-- 結果: sqrt 2 + 3
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

### 設計原則: 原子集合保存は型クラスで行う

CAS の演算で原子集合（`atoms`）を静的に保存したいかどうかは、**用途ごとに異なる**。設計はこれを 2 つの機構で表現する:

**用途 1: 原子集合に関心のない汎用処理** — 型シグネチャに `[..]` を直接書く。

```egison
def prettyPrint : Poly a [..] -> String := ...
def depth       : Poly a [..] -> Integer := ...
```

式レベル/関数シグネチャの `[..]` は **ワイルドカード（独立な any）** として振る舞う。引数間・戻り値間で原子集合は共有されず、静的追跡もされない。`coerce` が必要な場合はランタイム検証になる。

**用途 2: 原子集合を保存したい演算** — 型クラスで書く。

```egison
-- Ring a => a -> a -> a の形で書く。a が具体化されたときに原子集合も確定
def double {Ring a} (x : a) : a := x + x
```

型クラスのインスタンスヘッドにある `[..]` は **パラメトリック（型変数として束縛）** として扱われ、解決時に具体的な `[atoms]` に特化される。これにより、型クラス経由の演算は原子集合を自然に保存する。

**2 つの `[..]` の使い分け**:

| 出現位置 | 意味 | 原子集合の追跡 |
|---|---|---|
| 型クラスのインスタンスヘッド(例: `instance Ring (Poly a [..])`) | パラメトリック(型変数) | 解決時に具体化、保存される |
| 式レベル注釈 / 関数シグネチャ | ワイルドカード(独立 any) | されない |

### 閉性は演算ごと・型ごとに独立

`+` と `*` で型が閉じる/閉じないはそれぞれ独立に判断される。型クラスを **演算ごとに分離**(`AddMonoid` / `MulMonoid`)することで、`+` は閉じるが `*` は閉じない型を自然に表現できる:

```egison
-- Poly Integer [sqrt 2, sqrt 3]: + で閉じるが、
-- sqrt 2 * sqrt 3 = sqrt 6 の自動規則があれば * で閉じない
instance AddMonoid (Poly Integer [sqrt 2, sqrt 3])
instance AddGroup  (Poly Integer [sqrt 2, sqrt 3])
-- MulMonoid の instance は宣言しない
-- → p * q は汎用インスタンス MulMonoid (Poly a [..]) に解決され、
--   結果は Poly Integer [..] に開く
```

ユーザーは「扱いたい型」ごとに、閉じる演算の型クラスインスタンスだけを宣言する。閉じない演算は汎用インスタンスに自動的に落ちるので、明示的に何かを書く必要はない。

**ユーザーの責任**: `instance MulMonoid T` を宣言することは「T が `*` で閉じる」というユーザーの表明。実際には閉じない型に宣言すると、結果値が型に適合せず、後続の境界操作でランタイムエラーになる。システムは閉性の数学的検証はしない(`declare rule` を見て自動判定することはしない)。

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

### 等価性: `Eq MathValue` と subtype からの fallback

CAS 型の意味論的等価判定は `MathValue` レベルで 1 回定義するだけで済む。

```egison
instance Eq MathValue where
  (==) x y := (x - y) = 0
```

ここで `-` は `Ring MathValue` の減算、`= 0` は `=` プリミティブによる `CASInteger 0` との構造比較。`casNormalize` が空の `CASPoly []` や 分子 0 の `CASFrac` を `CASInteger 0` に正規化するため、差が数学的に 0 なら必ず `CASInteger 0` に到達する。

**設計上の利点**:

- `casNormalize` と簡約規則(`declare rule auto`)が `x - y` の計算過程で適用されるので、`casNormalize x == casNormalize y` より**強い等価判定**になる
- タワーの型昇格(Integer vs Frac vs Poly)が `-` で自動処理されるので、異種型の比較も自然に動く
- demote ポリシーに依存しない(差が `0` に正規化されれば OK)

**subtype からの fallback**: `GaussianInt` 等の subtype については個別の `Eq` インスタンスを定義しない。代わりに型クラス解決機構が subtype から `MathValue` への embed を自動挿入する:

```
p == q   where p, q : GaussianInt
  ↓ Eq GaussianInt インスタンスなし、GaussianInt ⊂ MathValue、Eq MathValue あり
  ↓ elaboration:
(embed p : MathValue) == (embed q : MathValue)
  ↓ Eq MathValue の定義:
((embed p) - (embed q)) = 0
```

これは `Embed` 型クラスによる coercive subtyping を equality 解決にも適用するもの。実装上は「ターゲット型で直接インスタンスが見つからない場合、親型で再試行 + `embed` 挿入」という fallback ルールが必要。

**例**:

```egison
-- 簡約規則込みの等価判定
(1 + i) * (1 - i) == 2
-- → ((1+i)(1-i) - 2) = 0
-- → casNormalize ((1 - i^2) - 2) = casNormalize (1 - (-1) - 2) = CASInteger 0 ✓

-- 因数分解形と展開形
(x+1)^2 == x^2 + 2*x + 1    -- true(差が 0 に正規化)

-- 異種型(タワー跨ぎ)
(3 : Integer) == ((6 : Integer) / (2 : Integer))    -- true
```

### インスタンスの2つのパターン

型クラスのインスタンスには2つのパターンがある:

**パターン1: MathValue に委譲する型クラス（`Ring` 等）**

`Ring` の演算（`+`, `*`）はすべて `MathValue` レベルの `casPlus`/`casMult` + `casNormalize` で実装される。subtype のインスタンスは MathValue の演算をラップ + coerce するだけで、**独自の簡約ロジックを持たない**。簡約規則は `declare rule auto` で `MathValue` レベルに統一されるため、**演算ロジックの特化は不要**。

ただし、**静的型保存のためのインスタンス特化は必要**: `Ring (Poly a [..])` のような汎用インスタンスのヘッドにある `[..]` はパラメトリックな型変数として扱われ、具体的な `[atoms]` への解決時に自動的に特化される（型レベルの特化）。これにより `a + b : GaussianInt` のように結果の型が保存される。ユーザーが `instance Ring GaussianInt` を明示的に書く必要はなく、汎用テンプレートから解決機構が自動派生する。

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
instance {Ring a} Ring (Poly a [..]) where
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
type GaussianInt := Poly Integer [i]
instance EuclideanDomain GaussianInt where
  gcd a b := ...  -- ガウス整数専用のアルゴリズム
  divMod a b := ...

-- 係数が体の多項式: 汎用の多項式GCD
instance {Field a} EuclideanDomain (Poly a [..]) where
  gcd a b := ...  -- 一般の多項式ユークリッド算法
  divMod a b := ...

-- 係数が GCDDomain の多項式: 内容と原始部分に分解する GCD
instance {GCDDomain a} GCDDomain (Poly a [..]) where
  gcd a b := ...  -- 内容 GCD × 原始部分 GCD
```

#### インスタンス解決メカニズム

インスタンス解決は**標準 Haskell 的な単一化ベース**で行う。

**基本原則**: インスタンスヘッドの `[..]` をフレッシュ型変数として扱い、ターゲット型との単一化で具体化する。

```
instance {Ring a} Ring (Poly a [..]) where ...
-- 内部表現: instance {Ring a} Ring (Poly a [α]) where ...  (α は fresh)
```

解決の流れ:

```
例: a + b の解決 (a, b : GaussianInt = Poly Integer [i])
目標: Ring (Poly Integer [i])

1. 候補インスタンスのヘッドとターゲットを単一化
   - instance Ring (Poly a [α])
   - ターゲット: Ring (Poly Integer [i])
   - 単一化: a = Integer, α = [i]
   - → 解決成功

2. 単一化成功のインスタンスから具体的な型を復元
   - Ring (Poly Integer [i]) が利用可能
   - 戻り値型として Poly Integer [i] が保存される
```

**部分特化**: 閉じたスロットと `[..]` の混在も同じ単一化で扱える。

```
instance Ring (Poly Integer [sqrt 2])
-- ターゲット: Ring (Poly Integer [sqrt 2])
-- → 解決成功
```

**sibling 型への fallback**(subtype 解決): ターゲット型で直接インスタンスが見つからない場合、subtype 関係を辿って親型のインスタンスを使う(coercive subtyping)。

```
-- Eq MathValue のみ定義、Eq GaussianInt なし
p == q   where p, q : GaussianInt

1. Eq GaussianInt を探す → なし
2. subtype 関係: GaussianInt ⊂ MathValue
3. Eq MathValue あり → embed 挿入
   (embed p : MathValue) == (embed q : MathValue)
```

#### インスタンス解決の優先順位

複数の候補インスタンスがターゲットと単一化可能な場合、**より具体的なもの**を優先する:

```egison
-- GaussianInt (= Poly Integer [i]) に gcd を適用
gcd (1 + i) (2 + i)
-- 候補1: instance EuclideanDomain GaussianInt             -- 具体的 ✓ 選ばれる
-- 候補2: instance {GCDDomain a} GCDDomain (Poly a [..])   -- 汎用

-- Poly (Frac Integer) [x] に gcd を適用
gcd (x^2 - 1) (x - 1)
-- 候補: instance {Field a} EuclideanDomain (Poly a [..])  -- a = Frac Integer, Field ✓
```

**具体性の判定**: 2 段階の比較。

1. **型引数の具体性**: 具体型(`Integer` 等) > 型変数(`a`)
2. **原子集合の具体性**: リテラル集合(`[i]`) > `[..]`(fresh 変数)

両方で「より具体的」な候補が選ばれる。競合(どちらも同じ具体性)は型エラー(OverlappingInstances は認めない)。CAS 型の特化インスタンスは有限個（`Z[i]`, `Z[ω]` 等）なので、この規則で十分。

#### 型推論の挙動

```egison
declare symbol x, i
declare rule auto term i^2 = -1

type GaussianInt := Poly Integer [i]

def a : GaussianInt := 1 + i
def b : GaussianInt := 2 + 3*i
a + b         -- Ring GaussianInt が選ばれる → : GaussianInt (= 3 + 4*i)
a * b         -- Ring GaussianInt が選ばれる → : GaussianInt (= -1 + 5*i)
gcd a b       -- EuclideanDomain GaussianInt（特化）が選ばれる

def p : Poly Integer [x] := 1 + x
def q : Poly Integer [x] := 2 + 3 * x
p + q         -- Ring (Poly Integer [x]) が選ばれる → : Poly Integer [x]

-- subtype が異なる場合は join で和集合を計算
a + p         -- GaussianInt と Poly Integer [x] の混合
              -- → join: [i] ∪ [x] = [i, x]
              -- → 双方を embed → Ring (Poly Integer [i, x]) → : Poly Integer [i, x]

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

-- 平のシンボル: 自分自身なら 1、それ以外は 0
instance Differentiable Symbol where
  ∂/∂ x s = if x == s then 1 else 0
```

合成原子(`sin x`, `sqrt 2` 等)の微分は、`Differentiable MathValue` の always-AF 規則で行う(下記)。型レベルで CF / AF を区別しない設計のため、`Differentiable ConstantFactor` / `Differentiable AppliedFactor` のような型別インスタンスは**持たない**。

#### 複合型のインスタンス

```egison
-- Term: 積の微分法則（係数 × 各モノミアル因子の product rule）
instance {Differentiable a} Differentiable (Term a [atoms]) where
  ∂/∂ t s = ...  -- 係数の微分 × モノミアル + 係数 × モノミアルの微分（product rule）
                 -- モノミアルは flat リスト。各原子 (sym, e) は Differentiable MathValue の
                 -- always-AF 規則で微分される

-- Poly: 線形性（各項の微分の和）
instance {Differentiable a} Differentiable (Poly a [atoms]) where
  ∂/∂ p s = sum [∂/∂ t s | t <- terms p]

-- Frac: 商の微分法則
instance {Differentiable a} Differentiable (Frac a) where
  ∂/∂ (n/d) s = (∂/∂ n s * d - n * ∂/∂ d s) / d^2

-- MathValue: 内部の CASValue コンストラクタに応じてディスパッチ
-- CASFactor は always-AF 規則で連鎖律を適用（ランタイム分類は行わない）
instance Differentiable MathValue where
  ∂/∂ v s = ...  -- CASInteger             → 0
                 -- CASFactor (Symbol _)    → s と一致で 1、それ以外 0
                 -- CASFactor (Apply1 f a)  → 常に連鎖律: f'(a) * ∂/∂ a s
                 --                           （a が定数なら ∂/∂ a s が 0 を返し自然に 0 になる）
                 -- CASFactor (Quote e)     → 引用式に対する連鎖律
                 -- CASPoly                 → Differentiable (Poly ...)
                 -- CASFrac                 → Differentiable (Frac ...)
```

**always-AF 規則の正しさ**: `Apply1 f a` を常に連鎖律で扱うと、`a` が定数（自由シンボルなし）の場合でも `∂/∂ a s` が再帰的に 0 を返すため、結果は `f'(a) * 0 = 0` となり、数学的に正しい。従って `sqrt 2` や `log 3` のような「実質的に定数的な」ケースも正しく 0 に微分される。これによりランタイムの `classifyFactor` / `hasFreeSymbol` 解析は不要になる。

**設計の利点**:
- Haskell にハードコードされた `casDeriv` ではなく、各型ごとの微分規則をユーザーが定義・拡張できる
- `Apply1` の外側関数の導関数（`sin` → `cos` 等）をユーザーが `declare derivative` で登録する
- `Poly`, `Frac`, `Term` の微分法則は数学的な構造（線形性、積の法則、商の法則）に直接対応
- 1スロット化により型レベルでの CF / AF 分類が消え、誤注釈による foot-gun が解消される

#### 合成原子の導関数定義

合成原子の外側関数の導関数は `declare derivative` で登録する:

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

**`declare derivative` 未登録の関数の微分はエラー**にする。`declare mathfunc f` だけで `declare derivative f` がない状態で `∂/∂ (f x) x` を計算しようとすると、宣言環境 (`DerivativeEnv`) を参照しても見つからないので、実行時エラーとなる。

抽象的な未知関数をシンボリックに微分したい場合は、次節の**関数シンボル (`function (x)`)** を使う(`declare mathfunc` とは別機構)。

#### 関数シンボル (`function (x)`) との使い分け

Egison には `declare mathfunc` とは別に、**関数シンボル**の機構がある(詳細は [function-symbol.md](./function-symbol.md)):

```egison
declare symbol x, y
def f := function (x)        -- 1変数関数 f(x)、具体実装なし
def g := function (x, y)     -- 2変数関数 g(x, y)
```

関数シンボルは:

- **具体実装を持たない**(抽象関数)
- 連鎖律で自動微分できる。偏微分は整数インデックス `f|1`, `f|2` で表現
- `∂/∂ (f x) x = f|1 x` のように、導関数が再び `function` として残る

**`declare mathfunc` と関数シンボルの使い分け**:

| | `declare mathfunc` | `function (x)` |
|---|---|---|
| 宣言場所 | トップレベル | 値レベル(式中) |
| 具体実装 | `declare apply` で定義 | なし(抽象関数) |
| 微分 | `declare derivative` で登録(なければエラー) | 連鎖律で自動、未定義のまま残る |
| 型 | `MathValue -> MathValue` | 後述(要検討) |
| 用途 | 具体的な数学関数(`sin`, `sqrt` 等) | シンボリック計算、公式導出 |

関数シンボルの**新 CAS 型システムへの統合**(どの型になるか、`Poly` の原子集合にどう入るか、偏微分インデックスの扱い等)は未決事項。[type-cas-issues.md](./type-cas-issues.md) の課題 P を参照。

#### 差分閉性の観察型による報告

差分閉性(`Poly c [atoms]` の式を `∂/∂` で微分した結果が同じ型に留まる性質)は、**観察型機構で事後報告される**。型レベルでの静的保証は行わない。

> `Poly c [atoms]` の式を `∂/∂` で微分した結果、観察型が入力と同じ `atoms` を持つ場合、その演算は微分で閉じていると言える。観察型機構がこれを自動的に検出・報告する。

返り値が `MathValue` なので、`∂/∂` の結果を `coerce` で同じ型に戻すことが観察型と宣言型が一致する場合に常に成功する:

```egison
def f : Poly Integer [x, y] := x^2 * y + 3 * x * y^2
def df : Poly Integer [x, y] := coerce (∂/∂ f x)   -- 観察型が [x, y] と一致すれば成功
```

詳細は「観察型」セクション参照。

#### `Symbol` を第2引数に取る根拠

1. **意味的明確性**: 数学的に微分は変数（シンボル）に対して定義される
2. **`Embed` 階層との整合**: `Symbol ⊂ Factor` なので、ユーザコードでは `∂/∂ f x` のように自然に書ける

#### 利用例

```egison
declare symbol x, y

def f : Poly Integer [x, y] := x^2 * y + 3 * x * y^2

∂/∂ f x   -- => 2 * x * y + 3 * y^2 : MathValue
∂/∂ f y   -- => x^2 + 6 * x * y     : MathValue

-- 観察型が入力と一致する場合は coerce で絞り込み
def df : Poly Integer [x, y] := coerce (∂/∂ f x)

-- 定数原子を含むケース
def g : Poly Integer [sqrt 2, x] := (sqrt 2) * x^2 + 3*x
∂/∂ g x   -- => 2 * (sqrt 2) * x + 3 : MathValue

-- 合成原子を含むケース（連鎖律）
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

def f : Poly Integer [x] := x^2 + 3 * x

Sd x f   -- => (1/3) * x^3 + (3/2) * x^2 : MathValue
-- 精密な型に絞り込みたい場合は coerce を使う
def result : Poly (Frac Integer) [x] := coerce (Sd x f)
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

**すべての簡約規則は `MathValue` レベルに紐づく**。`i^2 = -1` は `GaussianInt` (`Poly Integer [i]`) 固有の規則ではなく、`i` を含む任意の `MathValue` の演算で `casNormalize` により適用される。subtype の `Ring` インスタンスは `MathValue` の演算をラップして coerce するだけなので、簡約規則は自動的に全ての subtype に波及する。

```egison
-- シンボル宣言と簡約規則は常に別々に書く
declare symbol i
declare rule auto term i^2 = -1
```

### 構文

```
declare rule [auto|名前] [term|poly|frac] LHS = RHS
```

マッチレベルを明示的に指定する:

- **`term`**: 単一の Term のモノミアル内でマッチ（積/冪のパターン）
- **`poly`**: Poly の項リスト全体でマッチ（和のパターン、multiset マッチ）
- **`frac`**: Frac の分子・分母でマッチ（商のパターン）

自動/手動の区別:

- **`auto`**: `casNormalize` 実行時に常に適用される。正規形を一意に定める規則に使う。Maxima の `tellsimp` に相当。
- **名前付き**: `simplify expr using rule_name` で明示的に適用する。Maxima の `defrule` に相当。

### 自動規則と手動規則

```egison
-- term 規則（自動）: モノミアル内の積/冪をマッチ
declare rule auto term i^2 = -1
declare rule auto term (sqrt $x)^2 = x

-- poly 規則（手動）: 項リスト全体をマッチ
-- $x で束縛、#x で「同じ値」を要求（非線形パターンマッチ）
declare rule trig_pythagorean poly (sin $x)^2 + (cos #x)^2 = 1
declare rule trig_addition poly sin ($a + $b) = sin a * cos b + cos a * sin b

-- frac 規則（手動）: 分子・分母をマッチ
declare rule rationalize_sqrt frac $x / (sqrt $y) = x * sqrt y / y
```

**非線形パターン `#x` の等価判定**: `#x` は `$x` で束縛された値との等価性を要求する。CASValue に対するこの等価性は**意味論的等価**(`Eq MathValue` の `(==)`、すなわち `v1 - v2 = 0`)を使う。これにより `(sin (a+b))^2 + (cos (b+a))^2` のような「表現は違うが意味論的に等しい」引数でもルールが適用される。

性能への影響: 各 `#x` 照合で減算 + 正規化が走るので、構造的等価より重い。しかし規則適用は既にマッチング試行自体が O(n) 以上なので、追加コストは許容範囲。

`declare rule` はトップレベル宣言のみを許可する。RHS は**パターン変数を参照できる任意の Egison 式**であり、ユーザー定義関数の呼び出しも可能。

なお、`sqrt 8 → 2 * sqrt 2` のような関数適用時のアルゴリズム的簡約は `declare rule` ではなく `declare apply`（「数学関数の適用規則」セクション参照）で定義する。`declare rule` はあくまで数式パターンの書き換えに使う。

### 手動規則の適用

```egison
declare rule trig_pythagorean poly (sin $x)^2 + (cos #x)^2 = 1

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
data RuleLevel = TermRule | PolyRule | FracRule

data ReductionRule = ReductionRule
  { ruleName       :: Maybe String           -- 手動規則の名前（auto 規則は Nothing）
  , ruleLevel      :: RuleLevel              -- マッチ対象（term / poly / frac）
  , ruleFunc       :: CASValue -> CASValue   -- LHS → RHS の書き換え関数
  , triggerSymbols :: [SymbolExpr]           -- LHS が参照するシンボル(型フィルタ用)
  }

type ReductionEnv = [ReductionRule]
```

`ruleFunc` は `declare rule [auto|名前] [term|poly|frac] <lhs> = <rhs>` の LHS パターンと RHS 式から動的に生成される書き換え関数。`ruleLevel` がマッチ対象（モノミアル内 / 項リスト / 分子分母）を決定する。LHS のパターンマッチで変数を束縛し、RHS の Egison 式を評価して結果を返す。RHS ではユーザー定義関数を含む任意の式が使える。

`triggerSymbols` は LHS が参照するシンボル/関数のリスト。型情報による規則適用フィルタに使う(次節)。

正規化関数は規則環境と値の型を参照する:

```haskell
casNormalizeWithRules :: ReductionEnv -> Type -> CASValue -> CASValue
```

### 規則適用の型情報フィルタ

規則 `R` を値 `v : T` に適用するかは、以下で決定する。**正しさには影響しない最適化** であり、`T` から `R` が絶対に適用されないと分かる場合に省く:

1. `T = MathValue` → 常に適用(何が含まれるか不明)
2. `T` のスロットが `[..]` → 常に適用(原子集合 unknown)
3. 上記以外(スロット閉じている) → `triggerSymbols(R) ∩ atoms(T) ≠ ∅` のときのみ適用
   - `atoms(T) = atoms`(入れ子型は全レベルの和集合)
   - `triggerSymbols(R)` は LHS が参照するシンボルの集合:
     - リテラル参照(例: `i^2 = -1` の `i`): 具体的な `SymbolExpr`
     - パターン参照(例: `(sqrt $x)^2 = x` の `sqrt`): 関数の `SymbolExpr`

#### 例

```egison
declare symbol i, x
declare rule auto term i^2 = -1                    -- triggerSymbols = {i}
declare rule auto term (sqrt $y)^2 = y             -- triggerSymbols = {sqrt}

def p : Poly Integer [x] := x^2 + 1
-- atoms(T) = {x}, i ∉ atoms(T), sqrt ∉ atoms(T) → どちらの規則もスキップ

def q : GaussianInt := 1 + i
-- atoms(T) = {i}, i ∈ atoms(T) → i^2 規則のみ適用

def r : Poly Integer [sqrt 2] := ...
-- atoms(T) = {sqrt 2}, sqrt ∈ triggerSymbols → sqrt 規則のみ適用

def s : MathValue := ...           -- 全規則適用
def t : Poly Integer [..] := ...    -- 全規則適用([..] あり)
```

この最適化は**正しさを保存する**: 型フィルタで省かれた規則が適用されてもマッチしないはずなので結果は変わらない。省くことで `casNormalizeWithRules` のループ内での無駄なマッチング試行を削減する。

### 関数引数内のパターン分解

関数引数のパターンマッチを含む規則（例: `sin ($a + $b) = sin a * cos b + cos a * sin b`）では、規則の左辺 `sin ($a + $b)` が `SymbolExpr` の `Apply1` コンストラクタにマッチし、その引数（`CASValue`）を `poly` マッチャーで分解して `$a + $b` を束縛する。つまり、`declare rule` の規則適用エンジンは、CAS マッチャー（`poly`, `frac` 等）を内部的に利用して関数引数内のパターンを分解する。

`declare rule` 内のパターン変数の型はすべて `MathValue` である。LHS のパターンマッチは `CASValue` のコンストラクタに対するランタイムのパターンマッチであり、型推論は関与しない。RHS の式もパターン変数を `MathValue` として参照する。

---

## CASValue のパターンマッチ

`multiset` が `Matcher a → Matcher [a]` であるのと同様に、CAS の各層をパラメトリックなマッチャーとして設計する。マッチャーの合成により、入れ子の数式構造に対するパターンマッチを統一的に扱える。

### マッチャーの定義

```egison
-- 基本マッチャー（パラメータなし）
integer     : Matcher Integer
symbol      : Matcher Symbol
factor      : Matcher Factor

-- パラメトリックマッチャー（係数マッチャーを受け取る）
poly {a} (m : Matcher a) : Matcher (Poly a ..)
term {a} (m : Matcher a) : Matcher (Term a ..)
frac {a} (m : Matcher a) : Matcher (Frac a)
```

`poly`, `term` の原子集合スロットはランタイムのマッチャーには不要（CASValue の内部構造が型ごとに異なるため、マッチャーが区別する必要がない）。型レベルでのみ `Poly a [x]` / `Poly a [x, y]` 等を区別する。

`factor` は `Factor` 型全体のマッチャーで、`symbol`/`apply1`/`quote` の和として振る舞う。ユーザーは `factor` の内部で必要に応じて `symbol`/`apply1`/`quote` パターンを使って分類できる。

### `:+` パターンの意味論

`poly m` の `:+` パターンは、多項式を**項**と**残りの多項式**に分解する。項は `Term a` 型。`multiset m` の `::` と同じ意味論（順序不問の分解）。

```
poly m       の  $a :+ $rest  →  a : Term a [atoms], rest : Poly a [atoms]
multiset m   の  $a :: $rest  →  a : a,              rest : [a]
```

`a : Term a [atoms]` はそのまま `Poly a [atoms]` を期待する関数に渡せる（`Term a [atoms] ⊂ Poly a [atoms]` の embed による）。

項をさらに分解するには `term m` マッチャーを使う。ランタイム Monomial が flat なので、**1 スロット分解**で係数と flat モノミアルの組を返す:

```
term m  の  ($c, $mono)
  → c    : a
    mono : [(Factor, Integer)]   -- flat な (原子, 冪指数) のリスト
```

ユーザーが分類したい場合は、`$mono` の各要素に対して `factor` マッチャー(内部で `symbol`/`apply1`/`quote` パターンを使う)を適用する。

### 利用例

```egison
declare symbol x, y

-- Poly Integer [x] のパターンマッチ
-- poly: 項と残りに分解
match expr as poly integer with
  | $a :+ $rest -> ...   -- a : Term Integer [x], rest : Poly Integer [x]

-- term: 係数と flat モノミアルに分解
match a as term integer with
  | ($c, $mono) -> ...
  -- c : Integer, mono : [(Factor, Integer)]

-- 必要に応じて各要素を factor マッチャーで分解
match a as term integer with
  | ($c, multiset (symbol $name $idx, $exp) $syms) -> ...
  -- syms は (Symbol, Integer) のリスト
  -- symbol パターンにマッチするもののみ束縛される

-- x と y だけの項をマッチ(非線形パターン)
match a as term integer with
  | ($c, (#x, $n) :: (#y, $m) :: []) -> ...   -- coeff * x^n * y^m

-- 入れ子: poly (poly integer) で Z[x][y] を分解
match expr as poly (poly integer) with
  | $a :+ $rest -> ...   -- 係数が Poly Integer [x]

-- frac: 分子/分母に分解
match expr as frac (poly integer) with
  | $n / $d -> ...       -- n, d : Poly Integer [x]

-- factor マッチャーでシンボルの内部構造を分解
match (sin x) as factor with
  | apply1 $f $arg -> ...   -- f : sin 関数, arg : x
```

### 安全なダウンキャスト

型の絞り込みには **パターンマッチ**（`frac` の `$n / #1` で分母が1のときだけ抽出 — 常に安全）と **`coerce`**（型注釈による自動挿入 — ランタイム検証付き、「coerce（型の絞り込み）」参照）の2つがある。

---

## 観察型 (Observed Types)

### 動機

インタラクティブな数式計算では、ユーザーは計算を始める前に型を決められないことが多い。`sqrt 2 + sqrt 3` のような式の結果の型は、計算してみないと具体化しない。静的型注釈を最小にし、評価後の `CASValue` から観察される最具体型を報告することで、型付き CAS のもう一つの運用モデルを提供する。

### 観察型の定義

観察型 (observed type) とは、評価後の `CASValue` から、以下の手続きで逆算される最も具体的な型である:

- `CASInteger` → `Integer`
- `CASFrac num den` → `Frac (join of typeOf num, typeOf den)`
- `CASPoly terms` → `Poly (join of term coefficients) [collected atoms]`
- `CASFactor (Symbol ...)` → `Symbol`
- `CASFactor (Apply1 ...)` → `Factor`(または `Poly Integer [that apply1]` として昇格)

`typeOf :: CASValue → Type` として実装する。

### 静的型と観察型の関係

静的型推論で決まる型(典型的には `MathValue` や `Poly a [..]`)と、観察型は多くの場合異なる。前者は可能性の上界、後者は実現した最具体の型。両者のギャップが `coerce` が成功するための情報である。

`τ_static >= τ_observed` が常に成り立ち、`coerce : τ_static → τ_observed` は成功する(定義上)。

### 表示フォーマット

REPL および `inspect` プリミティブでは、静的型と観察型を両方表示する:

```
> (x + sqrt 2)^2
x^2 + 2*sqrt(2)*x + 2
  static:   MathValue
  observed: Poly Integer [x, sqrt 2]
```

観察型が静的型と一致する場合、観察型のみ表示する:

```
> def p : Poly Integer [x] := x + 1
p : Poly Integer [x]
```

### 型注釈の提案

REPL は観察型を、ユーザーがコピーして `def` の注釈に使える形式で提示できる。例:

```
> def p := expandAll ((x + sqrt 2)^2)
p = x^2 + 2*sqrt(2)*x + 2
  observed: Poly Integer [x, sqrt 2]
  suggest:  def p : Poly Integer [x, sqrt 2] := expandAll ((x + sqrt 2)^2)
```

これにより、REPL で探索的に書いたコードを、型付きプログラムに昇格させる workflow が自然になる。

### 差分閉性の観察型による報告

3スロット版で型レベルで保証していた差分閉性は、観察型機構で事後報告される:

```
> def f : Poly Integer [x, y] := x^2 * y + 3 * x * y^2
> ∂/∂ f x
2*x*y + 3*y^2
  observed: Poly Integer [x, y]
  note:     differential-closed (atoms preserved)
```

微分前後で観察型の `atoms` が等しいとき、差分閉性ラベルを付与する。型レベルでの静的保証は行わないが、実用上の確認には十分。

### 既存研究との関係

観察型の発想は success typing (Lindahl & Sagonas, 2006; Erlang/Dialyzer) と近い。成功した実行から得られる型情報を事後的に報告する点で共通する。CAS の文脈への適用は本設計の貢献。soft typing (Cartwright & Fagan, 1991) や gradual typing (Siek & Taha, 2006) とも関連がある。

### 実装の位置づけ

観察型の計算は `Math/CAS.hs` の `typeOf :: CASValue → Type` として実装する。実装は再帰的で、embed/coerce 機構の逆方向に対応する。REPL 出力の拡張は Phase 7 の後半または Phase 8 として追加する。

---

## Axiom/FriCAS との比較

Axiom は「ドメインタワー」（例: `Polynomial(Fraction(Integer))`）で正規形制御を実現している。
Egison は同じ問題意識を [実行時の型昇格タワー](#実行時の型昇格タワー) — 5 レベル（`Integer` / `Frac Integer` / `Poly Integer [..]` / `Poly (Frac Integer) [..]` / `Frac (Poly Integer [..])`）に固定した runtime の昇格規則 — として取り込む。
Egison の設計は以下の点で異なる。


|           | Axiom        | Egison                             |
| --------- | ------------ | ---------------------------------- |
| 型システム     | 独自（SPAD言語）   | HM型推論 + type class                 |
| 正規形の制御    | ユーザー定義のドメインタワー | 5 レベルに固定した runtime タワー + 型注釈による絞り込み |
| ドメインの選択   | ユーザーがドメインを明示  | タワーは組み込み、ユーザーは原子集合のみ指定        |
| 多項式の表現    | 標準多項式        | ローラン多項式（負の冪を許可）                    |
| 型変換       | `::` 演算子で明示的 | embed の自動挿入（demote は明示のみ）          |
| 内部表現      | ドメインごとに固定    | 型構造から構成的に決定                        |
| シンボル集合の制御 | なし（全シンボル対等）  | 1スロット (`[atoms]`) の閉じた/開いた集合を選択可能。観察型機構が評価後に具体的な原子集合を報告 |
| 微分の型保存    | なし           | 観察型で事後報告(静的保証なし)          |
| テンソルとの統合  | なし           | tensorMap の自動挿入（論文で証明済み）           |
| 新しい数の導入   | ドメイン定義（SPAD） | `declare mathfunc`           |
| 観察型        | なし           | 評価後に最具体型を自動報告（Success typing 系列） |


特に tensorMap 挿入と embed 挿入が単一の型推論パスで統一的に処理される点が新しい。

---

## 実装 TODO

### Phase 1: CASValue の基盤実装（完了）

`CASValue` データ型（`CASInteger`, `CASFactor`, `CASPoly`, `CASFrac`, `CASTerm`）を `Math/CAS.hs` に定義し、演算関数（`casPlus`, `casMult`）とローラン多項式の正規化（降冪順、零の除去、モノミアルGCD簡約）を実装した。`SymbolExpr` も `CASValue` を参照するよう移行済み。

> **設計方針（1スロット型 + flat ランタイム）**: 原子集合スロットは**型レベルのみ**に設け、ランタイムの `Monomial = [(SymbolExpr, Integer)]`（flat な単一リスト）は現状のまま維持する。これにより Phase 1 で確立した `casPlus`/`casMult`/`casNormalize` のロジックを変更する必要がなくなる。詳細は「構成的な内部表現 > Haskell での表現」および「境界での名前集合照合」を参照。

### Phase 1.5: SymbolSet の SymbolExpr 化

ランタイム分類関数（`classifyFactor` / `hasFreeSymbol`）も持たない。型注釈との照合に必要な最小限の変更のみ行う。

- [ ] `SymbolSet` を1スロット版 `SymbolSetClosed [SymbolExpr] | SymbolSetOpen | SymbolSetVar TyVar` に
- [ ] パーサーで `Poly Integer [sqrt 2, sin x]` のような1スロット注釈をパース
- [ ] `SymbolExpr` の構造的 `Eq` を確認（照合に使うため）
- [ ] ~~disjoint 制約の検証~~ → **不要**(1スロット化により不要)
- [ ] 名前集合メンバーシップ関数（`SymbolExpr -> [SymbolExpr] -> Bool`）を実装
- [ ] `casPlus`/`casMult`/`casNormalize` は**変更不要**（flat Monomial のままで OK）
- [ ] mini-test: 正規形比較のテスト

### Phase 2: 型システムへの統合（1スロット版）

- `Type` ADT に `TPoly Type SymbolSet`、`TFrac Type`、`TFactor` を追加（`Types.hs`）
- `SymbolSet` の定義（`SymbolSetClosed [SymbolExpr]` / `SymbolSetOpen` / `SymbolSetVar TyVar`）
- パーサーで `Poly Integer [x, y]` / `Poly Integer [..]` / `Frac a` / `Factor` をパース
- 型推論での `Poly` 型の単一化とシンボル集合の包含判定（`S₁ ⊆ S₂`）
- `join` の実装（`Join.hs`: `joinTypes`, `isSubtype`, `symbolSetSubset`）
  - `joinSymbolSets` の `otherwise` 分岐を和集合 `S₁ ∪ S₂` に変更する
- 開いた `[..]` のフレッシュ型変数への脱糖（`freshenOpenSymbolSets`）
- `TFactor` は単一型として維持、`TConstantFactor` / `TAppliedFactor` は導入しない
- `Embed` 型クラスと coercive subtyping は Phase 5.5 として後続実装予定

### Phase 3: ScalarData の CASValue 置換（完了）

`ScalarData` を完全に削除し、`CASData CASValue` に統一した。`Math/Expr.hs`（旧 `ScalarData`, `PolyExpr`, `TermExpr`, 旧 `SymbolExpr`）を削除し、全モジュール（`Primitives/Arith.hs`, `Core.hs`, `Data.hs`, `Tensor.hs` 等）を `CASValue` ベースに移行完了。`cabal test` 全21テストパス。

### Phase 4: CASValue のプリミティブパターンマッチ（完了）

`CASData CASValue` を直接パターンマッチできるプリミティブパターンを実装し、`fromMathExpr`/`toMathExpr` 変換関数を不要にした。

**実装したプリミティブパターン**:

実装は2層構造になっている。Layer 1 は AST レベルのプリミティブデータパターン（`PDxxxPat`, 先頭大文字）、Layer 2 は `inductive pattern MathValue` 経由でユーザーに公開される名前（先頭小文字）。Layer 2 のパターンは `mathValue` マッチャーの `with` 節で Layer 1 PDP に展開される。


| Layer 1（プリミティブデータパターン） | Layer 2（`inductive pattern MathValue` 経由） |
| ----------------------------- | ------------------------------------------ |
| `Frac $ $` (`PDFracPat`)      | `frac $ $` / `$ / $`                       |
| `Plus $` (`PDPlusPat`)        | `poly $` ※ Phase 5 で `:+` に移行予定             |
| `Term $ $` (`PDTermPat`)      | `term $ $`                                  |
| `Symbol $ $` (`PDSymbolPat`)  | `symbol $ $`                                |
| `Apply1 $ $` (`PDApply1Pat`)  | `apply1 $ $`                                |
| `Quote $` (`PDQuotePat`)      | `quote $`                                   |

各パターンの対象 `CASValue` コンストラクタと抽出内容:

- `Frac` / `frac` / `/` — `CASFrac num den` から分子・分母（各 `CASValue`）
- `Plus` / `poly` — `CASPoly terms` から項リスト（各 `CASValue`）
- `Term` / `term` — `CASTerm coeff mono` から係数（`CASValue`）とモノミアル
- `Symbol` / `symbol` — `CASFactor (Symbol name indices)` から名前とインデックスリスト
- `Apply1` / `apply1` — `CASFactor (Apply1 fn arg)` から関数と引数（各 `CASValue`）
- `Quote` / `quote` — `CASFactor (Quote expr)` から引用式（`CASValue`）


**現状の制約**: これらのプリミティブパターンは `mathExpr` マッチャー内でのみ使われ、係数やシンボル集合を型に応じて区別できない。次の Phase 5 でパラメトリックマッチャーに拡張する。

**保持するもの**: `inductive pattern MathValue` / `IndexExpr` 宣言、`CASIndexData`。

### Phase 5: パラメトリックマッチャー（poly, div, term）

**目標**: 現在の固定的な `mathExpr` マッチャーを、`list` や `multiset` と同様のパラメトリックなマッチャーに拡張する。型の入れ子構造に対応したマッチャーをユーザーが自由に合成できるようにする。

#### 背景

`mathExpr` マッチャーは固定的なモノリシック構造で、以下ができない:

- `Poly (Frac Integer) [x]` と `Poly Integer [x]` で異なるマッチャーを使い分ける
- 入れ子構造（`Poly (Poly Integer [x]) [y]`）の各層に適切なマッチャーを指定する

目標は型とマッチャーの構造を一致させること:

```
型                                  マッチャー
Integer                             integer
Symbol                              symbol
Factor                              factor
Frac Integer                        frac integer
Poly Integer [x]                    poly integer
Poly (Frac Integer) [x]             poly (frac integer)
Frac (Poly Integer [x])             frac (poly integer)
Poly (Poly Integer [x]) [y]         poly (poly integer)
Tensor (Poly (Frac Integer) [x])    tensor (poly (frac integer))
```

マッチャーは係数マッチャーのみを引数に取る。原子集合スロットはランタイムのマッチングに影響しないため、マッチャー引数には含めない（型レベルでのみ区別）。

#### 実装方針

`poly`, `frac`, `term` を純粋な Egison のマッチャー定義（`matcher` 式）として実装し、プリミティブは `casToTerms` 等の補助関数のみとする。既存の `PDPlusPat`, `PDFracPat`, `PDTermPat` への変更は最小限で済む。

パターン環境と値環境は分離されているため、`inductive pattern MathExpr` のパターンコンストラクタ `poly`, `div`, `term` と同名のマッチャー関数 `def poly ...` は衝突しない。`frac` はパターンコンストラクタ `div` と名前が異なるため衝突の問題は生じない。

#### Step 5.0: 基本マッチャーの定義

CAS 型に対応する基本マッチャーを定義する。`symbol`, `factor` の2種類を用意する(1スロット化により `constantFactor` / `appliedFactor` は不要)。

```egison
def integer : Matcher Integer := something

-- symbol: symbolName パターンで名前とインデックスを抽出
def symbol : Matcher Symbol := matcher | symbolName $ $ as ... | ...

-- factor: Factor 全体（symbol, apply1, quote パターンをすべて持つ）
def factor : Matcher Factor := matcher | symbol $ $ as ... | apply1 $ $ as ... | quote $ as ... | ...
```

`symbol` と `factor` は、`term` マッチャーの flat モノミアル分解が返す各要素にマッチする目的で使う。ランタイム Monomial は flat であり、`term` マッチャーは型情報を参照しない(「構成的な内部表現 > 境界での名前集合照合」参照)。既存の `PDSymbolPat`, `PDApply1Pat`, `PDQuotePat` に対応するプリミティブ関数を使う。

- [ ] `integer`, `symbol`, `factor` マッチャーを `lib/math/expression.egi` に定義
- [ ] `extractSymbolName`, `extractApply1`, `extractQuote` 等のプリミティブ関数追加

#### Step 5.1: `poly` パラメトリックマッチャーの実装

`:+` パターンは多項式を**項**と**残りの多項式**に分解する。項は `Term a` 型（係数 + flat モノミアルの組）。

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

`term m` の戻り値は係数と flat モノミアル(`[(Factor, Integer)]`)のタプル。ランタイム Monomial が flat であることを反映し、マッチャーで分類分解は行わない。

```egison
def term {a} (m : Matcher a) : Matcher (Term a ..) :=
  matcher
    | ($, $) as (m, assocMultiset factor) with
      | $tgt -> [(termCoeff tgt, termMonomial tgt)]
    | $ as (something) with
      | $tgt -> [tgt]
```

- 項を（係数, flat モノミアル）に分解
- 係数は `m` でマッチ、モノミアルは `assocMultiset factor` でマッチ
- `termCoeff` / `termMonomial` はプリミティブ関数として提供(型情報は不要)
- **実装ノート**:
  ```haskell
  termCoeff    (CASTerm c _) = c
  termMonomial (CASTerm _ m) = m   -- flat [(SymbolExpr, Integer)] をそのまま返す
  ```
- ユーザーが分類したい場合は、得た `$mono` の各要素に対して `factor` マッチャー(内部で `symbol`/`apply1`/`quote`)を適用する
- [ ] `term` マッチャー関数を `lib/math/expression.egi` に定義
- [ ] `termCoeff`, `termMonomial` のプリミティブ関数追加
- [ ] mini-test: `term integer` での flat 分解

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
  - `match expr as poly integer with | $a :+ _ -> ...` で `a : Term Integer [..]` を推論
  - `match a as term integer with | ($c, $m) -> ...` で
    `c : Integer`, `m : [(Factor, Integer)]` を推論

#### Step 5.6: テストと検証

- [ ] 基本テスト: `integer`, `symbol`, `factor`, `poly integer`, `frac integer`, `term integer`
- [ ] 入れ子テスト: `poly (poly integer)`, `frac (poly integer)`
- [ ] 複合テスト: `frac (poly (frac integer))`
- [ ] flat モノミアル分解テスト: `term integer` で `($c, $m)` のパターン
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
  - `Embed Symbol Factor` — 型レベルのみの埋め込み
  - `Embed Factor (Poly Integer [..])` — 原子集合に入る
  - `Embed (Poly a [..]) (Poly b [..])` where `Embed a b`
  - `Embed (Poly a [atoms₁]) (Poly a [atoms₂])` where `atoms₁ ⊆ atoms₂`
  - `Embed (Term a [..]) (Poly a [..])` — `CASTerm` → `CASPoly`（1要素リストで包む）

各 `[..]` はインスタンスヘッド内ではパラメトリックな型変数として扱われるため、これらのインスタンスは任意のスロット構成に対して機能する。演算ロジックの特化インスタンスは不要（簡約規則は `MathValue` の `casNormalize` で処理される）。ただし型レベルでは解決時に具体的な原子集合に特化され、戻り値の型が保存される。

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
    3. 閉じた Poly 同士でスロットが異なる場合は `join` で和集合を計算し双方を embed
    4. ターゲット型が既知で逆方向（一般 → 具体）なら `coerce` を挿入
    5. いずれでもなければ型エラー
- 推移的な変換（`embed . embed`）の合成
  - 例: `Integer → Poly Integer [x] → Poly (Frac Integer) [x]`
  - グラフ上の最短経路で `embed` を連鎖
- `Embed` 制約の解決と辞書渡し
  - 型クラス解決機構と連携

#### Step 5.5.3a: coerce の自動挿入

- 型注釈で具体型が指定されており、推論された型が包含関係の逆方向にある場合に `coerce` を挿入
  - 例: `def x : Poly Integer [sqrt 2] := sqrt 2` で `sqrt 2 : Poly Integer [..]` → `coerce (sqrt 2) : Poly Integer [sqrt 2]`
  - 例: `def n : Integer := sqrt 4` で `sqrt 4 : Poly Integer [..]` → `coerce (sqrt 4) : Integer`
- `Coerce` 型クラスの定義
  ```egison
  class Coerce a b where
    coerce :: a -> b    -- ランタイム検証付き
  ```
- 基本インスタンスの実装（`Math/CAS.hs` にランタイム検証関数）
  - `Coerce (Poly a [..]) (Poly a [atoms])` — 各原子が `atoms` の名前集合に含まれるかを検証
  - `Coerce (Poly a [..]) Integer` — 全モノミアルが空で定数項のみかを検証
  - `Coerce (Poly a [..]) Symbol` — 単一項で Monomial が単一原子1乗、その原子が `Symbol` コンストラクタかを検証
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
- 原子集合拡大テスト
- join テスト（異なる原子集合の和集合）
- 推移的 embed テスト（`Integer → Frac Integer → Poly (Frac Integer) [..]`）
- 型クラス `Ring` の解決テスト

### Phase 6: ライブラリ関数の再実装

Phase 5.5 の基盤（`Embed`, `MathValue`, `Ring MathValue`）が完成した後、既存のライブラリ関数を新しいマッチャー（`poly m`, `frac m`, `term m`）と `MathValue` 上の関数として再実装する。

#### Step 6.1: `expandAll` の再実装

`expandAll : MathValue -> MathValue` を再実装。`CASValue` のコンストラクタをパターンマッチし、`CASPoly` は展開、`CASFrac` は分子・分母に再帰適用。

#### Step 6.2: `substitute` の再実装

`substitute : List (Factor, MathValue) -> MathValue -> MathValue` を再実装。正規化は Phase 7.3 まで `mathNormalize` を使用。

#### Step 6.3: 偏微分演算子 `∂/∂` の再実装

「微分演算子 `∂/∂`」セクションの設計に基づき、`Differentiable` 型クラスの各インスタンス（`Integer`, `Symbol`, `Term`, `Poly`, `Frac`, `MathValue`）を実装する。`declare derivative` の仕組みも合わせて実装。テンソル対応は `tensorMap2` の既存ラッパーを維持。

#### Step 6.4: `coefficients` / `coefficient` の再実装

```egison
def coefficients {a} (f : Poly a [..]) (x : Symbol) : [a] :=
  ...  -- poly m マッチャーで各項から x の冪ごとに係数を収集
```

- `coefficients`, `coefficient` を再実装
- mini-test: `coefficients ((x^2 + 3*x + 1) : Poly Integer [x]) x` → `[1, 3, 1]`

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
Frac (Poly Integer [x])           → CASFrac { num: CASPoly [...], den: CASPoly [...] }
Poly (Frac Integer) [x]            → CASPoly [CASTerm (CASFrac ...) monomial, ...]
Poly (Poly Integer [x]) [y]       → CASPoly [CASTerm (CASPoly [...]) monomial, ...]
```

型によって入れ子の順序が変わるため、ルールの適用レベルは構文で明示する（`declare rule` の `term`/`poly`/`frac` キーワード）:

```egison
-- term 規則: CASTerm のモノミアル内に対して multiset マッチ
declare rule auto term i^2 = -1
declare rule auto term (sqrt $x)^2 = x

-- poly 規則: CASPoly の項リストに対して multiset マッチ
declare rule trig_pythagorean poly (sin $x)^2 + (cos #x)^2 = 1

-- frac 規則: CASFrac の分子・分母に対してマッチ
declare rule rationalize_sqrt frac $x / (sqrt $y) = x * sqrt y / y
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

- `TopExpr` に `DeclareRule (Maybe String) RuleLevel Expr Expr` コンストラクタを追加（`RuleLevel = TermRule | PolyRule | FracRule`）
- `declare rule [auto|名前] [term|poly|frac] <lhs> = <rhs>` のパーサー追加
- mini-test: パースだけのテスト（実行はまだしない）

#### Step 7.5: 自動規則のデシュガーと実行

`declare rule auto` をパースし、`ReductionEnv` に登録し、`casNormalizeWithRules` で適用する。

- `RuleLevel` に応じたマッチ対象の決定（`term` → モノミアル内、`poly` → 項リスト、`frac` → 分子分母）
- 書き換え関数の生成（LHS → RHS の `CASValue -> CASValue` 関数を動的に生成）
- `EnvBuilder.hs`: `DeclareRule Nothing` を規則環境に追加
- `EvalState` への規則環境の追加と伝搬
- mini-test: 自動規則の動作テスト
  ```egison
  declare symbol j
  declare rule auto term j^2 = -1
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
  declare rule trig_pythagorean poly (sin $x)^2 + (cos #x)^2 = 1
  def expr := (sin x)^2 + (cos x)^2 + 1
  simplify expr using trig_pythagorean  -- => 2
  ```

#### Step 7.7: 既存ハードコードルールの移行と `Rewrite.hs` 削除

`Rewrite.hs` の全ハードコードルールを `declare rule auto` に移行し、`Rewrite.hs` を削除する。

- term 規則の移行（`i^2=-1`, `(sqrt $x)^2=x` 等）
- poly 規則の移行（`w^2+w+1=0`, sin/cos, rtu 等）
- frac 規則の移行（有理化等）
- `declare apply` への移行（`sqrt` 等の関数適用時簡約）
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

### Phase 8: 観察型機構

観察型 (observed type) を実装する。評価後の `CASValue` から最も具体的な型を逆算してユーザーに報告する機構。

- [ ] `typeOf :: CASValue → Type` を `Math/CAS.hs` に実装
- [ ] 入れ子 `CASValue`（多段の `CASPoly`、`CASFrac` の内部など）の再帰的 typeOf
- [ ] REPL での静的型 + 観察型の表示
- [ ] `inspect` プリミティブの追加
- [ ] 型注釈提案機能（観察型をコピペ可能な注釈として出力）
- [ ] 差分閉性ラベルの付与（`∂/∂` 前後の atoms 比較）
- [ ] mini-test: REPL 出力、`inspect` 呼び出し、各型での typeOf
