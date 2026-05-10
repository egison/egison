# Egison CAS 型システム設計

> **関連ドキュメント**: ユーザによるタワー拡張の将来構想は [type-cas-tower.md](./type-cas-tower.md) を参照。

> **メモ**: 本ドキュメント中の `coerce` / `embed` (typeclass) は実装上は **`reshape` primitive 一本に統一**されている。型注釈 (`def x : T := e` または `(e : T)`) を書くだけで AST elaboration が `IReshape T e` を挿入し、runtime の `casReshapeAs T v` が CAS 構造を target type に書き換える。設計説明文中の `coerce`/`embed` 用語は意味的に `reshape` 経由として読み替えてよい。

## 概要

Egisonの数式処理システム(CAS)のための型システムの設計方針をまとめる。

### 基本原則

**`MathValue` が第一級の計算型**である。`1`, `1 + i`, `x + x^2`, `x + sin x` はすべて `MathValue` として自由に計算できる。算術演算（`+`, `*` 等）と簡約規則（`i^2 = -1` 等）は `MathValue` 上で動作する。一方 **微分 `∂/∂` は `MathValue` には定義しない** (導関数が登録されていない合成原子を含みうるため)。微分は具体型 (`Integer` / `Factor` / `Poly` / `Frac` 等) のインスタンスとして提供し、`MathValue` 値を微分するには `coerce` で具体型に絞り込む必要がある。

**`Poly a atoms` 等は `MathValue` の subtype** であり、「この式はこの代数構造に属する」という型注釈 + 「その構造の正規形で表示・保持せよ」という指示として機能する。内部表現は `MathValue` と同じ `CASValue` で、subtype への `coerce` 時に所属検証と正規化を行う。

```egison
-- 算術はすべて MathValue として自由に計算
1 + i                    -- : MathValue
x + x^2                  -- : MathValue
x + sin x                -- : MathValue

-- 微分は具体型に絞り込んでから (sin x : Factor) と注釈)
∂/∂ (sin x : Factor) x   -- : MathValue (= cos x)

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
| **subtype (`Poly a atoms`, `Frac a` 等)** | 所属検証 + 正規形の保証。subtype 上の `Ring` は `MathValue` の演算をラップ + coerce |
| **`Differentiable` 型クラス** | 微分演算の提供 |
| **観察型機構** | `typeOf :: CASValue → Type` による評価後の具体型報告 |

### 宣言環境 (Declaration Environment)

`declare symbol`, `declare mathfunc`, `declare apply`, `declare rule`, `declare derivative` などの **`declare` 系宣言**は、プログラム読み込み時の専用のプリパスで一括収集され、大域的な宣言環境 (`DeclareEnv`) に登録される。

型推論・評価・正規化・マッチングの各フェーズは、この環境を自由に参照できる。

**`class` / `instance` 宣言は別管理**: 型クラス機構は既に実装済みで、`EvalState` の独立フィールド (`classEnv :: ClassEnv` と `instanceEnv :: InstanceEnv`) で管理される。`declare` 系とは構文 (`class C a where ...` / `instance ... where ...`) もパーサ経路 (`ClassDeclExpr` / `InstanceDeclExpr`) も別。本節の `DeclareEnv` には含めない。

#### 構成要素

| 環境キー | 内容 | 参照箇所 |
|---|---|---|
| `"symbol"` | `declare symbol` で宣言されたシンボル | パース(注釈内の原子の正規化)、`Symbol` 型の値の識別 |
| `"mathfunc"` | `declare mathfunc` で宣言された関数(シグネチャ) | パース(原子式の認識)、型推論 |
| `"apply"` | `declare apply` の適用規則 | 関数適用時の簡約 |
| `"rule"` | `declare rule` の書き換え規則 | `casNormalizeWithRules` |
| `"derivative"` | `declare derivative` の導関数登録 | `Differentiable Factor` の連鎖律 |

#### 重要な帰結

**宣言の順序制約はない**: プリパスで全宣言を先に集めるので、`declare mathfunc sqrt` より前の位置に `def f : Poly Integer [sqrt 2] := ...` と書いても有効。Haskell の型クラス宣言と同じ扱い。

**相互参照が可能**: `declare apply sqrt x := ... sqrt ... ` のような再帰参照や、`declare rule` の RHS が他の `declare mathfunc` 関数を呼ぶことも許される。

**`casNormalize` は環境依存**: 正規化関数は `DeclareEnv` の `"rule"` エントリを参照する。同じ `CASValue` でも環境が異なれば(例: ルールを差し替えた `--no-normalize` 相当モード)結果が変わる。

**ロード時のエラー検出**: 宣言の整合性(例: `declare derivative f = g` の `f` が `declare mathfunc` 済みか、`declare rule` の LHS が有効か)はプリパスで検証する。

#### `declare derivative` の位置づけ

他の declare 種別 (`symbol` / `mathfunc` / `apply` / `rule`) は CAS の中核機構 (パーサ、関数適用、正規化) に直接紐づくのに対し、**`declare derivative` だけはユーザーランドの型クラス `Differentiable` のインスタンス補助データ**という性質を持つ。将来 `Integrable` などの類似機構が増えた場合に、それぞれを built-in declare として追加するのは持続可能ではない。

**短期方針 (Phase 6.3 〜 7.x)**: `declare derivative` を他のコア declare と同じく **built-in 構文** として実装する。`DeclareEnv` の `"derivative"` エントリで管理し、`Differentiable Factor` インスタンスがランタイムに参照する。実装が単純で、Phase 7 までの整合性が保てる。

**中期方針 (Phase 8 以降)**: 一般化された **`declare-key` 機構** を導入し、`declare derivative` をライブラリ層 (`lib/core/cas.egi`) に押し出す。

```egison
-- 中期 (移行後): ライブラリで declare-key を予約
declare-key derivative

-- 同じ構文で利用可能 (互換性保持)
declare derivative sin = cos
```

これにより:
- `Integrable`, `Substitutable` など独自型クラスを定義するユーザーが、自分のクラス用に `declare-key integral` 等を予約して同様の登録機構を使える
- コア層は `symbol` / `mathfunc` / `apply` / `rule` の 4 種に絞り込まれ、対称性が向上

詳細仕様は別途「`declare-key` 機構」セクションで定義予定 (Phase 8 着手前に確定する)。

#### 実装上の構造

`DeclareEnv` は **キーで型が変わる動的 Map** として実装し、その上に型付きアクセサを被せる。新しい `declare X` 構文を追加するときに `EvalState` の構造を変えなくて済む拡張性が利点:

```haskell
-- 基盤層: 動的 Map
newtype DeclareEnv = DeclareEnv (Map String Dynamic)

insertDeclare :: Typeable a => String -> a -> DeclareEnv -> DeclareEnv
lookupDeclare :: Typeable a => String -> DeclareEnv -> Maybe a

-- 型付きアクセサ層 (declare 種別ごと)
symbolEnv     :: DeclareEnv -> Map String SymbolExpr
symbolEnv     = fromMaybe Map.empty . lookupDeclare "symbol"

mathFuncEnv   :: DeclareEnv -> Map String MathFuncSig
mathFuncEnv   = fromMaybe Map.empty . lookupDeclare "mathfunc"

applyRuleEnv  :: DeclareEnv -> Map String ApplyRule
applyRuleEnv  = fromMaybe Map.empty . lookupDeclare "apply"

reductionEnv  :: DeclareEnv -> [ReductionRule]
reductionEnv  = fromMaybe []        . lookupDeclare "rule"

derivativeEnv :: DeclareEnv -> Map String CASValue
derivativeEnv = fromMaybe Map.empty . lookupDeclare "derivative"
```

`EvalState` には `declareEnv :: DeclareEnv` の **単一フィールド** を持たせる。既存の `classEnv :: ClassEnv` / `instanceEnv :: InstanceEnv` はそのまま (型クラス機構は実装済みで独立):

```haskell
data EvalState = EvalState
  { ...
  , declareEnv  :: DeclareEnv     -- declare 系の統一登録 (新規追加)
  , classEnv    :: ClassEnv       -- 既存: 型クラスの型推論用
  , instanceEnv :: InstanceEnv    -- 既存: ランタイムディスパッチ用
  , ...
  }
```

**2 パス構造**:
- Pass 1: プログラム全体を走査、`declare X` を `declareEnv` に登録
- Pass 2: `declareEnv` を参照しながら型推論・評価

`Ring MathValue` 等の型クラスメソッドが `declareEnv` を参照する場合は、評価モナド (`EvalT`) の `gets declareEnv` 経由でアクセスする (詳細は Phase 7.2 の `casNormalizeWithRules` の実装参照)。

---

## 型の構成要素

### 組み込み型


| 型                      | 意味                                      |
| ---------------------- | --------------------------------------- |
| `MathValue`            | **第一級の計算型**。全ての CAS 計算はこの型上で行う。ランタイム表現は `CASValue`。簡約規則もすべてこの型に紐づく |
| `Integer`              | 基本型。整数。`MathValue` の subtype                                  |
| `Frac a`                | `a` の分数体/分数環。`MathValue` の subtype          |
| `Symbol`               | `declare symbol` で宣言された平の不定元（例: `x`, `y`, `r`, `θ`）。`Factor` の真の部分型 |
| `Factor`               | 原子的な数式要素全体(`Symbol` / `ApplyN` / `FunctionData` / `Quote` ベースの合成原子の和)。`MathValue` の subtype |
| `Term a atoms`        | `Poly a atoms` の項。係数（型 `a`）とモノミアルを持つ補助型 |
| `Poly a atoms`        | `a` を係数とするローラン多項式環。`MathValue` の subtype。内部表現は `MathValue` と同じ `CASValue` で、型注釈による coerce 時に所属検証 + 正規化を行う |
| `Poly a [..]`            | 原子集合をフレッシュ型変数で開いたローラン多項式型（開いた多項式型）。`MathValue` の subtype |
| `Tensor a`             | `a` を成分とするテンソル                          |

**Symbol 型と Factor 型の扱いについての注記**: `Symbol` は型として残す(`∂/∂` の第2引数の型制約として機能するため)。`Factor` 型も維持する。ただし `ConstantFactor` / `AppliedFactor` のような Factor 3分類は型システムには持たない。理由: 誤注釈の foot-gun を避けるため、かつ `Differentiable Factor` が always-AF 規則 + `Symbol`/`ApplyN`/`FunctionData`/`Quote` のランタイム構成子で振り分けるので、型レベルでの分類は冗長。

### 型エイリアス

ユーザは `Poly` の特殊化に型エイリアスを定義できる:

```egison
type GaussianInt := Poly Integer [i]              -- Z[i]
type Zsqrt2      := Poly Integer [sqrt 2]         -- Z[√2]
type RatFunc     := Frac (Poly Integer [x])        -- Z[x] の分数体
```

### Poly はローラン多項式環

`Poly a atoms` は標準的な多項式環ではなく、ローラン多項式環を表す。
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
- 部分型関係: `Poly a atoms ⊂ Poly a [..]` が常に成立
- `join` で一方が `[..]` の場合、結果も `[..]`

```egison
-- ユーザーが書く型
polyAdd :: Poly a [..] -> Poly a [..] -> Poly a [..]

-- 各 [..] は独立のワイルドカードで、引数間・引数と結果の間で原子集合を共有しない
```

入出力で同じ原子集合を共有したい場合は、名前付き型変数を明示する:

```egison
polyAdd :: {atoms} Poly a atoms -> Poly a atoms -> Poly a atoms
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
polyJoinAdd :: {atoms₁, atoms₂} Poly a atoms₁ -> Poly a atoms₂ -> Poly a (atoms₁ ∪ atoms₂)
```

##### `[..]` 採用の理由

HM 互換で `{atoms₁ ⊆ atoms₂}` のような集合包含制約付き単一化を導入せずに済む。`[..]` を any にすることで:
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

**ランタイムの `Monomial` は flat な `(原子, 冪指数)` のリスト**として保持する。型の原子集合スロット (`atoms`) は「この原子集合がスロットに属することを型レベルで表明する」契約であり、ランタイム値には分類タグを保持しない。

原子のメンバーシップ判定は、以下の境界操作でのみ必要に応じて行う：

- `coerce : MathValue → Poly a atoms`（所属検証: 原子が `atoms` に含まれるかを検証）
- `Differentiable Factor` のディスパッチ（always-AF 規則で分類なしに動作）

`term m` マッチャーはランタイム Monomial が flat であることを反映し、**1 スロット(flat)で分解**する(`($c, $m)` で `$m : [(Factor, Integer)]`)。型レベルでは `Factor`、ランタイム表現は `SymbolExpr` で、両者は 1 対 1 対応する(ユーザーから見える型は常に `Factor`)。

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
- embed（`Factor → Poly`, 原子集合拡大 `Poly a atoms₁ → Poly a atoms₂` 等）の多くがランタイム no-op になり、coherence が自明に成立

分類が必要なのは以下の**境界操作**のみ（次節で詳述）:

1. `coerce : MathValue → Poly a atoms` の所属検証(原子が `atoms` に含まれるかの検証)
2. `Differentiable Factor` のディスパッチ(always-AF 規則)

どちらもワンショット操作であり、正規化ループの内側で走らないので、「`sin(x-x)` の正規化で分類が変わる」問題は起きない。

`term m` マッチャーは flat に分解するので、ランタイムに型情報を渡す必要がない。

### 境界での名前集合照合

ランタイム分類関数（`classifyFactor` や `hasFreeSymbol` 等の自由シンボル解析）は**持たない**。代わりに、`Poly a atoms` の閉じた型が指定された境界で、**型注釈に現れる原子の名前集合との照合**を行う。

#### 設計原則

- 原子のメンバーシップは**ユーザーの型注釈が決定**する。ランタイム解析はしない
- 境界操作では「この原子は `atoms` に含まれるか」を注釈集合のルックアップで決める

#### SymbolExpr の正規形比較

注釈 `[sqrt 2]` とランタイム値中の `ApplyN (DeclaredMathFunc "sqrt") [CASInteger 2]` を照合するため、パーサが注釈を**正規形の `SymbolExpr`** に変換し、照合は `SymbolExpr` の構造的 `Eq` で行う。Pretty-print ベースの文字列比較は脆弱なので避ける。

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

**1. `reshape` の構造書き換え** (旧 `coerce` 検証):

旧設計では `Poly a [..] → Poly a atoms` で原子が `atoms` に含まれているかをランタイム検証する `coerceToClosed` 関数を想定していた。Phase C で **「trust the annotation」原則** を採用したため、検証は行わず `casReshapeAs` が target type に向けて構造を書き換えるのみ。注釈と値が不整合な場合は canonical 形のまま残り downstream で問題が顕在化する設計。

**2. `Differentiable Factor` のディスパッチ**:

ランタイム分類はせず、**always-AF 規則**で動作する:

```haskell
diffFactor :: SymbolExpr -> Symbol -> CASValue
diffFactor sym@(Symbol _ _ _)         s = if sym == s then casOne else casZero
diffFactor (ApplyN ref args)          s = applyChainRule ref args s     -- 常に連鎖律
diffFactor (FunctionData name args)   s = applyFuncSymChainRule name args s
diffFactor (Quote expr)               s = applyChainRuleToQuote expr s
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
  | ApplyN MathFuncRef [CASValue]      -- declare mathfunc 関数の適用 (1〜N 引数)
  | Quote CASValue
  | FunctionData String [CASValue]     -- function (x, y, ...) の関数シンボル
                                        -- (名前 + 引数リスト、ApplyN とは独立)
  ...

-- ApplyN の第 1 引数は declare mathfunc 由来の関数のみ
data MathFuncRef = DeclaredMathFunc String
```

`CASValue` → `CASTerm` → `Monomial` → `SymbolExpr` → `CASValue` という相互再帰になるが、Haskell では問題ない。`sin(x + 1)` のような式で `ApplyN` の引数が `CASValue`（多項式 `x + 1`）になるケースを自然に表現できる。

**`ApplyN` と `FunctionData` の使い分け**:

| 由来 | 構成子 | 例 |
|---|---|---|
| `declare mathfunc` 由来の CAS 関数 | `ApplyN (DeclaredMathFunc name) args` | `sin x` → `ApplyN (DeclaredMathFunc "sin") [x]` |
| `function (x, y, ...)` 由来の関数シンボル | `FunctionData name args` | `def f := function (x, y)` → `FunctionData "f" [x, y]` |

両者を分ける根拠は egison-book [funcsym.tex](../egison-book/ja/funcsym.tex):

- `function (x, y)` は内部的に `Function name [args]` という独立構成子で保持される ([funcsym.tex:121-131](../egison-book/ja/funcsym.tex#L121-L131))
- `f 0 1` のような関数シンボルへの「適用」は通常の関数適用ではなく **引数リストの置換** ([funcsym.tex:48-55](../egison-book/ja/funcsym.tex#L48-L55))。結果も `Function f [0, 1]` のまま `FunctionData` 構成子に留まる
- 関数シンボルの名前は `def g := function (...)` の左辺の単純な識別子 (`g`) に固定される ([funcsym.tex:161-163](../egison-book/ja/funcsym.tex#L161-L163))。よって `FunctionData` の第 1 引数は `String` で十分

**`MathFuncRef` を別型にする利点**: `ApplyN` の第 1 引数の許容範囲が型レベルで「`declare mathfunc` 宣言済み関数」に絞られる。`lookupDerivative` は `MathFuncRef → String → Maybe CASValue` で曖昧性なくキーが取れる。ラムダや任意の関数を `ApplyN` の関数側に置けないので、不変条件が型で保証される。

### 演算の構成性

各層の演算は内側の型の演算を使って定義される。

- `Poly a atoms` の `(+)`: 同じ単項式の項をまとめ、係数の加算は `a` の `(+)` で行う
- `Poly a atoms` の `(*)`: 係数の乗算は `a` の `(*)` で行い、単項式を結合する（冪指数の加算、負も可）
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
| `Poly a atoms` | `CASPoly []`（空の項リスト） |
| `Frac a` | 分子が `a` の零 / 分母が `a` の1。例: `Frac Integer` → `CASFrac (CASInteger 0) (CASInteger 1)` |
| `Frac (Poly a atoms)` | `CASFrac (CASPoly []) (CASPoly [CASTerm <one> []])` （空 Monomial = 空リスト） |

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

不定元（`x`, `y`）とシンボル（`i` 等）は型システム上の区別がない。どちらも `declare symbol` で宣言し、`Poly a atoms` の `atoms` スロットに入る。簡約規則は `declare rule` で別途付与する（例: `declare rule auto term i^2 = -1`）。

```egison
declare symbol x, y           -- 不定元
declare symbol i              -- シンボル(簡約規則で意味を付与)
```

**合成原子(`sin x`, `sqrt 2` 等)は `declare symbol` の対象にしない**。合成原子は `declare mathfunc` で関数を宣言し、型注釈 `Poly Integer [sqrt 2, sin x]` 等に出現した時点で自動的に登録される(「数学関数の宣言」セクション参照)。

---

## Factor 型と数学関数

### Factor の扱い

`Factor` は原子的な数式要素全体を指す型で、`Symbol` / `ApplyN` / `FunctionData` / `Quote` ベースの合成原子の和として定義される。

分類はランタイムの `SymbolExpr` コンストラクタで識別する。型レベルには `ConstantFactor` / `AppliedFactor` のような真部分型は**持たない**。型として残るのは `Symbol` のみ(`∂/∂` の第2引数の型制約として必要)。

`∂/∂` は `Differentiable Factor` の **always-AF 規則**で動作する: `ApplyN ref args` を常に連鎖律で扱うと、引数が定数（自由シンボルなし）の場合も `∂/∂ arg s` が再帰的に 0 を返すため、数学的に正しい微分が得られる。

包含関係:

```
Symbol  ⊂  Factor
Factor  ⊂  MathValue
```

### 内部表現

Factor のランタイム表現は `CASFactor SymbolExpr`。`SymbolExpr` の 4 つのコンストラクタで実行時のケース分けを行う。

```
Symbol:        CASFactor (Symbol _ _ _)               -- 不定元 / 平のシンボル
ApplyN:        CASFactor (ApplyN ref args)            -- declare mathfunc の適用 (sin x など)
FunctionData:  CASFactor (FunctionData name args)     -- function (x, y, ...) の関数シンボル
Quote:         CASFactor (Quote _)                    -- 引用式
```

**利点**:

- `casNormalize` の内部ループでは分類判定を行わない
- `Poly` への embed は Monomial に `(sym, 1)` を追加するだけ
- ランタイム分類関数を持たない
- `coerce` は原子が `atoms` に含まれるかだけを検証
- `Differentiable Factor` は always-AF 規則で動作

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

`declare apply` が定義されていない場合のデフォルトの挙動は、`CASFactor (ApplyN (DeclaredMathFunc ...) [...])` を生成する。`declare apply` が定義されている場合は、その評価結果が返る（`Integer` / `Poly` / `Factor` 等のいずれか、ただし**静的型は常に `MathValue`**）。

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

`declare apply` の RHS でクォート `'sqrt x` と書くと、`declare apply` を再帰呼び出しせずに `CASFactor (ApplyN (DeclaredMathFunc ...) [...])` をそのまま生成する。クォートなしの `sqrt x` は `declare apply` を再帰呼び出しするため、フォールバックには必ずクォートを使う。

```egison
-- sin/cos の適用規則（特定の値に対する簡約）
declare apply sin x :=
  if x == 0 then 0
  else 'sin x                                             -- それ以外はそのまま Factor

declare apply cos x :=
  if x == 0 then 1
  else 'cos x
```

`declare apply` が定義されていない `declare mathfunc` は、常にデフォルトの挙動（`CASFactor (ApplyN (DeclaredMathFunc ...) [...])` を生成）になる。

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

`Factor` には `Ring` インスタンスを与えない。演算が必要な文脈では、subtype unification (`Factor ⊂ Poly Integer [..]`) により型チェッカーが Poly 型への昇格を許可し、必要なら型注釈経由で `reshape` が runtime 構造を書き換える。これにより `casPlus` / `casMult` に `CASFactor` のケースは不要で、演算関数の実装がシンプルになる。

### CASFactor コンストラクタ

内部表現には `CASFactor` コンストラクタを持つ。`declare mathfunc` で宣言された関数の適用結果を表現する。`Poly` に昇格が必要になった時点で変換する。

```
-- sqrtSym = ApplyN (DeclaredMathFunc "sqrt") [CASInteger 2] :: SymbolExpr
-- 関数側は MathFuncRef で名前のみ保持 (WHNFData は不要)

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
Integer  ⊂  Poly Integer atoms    -- 任意の原子集合

Frac Integer  ⊂  Poly (Frac Integer) atoms
Poly Integer atoms  ⊂  Poly (Frac Integer) atoms

Term a atoms  ⊂  Poly a atoms    -- 単一項の多項式として埋め込み

-- MathValue は全 CAS 型の上位型
Integer            ⊂  MathValue
Symbol             ⊂  MathValue
Factor             ⊂  MathValue
Poly a atoms     ⊂  MathValue       -- 任意の a, atoms に対して
Frac a             ⊂  MathValue       -- 任意の a に対して
```

**embed の内部表現:**
```
Term a atoms → Poly a atoms:
  CASTerm coeff mono → CASPoly [CASTerm coeff mono]
```

### 原子集合の包含

```
-- 原子集合の拡大
Poly a atoms₁  ⊂  Poly a atoms₂   when  atoms₁ ⊆ atoms₂

-- 開いた型への昇格
Poly a atoms  ⊂  Poly a [..]
```

### 包含の伝播規則

```
a ⊂ b  ならば  Poly a atoms ⊂ Poly b atoms    -- 係数の埋め込み
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

### ユーザによるタワー拡張 (将来課題)

上記 5 段階のタワーは **CAS 標準型のみ** を対象にしている。実用上、ユーザは中間段に独自の型を差し込みたい場面がある:

- **Gaussian 整数係数の多項式** `Poly (Poly Integer [i]) [..]` (`i^2 = -1` の rule 付き)
- **代数拡大体** `Poly (Poly Integer [α]) [..]` (`α` は最小多項式の根)
- **三角関数係数の多項式** `Poly (Term MathValue [sin θ, cos θ]) [r]` 等

これらは `Poly Integer [..] ⊊ X ⊊ Frac (Poly Integer [..])` のように既存レベルの **間** に挿入されるべき型であり、現在の固定 5 段タワーでは表現できない。一方、`reshape` 機構は **任意の nested CAS 型** に runtime 構造を書き換えられる (atom 分離を含む) ので、API 側はすでに extensible になっている。

将来、以下のような構文でユーザがタワーを編集可能にする構想がある:

```egison
-- ユーザ定義の中間型を宣言
declare cas-type GaussianPoly = Poly (Poly Integer [i]) [..]
  with rule i^2 = -1

-- タワーへの位置を指定 (半順序の追加辺)
declare cas-subtype GaussianPoly extends Poly Integer [..]
declare cas-subtype Frac (Poly Integer [..]) extends GaussianPoly

-- 正規化 (canonicalize) のフック
class CASCanonical a where
  canonicalize (x: a) : a

instance CASCanonical GaussianPoly where
  canonicalize x := iterateRules [rule.iSq] x
```

これにより:

- `join` がユーザ追加型を含めた **半順序 lattice** で計算される
- `reshape` が `declare cas-subtype` のチェーンを辿って中間型に着地できる
- 型ごとに `CASCanonical` 経由で normalize 規則を分離

実装は大規模 (5 phase 程度) で、構想は [type-cas-tower.md](./type-cas-tower.md) にまとめてある。本ドキュメントの以下の章 (タワー、`join`、`embed`/`coerce`) はすべて固定タワー前提だが、ユーザ拡張時には:

- 「タワーのレベル番号」は半順序チェーンの位置に置き換わる
- `embed`/`reshape` は半順序の上向きエッジ
- `coerce`/`reshape` は (条件付きで) 下向きエッジ + 検証

という形で自然に一般化される。

---

## 自動変換の仕組み

### 1. reshape (型注釈に基づく構造書き換え)

型注釈 (`def x : T := e` または `(e : T)`) を書くと、AST elaboration が `IReshape T e` を挿入し、runtime に `casReshapeAs T v` が CAS 構造を target type に書き換える。これが旧設計の `embed` (widening) と `coerce` (narrowing) を一本化した実装。

```egison
def n : Integer := 3
def q : Poly Integer [i] := n + i        -- 注釈ベースで自動 reshape
def f : Frac Integer := 6 / 2            -- runtime: 3 (Frac→Integer canonical)
def p : Poly (Poly Integer [z]) [..] :=
  2*x*z + 3*x + z^2 + 1                  -- atom 分離: (2z+3)*x + (z^2+1)
```

旧 `Embed` typeclass や `coerce`/`coerceToX` 関数は **Phase C で削除済**。型注釈による reshape 一本に統一されたので、ユーザは追加の関数呼び出しを書く必要がない。

#### 「trust the annotation」原則

`reshape` は **構造検証を行わない**。注釈に従って CAS 値の構造を書き換えるだけで、注釈と値が不整合な場合 (例: `(x + 1 : Integer)` で実際は多項式) は canonical 形のまま残り、downstream の演算で自然に問題が顕在化する。

これにより:
- 観察型と注釈型が一致するケースでは reshape が望ましい構造を構築
- 強い検証エラーを避け、REPL で「観察 → 注釈」の workflow を妨げない
- 注釈が意味を持つのは **構造の選好** (level 4 vs level 5、atom 分離など) に対して

#### 注釈粒度の選び方

同じ `sqrt 2` でも注釈型によって得られる静的情報量が異なる。原子集合まで指定した **閉じた `Poly` / `Term` 型** が最も情報量が多く、後続の計算で型推論が精密になるため推奨:

| 注釈型 | 静的に分かること | 主な用途 |
|---|---|---|
| `MathValue` | 何でも (最汎) | 型注釈なしと同等 |
| `Poly Integer [..]` | 原子集合は非固定 | 実行時値を含む場合の退行形 |
| `Poly Integer [sqrt 2]` | 原子が `sqrt 2` 限定、`(sqrt 2)^2 = 2` 等のルールが型上で追跡可 | **推奨** (引数が静的に既知の場合) |
| `Term Integer [sqrt 2]` | 単項であることも表明 | さらに精密に絞り込みたい場合 |

**REPL / inspect での観察型の表示**: 注釈を省いた場合、`typeOf`/`inspect` が評価後に観察型を表示する。ユーザーはこれをコピーして `def` の注釈にできる。詳細は「観察型」セクション参照。

### 2. join (二項演算時の最小上界の計算)

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
join(Integer, Poly Integer atoms) = Poly Integer atoms

-- 閉じた Poly 同士: 原子集合の和集合を自動計算
join(Poly a atoms₁, Poly b atoms₂) = Poly (join(a, b)) (atoms₁ ∪ atoms₂)

-- 閉じた Poly と埋め込み可能な型
join(Poly a atoms, b) = Poly (join(a, b)) atoms    -- b が a に embed 可能
join(a, Poly b atoms) = Poly (join(a, b)) atoms    -- a が b に embed 可能

-- 開いた Poly
join(Poly a [..], Poly b [..]) = Poly (join(a, b)) [..]
join(Poly a atoms, Poly b [..]) = Poly (join(a, b)) [..]
join(Poly a [..], Poly b atoms) = Poly (join(a, b)) [..]

-- Poly と Frac Integer（タワー: level 3 + level 2 → level 4）
join(Poly Integer atoms, Frac Integer) = Poly (Frac Integer) atoms
join(Frac Integer, Poly Integer atoms) = Poly (Frac Integer) atoms

-- Frac 同士（同レベルでの再帰）
join(Frac a, Frac b) = Frac (join(a, b))

-- Frac (Poly ...) が絡むケース（タワー: level 5 へ）
join(Poly a atoms₁, Frac (Poly b atoms₂))
  = Frac (Poly (join(a, b)) (atoms₁ ∪ atoms₂))
join(Frac (Poly a atoms₁), Frac (Poly b atoms₂))
  = Frac (Poly (join(a, b)) (atoms₁ ∪ atoms₂))

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

### 設計原則: 原子集合の追跡は名前付き型変数で行う

CAS の演算で原子集合を静的に保存したいかどうかは、**用途ごとに異なる**。設計はこれを **2 種類の構文で明示的に区別**する:

| 構文 | 意味 | 原子集合の追跡 |
|---|---|---|
| `Poly a [x, y]` (具体集合) | 閉じた具体的な原子集合 | 完全に追跡 |
| `Poly a atoms` (名前付き型変数、ブラケットなし) | 型変数として束縛、保存される | 追跡される |
| `Poly a [..]` | ワイルドカード(独立 any、原子集合消去) | 追跡されない |

**用途 1: 原子集合に関心のない汎用処理** — `[..]` を使う。

```egison
def prettyPrint : Poly a [..] -> String := ...
def depth       : Poly a [..] -> Integer := ...
```

式レベル/関数シグネチャの `[..]` は **ワイルドカード（独立な any）** として振る舞う。引数間・戻り値間で原子集合は共有されず、静的追跡もされない。`coerce` が必要な場合はランタイム検証になる。

**用途 2: 原子集合を保存したい演算** — 名前付き型変数 `atoms` を使う。

```egison
-- 引数と結果で同じ原子集合を共有
def polyDouble {atoms} (x : Poly Integer atoms) : Poly Integer atoms := x + x
```

`atoms` は通常の HM 型変数として扱われる。型クラスのインスタンスヘッドでも同様で、原子集合を保存したい場合は **`Poly a atoms` (ブラケットなし)** と書く:

```egison
instance {Ring a, atoms} AddMonoid (Poly a atoms) where ...    -- 原子集合保存(parametric)
instance {Ring a}        MulMonoid (Poly a [..]) where ...     -- 原子集合消去(wildcard)
```

**重要**: `[atoms]` (ブラケット付き) は誤った記法 — それは「`atoms` という名前の 1 原子を含む閉じた集合」を意味してしまう。型変数として原子集合全体を指したい場合は **必ずブラケットなしで `atoms` と書く**。`[..]` は唯一の専用ワイルドカード構文。

### `+` と `*` の閉性に応じたインスタンス設計

`+` と `*` は数学的性質が異なる:

- **`+`** は常に閉じる: `Poly a atoms₁` と `Poly a atoms₂` の和は `Poly a (atoms₁ ∪ atoms₂)` で、原子集合は和集合になり追加されない
- **`*`** は一般には閉じない: `sqrt 2 * sqrt 3 = sqrt 6` のような規則があれば、被演算子に存在しない新たな原子 (`sqrt 6`) が結果に現れうる

この違いを型に反映するため、**`+` は名前付き型変数 (parametric) の汎用インスタンス**、**`*` は具体型の特化インスタンス + ワイルドカードの fallback** で書き分ける:

```egison
-- + : 常に原子集合保存 (parametric)
instance {Ring a, atoms} AddMonoid (Poly a atoms) where
  (+) p q := casNormalizeWithRules (casPlus (unwrap p) (unwrap q))
-- 効果的な型: Poly a atoms₁ -> Poly a atoms₂ -> Poly a (atoms₁ ∪ atoms₂)
--   (atoms₁ ∪ atoms₂ は call site で join + embed により計算される)

-- * : 閉じる subtype は具体的に宣言
instance MulMonoid (Poly Integer [i]) where ...        -- Z[i]
instance MulMonoid (Poly Integer [w]) where ...        -- Z[ω]

-- * : 一般 fallback はワイルドカード (原子集合消去)
instance {Ring a} MulMonoid (Poly a [..]) where
  (*) p q := casNormalizeWithRules (casMult (unwrap p) (unwrap q))
-- 効果的な型: Poly a [..] -> Poly a [..] -> Poly a [..]
```

**インスタンス解決の挙動**:

```egison
def a : GaussianInt := 1 + i
def b : GaussianInt := 2 + 3*i
a * b
-- 候補: MulMonoid (Poly Integer [i]) (具体)、MulMonoid (Poly a [..]) (wildcard)
-- → 具体 instance が選ばれる → 結果型 Poly Integer [i] (閉じる)

def p : Poly Integer [sqrt 2] := sqrt 2
def q : Poly Integer [sqrt 3] := sqrt 3
p * q
-- joinTypes で Poly Integer [sqrt 2, sqrt 3]
-- 候補: MulMonoid (Poly Integer [sqrt 2, sqrt 3]) は宣言なし
-- → wildcard fallback → 結果型 Poly Integer [..] (開く、原子集合消去)
```

**この設計が解く問題**: 単純に `MulMonoid (Poly a atoms)` (parametric) を書くと、`p * q` の結果型が `Poly Integer [sqrt 2, sqrt 3]` になるが、ランタイム値は `sqrt 6` を含み得る。**静的型と実態が乖離する soundness gap**。wildcard fallback により結果型を `[..]` に開くことで、この乖離を防ぐ。

**ユーザーの責任**: `instance MulMonoid T` を具体型 `T` に宣言することは「`T` が `*` で閉じる」というユーザーの表明。実際には閉じない型 (例: `Poly Integer [sqrt 2, sqrt 3]` で `sqrt 2 * sqrt 3 = sqrt 6` の規則がある) に宣言すると、結果値が型に適合せず、後続の境界操作でランタイムエラーになる。システムは閉性の数学的検証はしない (`declare rule` を見て自動判定することはしない)。

ユーザーは「閉じることを表明したい」具体型のみインスタンスを宣言すればよく、それ以外は wildcard fallback で自動的に開く。

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

ただし、**静的型保存のためのインスタンス特化は必要**: `Ring (Poly a atoms)` のように **名前付き型変数 `atoms` を使った汎用インスタンス** を書くことで、解決時に具体的な原子集合 (例: `[i]`) に特化される（型レベルの特化）。これにより `a + b : GaussianInt` のように結果の型が保存される。ユーザーが `instance Ring GaussianInt` を明示的に書く必要はなく、汎用テンプレートから解決機構が自動派生する。

`Ring (Poly a [..])` のように `[..]` で書くと、これは原子集合消去のワイルドカードになり、結果が `Poly a [..]` に開く。両者を意図的に使い分ける (詳細は次の「`+` と `*` の閉性に応じたインスタンス設計」節)。

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
-- 算術関数 (casPlus/casMult/casNeg) は構造的正規化のみ行い、
-- 簡約規則の適用は最終結果に対して casNormalizeWithRules で行う(Phase 7.2)
instance Ring MathValue where
  (+) a b := casNormalizeWithRules (casPlus a b)
  (*) a b := casNormalizeWithRules (casMult a b)
  zero := CASInteger 0
  one := CASInteger 1
  neg := casNeg

-- subtype: MathValue の演算をラップ + coerce
instance Ring Integer

-- + は parametric (原子集合保存)、* は wildcard fallback (原子集合消去)
-- (詳細は「+ と * の閉性に応じたインスタンス設計」節を参照)
instance {Ring a, atoms} AddMonoid (Poly a atoms) where
  (+) p q := coerce (unwrap p + unwrap q)
  zero    := coerce (CASInteger 0)
instance {Ring a, atoms} AddGroup (Poly a atoms) where
  neg p := coerce (- unwrap p)

instance {Ring a} MulMonoid (Poly a [..]) where
  (*) p q := coerce (unwrap p * unwrap q)
  one     := coerce (CASInteger 1)
-- 閉じることを表明したい具体型は個別に宣言:
-- instance MulMonoid (Poly Integer [i]) where ...

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

-- 係数が体の多項式: 汎用の多項式GCD (atoms 保存)
instance {Field a, atoms} EuclideanDomain (Poly a atoms) where
  gcd a b := ...  -- 一般の多項式ユークリッド算法
  divMod a b := ...

-- 係数が GCDDomain の多項式: 内容と原始部分に分解する GCD (atoms 保存)
instance {GCDDomain a, atoms} GCDDomain (Poly a atoms) where
  gcd a b := ...  -- 内容 GCD × 原始部分 GCD
```

#### インスタンス解決メカニズム

インスタンス解決は**標準 Haskell 的な単一化ベース**で行う。

**基本原則**: インスタンスヘッドの **名前付き型変数 `atoms`** を通常の型変数として、ターゲット型との単一化で具体化する。`[..]` はワイルドカードで、束縛を生まずに任意とマッチする。

```
instance {AddMonoid a, atoms} AddMonoid (Poly a atoms) where ...   -- atoms 保存
instance {Ring a}             MulMonoid (Poly a [..]) where ...     -- atoms 消去
```

解決の流れ:

```
例: a + b の解決 (a, b : GaussianInt = Poly Integer [i])
目標: AddMonoid (Poly Integer [i])

1. 候補インスタンスのヘッドとターゲットを単一化
   - instance AddMonoid (Poly a atoms)
   - ターゲット: AddMonoid (Poly Integer [i])
   - 単一化: a = Integer, atoms = [i]
   - → 解決成功

2. 単一化成功のインスタンスから具体的な型を復元
   - AddMonoid (Poly Integer [i]) が利用可能
   - 戻り値型として Poly Integer [i] が保存される
```

`[..]` (ワイルドカード) を使ったインスタンスは、単一化で `atoms` 部分に対応する型変数に何も束縛しない:

```
例: a * b の解決 (a, b : Poly Integer [sqrt 2, sqrt 3])
目標: MulMonoid (Poly Integer [sqrt 2, sqrt 3])

候補1: instance MulMonoid (Poly Integer [i])     -- 具体集合だが atoms 不一致 → 失敗
候補2: instance MulMonoid (Poly a [..])          -- ワイルドカード → 成功
   - 単一化: a = Integer, [..] は無条件マッチ (束縛なし)
   - → 戻り値型は宣言された通り Poly a [..] = Poly Integer [..] (atoms 消失)
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
-- 候補2: instance {GCDDomain a, atoms} GCDDomain (Poly a atoms)   -- 汎用

-- Poly (Frac Integer) [x] に gcd を適用
gcd (x^2 - 1) (x - 1)
-- 候補: instance {Field a, atoms} EuclideanDomain (Poly a atoms)  -- a = Frac Integer, atoms = [x] ✓
```

**具体性の判定**: 2 段階の比較。

1. **型引数の具体性**: 具体型(`Integer` 等) > 型変数(`a`)
2. **原子集合の具体性**: 3 段階で順序付け
   - 具体集合 (`[i]`, `[sqrt 2, x]` 等)  — 最も具体的
   - 名前付き型変数 (`atoms`、parametric)  — 中間
   - ワイルドカード (`[..]`)  — 最も汎用

両方で「より具体的」な候補が選ばれる。競合(どちらも同じ具体性)は型エラー(OverlappingInstances は認めない)。CAS 型の特化インスタンスは有限個（`Z[i]`, `Z[ω]` 等）なので、この規則で十分。

**例**:

```
ターゲット: MulMonoid (Poly Integer [i])
候補1: MulMonoid (Poly Integer [i])        — 具体集合
候補2: MulMonoid (Poly a atoms)             — 名前付き型変数 (parametric)
候補3: MulMonoid (Poly a [..])              — ワイルドカード
→ 候補1 が選ばれる

ターゲット: MulMonoid (Poly Integer [sqrt 2, sqrt 3])
候補1: MulMonoid (Poly Integer [i])        — 具体集合だが atoms が一致しない (除外)
候補2: MulMonoid (Poly a atoms)             — 候補なし (* には parametric instance を用意しない設計)
候補3: MulMonoid (Poly a [..])              — ワイルドカード
→ 候補3 が選ばれ、結果型は Poly Integer [..] (atoms 消去)
```

#### 型推論の挙動

```egison
declare symbol x, i
declare rule auto term i^2 = -1

type GaussianInt := Poly Integer [i]

def a : GaussianInt := 1 + i
def b : GaussianInt := 2 + 3*i
a + b         -- AddMonoid (Poly a atoms) が選ばれる → : GaussianInt (= 3 + 4*i)
a * b         -- MulMonoid (Poly Integer [i]) (具体特化) が選ばれる → : GaussianInt (= -1 + 5*i)
gcd a b       -- EuclideanDomain GaussianInt（特化）が選ばれる

def p : Poly Integer [x] := 1 + x
def q : Poly Integer [x] := 2 + 3 * x
p + q         -- AddMonoid (Poly a atoms) が選ばれる → : Poly Integer [x]
p * q         -- MulMonoid (Poly Integer [x]) は宣言なし → wildcard fallback
              --   → : Poly Integer [..] (原子集合消去)
              -- 閉じることを意図する場合は instance MulMonoid (Poly Integer [x]) を宣言する

-- subtype が異なる場合は join で和集合を計算
a + p         -- GaussianInt と Poly Integer [x] の混合
              -- → join: [i] ∪ [x] = [i, x]
              -- → 双方を embed → Ring (Poly Integer [i, x]) → : Poly Integer [i, x]

-- MathValue には GCDDomain インスタンスがないので型エラー
-- gcd (unwrap a) (unwrap p)   -- 型エラー: GCDDomain MathValue は存在しない
```

### 微分演算子 `∂/∂`

偏微分演算子 `∂/∂` は **型クラス `Differentiable` のメソッド** として定義する。各 CAS 型ごとにインスタンスを定義する。返り値は常に `MathValue`。

**`Differentiable MathValue` インスタンスは持たない**: `MathValue` は導関数が `declare derivative` で登録されていない `ApplyN` を含みうるため、`Differentiable` のインスタンスとして適切ではない (微分操作が部分関数になり、型クラスの「全域性」契約に反する)。`MathValue` の値を微分したい場合は、`Integer` / `Symbol` / `Factor` / `Poly` / `Frac` 等の具体型に `coerce` してから `∂/∂` を呼ぶ。`coerce` 時のランタイム検証で「微分可能な要素のみを含む」ことが保証される。

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

-- Factor: ランタイム形に応じてディスパッチ (always-AF 規則)
instance Differentiable Factor where
  ∂/∂ v s = case v of
    CASFactor (Symbol _ _ _)            -> if v == s then 1 else 0
                                            -- 平のシンボル: 自分自身なら 1、それ以外は 0
    CASFactor (ApplyN ref [a])          -> (lookupDerivative ref) a * ∂/∂ a s
                                            -- 1 引数 declare mathfunc (sin x など)
                                            -- lookupDerivative は通常の関数を返すので
                                            -- ApplyN で包まずに直接適用する
                                            -- (declare apply 経由で評価される)
                                            -- a が定数なら ∂/∂ a s が 0 を返し自然に 0 になる
    CASFactor (ApplyN ref args)         -> sum [(lookupPartial ref i) args * ∂/∂ a s
                                                | (i, a) <- zip [1..] args]
                                            -- 多引数 declare mathfunc (将来拡張)
                                            -- 各引数の偏微分 × 内側微分の連鎖律
    CASFactor (FunctionData name args)  -> sum [funcSymPartial name i args * ∂/∂ a s
                                                | (i, a) <- zip [1..] args]
                                            -- function (x, y, ...) の関数シンボル
                                            -- 偏微分インデックス name|i を生成して連鎖律
    CASFactor (Quote e)                 -> applyChainRuleToQuote e s
```

`Symbol ⊂ Factor` なので、`Differentiable Symbol` は別途定義しなくても、`x : Symbol` に対する `∂/∂ x s` の呼び出しは sibling fallback ([「sibling 型への fallback」節](#インスタンス解決メカニズム)) により `embed (x : Symbol → Factor)` を挿入してから `Differentiable Factor` で解決される。

**`lookupDerivative` のシグネチャ**:

```haskell
lookupDerivative :: MathFuncRef -> CASValue   -- 1 引数 mathfunc 用、Egison 関数を返す
lookupPartial    :: MathFuncRef -> Int -> CASValue   -- 多引数 mathfunc の i 番目偏微分用
funcSymPartial   :: String -> Int -> [CASValue] -> CASValue
                                              -- function symbol の name|i [args] を生成
```

`ApplyN` の関数側は型レベルで `MathFuncRef` に絞られているので、`lookupDerivative` は曖昧性なく `String` キーを取り出せる (現状は単一構成子 `DeclaredMathFunc String` のみだが、将来拡張時にも型で守られる)。

**`lookupDerivative` の失敗扱い**: `declare derivative` で登録されていない関数を含む `ApplyN` の微分は **ランタイムエラー** (`DerivativeNotDeclared` 等)。これは `Differentiable Factor` のインスタンスが「`declare derivative` 未登録の合成原子は実質的に Factor インスタンスの定義域外」であることを意味する。型レベルでは検出できないが、`Differentiable MathValue` を提供しないことで「未登録関数を含む MathValue は静的に弾かれる」境界を作っている。`FunctionData` の偏微分は `name|i` シンボル生成で自動完結するためエラーにならない (連鎖律は構造的に閉じる)。

型レベルで CF / AF を区別しない設計のため、`Differentiable ConstantFactor` / `Differentiable AppliedFactor` のような型別インスタンスは**持たない**。

#### 複合型のインスタンス

```egison
-- Term: 積の微分法則（係数 × 各モノミアル因子の product rule）
instance {Differentiable a, atoms} Differentiable (Term a atoms) where
  ∂/∂ t s = ...  -- 係数の微分 × モノミアル + 係数 × モノミアルの微分（product rule）
                 -- モノミアルは flat リスト。各原子は Differentiable Factor の
                 -- always-AF 規則で微分される

-- Poly: 線形性（各項の微分の和）
instance {Differentiable a, atoms} Differentiable (Poly a atoms) where
  ∂/∂ p s = sum [∂/∂ t s | t <- terms p]

-- Frac: 商の微分法則
instance {Differentiable a} Differentiable (Frac a) where
  ∂/∂ (n/d) s = (∂/∂ n s * d - n * ∂/∂ d s) / d^2
```

`Differentiable MathValue` を**提供しない**ことで、`MathValue` の値を直接微分しようとすると静的型エラーになる。ユーザーは `coerce` で具体型 (`Poly Integer atoms` 等) に絞り込んでから `∂/∂` を呼ぶ必要があり、その coerce で「微分可能な要素のみを含むこと」がランタイム検証される。

**always-AF 規則の正しさ**: `ApplyN ref args` を常に連鎖律で扱うと、引数が定数（自由シンボルなし）の場合でも `∂/∂ arg s` が再帰的に 0 を返すため、結果は `(f' arg) * 0 = 0` となり、数学的に正しい。従って `sqrt 2` や `log 3` のような「実質的に定数的な」ケースも正しく 0 に微分される。これによりランタイムの `classifyFactor` / `hasFreeSymbol` 解析は不要になる。

**設計の利点**:
- Haskell にハードコードされた `casDeriv` ではなく、各型ごとの微分規則をユーザーが定義・拡張できる
- `ApplyN` の外側関数の導関数（`sin` → `cos` 等）をユーザーが `declare derivative` で登録する
- `Poly`, `Frac`, `Term` の微分法則は数学的な構造（線形性、積の法則、商の法則）に直接対応
- 1スロット化により型レベルでの CF / AF 分類が消え、誤注釈による foot-gun が解消される

#### 実装注 (2026-05-01): inductive pattern 経由の静的 dispatch

設計上の `case v of CASFactor (Symbol _ _ _) -> ... | CASFactor (ApplyN ...) -> ...` という Haskell 風の case 解析は、Egison の matcher / inductive pattern 機構で次のように実現されている:

**Factor 型の静的 dispatch を可能にする 3 点セット**:

1. `def factor : Matcher Factor` (`lib/math/expression.egi`) — symbol/apply1-4/quote/func パターンを持つ matcher。target 型は `Factor`。

2. `inductive pattern MathValue := | (^) Factor Integer` — `^` パターン構成子の第一引数を `Factor` に。これにより `match e as mathValue with | $a * $fx ^ $n -> ...` の `$fx` の **静的型が `Factor`** になる。

3. `instance Differentiable Factor where partialDiff f x := match f as factor with | #x -> 1 | symbol _ _ -> 0 | _ -> chainPartialDiff f x` — Factor inst が `^` 経由で extract された `$fx` から **コンパイル時 dispatch** される。

**Term 型側の対称化** (2026-05-01):

1. `def term {a} (m: Matcher a) : Matcher (Term a [..])` — Term 型を返す matcher (旧 `Matcher MathValue`)。

2. `inductive pattern MathValue := | poly [Term MathValue [..]] | (+) (Term MathValue [..]) MathValue` — `poly $ts` で `$ts : [Term MathValue [..]]`、`$t + $rs` で `$t : Term MathValue [..]`。

3. `instance Differentiable (Term MathValue [..]) where ...` — Term inst が poly 分解の各 term に対して **コンパイル時 dispatch** される。

**結果**: Factor と Term の両方が `IRuntimeDispatch` を経由しない静的 dispatch で動く。Frac inst の `$p1 / $p2` のフォールバック路 (`_ -> chainPartialDiff f x`) と、`runtimeTypeOfCAS` 経由で MathValue → 各 instance への runtime dispatch (`IRuntimeDispatch`) は維持。

**`lookupDerivative` の現状**: 設計通り `declare derivative` の登録 → Factor inst での参照という流れ自体は、現状 `chainPartialDiff` shadowing 再定義方式で代替されている。Haskell-side primitive 化は [§ 既知の制限と未解決課題](#既知の制限と未解決課題) の残課題として記載。

#### 合成原子の導関数定義

合成原子の外側関数の導関数は `declare derivative` で登録する:

```egison
declare derivative sin = cos
declare derivative cos = \x -> (-1 * sin x)    -- マイナスをつける
declare derivative log = \x -> 1 / x
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

関数シンボルの**新 CAS 型システムへの統合**(どの型になるか、`Poly` の原子集合にどう入るか、偏微分インデックスの扱い等)は未決事項。[§ 既知の制限と未解決課題](#既知の制限と未解決課題) を参照。

#### 差分閉性の観察型による報告

差分閉性(`Poly c atoms` の式を `∂/∂` で微分した結果が同じ型に留まる性質)は、**観察型機構で事後報告される**。型レベルでの静的保証は行わない。

> `Poly c atoms` の式を `∂/∂` で微分した結果、観察型が入力と同じ `atoms` を持つ場合、その演算は微分で閉じていると言える。観察型機構がこれを自動的に検出・報告する。

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

-- 合成原子を含むケース（連鎖律）— 微分対象の型を具体化する必要がある
∂/∂ (sin x : Factor) x                       -- => cos x : MathValue
∂/∂ ((x^2 + sin x) : Poly Integer [x, sin x]) x   -- => 2*x + cos x : MathValue

-- 型エラー
-- ∂/∂ f (log x)   -- 型エラー: log x は Symbol でない
```

#### 微分演算子の合成

`∂/∂` の戻り値は `MathValue` だが、`Differentiable MathValue` インスタンスは存在しないため、合成するには結果を具体型に絞り込む必要がある。典型的には引数も具体型で受け、各 `∂/∂` 呼び出しの結果を同じ具体型に `coerce` する:

```egison
-- 引数を具体型 (Poly Integer atoms) で受け、各微分結果を同型に coerce
def laplacian2D {atoms} (f : Poly Integer atoms) : Poly Integer atoms :=
  let fx  : Poly Integer atoms := coerce (∂/∂ f x)
      fxx : Poly Integer atoms := coerce (∂/∂ fx x)
      fy  : Poly Integer atoms := coerce (∂/∂ f y)
      fyy : Poly Integer atoms := coerce (∂/∂ fy y)
   in fxx + fyy
```

`coerce` は観察型と宣言型が一致するとき (= `f` が `x`, `y` で微分閉じている多項式環に住むとき) に成功する。閉じていない場合はランタイムエラーになり、ユーザーに型注釈の問題を知らせる。

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
| `∇` / `grad` / `nabla` | 具体型 (`Poly Integer atoms` 等) 上で `∂/∂` を合成、結果は coerce で型を保つ |
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

**無限再帰の懸念について**: 「`#x` 照合 → `Eq MathValue` → `casNormalizeWithRules` → 同じ規則が再びマッチして `#x` 照合 → ...」のような再帰が起きないかという懸念がある。実用上は問題にならない:

- `#x` が束縛されるのは関数引数 (`sin x` の `x` 等) など**構造的に内側の値**であり、外側の `poly` 規則とは型・構造が異なる。内側の値に対して再正規化が走っても、外側の `poly` パターンに再びマッチする条件が成立しにくい
- `casNormalize` は構造帰納的 (子を先に正規化してから親) なので、`#x` 照合時点で内側の値はすでに正規形になっており、追加の規則適用が空回りする
- そもそも `auto` 規則は終端性 (収束) がユーザーの責任である ([設計の根拠](#設計の根拠) 参照)。終端しない規則を書いた場合の発散は規則設計の問題であり、`#x` 機構固有のリスクではない

実装上は念のため、`#x` 照合中の `casNormalizeWithRules` 呼び出しでは規則適用の再帰深度を制限する/規則環境を一時的に制限する等のガードを入れてもよいが、必須ではない。

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

関数引数のパターンマッチを含む規則（例: `sin ($a + $b) = sin a * cos b + cos a * sin b`）では、規則の左辺 `sin ($a + $b)` が `SymbolExpr` の `ApplyN` コンストラクタ (`ApplyN (DeclaredMathFunc "sin") [arg]`) にマッチし、その引数（`CASValue`）を `poly` マッチャーで分解して `$a + $b` を束縛する。つまり、`declare rule` の規則適用エンジンは、CAS マッチャー（`poly`, `frac` 等）を内部的に利用して関数引数内のパターンを分解する。

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
poly m       の  $a :+ $rest  →  a : Term a atoms, rest : Poly a atoms
multiset m   の  $a :: $rest  →  a : a,              rest : [a]
```

`a : Term a atoms` はそのまま `Poly a atoms` を期待する関数に渡せる（`Term a atoms ⊂ Poly a atoms` の embed による）。

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
- `CASFactor (ApplyN (DeclaredMathFunc ...) [...])` → `Factor`(または `Poly Integer [that applyN]` として昇格)
- `CASFactor (FunctionData ...)` → `Factor`(関数シンボルとして昇格)

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
| シンボル集合の制御 | なし（全シンボル対等）  | 1スロット原子集合の閉じた/開いた集合を選択可能。観察型機構が評価後に具体的な原子集合を報告 |
| 微分の型保存    | なし           | 観察型で事後報告(静的保証なし)          |
| テンソルとの統合  | なし           | tensorMap の自動挿入（論文で証明済み）           |
| 新しい数の導入   | ドメイン定義（SPAD） | `declare mathfunc`           |
| 観察型        | なし           | 評価後に最具体型を自動報告（Success typing 系列） |


特に tensorMap 挿入と embed 挿入が単一の型推論パスで統一的に処理される点が新しい。

---

## 実装 TODO

### Phase 1〜4: 基盤（完了）

`CASValue` 基盤、`ScalarData → CASValue` 置換、1 スロット型、プリミティブパターンマッチは完了済。実装上の現状:

- `CASValue` (`CASInteger` / `CASFactor` / `CASPoly` / `CASFrac` / `CASTerm`) を `Math/CAS.hs` に定義、`casPlus` / `casMult` / `casNormalize`（降冪順 + ゼロ除去 + モノミアル GCD 簡約）を実装。
- `SymbolSet` は1スロット (`SymbolSetClosed [TypeAtom]`)。原子集合は型レベルのみで保持し、ランタイム `Monomial = [(SymbolExpr, Integer)]` は flat。詳細は「構成的な内部表現 > 境界での名前集合照合」を参照。
- 型システムは `TPoly Type SymbolSet` / `TTerm Type SymbolSet` / `TFrac Type` / `TFactor` を持ち、unifier (`Type/Unify.hs`) で `MathValue ↔ {Factor, Frac _, Poly _ _}` の双方向ユニフィケーションを行う。
- プリミティブパターンは Layer 1 (`PDxxxPat`) / Layer 2 (`inductive pattern MathValue` 経由のユーザー公開名) の 2 層構造。詳細は `lib/math/expression.egi` を参照。

**残されたサブ課題**:
- `Apply1〜Apply4` を `ApplyN MathFuncRef [CASValue]` に一般化するリファクタ。実用上 5 変数以上の math function はほぼ無いため意図的に保留中。

### Phase 5: パラメトリックマッチャー（完了）

`poly` / `frac` / `term` パラメトリックマッチャーを `lib/math/expression.egi` に実装済。型とマッチャーの構造が一致するよう設計されている:

```
型                                  マッチャー
Integer                             integer
Symbol                              symbol
Factor                              factor
Frac Integer                        frac integer
Poly Integer [x]                    poly integer
Poly (Frac Integer) [x]             poly (frac integer)
Frac (Poly Integer [x])             frac (poly integer)
Tensor (Poly (Frac Integer) [x])    tensor (poly (frac integer))
```

**設計のポイント**:
- マッチャーは係数マッチャーのみを引数に取る。原子集合スロットはランタイムのマッチングに影響しないため、マッチャー引数には含めない（型レベルでのみ区別）。
- `:+` パターンは多項式を **項** と **残りの多項式** に分解する。項は `Term a` 型（係数 + flat モノミアルの組）。
- `term m` は `(係数, flat モノミアル)` を返す。flat モノミアルは `[(Factor, Integer)]` で、各要素に `factor` マッチャー (内部で `symbol`/`apply1`/`quote`) を適用して分類できる。
- `mathExpr` は後方互換のため `poly` / `frac` / `term` の合成として残置。

**残されたサブ課題**:
- `frac (poly (frac integer))` 等の複合 nested matcher の専用テスト未追加（動作はする）。

### Phase 5.5: 型注釈に基づく自動変換 (`reshape`)

型注釈 (`def x : T := e` または `(e : T)`) を elaboration trigger として、AST node `IReshape Type IExpr` を post-typecheck で挿入。runtime の `casReshapeAs T v` が CAS 構造を target 型に書き換える（atom 分離を含む）。

旧設計の `Embed` / `Coerce` typeclass と `coerceToX` 関数群は廃止済（戻り値型 dispatch を含む typeclass method を AST elaboration で wrap すると内側の dispatch context が壊れる問題があり、`reshape` primitive 一本に統一）。

### Phase 6: ライブラリ関数の再実装（完了）

`expandAll` / `substitute` / `partialDiff` / `coefficients` / `coefficient` / `taylorExpansion` / `maclaurinExpansion` / `Sd` をすべて新マッチャー（`poly m`, `frac m`, `term m`）と `MathValue` 上の関数として再実装済。

**`Differentiable` 型クラス**:
- `Differentiable a where partialDiff (f: a) (x: MathValue) : MathValue` を `lib/math/analysis/derivative.egi` に定義。
- インスタンス: `Integer`, `Factor`, `Term`, `Poly`, `Frac`。
- **`Differentiable MathValue` は提供しない**（導関数未登録の `ApplyN` を含む可能性があるため）。`Differentiable Symbol` は `Symbol ⊂ Factor` の sibling fallback で `Differentiable Factor` に委譲。

**`declare derivative` / `declare mathfunc`**:
- `declare derivative <name> = <expr>` は `def deriv.<name> := <expr>` に desugar。`deriv.sin` 等で直接参照可。
- `declare mathfunc <name>` は `def <name> := \x -> '<name> x` ラッパーを生成し、`declare derivative` と組み合わせて「関数 + 導関数」ペアを宣言可。
- `chainPartialDiff` は拡張可能ディスパッチャ。各 `declare derivative` がパターン分岐を蓄積し、再帰呼び出しで `f(g(x))` のネスト合成にも連鎖律が正しく適用される。

**残されたサブ課題**:
- `coefficients` の Frac 係数バグ — `coefficients (y^2 + 3/4) y` が誤った係数を返す。`quadratic-equation.egi` で観測。後述の「既知の制限」も参照。

### Phase 7: 簡約規則

#### アーキテクチャ

正規化ロジックは以下の役割分担で動く（Step 7.7 の統合により完成）:

```
[Layer 1] casNormalize (CAS.hs)
  - 構造的正規化: 項の結合、ゼロ除去、降冪順ソート、GCD 簡約
  - casPlus/casMult/casDivide の内部で自動呼び出し

[Layer 2] casRewriteSymbol (Rewrite.hs, 残置 1 関数)
  - casRewriteDd のみ — FunctionData 同形項マージ (poly 級多項マッチ)。
    性能上の理由で declare rule 化せず Haskell 残置。

[Layer 3] declare rule auto (lib/math/normalize.egi)
  - Egison レベルで宣言されたユーザー拡張可能な書き換え規則
  - mathNormalize を経由して算術演算 (+, -, *, /, ^) ごとに自動適用
  - 固定点ループは Haskell 側 `iterateRulesCAS` (trigger filter cache 付)
```

`declare rule auto` で登録されたルールは `iterateRulesCAS` が `casNormalize` 後に固定点まで適用する。手動規則は `simplify ... using ...` で明示的に適用。

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

#### Step 7.1–7.7: アーキテクチャ統合と Rewrite.hs 移行（完了）

- `declare rule [auto|名前] [term|poly|frac] <lhs> = <rhs>` 構文。`auto` は自動適用、名前付きは `simplify ... using <name>` で明示適用。
- 規則適用エンジンは非線形パターン `#x` をサポート。
- 旧 `Rewrite.hs` のハードコードルールはすべて `declare rule auto` に移行済（残置は `casRewriteDd` のみ — 詳細は次節）。
- `sqrt` 等の関数適用時簡約は `declare apply` に移行済。

#### Step 7.8: 実装の現状と知見

`Rewrite.hs` 削除目標は **ほぼ達成**。残るのは `casRewriteDd` 1 関数のみ（性能上の最適化として明示的に Haskell 残置）。

**現状サマリー**
- `Math/Rewrite.hs`: `casRewriteDd` 1 関数 + `casHasFunctionData` のみ（性能上の最適化として意図的に Haskell 残置。詳細は `lib/math/normalize.egi` 末尾コメント参照）
- 自動規則は `lib/math/normalize.egi` に `declare rule auto` で定義
- `declare apply` 化済の関数: `sqrt`, `rt`, `rtu`, `sin`, `cos`, `exp`, `log`, `abs`
- 性能ベースライン: Riemann S2 約 4.4s, eulers-formula 約 7.7s
- 移行に伴う網羅的な完了履歴は git log を参照（`086b0005 implement declare rule` 周辺）

##### 未移行（Haskell 残置）

| 関数 | 理由 |
|---|---|
| `casRewriteDd` | poly 級多項マッチ + same-binding 制約が `riemann-curvature-tensor-of-S2xS3` で大幅 slowdown。declare rule での実装は技術的に可能（スパイク確認済）だが、当面 Haskell 維持。 |

##### 重要な設計知見

**1. term 級 declare rule の係数バインディング**

bare `(sqrt $a)^$n` パターンは `term _ (...)` でマッチするため **係数を喪失**する（`5 * (sqrt 5)^2` が `5` になる）。これを避けるには明示的に係数をバインドする：

```egison
declare rule auto term term $c ((apply1 #sqrt $a, $n) :: []) =
  if n >= 2
    then c *' a^'(i.quotient n 2) *' ('sqrt a)^'(i.modulo n 2)
    else c *' ('sqrt a)^'n
```

→ パーサに `(a, b)` タプルパターンと `[]` 空リストパターンの追加が必要。`hs-src/Language/Egison/Parser/NonS.hs` の `ruleLhsAtom` を拡張。

**2. RHS 内の `^` を `^'` に振り替える**

declare rule の RHS で `x^n` (n はパターン変数) を書くと、lib の `(^)` 経由で `power` が呼ばれ `mathNormalize` が再起動する → **無限ループ**。`unNormalizeOps` で `^` を `^'` (un-normalize 版) に振り替える：

```haskell
IApplyExpr (IVarExpr "^") [base, expn] ->
  IApplyExpr (IVarExpr "^'") [unNormalizeOps base, unNormalizeOps expn]
```

`power'` 自体も `take`/`foldl` 経由の `n - 1` (= `minusForMathValue` → `mathNormalize`) サイクルを持っていたため、直接再帰 + `i.-` に書き直し。

**3. `wrapLhsForTermLevel`：`f $x * g $y` の自動ラップ**

`mathValue` の `$ * $` は (係数, モノミアル全体) に分解する一方、`multExpr` の `$ * $` は (factor, rest) に分解する。multi-factor の declare rule LHS (`f $x * g $y`) は **自動的に** `mult _ ((f $x ^ #1) * (g $y ^ #1))` に変換することで `multExpr` 経由のマッチングに乗せる。

```haskell
wrapLhsForTermLevel :: Pattern -> Pattern
wrapLhsForTermLevel p
  | isMultChainOfFactorShapes p = InductivePat "mult" [WildCard, wrapFactorsInChain p]
  | otherwise = p
```

これにより `multExpr` 自体に apply1-4 パターンを追加する必要がなくなり、設計の重複を回避。

**4. trigger filter のセクション式対応**

`apply2 #(^) $x $y` パターンの `#(^)` は parser で `SectionExpr` になる。`extractTriggerSymbols` の `exprNames` 関数が `SectionExpr` を処理しないと trigger が空になり、規則が **常に発火**する（性能 regression）。

```haskell
exprNames (SectionExpr op ml mr) = [repr op] ++ ...
```

**5. `declare apply` Phase A 実装 (糖衣構文)**

`declare apply foo x := <body>` は **`def foo := \x -> body` への糖衣構文** として実装。`declare mathfunc foo` のデフォルトラッパーを上書きする。

EnvBuilder で `declare mathfunc` 必須チェック：未宣言の `foo` に対して `declare apply foo` を書くとエラー。

**6. iterateRulesCAS の Haskell 化と trigger filter cache**

declare rule の発火経路を Haskell 内のループに移行（`iterateRulesCAS` プリミティブ）。trigger filter は `EvalState.autoRuleTriggers :: [Set String]` に **declare 時に Set 化済**で保存し、算術 op ごとの再構築コストを排除。

性能影響大：Riemann S2 で 25 s → 5 s 程度の高速化。

**7. lib の `sqrt`/`sin`/`cos`/`exp`/`log` の declare apply 化**

各関数を `declare mathfunc f` + `declare apply f x := ...` に書き換え。設計書の sqrt 全体像 (Section 763) に沿った構造に統一。

##### 残された Phase 7 系課題

- `casRewriteDd` の declare rule 化（性能上の理由で意図的に Haskell 残置。spike 検証済 = 実装可能だが `riemann-curvature-tensor-of-S2xS3` で >120s vs few-seconds の slowdown）
- `lib/math/normalize.egi` の `rewriteRuleForRtu` の declare rule 化（`sin/cos` の pythagorean 同一性は移行済）
- `mathNormalizeBuiltin` 残存の `containFunction1 rtu/sin/cos` 分岐削減（上記 rtu 移行後）
- `riemann-curvature-tensor-of-S2xS3` の根本性能（declare rule 化と独立した既存課題）

その他の現存バグ（`coefficients` の Frac 係数、nested radical の固定点未到達、mini-test 残課題）は「既知の制限と未解決課題」を参照。

### Phase 8: 観察型機構

観察型 (observed type) — 評価後の `CASValue` から最も具体的な型を逆算してユーザーに報告する機構。

**実装済**:
- `prettyTypeOf :: CASValue → String` (`Math/CAS.hs`)、入れ子 `CASValue` の再帰的 typeOf (join で広げる版)
- `typeOf` / `inspect` プリミティブ — `inspect` は値と観察型を `value : type` 文字列で返す
- `isInPolyAtoms`, `isPureInteger`, `isPureFraction`
- 差分閉性判定 `differentialClosed : MathValue -> MathValue -> Bool` (`∂/∂` 前後の atoms 比較)

**未実装**: REPL での自動表示、型注釈提案機能（後述「既知の制限」を参照）。

### Phase 9: `declare-key` 機構と `declare derivative` のライブラリ化

`declare derivative` 等は現状 built-in だが、汎用化して `declare-key` 構文でユーザーランドから登録テーブルを定義できるようにする構想。詳細は後述「既知の制限と未解決課題 > Phase 9 declare-key 詳細」を参照。

未着手。

---

## 既知の制限と未解決課題

実装は概ね設計通り完了している。残された課題:

### 意図的に実装しない項目

| 項目 | 理由 |
|---|---|
| `Apply1〜Apply4` → `ApplyN` 一般化 | 5 変数以上の math function は実用上ほぼ無く、一般化のための広範囲リファクタコストに対するベネフィットが小さい |

### 残された未着手項目 (新機能)

| 項目 | 内容 | 工数見込 |
|---|---|---|
| **拡張可能 CAS タワー** | `declare cas-type` / `declare cas-subtype` / `class CASCanonical` でユーザが半順序タワーを構築。Gaussian poly などの中間型を扱える ([type-cas-tower.md](./type-cas-tower.md) 参照) | 大規模 (5 phases) |
| **reshape の関数引数・match 注釈拡張** | 現状 `def x : T := e` と `(e : T)` のみで reshape が挿入される。`def f (x : T) := ...` の引数注釈、`match` clause 注釈、typed `let` も同様に対応 | 中規模 |
| **`def n : T := zero` 既存 bug** | 引数なし typeclass method (`zero`/`one`) を具体型注釈で呼ぶと "Expected function, but found: 0" エラー。reshape 無関係の既存 dispatch 問題 | 中規模 (調査必要) |
| **観察型 join の subtype-aware 化** | `joinObservedTypes` は文字列ベース heuristic。`Integer` ⊂ 他型は対応済だが、`Frac Integer` ⊕ `Poly Integer [x]` 等の真の lattice join は未対応 | 中規模 |
| **`lookupDerivative` の Haskell-side primitive 化** | `declare derivative` を辞書 lookup 方式に。型推論を経由せず DerivativeEnv を引く primitive を `Type/Check.hs` に登録 | 中規模 |
| **Phase 9 declare-key 機構** | `declare-key derivative` 等の汎用宣言キー仕組み。`declare derivative` 等をライブラリ層に押し出す | 中規模 (新構文 + desugar) |
| **`coefficients` の Frac 係数バグ** | `coefficients (y^2 + 3/4) y` が誤った係数を返す。`quadratic-equation.egi` で観測。level-5 `Poly (Frac Integer) [..]` の coefficient extraction の不具合 | 中規模 (CAS lib バグ) |
| **nested radical の固定点未到達** | `((-1+√5+√(-10-2√5))/4)^5 = 1` 等、nested radical を含む高次冪展開が declare rule の固定点で簡約されない。Apply3 (`#(...)` 形) の root expansion 規則が要追加 | 中規模 (declare rule 拡充) |
| **mini-test/107 deep-nested-types** | `Poly (Frac Integer) [x, y]` などの canonical 形比較で mismatch | 中規模 (reshape / canonical) |
| **mini-test/68 ppp-and-discard** | `PPDiscard` 構文がパーサで未対応 | 軽量 (parser) |
| 異種 Tensor 型の扱い | `Tensor (Poly Integer [sqrt 2])` と `Tensor (Frac Integer)` の join は `Tensor MathValue` にフォールバック。**方針**: Tensor は homogeneous を要求し、異種混在は明示的に統一型に揃えてから格納 | 軽量 (方針既決) |
| 関数シンボル (`function (x)`) の CAS 型統合 | 既存の関数シンボル機構 ([function-symbol.md](./function-symbol.md)) と新 CAS 型システムの統合方針が未定。当面は既存挙動を保持し、原子集合 (`Poly ... [f x]`) への出現は禁止 | 中規模 (個別設計) |
| `inspect` の REPL 統合 | REPL で式評価時に静的型 + 観察型を自動表示 | 小〜中 (UI 系) |
| 型注釈提案機能 | 観察型をコピペ可能な形で出力 (`suggest:` ラベル付け) | 小 (UI 系) |
| Pattern function 内での CAS 型の扱い | 部分的 | 別途必要に応じて |

### Phase 9 declare-key 詳細 (参考)

`declare derivative <name> = <expr>` のような宣言は現状 hardcoded だが、汎用化して
`declare-key derivative` で予約後、ライブラリで処理を書ける形にする。

```egison
-- 想定
declare-key derivative
declare derivative sin = cos
-- ↓ desugar
-- => onDeclareKey "derivative" "sin" cos  (ライブラリで定義)
```

これにより `Integrable`、`Substitutable` 等のユーザ独自クラスも同じ宣言機構で拡張可能になる。

### 細かい摩擦点 (回避策で動作)

| 項目 | 内容 |
|---|---|
| Egison 識別子に `_` が使えない | `identChar = alphaNumChar` のため `trig_pythagorean` 等は parse できない。`trigPythagorean` (camelCase) を推奨。subscript 構文 (`x_1`) と衝突するため修正困難 |
| `$y` の expression context での silent parse | `def y := $a` は parse error にならず奇妙な解釈をする。parser に check 追加が望ましい |
| `∂/∂'` のループ on unknown apply1 | hardcoded ケースのいずれにもマッチしない user mathfunc apply1 が後続 `term`/`poly` ケースに落ちて無限ループ。`chainPartialDiff` の再帰アームで `chainPartialDiff` 自身を呼ぶことで吸収済み |
| Cabal キャッシュ | `cabal run` がキャッシュを返すことがあり、再ビルド漏れに注意。`cabal clean` で対応 |
| `Sd` (積分) の lib バグ | pre-existing。未対応 |
| LHS 中の `=` 演算子 | `declare rule`/`declare derivative` の LHS で `=` が二項演算子として解釈される問題あり。回避策で動作中 |
