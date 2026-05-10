# Egison CAS 型タワーの拡張可能化 (将来構想)

このドキュメントは、Egison の CAS 型システムにおける **型昇格タワー (type promotion tower)** をユーザが拡張可能にする将来の設計をまとめる。実装はまだ着手しておらず、現状の 5 段階固定タワーで運用している。

最終更新: 2026-05-06 (初版)

---

## 0. 一言サマリー

現状の Egison CAS 型タワーは `Integer ⊂ Frac Integer ⊂ Poly Integer [..] ⊂ Poly (Frac Integer) [..] ⊂ Frac (Poly Integer)` の **5 段階固定の全順序**。これでは Gaussian 整数係数の多項式 (`Poly (Poly Integer [i]) [..]`) のような **中間型** を体系的に扱えない。

将来的にユーザが `declare cas-type` で新しい CAS 型を宣言し、`declare cas-subtype` で半順序関係を構築し、`class CASCanonical` で型ごとの正規化を提供できる仕組みを設計する。`coerce` 関数は target type に応じてユーザ定義の正規化に dispatch する API として進化する。

---

## 目次

1. [背景: 現状のタワーと限界](#1-背景-現状のタワーと限界)
2. [動機: 中間型の必要性](#2-動機-中間型の必要性)
3. [基本構想](#3-基本構想)
4. [実装フェーズ](#4-実装フェーズ)
5. [既存機構との関係](#5-既存機構との関係)
6. [具体例](#6-具体例)
7. [代替案・トレードオフ](#7-代替案トレードオフ)

---

## 1. 背景: 現状のタワーと限界

### 1.1 現状の 5 段階タワー

[type-cas.md §実行時の型昇格タワー](./type-cas.md) で定義された全順序:

| Level | 型 | 内部表現 | 数学的意味 |
|---|---|---|---|
| 1 | `Integer` | `CASInteger n` | $\mathbb{Z}$ |
| 2 | `Frac Integer` | `CASFrac (CASInteger _) (CASInteger _)` | $\mathbb{Q}$ |
| 3 | `Poly Integer [..]` | `CASPoly [CASTerm (CASInteger _) _]` | $\mathbb{Z}[x_1, ..., x_n]$ |
| 4 | `Poly (Frac Integer) [..]` | `CASPoly [CASTerm (CASFrac _ _) _]` | $\mathbb{Q}[x_1, ..., x_n]$ |
| 5 | `Frac (Poly Integer [..])` | `CASFrac (CASPoly _) (CASPoly _)` | $\mathbb{Q}(x_1, ..., x_n)$ |

`casNormalize` (Math/CAS.hs) が一意の canonical form を選択 (level の小さい方を優先)。`Type/Join.hs` の `isSubtype` で部分順序関係 (実は全順序) を表現。

### 1.2 限界

タワーが **線形** であるため、以下が表現できない:

1. **代数的拡大係数の多項式**: $\mathbb{Z}[i][x]$, $\mathbb{Z}[\sqrt{2}][x, y]$
2. **複数原子集合の組合せ**: $\mathbb{Q}(i)[x, y]$, $\mathbb{Z}[i, \sqrt{2}]$
3. **異なる代数構造**: 四元数 $\mathbb{H}$, 八元数 $\mathbb{O}$
4. **Modular arithmetic**: $\mathbb{Z}/n\mathbb{Z}$
5. **異なる多項式表現基底**: 標準基底 vs Chebyshev vs Bernstein
6. **連分数 / 部分分数分解** など特殊表現

これらは線形タワーに「挿入」できず、独自の代数的構造を持つ。

---

## 2. 動機: 中間型の必要性

### 2.1 具体例: Gaussian 整数係数の多項式

`Poly (Poly Integer [i]) [..]` を考える。例: $(2 + 3i) + (1 - i)x + 4x^2$

**有意義な内部表現の選択肢**:

| 型 | 表現 | 用途 |
|---|---|---|
| `Poly (Poly Integer [i]) [..]` | $x$ を主変数、係数が $\mathbb{Z}[i]$ の多項式 | $x$ について微分・展開 |
| `Poly Integer [i, x]` | flat な 2 原子多項式 | 全展開・symbol 操作 |
| `Frac (Poly Integer [i, x])` | level 5 形式 | rational 関数演算 |

同じ runtime 値が **複数の canonical form** を持つ。`casNormalize` だけでは決定不可能 — **ユーザの意図** (target type) が必要。

### 2.2 なぜ `coerce` が本質的に必要か

現状 (全順序タワー) では:
- `casNormalize` が一意の canonical form を返す
- `coerce` は `casNormalize` を呼ぶだけで済む (構造的に identity-equivalent)

将来 (半順序タワー) では:
- 同じ値に複数の canonical form が併存
- ユーザの target type 注釈で選択
- `coerce` は target-type-aware に進化する必要がある

つまり **`coerce` API は現状でも将来の拡張に向けた重要な抽象**。

---

## 3. 基本構想

### 3.1 4 つの component

```
┌──────────────────────────────────────────────────────────┐
│  Component 1: declare cas-type で新型を宣言               │
│  Component 2: declare cas-subtype で半順序関係を追加      │
│  Component 3: class CASCanonical で型ごとの正規化を提供   │
│  Component 4: coerce が target type に dispatch する      │
└──────────────────────────────────────────────────────────┘
```

### 3.2 Component 1: 新しい CAS 型の宣言

ユーザが既存型の組合せから新型を導出する構文:

```egison
-- 単純 alias (既存型の別名)
declare cas-type GaussianInt := Poly Integer [i]

-- 中間型 (内部表現は CASValue だが新しい意味づけ)
declare cas-type GaussianPoly := Poly (Poly Integer [i]) [..]

declare cas-type RationalGaussianPoly := Poly (Frac (Poly Integer [i])) [..]

-- 完全に新しい構造 (Haskell 拡張も必要)
declare cas-type Quaternion
  representation CASQuaternion (CASValue, CASValue, CASValue, CASValue)
  -- 新しい CASValue constructor を Haskell に追加
```

**実装方針**:
- 単純 alias: `Type/Types.hs` に type alias 機構を追加 (現状なし)
- 中間型: 既存の構造を組み合わせるだけなので type-level のみで完結
- 新構造: `data CASValue` への constructor 追加が必要 (Haskell 改変)

### 3.3 Component 2: 部分順序関係の宣言

```egison
-- 単純な subtype edge
declare cas-subtype Integer ⊂ GaussianInt where
  embed n := n   -- value-level identity (CAS 表現が共通の場合)

declare cas-subtype GaussianInt ⊂ GaussianPoly where
  embed v := primCASReshape v <GaussianPoly>

declare cas-subtype GaussianPoly ⊂ Frac (Poly Integer [i, ...]) where
  embed v := primCASReshape v <Frac form>
```

各 edge は **canonical embedding 関数** (向きあり) を持つ。

**実装方針**:
- `Type/Join.hs` の `isSubtype` を dynamic にする
  - 現状: hard-coded match (e.g., `isSubtype TInt (TFrac TInt) = True`)
  - 将来: `DeclareEnv` の `cas-subtype` エントリを参照 + 推移閉包計算
- subtype 関係の **lattice 化**: 半順序 → 任意 2 型の **join** (LUB) を計算可能に
- 推移閉包は `Type/Subtype.hs` (新規) で実装

### 3.4 Component 3: 型ごとの正規化

```egison
class CASCanonical a where
  canonicalize (v : MathValue) : a

-- ユーザ定義
instance CASCanonical GaussianPoly where
  canonicalize v := primCASNormalizeAs <GaussianPoly typedef> v

instance CASCanonical RationalGaussianPoly where
  canonicalize v := primCASNormalizeAs <RationalGaussianPoly typedef> v
```

**実装方針**:
- `class CASCanonical a` を lib に追加
- 標準型 (`Integer` / `Frac Integer` / `Poly` / etc.) の instance は組み込み
- ユーザは新型を declare すると同時に instance を登録
- 内部で `primCASNormalizeAs :: TypeRep -> CASValue -> CASValue` Haskell primitive を呼ぶ
- `primCASNormalizeAs` は target type ごとの reshape ロジックを Haskell 側に持つ

### 3.5 Component 4: `coerce` の進化

```egison
-- 現状 (5 段階固定)
def coerce (x : MathValue) : MathValue := primCoerce x  -- = casNormalize

-- 将来 (拡張可能タワー)
def coerce {CASCanonical a} (x : MathValue) : a := canonicalize x
```

annotation の target type が `CASCanonical` instance を持つ任意の型に dispatch する。**ユーザ API は変更なし**:

```egison
def n : Integer := coerce expr               -- 現状でも将来でも同じ
def g : GaussianPoly := coerce expr          -- 将来追加された型でも同じ書き方
def r : Frac (Poly (Frac Integer) [x]) := coerce expr  -- 深いネストでも
```

---

## 4. 実装フェーズ

| Phase | 内容 | 工数 | 依存 |
|---|---|---|---|
| **α** | type alias 機構 | 小 | なし |
| **β** | `declare cas-subtype` + dynamic `isSubtype` | 中 | α |
| **γ** | `class CASCanonical` + 標準 instance + `primCASNormalizeAs` Haskell primitive 群 | 大 | β |
| **δ** | `coerce` を `CASCanonical` ベースに dispatch 化 | 中 | γ |
| **ε** | 新 `CASValue` constructor 用の Haskell-side 拡張機構 (Quaternion 等) | 大 | γ + Math/CAS.hs 改変 |

**Phase α の最小実装**: type alias だけでも `GaussianInt := Poly Integer [i]` のような書き方を許容するだけで多くの用途をカバーできる。

**Phase β-δ がコアの拡張**: 半順序タワー + ユーザ canonical form 選択。

**Phase ε は別線**: 新しい `CASValue` constructor (Quaternion / Modular 等) は Haskell 改変必須なので、本拡張とは独立した path で進める。

---

## 5. 既存機構との関係

### 5.1 `Coerce` typeclass (廃止済) との関係

- 旧 (typeclass): user-extensible だが識別 instance は意味なし、A1 elaboration を阻害
- 現 (named function + Haskell primitive): trust the annotation、`casNormalize` を再実行
- 将来 (CASCanonical class): user-extensible で各型の canonical 化を提供

将来は **typeclass-based** に戻る形だが、目的が「conversion definition」ではなく「**canonicalization** definition」になる点が異なる。

### 5.2 `reshape` 機構との関係 (旧 `Embed` typeclass)

> **2026-05 更新**: 旧設計案では `Embed` typeclass を介して widening を行う想定だったが、Phase C で `Embed` typeclass と `coerceTo*` 関数群はすべて廃止され、**`reshape` primitive 一本に統一**された。型注釈 (`def x : T := e` または `(e : T)`) を書くだけで AST elaboration が `IReshape T e` を挿入し、`casReshapeAs T v` が runtime に CAS 構造を target type に書き換える。

将来の `declare cas-subtype` は `reshape` 経路上で **target-type-specific な `casReshapeAs` 拡張** として統合する形になる:
```egison
declare cas-subtype A ⊂ B where reshape v := f v
-- ↓ desugar
-- (内部で casReshapeAs を拡張する Haskell primitive 経路に登録)
```

つまり旧 Embed instance 生成の代わりに、`reshape` primitive の dispatch table 拡張という形で統合する。

### 5.3 `casNormalize` (Math/CAS.hs) との関係

現状の `casNormalize` は **デフォルト canonical form** を返す。将来の `primCASNormalizeAs` は **target-type-specific canonical form** を返す。両者は共存し:
- 演算経由の値: `casNormalize` でデフォルト形式
- 明示的 `coerce`: `primCASNormalizeAs` で target type 形式

### 5.4 `declare-key` 機構 (Phase 9) との関係

[type-cas.md §Phase 9](./type-cas.md) で構想されている `declare-key` は本拡張の generalization と見なせる:
- `declare-key cas-type` を予約
- `declare cas-type GaussianPoly := ...` がライブラリで処理される
- Phase 9 が先行すれば、本拡張はその上に library で実現可能

---

## 6. 具体例

### 6.1 Gaussian 整数の polynomial 拡張

```egison
declare symbol i
declare rule auto term i^2 = -1

-- 中間型を declare
declare cas-type GaussianInt := Poly Integer [i]
declare cas-type GaussianPoly := Poly (Poly Integer [i]) [..]

-- subtype 関係
declare cas-subtype Integer ⊂ GaussianInt where embed n := n
declare cas-subtype GaussianInt ⊂ GaussianPoly where embed g := g

-- canonical 化
instance CASCanonical GaussianPoly where
  canonicalize v := primCASNormalizeAs <GaussianPoly> v

-- 使用
declare symbol x

def p : GaussianPoly := coerce ((2 + 3*i) + (1 - i)*x + 4*x^2)
-- 内部: CASPoly [CASTerm <2 + 3i as Poly Integer [i]> [],
--                CASTerm <1 - i> [(x, 1)],
--                CASTerm <4> [(x, 2)]]
```

### 6.2 異なる正規化基底

```egison
declare cas-type ChebyshevPoly := Poly Integer [..]
  -- 表現は同じ Poly だが意味づけが異なる

instance CASCanonical ChebyshevPoly where
  canonicalize v := primConvertToChebyshev v
  -- Haskell-side の標準基底 → Chebyshev 基底変換

-- 使用
def p : Poly Integer [x] := x^3 - 2*x + 1
def cp : ChebyshevPoly := coerce p
-- cp の内部: $T_3(x) - 2 T_1(x) + ...$ の係数で表現
```

### 6.3 連分数表現

```egison
declare cas-type ContinuedFraction := ...  -- 新しい構造、Haskell 改変必要

instance CASCanonical ContinuedFraction where
  canonicalize v := primToContinuedFraction v

def cf : ContinuedFraction := coerce (355 / 113)
-- 連分数展開: [3; 7, 16] = 3 + 1/(7 + 1/16) ≈ π
```

---

## 7. 代替案・トレードオフ

### 案 A: 何もしない (現状維持)

- 利点: 単純、5 段階で多くの用途は十分
- 欠点: 中間型を扱えない、ad-hoc に新型を追加する余地が無い

### 案 B: 本構想 (拡張可能タワー)

- 利点: 任意の代数構造をユーザが宣言できる
- 欠点: 大規模実装、型推論・正規化の dynamic 化が必要

### 案 C: ハードコード追加 (case-by-case 拡張)

新しい中間型が必要になるたび、Egison maintainer が:
- `Type/Types.hs` に新 constructor 追加
- `Math/CAS.hs` の `casNormalize` に新ケース追加
- `lib/core/base.egi` に新 instance 追加

- 利点: 各拡張は独立、ユーザに学習コストなし
- 欠点: maintainer 依存、頻出する場合スケールしない

### 推奨

**短期**: 案 A (現状維持) で運用。実用上必要な型は既存の 5 段階で十分。

**中期**: 案 C で個別拡張を 2-3 件積む。具体的なニーズ (Gaussian poly など) が出てきた時点で hard-code 追加し、共通パターンを観察。

**長期**: パターンが見えてきたら案 B に移行。Phase α (type alias) → Phase β (subtype declare) → Phase γ-δ (canonical class + coerce dispatch) の段階的実装。

---

## 8. 関連ドキュメント

- [type-cas.md](./type-cas.md) §実行時の型昇格タワー — 現状の 5 段階タワーの定義
- [type-cas.md](./type-cas.md) §「自動変換の仕組み」 — 現状の `reshape` primitive 実装 (将来拡張への hook)
- [type-cas.md](./type-cas.md) §「既知の制限と未解決課題」 — 残された課題

## 9. 改訂履歴

- 2026-05-06: 初版。半順序タワー拡張の構想をまとめる。実装は将来課題。
- 2026-05-07: §5.2 を更新 — `Embed` typeclass は Phase C で廃止済 (`reshape` primitive に統一) のため、cas-subtype 宣言の統合先を `reshape` dispatch table に変更。
