# Egison CAS 型タワーの拡張可能化 (設計と決定記録)

このドキュメントは、Egison の CAS 型システムにおける **型昇格タワー (type promotion tower)** のユーザ拡張の設計過程と決定 (D1–D5) を記録する。**実装済** — 実装フェーズの記録は [type-cas-tower-implementation.md](./type-cas-tower-implementation.md)、現状のユーザ向け仕様は [type-cas.md §ユーザによるタワー拡張](./type-cas.md) を参照。

最終更新: 2026-07-06 (実装完了を反映し、計画時制の記述を整理)

---

## 0. 一言サマリー

現状の Egison CAS 型タワーは `Integer ⊂ Frac Integer ⊂ Poly Integer [..] ⊂ Poly (Frac Integer) [..] ⊂ Frac (Poly Integer)` の **5 段階固定の全順序**。これでは Gaussian 整数係数の多項式 (`Poly (Poly Integer [i]) [..]`) のような **中間型** を体系的に扱えない。

ユーザは `declare cas-type` で正規形に透明なエイリアスを与え、`declare cas-subtype` で半順序に辺を追加できる (D1 半束検査つき)。昇格・型注釈の組み替えは既存の `casReshapeAs` 一本が担い (D5 制限言語)、型ごとの正規化フック (`class CASCanonical`) は不要になったため廃案。商は独立機構 `declare cas-quotient` に分離 (D4)。

---

## 目次

1. [背景: 現状のタワーと限界](#1-背景-現状のタワーと限界)
2. [動機: 中間型の必要性](#2-動機-中間型の必要性)
3. [基本構想](#3-基本構想)
4. [実装フェーズ](#4-実装フェーズ)
5. [既存機構との関係](#5-既存機構との関係)
6. [具体例](#6-具体例)
7. [代替案・トレードオフ](#7-代替案トレードオフ)
8. [決定事項 D1–D5](#8-決定事項-d1d5)

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

`casNormalize` (Math/CAS.hs) が一意の canonical form を選択 (level の小さい方を優先)。部分順序関係 (実は全順序) は当時 `Type/Join.hs` の `isSubtype` が表現していた (実装後は `Type/Subtype.hs` に一本化され `Type/Join.hs` は削除済み)。

### 1.2 限界

タワーが **線形** であるため、以下が表現できない:

1. **代数的拡大係数の多項式**: $\mathbb{Z}[i][x]$, $\mathbb{Z}[\sqrt{2}][x, y]$ — **本拡張 (タワー) の対象**
2. **複数原子集合の組合せ**: $\mathbb{Q}(i)[x, y]$, $\mathbb{Z}[i, \sqrt{2}]$ — **本拡張 (タワー) の対象**
3. **異なる代数構造**: 四元数 $\mathbb{H}$, 八元数 $\mathbb{O}$ — Phase ε 必須 (将来課題、スコープ外)
4. **Modular arithmetic**: $\mathbb{Z}/n\mathbb{Z}$ — タワー外の**商機構** ([type-cas-quotient.md](./type-cas-quotient.md)、§8 D4)
5. **異なる多項式表現基底**: 標準基底 vs Chebyshev vs Bernstein — **スコープ外** (再解釈型、§6.2)
6. **連分数 / 部分分数分解** など特殊表現 — Phase ε 必須 (将来課題、スコープ外)

1–2 が本拡張の守備範囲。3–6 は「線形タワーで表現できない」こと自体は事実だが、その解決策はタワーの拡張ではない (2026-07-04 の整理)。

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
-- 辺は関係のみを宣言する (D5 決定: where embed 句は存在しない)
declare cas-subtype Integer ⊂ GaussianInt
declare cas-subtype GaussianInt ⊂ GaussianPoly
declare cas-subtype GaussianPoly ⊂ Frac (Poly Integer [i, ...])
```

**埋め込み写像はユーザが書かない** ([§8 D5](#8-決定事項-d1d5) の制限言語決定): 各辺の昇格は処理系が一律に `casReshapeAs <target>` (構造書き換え・値不変) で行う。これにより coherence (経路独立性) が構成から成立し、値を変える「埋め込み」— `ℤ → ℤ/nℤ` の射影 (非単射) など — は**構文的に書けない**。商型は ⊂ 辺ではなく商機構で扱う ([§8 D4](#8-決定事項-d1d5))。

**実装方針**:
- `declare cas-subtype` は**型レベルの順序情報のみ**を登録する (実装は `EvalState.casSubtypeEdges`; runtime 側に登録するものは無い)
- `isSubtype` を dynamic にする (実装では `Type/Subtype.hs` 新設に発展し、旧 `Type/Join.hs` は削除)
  - 現状: hard-coded match (e.g., `isSubtype TInt (TFrac TInt) = True`)
  - 実装後: `EvalState.casSubtypeEdges` の宣言辺 + 骨格規則の閉包 (`Type/Subtype.hs`)
- subtype 関係の **lattice 化**: 半順序 → 任意 2 型の **join** (LUB) を計算可能に。declare 時に join 半束性を検査 ([§8 D1](#8-決定事項-d1d5))
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

**規則の帰属の原則 (2026-07-04 決定)**: `declare rule` は従来どおり **`MathValue` 大域のみ**とし、型スコープ規則 (`in T`) は導入しない ([§8 D2](#8-決定事項-d1d5))。等式理論の置き場は次の 3 分類で完結する:

1. **タワー型** — 単一の大域理論を共有する部分環の束なので、規則は大域 `declare rule auto` (現状のまま)
2. **シンボル担持商** (`i^2 = -1`、`ε^5 = 0`、1 の冪根) — シンボルの大域一意性がスコープを代替するため、これも大域規則で健全 ([type-cas-quotient.md §2](./type-cas-quotient.md))
3. **係数領域の商** (ℤ/nℤ 等) — タワー外の商機構の `reduce` に封じ込める ([type-cas-quotient.md](./type-cas-quotient.md)、[§8 D4](#8-決定事項-d1d5))

タワー型に「追加の等式」を課したくなったら、それは生成イデアルによる商の偽装なので 3 の機構を使う。

`canonicalize` の責務は**構造選択のみ** — target type の入れ子形への書き換え (`primCASNormalizeAs` = `casReshapeAs` の拡張)。大域規則の適用は従来どおり算術演算経路 (`mathNormalize` / `iterateRulesCAS`) の仕事であり、`canonicalize` は規則環境に触れない (**規則機構は現状実装から一切変更不要**)。

`canonicalize` のシグネチャは `MathValue → a` の **retraction** (型内の endo 関数 `a → a` ではない)。coerce/reshape の入口では値がまだ target 型の形をしておらず、trust-the-annotation 原則により所属保証もないため。像上での冪等性 (`canonicalize (canonicalize v) = canonicalize v`) は法則として課す。

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

## 4. 実装フェーズ (完了)

実際に実装したフェーズ (手順と検収は [type-cas-tower-implementation.md](./type-cas-tower-implementation.md)):

| Phase | 内容 | 状態 |
|---|---|---|
| **α** | 透明 type alias (`declare cas-type`) | 実装済 |
| **β** | `declare cas-subtype` + dynamic `isSubtype` + D1 半束検査 | 実装済 |
| **γ′** | 平坦既定形 + reshape 吸収律 (`casReshapeAs C . casReshapeAs B = casReshapeAs C`) | 実装済 |
| (旧 γ/δ) | `class CASCanonical` + `primCASNormalizeAs` + coerce dispatch 化 | **廃案** — D5 制限言語の採用で、昇格は既存 `casReshapeAs` 一本に集約され型ごとの正規化フックが不要になった |
| **ε** | 新 `CASValue` constructor 用の Haskell-side 拡張機構 (Quaternion 等) | 未着手 (スコープ外の将来課題) |

商機構 q1–q4 は独立線として実装済 ([type-cas-quotient.md](./type-cas-quotient.md))。

---

## 5. 既存機構との関係

### 5.1 `Coerce` typeclass (廃止済) との関係

- 旧 (typeclass): user-extensible だが識別 instance は意味なし、A1 elaboration を阻害
- 現 (named function + Haskell primitive): trust the annotation、`casNormalize` を再実行

一時は CASCanonical typeclass への回帰も構想したが、D5 (制限言語) と γ′ (平坦既定形 + 吸収律) の決定で正規化フック自体が不要になり、廃案となった。

### 5.2 `reshape` 機構との関係 (旧 `Embed` typeclass)

> **2026-05 更新**: 旧設計案では `Embed` typeclass を介して widening を行う想定だったが、Phase C で `Embed` typeclass と `coerceTo*` 関数群はすべて廃止され、**`reshape` primitive 一本に統一**された。型注釈 (`def x : T := e` または `(e : T)`) を書くだけで AST elaboration が `IReshape T e` を挿入し、`casReshapeAs T v` が runtime に CAS 構造を target type に書き換える。

> **2026-07-04 更新 (D5 決定)**: 旧構想では辺ごとのユーザ embed を `casReshapeAs` の dispatch table に登録する予定だったが、制限言語の採用によりユーザ embed 自体が消えた。`declare cas-subtype A ⊂ B` は**型レベルの順序情報のみ**を登録し、runtime の昇格は既存の `casReshapeAs` 一本がそのまま担う — **dispatch table の拡張は不要**。`casReshapeAs` は既に任意の nested CAS 型への構造書き換え (atom 分離を含む) を実装済みなので、runtime 側は現状のままで拡張可能タワーに対応できる。

### 5.3 `casNormalize` (Math/CAS.hs) との関係

`casNormalize` は **デフォルト (平坦) canonical form** を返す。target-type-specific な形は `casReshapeAs` が担う:
- 演算経由の値: `casNormalize` でデフォルト形式 (演算結果は常に平坦形で返る — γ′)
- 型注釈: `casReshapeAs` で target type 形式 (注釈を書いた場所にだけ入れ子形が現れる)

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

-- subtype 関係 (辺は関係のみ — 昇格は casReshapeAs が担う。D5)
declare cas-subtype Integer ⊂ GaussianInt
declare cas-subtype GaussianInt ⊂ GaussianPoly

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

### 6.2 スコープ外とした例 (2026-07-04)

旧版がここに載せていた 2 例は、枠組みの整理 (ユーザ判断) により削除した:

- **Chebyshev 基底 (`ChebyshevPoly`)** — 同じ `CASPoly` 係数列を別の基底 ($T_k$) で読む「**再解釈型**」。タワーの基盤前提「**表現の読み ⟦·⟧ : CASValue → 値 が型に依存しない**」(観察型・trust-the-annotation・semantic Eq はすべてこの上に立つ) を壊す点で商型と同型の問題であり、タワーの管轄外。商 (値領域を変える) は商機構 ([type-cas-quotient.md](./type-cas-quotient.md)) が引き取ったが、再解釈 (読みを変える) は当面**スコープ外**とし、必要になったら「全単射再解釈型」として別機構を設計する。透明に扱うと `cp + p` のような基底違いの係数列加算が通ってしまう unsoundness も、この前提違反の症状。実装する日のための数学的補足: base は `Poly (Frac Integer) [..]` が必須 ($T_k$ の ℤ-span は乗法で閉じない — $x^2 = (T_2+T_0)/2$)、乗算は線形化規則 $T_m T_n = (T_{m+n} + T_{|m-n|})/2$ で基底内に閉じる
- **連分数表現 (`ContinuedFraction`)** — 新しい `CASValue` constructor (Phase ε、Haskell 改変) が必須で、既定 3 のとおり最初から α–δ スコープ外

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

## 8. 決定事項 D1–D5

**設計全体の指針 (2026-07-04、ユーザ)**: **理論をできるだけ単純に保つ** — 選択肢が拮抗する場合は、メタ理論が単純になる方を採る。

**D1–D5 はすべて決定済み (2026-07-04)**。確定事項の一覧:

- **D1**: join は単一宣言順序上の一意 LUB。declare 時に join 半束性を検査 (欠落辺提案つきエラー)、細化は warning + 細化単調性定理
- **D2**: 型スコープ規則は導入しない — `declare rule` は `MathValue` 大域のみ (規則機構は現状実装から変更不要)
- **D3**: cas-type は `:=` の**透明エイリアスのみ** (明示的異議なしのため確定扱い。nominal は商機構に内在)
- **D4**: 商型はタワー外の商機構 ([type-cas-quotient.md](./type-cas-quotient.md)) に分離
- **D5**: 制限言語 — `where embed` 句を廃止し、cas-subtype は `A ⊂ B` の**関係宣言のみ**。昇格は `casReshapeAs` 一本、coherence は吸収律 1 本の無条件定理

**未決事項は残っていない — 全フェーズ実装済 ([type-cas-tower-implementation.md](./type-cas-tower-implementation.md))。**

### D1: join の曖昧性ポリシー — **決定 (2026-07-04): (i) declare 時検査 + 完備化**

設計指針「理論をできるだけ単純に」に基づき **(i) を採用**。join は常に**単一の宣言順序 (骨格 + ユーザ辺 + `MathValue` top) 上の一意な LUB** とする。検討した代替案は不採用: (ii) MathValue フォールバック (静的型が退化し下流の instance 解決が壊れる)、(iii) 層化 join (骨格/ユーザ型でjoin の探索空間を切り替える 2 段規則 — 保守性は構成的に立つが、join が単一順序の LUB でなくなる複雑さの方が高くつく)。

**不変条件**: 宣言順序は常に **join 半束** (任意ペアの極小上界が一意)。`declare cas-subtype` は declare 時にこれを検査し、破れる場合は**欠落辺を提案してエラー**にする:

```egison
declare cas-subtype Poly Integer [..] ⊂ RationalGaussianPoly   -- ℤ[x..] ↪ ℚ(i)[x..]
declare cas-subtype Frac Integer ⊂ RationalGaussianPoly        -- ℚ ↪ ℚ(i)
-- 2 本目でエラー: pair (Poly Integer [..], Frac Integer) の極小上界が
--   {Poly (Frac Integer) [..], RationalGaussianPoly} の 2 つに割れる。
-- 提案: declare cas-subtype Poly (Frac Integer) [..] ⊂ RationalGaussianPoly
--   (数学的に真 (ℚ[x..] ⊂ ℚ(i)[x..])。宣言すれば level 4 が一意の極小に戻る)
```

**完備化はユーザの義務だが、常に真の辺で果たせる**: 曖昧ペア (a, b) の複数の極小上界はいずれも a ∪ b の生成部分環を含むので、数学的に偽の辺を強いられることはない (最悪でも生成構造 1 ノードの命名が必要になるだけ)。

**join の細化は許容し warning**: 一意性検査は「既存 join が変わらないこと」までは保証しない。既存 join の真下にノードを挿す宣言 (例: N = ℚ(y)[x] を ℤ[x] と ℚ(y) の上・旧 join ℚ(x,y) の下に置き、`N ⊂ Frac (Poly Integer [..])` まで完備化) は、既存ペアの join を level 5 → N に**細化**する。これは禁止せず warning に留め、保守性の代わりに次を保証とする:

> **細化単調性定理**: 宣言の追加に対して join は細化方向 (⊑) にしか動かず、プログラムの**値は決して変わらない**。変わりうるのは静的型の精密化 (とそれに伴う instance 選択・表示) のみ。

**実装注**:

- 検査は型スキームレベル (原子集合の側条件つき)。宣言ノードは有限個なので決定可能で、dynamic `isSubtype` (Phase β) と同じ機構に乗せる
- 独立に開発された宣言集合の合成 (各々単独では合法、同時 load で曖昧) では、解消する glue 辺がどちらにも属さないという合成脆弱性があるが、単一キュレータ運用の現状では実害なし。将来問題化したら「declare 時エラー」を「曖昧ペアの使用地点エラー」に遅延する変種 (不変条件を「実際に使われる join が一意」に弱める) で緩和できる

### D2: 型スコープ規則の要否 — **決定 (2026-07-04): 導入しない**

旧 D2 は「型スコープ規則の発火点 (境界のみ vs 演算ごと)」を、D4 決定後の再定式化では「型スコープ規則はそもそも必要か」を問うていた。ユーザ確認により**不要**と決定。根拠:

- 動機例だった ℤ/5ℤ と打ち切り羃級数 Series5 = ℤ[x]/(x⁵) は**どちらも商**であり、商機構 ([type-cas-quotient.md](./type-cas-quotient.md)) の per-op reduce が担う (旧 D2 の分析 — 表示乖離・中間肥大・商準同型なら境界簡約でも意味論的に正しい — は quotient.md §1/§5 に移設済み。発火点ジレンマ自体が商側で構造的に解消される)
- タワー型への「追加の等式」は生成イデアルによる**商の偽装**なので、原理的に商機構の管轄
- シンボルが担える関係式 (`i^2 = -1`、`ε^5 = 0`、1 の冪根) は既存の `declare symbol` + 大域 `declare rule auto` で健全 — シンボルの大域一意性がスコープを代替 (quotient.md §2)
- 基底変更 (Chebyshev) は新しい等式ではなく表現の読み替えであり、等式理論 (規則) の問題ではない (その後 2026-07-04 (5) で再解釈型としてスコープ外に — §6.2)

帰結: 規則帰属は「`declare rule` は `MathValue` 大域のみ」で最終確定 (§3.4 に最終形を記載)、`in T` 構文は導入しない (既定 2 は撤回)。**規則機構は現状実装から一切変更不要**。

### D3: cas-type の透明性 — **決定 (2026-07-04): 透明エイリアスのみ**

nominal (不透明、newtype 相当) を強制していた例が、2026-07-04 の整理で両方ともタワー外に出た: Chebyshev = 再解釈型 → スコープ外 (§6.2)、ℤ/nℤ = 商 → 商機構 (D4)。タワーに残る cas-type はすべて「構造そのものが意味を決める」型であり、**透明エイリアス (単一化で展開) で十分**:

```egison
declare cas-type GaussianInt := Poly Integer [i]
def a : GaussianInt := 1 + i
def b : Poly Integer [i] := a    -- 同一型なのでそのまま通る
gcd a (2 + i)                     -- instance EuclideanDomain GaussianInt は
                                  -- Poly Integer [i] の instance と同一視される
```

これはタワーの基盤前提の帰結でもある: タワー内では表現の読み ⟦·⟧ が型に依存しない (§6.2) ので、「表現が同じで意味づけだけ違う」型はタワーにそもそも存在できず、nominal の出番がない。nominal 性が必要な商型は、`declare cas-quotient` が**新しい型を導入する**形で商機構側に内在する (透明の選択肢が最初からない — [type-cas-quotient.md §3](./type-cas-quotient.md)。`x ^ (a : Zmod5)` のような型混同を静的に弾く例もそちらに移設)。

帰結: **Phase α は当初見積どおり「透明 type alias 機構」のみ (小)**。unifier への不透明型コンストラクタ追加も不要。(2026-07-04、明示的異議なしのため確定扱い。)

### D4: 商型の扱い — **決定 (2026-07-04): タワー外の独立機構に分離**

ユーザ判断: **商型 (ℤ/nℤ 等) はタワーの枠組みでは扱わない**。根拠:

- **値保存の原理的不成立**: タワーの意味論は「同一の値の上にファイバーする表現たち」(⟦embed v⟧ = ⟦v⟧、単射 = mono)。射影 (`8 ↦ 3`) は値そのものを変える epi であり、この枠に入らない
- **等価性が型依存になる**: タワー型の等価性は ambient 領域から `Eq MathValue` 一本で一様に継承される。商では x ~ y ⟺ reduce(x−y) = 0 という**型ごとの等価性**が必要で、境界の正規化規律では隠しきれない — 未簡約の中間値 12 と 2 は mod 5 で等しいが `x − y = 0` 判定では不等になり、`#x` 非線形パターンの意味論的照合も誤る
- join・保守性・観察型など、タワーの主要概念がいずれも商には適用できない (ℤ と ℤ/5ℤ に共通の包含先はない)

原則として整理すると: **タワー = 単一の大域理論を共有する部分環の、単射埋め込みによる束。理論が変わる場所は商であり、別機構で扱う**。

分離した機構の構想は [type-cas-quotient.md](./type-cas-quotient.md) に切り出した — 宣言形 `declare cas-quotient Q := T by reduce`、順序に不参加 (横断は明示 `proj`/`repr` のみ、混在演算は型エラー)、準同型演算の自動導出 + per-op reduce、型 dispatch される `Eq`、合同律の宣言時検査。旧 D4 の技術的内容 (準同型演算の base 委譲可能性、`inv`/`gcd`/比較のパターン2 必須) はそちらへ引き継いだ。

タワー側に残る要件は **⊂ 辺は単射 embedding に限定** — これは維持する。なお D5 (制限言語) の決定により embed はユーザが書くものではなくなったため、単射性・値保存は `casReshapeAs` の性質として一括保証され、射影を ⊂ 辺に書く誤りは**構文的に不可能**になった。

### D5: coherence 検査の実装水準 — **決定 (2026-07-04): 制限言語 (`where embed` 句の廃止)**

**問題**: 半順序では同じ 2 型間に複数の昇格経路が生じる。§6.1 の宣言群では

```
         Integer
     (組み込み) ↙   ↘ (宣言辺)
Poly Integer [..]   GaussianInt
     (宣言辺) ↘   ↙ (宣言辺)
        GaussianPoly
```

のような**菱形**が生じ、`coerce 3 : GaussianPoly` の結果は経由に依存してはならない (coherence)。embed がユーザの書く任意の Egison 関数だと、この可換性はプログラムの外延的性質なので**検証は決定不能**。入れ子位置を誤った embed は `CASPoly [CASTerm (CASInteger 3) []]` (係数が生の ℤ — 誤) のような値を静かに生み、semantic Eq (`x - y = 0`) では等しいまま**構造マッチ・term 級規則の発火・表示だけが分岐**する。検討した水準: 法則文書化のみ (保証なし) / declare 時 PBT (菱形列挙 + サンプル検査 — 証明ではなく、定理も条件付きになる) / 制限言語。

**決定**: 設計指針「理論をできるだけ単純に」に基づき**制限言語**を採用。`where embed` 句を構文から**廃止**し、辺は関係のみを宣言する (`declare cas-subtype A ⊂ B` — §3.3)。埋め込み写像は常に処理系側の `casReshapeAs <target>` (構造書き換え・値不変) が担う。帰結:

- **coherence は無条件の定理になる**: どの昇格経路も最終ステップが `casReshapeAs <最終 target>` なので、証明義務はユーザ宣言ごとの可換図式ではなく、Haskell 関数 1 本の**吸収律**に潰れる:

  > `casReshapeAs C (casReshapeAs B v) = casReshapeAs C v`

  これは我々が一度証明 (+ mini-test / QuickCheck で検査) すればよく、ユーザはどんな宣言をしても壊せない
- 菱形列挙器・宣言時 PBT・法則文書は**すべて不要** (書きようがないバグを検査する装置は要らない)
- ⊂ 辺の単射性・値保存 (D4) も自動: `casReshapeAs` は値を変えないので、商の射影を ⊂ 辺に書く誤りが**構文的に不可能**になる
- 実装も減る: 辺宣言は型レベルの順序情報のみ (§3.3)、runtime は既存の `casReshapeAs` 一本で dispatch table 拡張が不要 (§5.2)

**表現力の確認**: α–δ スコープの全辺 — `Integer ⊂ GaussianInt` (恒等)、`GaussianInt ⊂ GaussianPoly` / `Frac Integer ⊂ RationalGaussianPoly` (入れ子化) — は構造書き換えで書ける。構造書き換えでない変換は既にタワー外に出ている (射影 = 商機構 [D4]、基底変換 = スコープ外 [§6.2]、関係式 = シンボル + 大域規則 [D2])。将来 reshape で書けない正当な embed が現れたら、`where embed` + 法則 + PBT を後付けする退避路を残す。

### 既定として採用済み (異議があれば変更)

**1. `canonicalize` 失敗は素通し (trust-the-annotation 踏襲)**

値が target 型に所属しない場合、エラーにせずデフォルト canonical form のまま残す:

```egison
(x + 1 : Integer)                          -- 現行 reshape と同じ: x + 1 のまま
def g : GaussianPoly := coerce (1/(x+1))   -- 非モノミアル分数は GaussianPoly の構造を作れない
                                            -- → level 5 (CASFrac) 表現のまま (静的型と表現が乖離)
```

乖離した値は下流で自然に顕在化する (`poly (poly integer)` マッチャーが失敗する、term 級規則が発火しない等)。エラーにしない理由は、REPL の「観察 → 注釈」workflow を止めないことと、既存 reshape の意味論との一貫性。代替案 (ランタイムエラー / warning 付き素通し) を選ぶ余地は γ の `primCASNormalizeAs` 実装時まで残る。

**2. 型スコープ規則の構文 `in T` — 撤回 (2026-07-04 決定、D2)**

旧案は Series5 = ℤ[x]/(x⁵) を例に `declare rule auto term in Series5 ...` を仮置きしていたが、Series5 は商であり管轄が商機構 ([type-cas-quotient.md](./type-cas-quotient.md)) に移った。専用シンボルで足りる場合は既存機構で書ける (`declare symbol ε` + `declare rule auto term ε^5 = 0` — 双対数/ジェットのイディオム)。**`in T` は導入しない**。

**3. 実装・論文スコープは Phase α–δ (ε 除外)**

ケーススタディと必要フェーズの対応:

| ケーススタディ | 必要なもの | ε 不要の理由 |
|---|---|---|
| ℤ[i] (`GaussianInt`) | α (エイリアス) | 既存 `Poly Integer [i]` そのもの |
| ℤ[i][x] (`GaussianPoly`) | β (辺) + γ (canonicalize) | 入れ子 `CASPoly` で表現可、reshape 機構は実装済み |
| ℚ(i) / ℚ(i)[x] (`RationalGaussianPoly`) | β + γ | 同上 (`Frac`/`Poly` の入れ子) |
| ℤ/5ℤ (`Zmod5`) | **商機構** ([type-cas-quotient.md](./type-cas-quotient.md) q1–q4、タワー外) | 表現は `CASInteger` のまま、reduce = mod 射影 |

ε が本当に必要なのは Quaternion (非可換乗算 — 可換性前提の `CASTerm`/Monomial 表現では表せない) や ContinuedFraction (新しい再帰構造) 等のみで、これらは論文の主張に新しい論点を足さないため将来課題に切る。商機構 (q1–q4) はタワー α–δ と独立の実装線であり、論文ではタワー・シンボル担持商・係数商の **3 直交機構** ([type-cas-quotient.md §9](./type-cas-quotient.md)) として並置する。

---

## 9. 関連ドキュメント

- [type-cas.md](./type-cas.md) §実行時の型昇格タワー — 現状の 5 段階タワーの定義
- [type-cas.md](./type-cas.md) §「自動変換の仕組み」 — 現状の `reshape` primitive 実装 (将来拡張への hook)
- [type-cas.md](./type-cas.md) §「既知の制限と未解決課題」 — 残された課題

## 10. 改訂履歴

- 2026-05-06: 初版。半順序タワー拡張の構想をまとめる。実装は将来課題。
- 2026-05-07: §5.2 を更新 — `Embed` typeclass は Phase C で廃止済 (`reshape` primitive に統一) のため、cas-subtype 宣言の統合先を `reshape` dispatch table に変更。
- 2026-07-04: type-cas.md 側スケッチとの不整合を解消 (cas-type は `:=`、cas-subtype は `⊂ where embed` 形式に統一 — §5.2 の `where reshape` 表記も `embed` に修正、`canonicalize` は `MathValue → a` の retraction に統一)。規則帰属の原則 (等式理論は型スコープ、global は MathValue スコープとして下位に継承) を §3.4 に明記。§8 未決事項 (D1–D5) を新設。
- 2026-07-04 (2): §8 を具体例で拡充。D1 の曖昧性例を `RationalGaussianPoly` ベースに**修正** (旧例の GaussianPoly は係数 ℤ[i] のため `Frac Integer` の上界にならず誤りだった)。D2 に Zmod5 / Series5 での (a)(b) 対比・商準同型なら (a) でも意味論的に正しいという注意・MathValue 中間コードは両案共通の限界である旨を追加。D3 に unsound 具体例 (`cp + p`、`x ^ (a : Zmod5)`) と全 nominal 案のコスト。D4 に非単射 embed の反例・環準同型による正当化・パターン2 必須演算 (inv/gcd/比較)。D5 に菱形の具体構造・構造マッチ分岐の失敗例・「α–δ の全 embed は primCASReshape 合成で書ける」観察と制限言語出発案 (推奨を二択に更新)。既定に素通し例・Series5 での `in T` 構文例・ケーススタディ×フェーズ対応表を追加。
- 2026-07-04 (3): **D4 を決定に更新 — 商型はタワー外の独立機構に分離** (ユーザ判断)。根拠 (値保存の不成立 = mono/epi、型依存の等価性で `Eq MathValue`/`#x` が破綻、join・保守性・観察型の不適用) を明記し、機構の構想を [type-cas-quotient.md](./type-cas-quotient.md) に切り出し。原則「タワー = 単一の大域理論を共有する部分環の単射埋め込み束、理論が変わる場所 = 商 = 別機構」を明文化。これに伴い D2 を「型スコープ規則はそもそも必要か」に再定式化 (動機例が全て商だった; タワー型への追加等式 = 商の偽装、シンボル担持商は既存の大域規則で健全)、既定 2 (`in T`) を保留に格下げ、既定 3 の Zmod5 を商機構へ移動。規則帰属原則の型スコープ部分の取り消しは要ユーザ確認。
- 2026-07-04 (4): **D2 を決定に更新 — 型スコープ規則 (`in T`) は導入しない** (ユーザ確認)。§3.4 の規則帰属原則を最終形に書き換え (大域規則 / シンボル担持商 / 係数商の 3 分類で完結、`canonicalize` は構造選択のみ、規則機構は現状実装から変更不要)、既定 2 を撤回に更新。§6.2 Chebyshev 例の 2 点を修正 — base 型を `Poly (Frac Integer) [..]` に (T_k の ℤ-span は乗法で閉じない)、展開例を (1/4)T₃ − (5/4)T₁ + T₀ に訂正し線形化規則 T_m T_n = (T_{m+n} + T_{|m−n|})/2 を追記。
- 2026-07-04 (5): **Chebyshev 例と連分数例をスコープ外に** (ユーザ判断)。Chebyshev は「表現の読み ⟦·⟧ が型依存になる再解釈型」でタワーの基盤前提 (⟦·⟧ の型非依存性 — 観察型・trust-the-annotation・semantic Eq の土台) を壊すため管轄外と整理し、§6.2 を削除理由の記録に差し替え (数学的補足は残置)。連分数は ε 必須で当初からスコープ外。§1.2 の限界リストに各項目の帰属先 (タワー / 商機構 / ε / スコープ外) を注記。この帰結として **D3 を「透明エイリアスのみで足りる (実質決定候補)」に更新** — nominal 強制例が全てタワー外に出たため、Phase α は透明 alias のみ (小) に戻り、nominal は商機構 (q1) に内在。D5 の観察と既定 3 の表から Chebyshev を除去。
- 2026-07-04 (6): **D1 を決定に更新 — (i) declare 時検査 + 完備化を採用** (ユーザ判断)。設計指針「**理論をできるだけ単純に**」を §8 冒頭に明文化。join は単一宣言順序上の一意 LUB を不変条件とし (join 半束性を declare 時に検査、欠落辺提案つきエラー)、(ii) MathValue フォールバック・(iii) 層化 join は不採用。既存 join の細化は warning で許容し、保守性の代わりに**細化単調性定理** (join は細化のみ・値不変) を保証に。完備化が偽の辺を強いないこと (生成部分環論法) と、合成脆弱性への将来の緩和策 (使用地点エラーへの遅延) を実装注に記載。
- 2026-07-04 (7): **D5 を決定に更新 — 制限言語を採用し `where embed` 句を廃止** (ユーザ判断)。cas-subtype は `A ⊂ B` の関係宣言のみとなり、昇格は常に `casReshapeAs <target>`。coherence は吸収律 `casReshapeAs C (casReshapeAs B v) = casReshapeAs C v` 1 本の**無条件の定理**に潰れ、菱形列挙器・宣言時 PBT は不要、runtime の dispatch table 拡張も不要 (§3.3 / §5.2 / §6.1 / D1 例 / D4 注を一括更新)。**D3 も明示的異議なしのため確定扱い** (透明エイリアスのみ)。これで **D1–D5 全決定・未決事項ゼロ** — Phase α に着手可能。
