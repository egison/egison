# Matcher capability 実装契約

> 状態：2026-09-03（単一 `Matcher` 型と等式単一化への再実装後）

本文書は，Egison 本体における matcher の型付けの要点を記す．形式規則と証明の正本は
`type-pm-mech4`（統合 core，推論の健全性・主要性・完全性，実行時安全性）であり，
Haskell 実装は論文（type-pm-paper 論文1）§4–§5 の規則をそのまま実装する．
[type-pm-conformance.md](./type-pm-conformance.md) と
[type-pm-compatibility.md](./type-pm-compatibility.md) は旧 slot 設計
（`MatcherSlot`，A/R 変数，一方向 checking）に基づく記述であり，歴史的参照として残す．

## 1. 型と変数

matcher の内部型は一つだけである．

```haskell
TMatcher Capability Type      -- Matcher κ τ
```

capability κ と ordinary type（通常の値の型）τ は別の sort であり，別の代入を持つ．

```haskell
Capability = CapAny | CapVar CapVar | CapSkolem CapVar
           | CapCon TypeFormer [Capability] | CapTuple [Capability]
```

ordinary type 変数に A/R などの用途フラグはない．明示注釈の検査には両 sort の
skolem（特殊化できない定数）を使う．

## 2. 正準形と単一化

`applySubst` は代入の後に `normalizeMatcherProducts` を適用する．
`Matcher (κ1,…,κn) (τ1,…,τn)`（n ≥ 2）は `(Matcher κ1 τ1, …, Matcher κn τn)` へ正規化
する．`Matcher Any (τ1, τ2)` は積ではないので正規化しない．

型等式は両 sort の通常の最汎単一化（`unifyG`）で解く．方向性のある変換や subsumption
はない．`Matcher κ τ ≐ (σ1,…,σn)` は head expansion（`unifyMatcherProductG`）で解く：
κ が変数なら `(κ.1,…,κ.n)`，τ が変数なら `(τ.1,…,τ.n)` へ展開し，成分ごとに
`Matcher κ.i τ.i ≐ σi` を解く．κ が `Any` などの非積なら失敗する．capability の等式は
`unifyCapability` で解き，`Any` は定数であって wildcard ではない．

## 3. matcher literal

`matcher | pp_1 as e_1 with arms_1 | …` の推論（論文の G-Literal／Q-* 規則，
`Infer.hs` の `IMatcherExpr` 節と `inferPatternDef`）．

- literal 全体で一つの target 型 τ と一つの capability κ を共有する．
- header pp の推論（`inferHeader`）：
  - hole `$`：fresh な (χ, α) を hole の要求として返す．
  - wildcard／value pattern `#$x`：fresh な header capability．hole はない．
  - tuple `(pp_1,…,pp_n)`：capability `(κ_1,…,κ_n)`，target `(τ_1,…,τ_n)`．
  - 宣言済み pattern constructor `c pp_1 … pp_n`：constructor scheme を instantiate して
    field 型と result 型を得る．capability は宣言を fresh な capability 変数へ射影した
    もの（`capabilityTemplates`／`capabilitySkeleton`）：型変数 ↦ fresh χ，pattern 宣言を
    持つ型 `T τ̄` ↦ `T κ̄`，pattern 宣言を持たない閉じた型（`Integer`，`Char` など）↦
    `Any`．各 sub-header の matched 型と capability を field の target・capability と単一化
    する．
- header の matched 型を共有 target と，constructor／tuple header の capability を共有
  capability と単一化する．
- next matcher 式 e は通常の式として推論し，その型を hole が要求する型
  `Matcher χ_1 α_1`（1 hole）または `(Matcher χ_1 α_1, …, Matcher χ_n α_n)`（n hole）と
  単一化する．正準形により，積 matcher 型を持つ変数や application が複数の hole を
  同時に埋められる．
- data arm は target 型 τ の値を受け取り，hole の target の組のリスト `[(α_1,…,α_n)]` を
  返す（`inferDataClauseWithCheck`）．
- literal の型は `Matcher κ τ`．

静的条件：CatchAllLast（bare hole header の clause がちょうど一つ，最後）と ArmCoverage
（最後の arm が変数／wildcard であるか，言及した data former の全 constructor を general
arm で網羅）は型エラー．RootCoverage（言及した pattern former の全 pattern constructor に
general clause）は論文の CoverageOK に対応するが，production の部分 matcher（CAS view，
`string` の regex clause）を維持するため `--matcher-consistency-warnings` のときだけ
警告する．

### legacy CAS pattern view（core 外）

`MathValue`，`IndexExpr` などの pattern 宣言は数式の実行時 view を名付けるもので，
field と result の宣言型は target の証拠ではない（`legacyCasLeafFormer`）．この header では
matched 型と各 hole の (capability, target) を fresh にし，header capability だけを宣言
から取る．`--outside-egison-core-warnings` で報告する．

## 4. match の検査

`matchAll`／`match` は target 式，matcher 式，各 arm の pattern，本体を推論し，pattern の
要求 `Matcher κ_p τ_p` を matcher 式の型と等式で結ぶ（`checkMatcherAtSlot`；名前は旧設計
の名残で，現在は等式単一化）．`match` の `else` は arm 束縛の外で検査し，本体と同じ
結果型を持つ．TypePM では `else` は必須だが，Egison では省略可能なままとし，
`--match-without-else-warnings` で報告する．

## 5. 公開 signature

data constructor，pattern constructor，pattern function の scheme は閉じていなければ
ならず，field に現れる型／capability parameter は result から決定できなければならない
（`ensureDeclaredTypeVariables`，`ensureParametersDetermined`）．

## 6. 診断と計測

診断 option は動作を変えない．

```text
--matcher-consistency-warnings
--match-without-else-warnings
--pattern-hole-before-primitive-value-pattern-warnings
--nested-structured-primitive-pattern-pattern-warnings
--outside-egison-core-warnings
```

`--type-pm-metrics` は推論のカウンタを出力する：`match-sites`（match／matchAll 式），
`matcher-literals`，`matcher-clauses`，`product-next-matchers`（複数 hole を一つの積型式
で埋めた clause），`capability-combines`（and／or／forall／loop pattern の子同士の
capability 等式）．

主な実装箇所は次のとおりである．

- `Type/Types.hs`：二添字型，`normalizeMatcherProducts`，`capabilitySkeleton`．
- `Type/Subst.hs`：二 sort の代入と正準化．
- `Type/Unify.hs`：`unifyG`，`unifyMatcherProductG`，`unifyCapability`．
- `Type/Infer.hs`：`IMatcherExpr`，`inferHeader`，`checkMatcherAtSlot`，match の検査順．
- `EnvBuilder.hs`：公開 signature の閉性と parameters-determined 検査．
- `CmdOptions.hs`／`Eval.hs`：診断 option と計測．
