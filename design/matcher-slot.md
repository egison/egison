# MatcherSlot の実装設計

## 背景

論文 *A Calculus and Type System for Ad-hoc Polymorphic Pattern Matching on
Non-free Data Types* (λ_P) の型システムは、**`Matcher τ` と `MatcherSlot τ_s τ_t`
の区別**を中核に置く。現在の Egison 処理系の型検査器は単一引数の `Matcher τ`
（`Type/Types.hs` の `TMatcher Type`）のみを持ち、`MatcherSlot` は存在しない
（`MatcherSlot` は実装後、論文執筆中に発見された概念）。本ドキュメントはその差を
埋める実装の設計をまとめる。

## 論文側の意味論（要点）

- **`Matcher τ`**: マッチャー「値」の型。`something : Matcher α`、ユーザ定義
  `matcher [...]` 式の結果、`multiset m : Matcher [a]` など。
- **`MatcherSlot τ_s τ_t`**: マッチャー「消費位置（スロット）」の型。2 引数:
  - `τ_s` = スロットの**構造型（パターン型）**。そのスロットがどんなパターンを
    分解できるか。
  - `τ_t` = **対象型（target）**。マッチ対象の値の型。
  - 構文糖: `MatcherSlot a` ≡ `MatcherSlot a a`（パターン型と対象型が一致する場合）。
- マッチャー**パラメータ**はスロット型を持つ。例: `def multiset {a} (m : MatcherSlot a a) : Matcher [a]`。
- **COERCE-MATCHER-TO-SLOT**: `Matcher τ_m` の値が `MatcherSlot τ_s τ_t` の
  スロットを埋めるのは、以下の**二重検査**が成り立つとき:
  - 構造: `fresh_rename(τ_m) ⊑ fresh_rename(τ_s)`（per-use-site structural admissibility）
  - 対象: `τ_m ∼ τ_t`（target unifiability）
  - fresh-rename により両検査は独立（評価順非依存）。
- **T-SOME**: `something : Matcher α`（bare 変数）。`τ_s` が変数頭のスロットには
  admissible、構成子頭のスロットには不可。
- **積 matcher**: `(m_1,…,m_k)`。COERCE-TUPLE-MATCHER（全成分が `Matcher` 値なら
  `Matcher (τ_1×…×τ_k)` に畳む）、COERCE-SLOT-TUPLE（積スロットを成分スロットに分解）。
- **matcher polymorphism**: パターンの型はマッチャー非依存（スロット情報はマッチ
  サイトでのみ流入）。

## 現在の実装の該当箇所（`TMatcher` タッチポイント）

型レベル中核:
- `Type/Types.hs`: `data Type` の `TMatcher Type`(122), `freeTyVars`(209),
  `typeConstructorName`(318), `typeExprToType`(374), `normalizeInductiveTypes`(413)
- `Type/Subst.hs`: `applySubst`(67)
- `Type/Unify.hs`: `TMatcher`×`TMatcher`(197), `TMatcher`×`TTuple`(191/193 + helper 373-398)
- `Type/Pretty.hs`(48/145), `Type/Error.hs`(231)
- `AST.hs`: `TEMatcher TypeExpr`(487); パーサ `Parser/NonS.hs`(781/807)
- walker: `Type/Env.hs`(216), `Type/TypeClassExpand.hs`(103/132), `EnvBuilder.hs`(447),
  `Type/Tensor.hs`(28), `Type/Infer.hs` freshenOpenSymbolSets(254)

推論ロジック:
- `something`: `Infer.hs`(419-422) と built-in scheme `Check.hs`(237) → ともに `Matcher α`（値）。**スロットではないので据え置き**。
- matcher 値（`matcher [...]`）: `Infer.hs`(972) で `TMatcher matchedTy`。
- `matchAll`/`match`: `Infer.hs`(1605-1641 / 1664-1700) でマッチャー型を `TMatcher matchedTy`
  に単一化し、`matchedTy ∼ target` を課し、クローズを inner 型で型検査。
- 積 matcher の畳み込み: `ITupleExpr`(722-749) で全成分 matcher なら `Matcher (tuple)`。
- matcher パラメータ: 専用経路なし。注釈 `TEMatcher`→`TMatcher` が `IReshape`(2011-2028)
  経由で単一化されるだけ。

## 実装方針（段階）

### Stage 1: 型の基盤（加算的・stdlib 未変更）
`TMatcherSlot Type Type` を追加し、全タッチポイントを鏡写しにする。この段階では
stdlib は `Matcher a` のままで挙動不変（既存テストを壊さない）。
- `Types.hs`: `TMatcherSlot Type Type` 追加。`freeTyVars`= 両引数の和、
  `typeConstructorName`= "MatcherSlot"、`typeExprToType`/`normalizeInductiveTypes`= 両引数再帰。
- `Subst.hs`/`Unify.hs`/`Pretty.hs`/`Error.hs`/各 walker: 両引数を再帰。
  `Unify`: `(TMatcherSlot s1 t1)`×`(TMatcherSlot s2 t2)` は `THash`/`TFun` と同様に両引数を単一化。
- `AST.hs`: `TEMatcherSlot TypeExpr TypeExpr`。パーサ: `MatcherSlot a`→`TEMatcherSlot a a`、
  `MatcherSlot a b`→`TEMatcherSlot a b`。

### Stage 2: COERCE-MATCHER-TO-SLOT（意味論）
`Matcher τ_m` 値（および `something : Matcher α`）が `MatcherSlot τ_s τ_t` を要求する
位置に来たときの二重検査と coercion を実装。構造 `⊑`（one-way matching）と
fresh-rename を導入。matcher パラメータがスロット型を持つよう推論を調整。

### Stage 3: 積 matcher ＋ stdlib 移行
COERCE-TUPLE-MATCHER / COERCE-SLOT-TUPLE。stdlib の matcher パラメータ注釈を
`Matcher a`→`MatcherSlot a a` に移行。`-t` / mini-test / `cabal test` で stdlib が
型検査を通ることを検証。

## スコープ外（今回の依頼で除外）
- PAT-OR の強制（両分岐が同一変数を束縛する検査）
- Coverage（Def 4.2(3)）の検査
- structural equality `≡` の変更（処理系の現状＝名前付き関数は名前比較・他は False、を維持）

## 進捗

### Stage 1 完了（2026-06-01）
`TMatcherSlot Type Type` を追加し、全 plumbing を実装。ビルド成功・回帰なし。
- 型レベル: `Types.hs`（def/freeTyVars/typeConstructorName/typeExprToType/normalizeInductiveTypes）、
  `Subst.hs`、`Unify.hs`（2引数を THash と同様に単一化）。
- 表示: `Type/Pretty.hs`(2), `Pretty.hs`(2: pretty/prettyTypeDoc), `Type/Error.hs`。
- 構文: `AST.hs`（`TEMatcherSlot TypeExpr TypeExpr`）、`Parser/NonS.hs`
  （`matcherSlotTypeExpr`、`MatcherSlot a`→`MatcherSlot a a` 糖衣、予約語追加）。
- walker: `Type/Env.hs`、`Type/TypeClassExpand.hs`(2)、`EnvBuilder.hs`、`Type/Tensor.hs`、
  `Type/Infer.hs`(freshenOpenSymbolSets)。
- 検証: `mini-test/60-matcher-slot-parse.egi`（`MatcherSlot a a`/糖衣/多相/恒等）型検査 OK、
  `sample/mahjong.egi` 回帰なし。
- 注: `Infer.hs` の意味論的な `TMatcher` 使用（`something`(422)、matcher 値(972)、tuple(737)、
  match/matchAll(1612/1671)、抽出ヘルパ等）は **Stage 2 まで `TMatcher` のまま据え置き**。
  stdlib も `Matcher a` のまま（挙動不変）。

### Stage 2 完了（2026-06-01）
COERCE-MATCHER-TO-SLOT の意味論を `Type/Unify.hs` に実装。
- 単一化器に `Matcher τ_m` ↔ `MatcherSlot τ_s τ_t` の coercion ケース（双方向）を追加。
  既存コードには matcher↔slot が現れないため**不発火＝回帰なし**。
- `coerceMatcherSlot`: 二重検査 = 構造 `τ_m ⊑ τ_s`（`matchOneWay`、one-way matching）
  ＋ 対象 `τ_m ∼ τ_t`（`unifyNormalized`）。**構造検査は対象単一化の前に intrinsic 型で
  実施**（さもないと `something` の `α` が対象で具体化され、具体スロットでの拒否が効かない）。
- `matchOneWay τ_s τ_m`: `τ_s` の型変数のみを束縛して `τ_s = τ_m` にできるか（`τ_m` は rigid）。
  変数頭 `τ_s` は任意の matcher を受理、具体/構成子頭 `τ_s` は `something`(`Matcher α`)を拒否。
- 検証（`mini-test/61-matcher-slot-coerce.egi` ＋ 否定例）:
  - 受理: `acceptSlot integer`、`acceptSlot something`（変数頭スロット）、`acceptIntSlot integer`。
  - 拒否（Type error）: `acceptIntSlot something`（something を具体スロットへ）、
    `acceptCharSlot integer`（型不一致）。
  - 回帰なし: `sample/mahjong.egi`・`quadratic-equation`・`cubic-equation` 型検査クリーン。
- 注: slot-as-matcher（body 内でスロット param を matcher として使う）も同じ coercion ケースで
  双方向にカバー。`something` の built-in 型は `Matcher α` のまま（値なので正しい）。

### Stage 3 完了（2026-06-01）— stdlib 移行ずみ

matcher 本体推論で「スロットを対象型 `τ_t` の matcher として扱う」対応（`Type/Infer.hs`）:
- `extractMatcherInner (TMatcherSlot _ tt) = Just tt`（タプル内のスロット要素）。
- `extractInnerTypesFromMatcher`: 先頭スロットを `slotAsMatcher` で `TMatcher tt` に正規化。
- `extractMatcherType (TMatcherSlot _ tt) = Just tt`（ITupleExpr の COERCE-TUPLE-MATCHER 畳み込み）。
- パターン構成子の引数 inner 抽出（L1110 付近）にスロット対応。

**COERCE-SLOT-TUPLE**（`Type/Unify.hs`）: `MatcherSlot τ_s τ_t` × `TTuple` を、既存の
`Matcher × Tuple` 規則（`unifyMatcherWithTupleG`）にスロットの対象型 `τ_t` で委譲。
**これが当初のブロッカーの真因**だった ── `list` を**タプルマッチャー `(m, integer)` で呼ぶ
使用箇所**（ペアのリストの matcher）で、旧 `Matcher a` なら `Matcher↔Tuple` coercion が
効いていたのにスロットには無かった。当初「再帰 matcher の一般化」と誤診断したが、
実際は単に Slot↔Tuple coercion 欠如（非再帰・タプルの問題）。

**`matchOneWay` の ground 同値**（`Type/Unify.hs`）: 両辺が変数を含まない（ground）場合は
単一化で同値判定。Integer~MathValue や CAS 族（Factor/Term/Frac/Poly）を具体スロットで受理。
`something`（τ_m が変数＝非 ground）には不適用なので **per-use-site 拒否は維持**。

**stdlib 移行**: 全12の matcher コンストラクタの要素 param を `Matcher a` → `MatcherSlot a a`:
list, sortedList, multiset, set, unorderedPair, maybe（core）, R.multiset, R.set（random）,
assocMultiset（assoc）, term, poly, frac（math/expression）。**関数**（戻り値が Bool/[a]/Integer 等、
例 deleteFirstAs/memberAs/…）は `Matcher a` のまま ── matcher コンストラクタへ渡す際に
COERCE が橋渡しするため移行不要。

**検証**: `cabal build` 成功、`mini-test` **87/87 pass**、`cabal test` **21/21 PASS**、
代表 math サンプル（quadratic/cubic/quartic/5th-root/eulers）・`sample/mahjong.egi` 全通過、
`something` の具体スロット拒否も維持（`/tmp/neg1.egi`）。**回帰なし**。

## 積 matcher の扱い（設計判断 2026-06-01）

論文は積 matcher を3規則（COERCE-TUPLE-MATCHER＝全成分 Matcher 値のみ畳む／
COERCE-SLOT-TUPLE＝スロット成分は消費側で分解・**Matcher 値にしない one-way**／
COERCE-MATCHER-TO-SLOT）で position-aware に区別する。

処理系はこれを簡略化し、**次マッチャーを位置に関係なく `Matcher` 値として扱う**
（タプルは matcher/スロットいずれの成分でも `Matcher (積)` に畳む）。理由：next matcher を
MatcherSlot にする実益が無く、この単純化で matcher 本体（list/multiset/set/unorderedPair の
`(m, list m)`・`(m, m)` 等の次マッチャー）が素直に型検査できる。`extractMatcherType` /
`unifyMatcherWithTupleG`（`unifyEachAsMatcher`）でスロットを「対象型 τ_t の Matcher」とみなす。

**帰結（論文との差・健全）**: 処理系は**スロット成分を含むタプルを `Matcher` 値として
構成できる**（例：`(m, m) : Matcher (a,a)`、`(m, list m) : Matcher (a,[a])` を**受理**）。
論文は one-way の COERCE-SLOT-TUPLE のためこれを拒否（`f = (m,m)` を "rejected (out of scope)"
としている）。実行時にはスロット param `m` は本物の matcher 値なので、この受理は**健全**で
あり、処理系の方が表現力が高い（積 matcher を直接タプルで書ける）。
`unorderedPair` は `(m, m)` を**次マッチャー**として使う（消費位置）ので両者とも問題なし。

検証: `mini-test` 87/87・`cabal test` 21/21・mahjong・math サンプル全通過。

## まとめ
論文の MatcherSlot 機構を egison 処理系に実装完了:
- 型 `MatcherSlot τ_s τ_t`（Stage 1）
- COERCE-MATCHER-TO-SLOT 二重検査＋per-use-site structural admissibility（Stage 2）
- COERCE-SLOT-TUPLE＋ground 同値＋stdlib 移行（Stage 3）
- 積 matcher は論文よりやや寛容（スロット成分から `Matcher` 値を構成可、健全）

## 検証手順（CLAUDE.md 準拠）
各 stage 後に `cabal build`。意味論変更後は `mini-test/` に小テストを追加し
`gtimeout 60 cabal run egison -- -t mini-test/NN-matcher-slot-*.egi`。最終的に代表
サンプルと `cabal test` で回帰確認。自動 git commit はしない。
