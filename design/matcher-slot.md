# MatcherSlot の実装設計（再挑戦）

## 背景

論文 *A Calculus and Type System for Ad-hoc Polymorphic Pattern Matching on
Non-free Data Types*（λ_P）の型システムは **`Matcher τ` と `MatcherSlot τ_s τ_t`
の区別**を中核に置く。現行 Egison の型検査器は単一引数の `Matcher τ`
（`Type/Types.hs` の `TMatcher Type`）のみを持ち、`MatcherSlot` は存在しない。
本ドキュメントはその差を埋める実装を、論文の現行版に沿って再実装する記録である。

（注: 旧ドキュメントは「完了」と記録していたが、その実装変更は未コミットで失われ、
現コードには MatcherSlot が全く無かった。本ドキュメントはクリーンな再実装。
旧ドキュメントから得た知見＝タッチポイント・過去のブロッカーは下記に反映済み。）

## 論文側の意味論（要点）

- **`Matcher τ`**: マッチャー「値」の型。`something : Matcher α`、`matcher [...]` 式の
  結果、`multiset m : Matcher [a]` など。
- **`MatcherSlot τ_s τ_t`**: マッチャー「消費位置（スロット）」の型。
  - `τ_s` = スロットの**構造型（パターン型）**。
  - `τ_t` = **対象型（target）**。
  - 構文糖: `MatcherSlot a` ≡ `MatcherSlot a a`。
- マッチャー**パラメータ**はスロット型を持つ（例: `def multiset {a} (m : MatcherSlot a a) : Matcher [a]`）。
- **COERCE-MATCHER-TO-SLOT**: `Matcher τ_m` 値が `MatcherSlot τ_s τ_t` を埋めるのは、
  **二重検査**が成り立つとき:
  - 構造: `fresh_rename(τ_m) ⊑ fresh_rename(τ_s)`（per-use-site structural admissibility）
  - 対象: `τ_m ∼ τ_t`（target unifiability）
  - fresh-rename により両検査は独立。**構造検査は対象単一化の前に**行う（さもないと
    `something` の `α` が対象で具体化され、具体スロットでの拒否が効かなくなる）。
- **T-SOME**: `something : Matcher α`。変数頭 `τ_s` のスロットには admissible、構成子頭には不可。
- **積 matcher**: COERCE-TUPLE-MATCHER（全成分 `Matcher` 値なら `Matcher (τ_1×…×τ_k)` に畳む）、
  COERCE-SLOT-TUPLE（積スロットを成分スロットに分解、one-way）。
- **matcher polymorphism**: パターン型はマッチャー非依存（スロット情報はマッチサイトでのみ流入）。

## タッチポイント（`TMatcher` を `TMatcherSlot` にミラーする箇所）

型レベル plumbing（Stage 1 で対応済み）:
- `Type/Types.hs`: `data Type`(TMatcherSlot Type Type), `freeTyVars`, `typeConstructorName`,
  `typeExprToType`, `normalizeInductiveTypes`
- `Type/Subst.hs`(applySubst), `Type/Unify.hs`(TMatcherSlot×TMatcherSlot は2成分単一化),
  `Type/Pretty.hs`, `Type/Error.hs`, `Type/Env.hs`, `Type/Tensor.hs`, `Type/TypeClassExpand.hs`(2),
  `Type/Infer.hs`(freshenOpenSymbolSets), `Pretty.hs`(2: prettyTypeDoc, TypeExpr), `EnvBuilder.hs`
- `AST.hs`(TEMatcherSlot TypeExpr TypeExpr), `Parser/NonS.hs`(matcherSlotTypeExpr ＋ 2箇所の選択肢
  ＋ 予約語 "MatcherSlot"。`MatcherSlot a`→`a a` 糖衣)

推論ロジック（Stage 2/3 で対応予定。現状は `TMatcher` のまま）:
- `something`: `Infer.hs`(~423) と `Check.hs` built-in scheme → `Matcher α`（値なので据え置き）
- matcher 値（`matcher [...]`）: `Infer.hs`(~973)
- 積 matcher 畳み込み: `ITupleExpr`(~737/748 `extractMatcherType`)
- 次マッチャー抽出: `Infer.hs`(~988-1238 `extractMatcherInner`/`extractInnerTypesFromMatcher`)
- `matchAll`/`match`: `Infer.hs`(~1612/1671)

## 実装方針（段階）

- **Stage 1（型基盤・加算的）**: `TMatcherSlot Type Type` を全 plumbing にミラー。stdlib は
  `Matcher a` のまま挙動不変。← **完了**
- **Stage 2（COERCE-MATCHER-TO-SLOT）**: `Matcher τ_m`（`something` 含む）が slot 要求位置に
  来たときの二重検査と coercion を `Type/Unify.hs` に実装（双方向ケース）。`matchOneWay τ_s τ_m`
  ＝ `τ_s` の型変数のみ束縛して `τ_s = τ_m`（`τ_m` rigid）。変数頭は任意受理、構成子/具体頭は
  `something` 拒否。matcher パラメータが slot 型を持つよう推論調整。
- **Stage 3（積 matcher ＋ stdlib 移行）**: COERCE-SLOT-TUPLE（`MatcherSlot × TTuple` を既存
  `Matcher × Tuple` 規則に対象型で委譲）＋ ground 同値（Integer~MathValue/CAS 族を具体スロットで受理）。
  stdlib の matcher コンストラクタ要素 param を `Matcher a`→`MatcherSlot a a` へ移行（関数は据え置き）。

## 過去の attempt から得た重要な教訓

- **真のブロッカーは Slot↔Tuple coercion 欠如**だった。`list` 等を**タプルマッチャー `(m, integer)`
  で呼ぶ箇所**（ペアのリストの matcher）で、旧 `Matcher a` なら `Matcher↔Tuple` coercion が効くが
  slot には無く失敗。当初「再帰 matcher の一般化」と誤診断したが、実体は非再帰の Slot↔Tuple 欠如。
- **構造検査を対象単一化の前に**実施しないと `something` の per-use-site 拒否が効かない。
- stdlib 移行対象は **matcher コンストラクタ12個**の要素 param のみ（list/sortedList/multiset/set/
  unorderedPair/maybe, R.multiset/R.set, assocMultiset, term/poly/frac）。**関数**（戻り値 Bool/[a]/Integer）は
  `Matcher a` のまま（コンストラクタへ渡す際に COERCE が橋渡し）。

## スコープ外
- PAT-OR の強制、Coverage(Def 4.2(3)) の検査、structural equality `≡` の変更。

## 進捗

### Stage 1 完了（再実装, today）
`TMatcherSlot Type Type` を追加し全 plumbing をミラー。`cabal build` 成功（コンパイルエラー0）。
- 検証: `mini-test/60-matcher-slot-parse.egi`（`MatcherSlot Integer Integer`/糖衣/多相の注釈）型検査 exit=0・エラー無し。
- 回帰: `sample/math/algebra/quadratic-equation.egi` exit=0・clean（加算的なので当然）。
- 据え置き: `Infer.hs` の意味論的 `TMatcher` 使用は Stage 2 まで `TMatcher` のまま。stdlib も `Matcher a`。

### Stage 2 完了（再実装, today）
COERCE-MATCHER-TO-SLOT の二重検査を `Type/Unify.hs` に実装。
- `unifyG` に `Matcher τ_m` ↔ `MatcherSlot τ_s τ_t` の coercion ケースを双方向で追加。既存コードに
  matcher↔slot は現れないため**不発火＝回帰なし**。
- `coerceMatcherToSlot`: (1) 構造 `matchOneWay τ_s τ_m`（**対象単一化の前に intrinsic τ_m で**実施）
  ＋ (2) 対象 `τ_m ∼ τ_t`。
- `matchOneWay slot matcher`: `slot` の型変数のみ束縛して `slot = matcher`（matcher rigid）。
  変数頭 slot は任意受理、構造的に再帰、構成子/具体頭 slot は bare 変数 matcher（something）を拒否。
  繰り返し変数は accumulated subst 経由で整合判定。
- 検証（`cabal build` 成功）:
  - 受理: `mini-test/61-matcher-slot-coerce.egi`（`acceptSlot integer`/`acceptSlot something`（変数頭）/`acceptIntSlot integer`）exit=0・エラー無し。
  - **拒否（Type error）**: `acceptIntSlot something`（something を Integer スロットへ）、
    `acceptListSlot something`（something を `[a]` スロットへ）。← per-use-site structural admissibility が機能。
  - 回帰なし: `quadratic-equation`・`60-parametric-matcher` クリーン。
- 注: `something` の built-in 型は `Matcher α` のまま（値なので正しい）。slot 型は注釈経由でのみ発生
  （stdlib は Stage 3 まで `Matcher a`）。

### Stage 3-1 完了（match-site のスロット化, 論文 T-MATCHALL）
`matchAll`/`match` の推論で、マッチャーの intrinsic inner 型が bare `TVar` かつ
いずれかの節パターンが**構築子頭**のとき拒否する加算的ガードを `Infer.hs` に追加。
- `isConstructorHeadedPattern :: IPattern -> Bool`（`inferMatchClauses` 直前, ~2034）:
  IInductivePat/IInductiveOrPApplyPat/IDApplyPat/IPApplyPat/ISeqNilPat/ISeqConsPat は True、
  ILoopPat/INotPat/IAndPat/IOrPat/IForallPat/ILetPat/IIndexedPat は再帰、他は False。
- ガード本体を `IMatchAllExpr`・`IMatchExpr` 両方に挿入。**intrinsic inner 型は対象単一化の前に**
  捕捉（さもないと `something` の `α` が対象で具体化され拒否が効かない）。エラーは
  `TE.TypeMismatch`（4引数）で「bare-variable matcher is not structurally admissible …」。
- 検証: `matchAll [1,2] as something with $x :: $xs` → **Type error**（Expected: `MatcherSlot [Integer] [Integer]`）。
  `as multiset integer with $x :: $xs`（構造マッチャー@cons）と `as something with $x`（something@変数）は受理。
- 設計上の安全性: `Matcher a` パラメータを **非構築子パターン**（`#y`/`_`/`$x`）で使う stdlib（`eqAs` 等）は
  inner が bare TVar でもパターンが構築子頭でないため不発火。構築子パターンで bare マッチャーを使う箇所は
  意味論的に ill-typed なので stdlib に存在しない。
- 回帰: **`cabal test` 21/21 PASS**（warning/error 0）, mini-test 87/87, math サンプル（quadratic/cubic/quartic）クリーン。

### Stage 3-2 完了（COERCE-SLOT-TUPLE）
タプル（マッチャー列）が積スロットを埋める規則を `Type/Unify.hs` に実装。
- 畳み込みロジック `unifyEachAsMatcher`/`getTyVarName` を `unifyMatcherWithTupleG` の `where` から
  top-level に持ち上げて共有。`unifyEachAsMatcher` に `TMatcherSlot _ tt` 要素ケース（slot の target
  成分を inner とする）を追加（次マッチャータプル `(m, list m)` で slot 化した param `m` を扱うため）。
- `coerceSlotTuple`: タプルを `Matcher (inner のタプル)` に畳んでから `coerceMatcherToSlot` の二重検査を適用。
- `unifyG` に `TMatcherSlot × TTuple`（双方向, TensorConstraintAware）ケースを追加。
- 検証（`mini-test/80-matcher-slot-tuple.egi`）: `acceptSlot (integer, integer)`・`acceptPairSlot (integer, integer)`・
  `acceptSlot (integer, integer, char)` 受理。負例 `acceptPairSlot (something, integer)`（concrete pair slot）拒否。

### Stage 3-3 完了（stdlib 移行）
matcher コンストラクタ**12個**の要素 param を `Matcher a`→`MatcherSlot a a` に変更（論文のシグネチャに一致）:
- core: `list`/`sortedList`/`multiset`/`set`（collection.egi）, `maybe`, `unorderedPair`（base.egi）,
  `assocMultiset`（assoc.egi）, `R.multiset`/`R.set`（random.egi）
- math: `term`/`poly`/`frac`（expression.egi）
- **関数**（`splitAs`/`memberAs`/`deleteAs`/`eqAs` 等、戻り値 Bool/[a]/Integer）は `Matcher a` のまま据え置き。
- `Infer.hs` の `extractInnerTypesFromMatcher`/`extractMatcherInner` に `TMatcherSlot _ tt` ケースを追加
  （slot を次マッチャー位置で使うとき target 成分を inner として抽出）。
- 本体は slot→Matcher 逆 coercion（双方向ルール）と Slot×Tuple（3-2）で従来通り型検査が通る。

### Stage 3-4 完了（CAS ground 同値）
`term`/`poly`/`frac` の本体は係数 hole を MathValue として束縛するため slot が具体 `MatcherSlot MathValue MathValue`
に固定される。使用箇所 `term integer`（`integer : Matcher Integer`）を受理するため、`matchOneWay` の
base 型 fallthrough に `groundEquiv`（Integer/MathValue/Factor/Term/Frac/Poly が ground レベルで相互同値、
`unifyG` の閉じた CAS 規則のミラー）を追加。非CAS型（Float 等）は対象外なので過剰受理なし。

### フル回帰確認（完了）
- **`cabal test` 21/21 PASS**（error/warning 0）。
- **mini-test 一括 88/88 PASS**（新規 `80-matcher-slot-tuple.egi` 含む）。
- math サンプル（riemann-S2/cubic/quadratic/quartic/eulers/5th・7th-root-of-unity）全て clean。
- 負例維持: `matchAll [1,2] as something with $x :: $xs` 拒否、`float` を `MatcherSlot Integer Integer` へ拒否。

## 総括（論文 λ_P との対応）
Stage 1〜3 完了により、現行 Egison は論文の MatcherSlot 型システムを実装:
- `Matcher τ`（値）と `MatcherSlot τ_s τ_t`（消費位置）の区別、糖衣 `MatcherSlot a`≡`MatcherSlot a a`。
- COERCE-MATCHER-TO-SLOT 二重検査（構造 one-way ＋ 対象単一化、構造を先に）。
- per-use-site structural admissibility（T-SOME）: 変数頭スロットは `something` 受理、構成子頭は拒否。
- T-MATCHALL: match-site で構成子パターンに対し bare 変数マッチャーを拒否。
- COERCE-SLOT-TUPLE（積スロット）。
- stdlib matcher コンストラクタ12個が slot 型 param を宣言（論文シグネチャと一致）。
- CAS ground 同値（Integer~MathValue/CAS 族）。

スコープ外（変更なし）: PAT-OR の強制、Coverage(Def 4.2(3)) の検査、structural equality `≡` の変更。

## 検証手順（CLAUDE.md 準拠）
各 stage 後に `gtimeout 590 cabal build`。意味論変更後は `mini-test/` に小テストを追加し
`gtimeout 60 cabal run egison -- -t mini-test/NN-matcher-slot-*.egi`。最終的に代表サンプルと
`cabal test` で回帰確認。自動 git commit はしない。
