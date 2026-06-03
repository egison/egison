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

## Stage 4: 残ギャップの精査と追加実装（論文との完全一致に向けて）

論文の型システム規則を実装と突き合わせた監査で、Stage 1〜3 では未対応だったギャップが判明:
- **A** match-site の構造的許容性が構文近似(偽陽性＋偽陰性)
- **B** マッチャー定義時の next-matcher 許容性(PP-Con)未実装
- **C** Matcher Consistency(Coverage/catch-all)未実装
- **D** PAT-OR 出力文脈一致未実装
- **E** structural equality ≡

### 重要な構造的制約（Egison の HM の限界）
Egison の型推論は **rigid/skolem 変数を持たず**、再帰マッチャー参照に未解決の fresh 変数を与える
(論文は「再帰マッチャーの名前は宣言型でスコープに入る」と仮定)。このため、構造的許容性検査で
**真に多相な `something`(∀a. Matcher a, 拒否すべき)** と **単相・解決待ちの再帰マッチャー
(`multiset m` 等, 受理すべき)** を型レベルで区別できない。両者とも「具体スロット vs bare 変数マッチャー」
で構造的に同一。`algebraicDataMatcher` は内部で再帰マッチャーを `match (val,tgt) as (M,M) with (less,less)`
に desugar するため、この区別が必須になる。

### A 完了（match-site の真のスロット推論）
構文ガードを廃し、パターンから独立に構造型 `τ_p` を導出(`patternStructuralType`, 論文 PAT-*)して
`matcher ~ MatcherSlot τ_p τ_t` を単一化する真の二重検査に置換(`checkMatcherAdmissibility`, `Infer.hs`)。
- 検査は **抽出の前** に生の matcher 型で実行 → 未注釈の matcher パラメータ(bare 型変数)を slot 型に
  コミット(偽陽性解消)。`something`(`Matcher β`)は COERCE で構造検査 → 拒否。
- 抽出に `TMatcherSlot _ tt` ケース追加(slot の target を inner に)。
- `coerceSlotTuple`(`Unify.hs`)を **成分分解**に再実装 → タプル成分の param を成分スロットに deferred
  (論文 COERCE-SLOT-TUPLE)。`g (list integer)` 受理・`g something` 拒否。
- **本体内 match-site 検査(保留を撤去 — ユーザ要求「きちんと型検査」)**: 以前は
  `InferState.inferInMatcherBody` フラグで**マッチャー本体内の match-site 許容性検査を保留**していたが、
  これを撤去した。マッチャー本体内にネストした `match`/`matchAll` も `checkMatcherAdmissibility` で
  **genuine に型検査される**。フラグ自体は残すが、現在は Coverage 警告の抑制のみに使う。
  - **撤去は正しく不健全節を拒否する**: 本体内の `match v as integer with $x::$xs`(integer を `::` スロットに)
    や `something @ cons` を**正しく拒否**(以前は素通り = false-negative)。トップレベルの同じ誤りと同様に検査される。
  - **実コードへの回帰はゼロ(厳密検証)**: `-t` は permissive(型エラーは出力するが untyped 評価にフォールバック
    し exit 0)なので、**exit code でなく出力の `Type error:` を grep** して比較した。保留**あり**版と**なし**版で
    mini-test(唯一 `50-multiset-mathexpr` の既存エラー、両者同一)・math サンプル(全 0)・`cabal test`
    (21/21, Type error 0, Warning 0)が**完全一致**。保留は実コードに対し既に vestigial だった。
  - **明示 self-match-site 再帰の false-positive = 解決済み(`concretizeDeclaredTypes`)**。concrete 型上の手書き
    再帰マッチャーが再帰を **next-matcher スロットでなく明示的 match-site**(`match v as self`)で行う例:
    `def nat : Matcher Nat := matcher | suc $ as (nat) with | $tgt -> match tgt as nat with | suc $n -> [n] …`。
    - **根本原因**: `typeExprToType (TEVar name) = TVar (TyVar name)`(`Type/Types.hs`)が bare 型名を大文字でも
      型変数化するため、署名 `Matcher Nat` が `∀Nat. Matcher Nat`(`Nat` が型変数)に一般化され、self-ref がそれを
      instantiate して fresh inner `Matcher t4` を得る。`MatcherSlot Nat Nat` への片方向 `⊑`(matcher 側 rigid)が
      `t4` を bind できず誤拒否していた(probe: `nat scheme = Forall [TyVar "Nat"] (TMatcher (TVar (TyVar "Nat")))`)。
    - **修正(`EnvBuilder.hs` の `concretizeDeclaredTypes`)**: `buildEnvironments` でこのバッチの `inductive` 宣言名
      (`InductiveDecl`)を集め、`DefineWithType` 署名の `funType` 内でそれら**宣言済み型名の `TVar` を `TInductive` に
      補正**してから一般化する。これで `nat : Matcher Nat` は concrete に格納され、self-ref が `Matcher Nat` に解決して
      match-site 検査を通る。**宣言済み型のみ**対象なので、未宣言名(後述の旧名 `MathExpr` 等)は型変数のまま=既存挙動を維持。
    - **健全性は不変**: `something`(真の多相変数 `Matcher α`、宣言済み型ではない)は影響を受けず、`something@cons`/
      `something@#1`/`nat@::` 等の不正は依然 REJECT。実測で `nat` は strict mode でも accept、`something@::` 等は拒否。
    - **回帰ゼロ**: 出力ベースで mini-test 0・math 0・cabal test 21/21(Type error 0, Warning 0)。`algebraicDataMatcher`
      / next-matcher スロット / 外部 match-site も従来どおり通る。
    - **スコープ(未カバーの稀ケース)**: 宣言済み型名は**同一ロード単位(バッチ)内**で収集する。型を A ファイルで宣言し、
      別の B ファイルでその型の明示 self-match-site 再帰マッチャーを書く場合は補正されない(B のバッチに `inductive` が
      無いため)。prelude 型自身のマッチャーは prelude バッチ内で定義されるため問題ない。型とマッチャーを同一ファイルに
      書く通常パターンは全てカバー。
  - **旧名 `MathExpr` の補足**(上の「宣言済みのみ対象」の根拠): `MathExpr` は **`MathValue` の旧名**で、リネーム後は
    型キーワード一覧から外れ未知名=型変数に退化していた。これを使う名残りの mini-test 8 件(`50-multiset-mathexpr`/
    51〜57; 「Tensor Matcher [Tensor MathExpr] issue」隔離シリーズ)は **obsolete として削除済み**。実テスト
    `test/lib/core/assoc.egi` は `declare symbol …: MathExpr` を `MathValue` に直すと**未解決の CAS matcher 問題**
    (`multiset`/`assocMultiset` の自己再帰＋`something` を具体キーに使う、型名に依らず露呈)で 2 件 Type error が出るため、
    stale 名のまま type-clean を維持(別件)。`concretizeDeclaredTypes` は未宣言の `MathExpr` を触らないので assoc.egi に無影響。
- 検証(論文 §B.2 の 4×2 マトリクス＋ネスト＋slot-tuple、全て論文通り):
  - 拒否: `something with #1`(値@具体), `multiset something with #1::$y`(ネスト), `f something`(λ経由),
    `something with $x::$xs`(構築子)
  - 受理: `def f m := matchAll xs as m with $x::$rest`(combinator, **偽陽性解消**),
    `g (list integer)`(slot-tuple), `something with $x`, `multiset integer` 各種
- 回帰: `cabal test` **21/21**, mini-test **88/88**, math サンプル全 clean。
  - `test/syntax.egi` の `match-all-lambda-multi` を `multiset something`→`multiset integer` に更新
    (論文が意図的に拒否する「bare 変数マッチャー＠具体値パターン」のため。論文推奨の置換)。
- **残差(論文との微差)**: (1) マッチャー本体内の match-site も**検査される(保留撤去済)**。**明示 self-match-site 再帰
  も `concretizeDeclaredTypes` で解決済**(`match v as self` で concrete 型を再帰; 上記参照。同一ファイル宣言なら通る)。
  (2) `eq`(`{Eq a}=>Matcher a`)は多相変数のため `multiset eq with #concrete` を過剰拒否(論文は
  Eq 辞書で受理。制約付き変数の接地が未対応)。(3) PAT-VAR 線形性 `$x :: $x` 未強制(監査で発見)。

### D 完了（PAT-OR 出力文脈一致）
`inferIPattern` の `IOrPat` を論文 PAT-OR に実装(`Infer.hs`)。両分岐を**独立に**型付け(従来は p2 を p1 の束縛下で
推論していたが除去)し、**同じ変数集合**を束縛することを要求、共有変数の型を単一化。union 取りを廃止。
- 検証: `($x | #1)`(束縛が異なる)拒否、`(#1 | #2)`/`($x::_ | $x::_)`(同一)受理。
- 回帰: mini-test 88/88・cabal test 21/21 clean(stdlib の or-pattern は全分岐同一束縛)。

### B 実装（マッチャー定義時 next-matcher 許容性, PP-Con）— opt-in warning
**この gap について私(実装者)は調査中に三度誤った。最終的な事実は以下。** 診断(`inferPatternDef` /
`inferPrimitivePatPattern` に一時的に仕込んだ PPCON/PPIND 診断)で確認:

1. **再帰参照は解決される(誤り訂正①)**。当初「再帰マッチャー `multiset m` が未解決の fresh 変数になり
   `something` と区別できない(rigid 変数欠如)」としたが**誤り**。`poly m`/`list m`/`multiset m` 等は
   `inner=resolved([a]/MathValue)`。宣言型を持つトップレベル再帰マッチャーはスコープに入っている。
2. **`::` の hole は `[a]` に正しく解決される(誤り訂正②)**。当初「cons hole が型変数のまま」としたが**誤り**。
   PPIND2 診断で `::` の `argTypes=[a, [a]]`・`holes=[a, [a]]` を確認。cons 末尾 hole は `[a]`(TCollection)。
3. **`weird` は捕捉できる(誤り訂正③)**。当初「捕捉不可」としたが、それは私のテストファイル `weird.egi` が
   **パースエラー**(コンパクトな matcher 構文)で型検査されていなかっただけ。正しい構文では、型ベース検査
   (構築子頭 hole に bare 変数 inner)が `weird` の `something @ [a]` を正しく警告する。

正しい結論: **B は型レベルで実装可能**。検査(構築子頭 hole の bare 変数 inner)は:
- `weird` の `something @ [a]` を捕捉(論文の看板例)。✓
- 再帰参照 `list m`/`multiset m`(inner が `[a]` に解決)は警告しない。✓
- CAS マッチャーの `something` @ MathValue/String を警告(論文も拒否する非準拠コード、C/Coverage と同根)。

実装(`Infer.hs` の `inferPatternDef`、`MatcherNextMatcherWarning`): **構築子頭 hole(非型変数 hole)に
リテラル `something`(`IConstantExpr SomethingExpr`)** が来たら警告。構文的に `something` を判定するため、
slot 型パラメータ `m : MatcherSlot a a`(deferred で許容)も再帰/構造化 next-matcher(`list m`/`multiset m`)も
**誤検知しない**。検証: `weird` の `something@[a]` 警告(論文の看板例)、stdlib では CAS の `something` のみ22件、
`m`/`list m` は0件、デフォルト OFF で無音。
- **Coverage と同様 opt-in warning**(`--matcher-consistency-warnings`)。hard error 化は CAS マッチャーが `something` を
  具体 hole で使う非準拠コードなので stdlib を壊す。忠実化には CAS マッチャーの `something`@具体 hole を具体
  マッチャーに置換する stdlib 改修が必要(C/Coverage と同根)。**rigid 変数欠如とは無関係だった。**

### C 部分実装（Matcher Consistency, Def 4.2）
- **catch-all (4.2(2)) 実装**: `matcher` 式に catch-all 節 `$ as M with $tgt -> N`(pp が bare `$` = PPPatVar)が
  無ければ拒否(`Infer.hs` の `IMatcherExpr`)。全 stdlib マッチャーが catch-all を持つため安全。
  検証: catch-all 無しマッチャー拒否、有り受理。
- **Coverage (4.2(3)) を opt-in warning 診断として実装**: 当初「Σ_P が開いているため実装不可」と判断したが
  これは**誤り**だった。`inductive pattern` 宣言が存在し(Ordering/[a]/Maybe/Integer/String/MathValue/IndexExpr/
  Matrix)、構築子は結果型付きで登録され `patternEnvToList` で列挙できる。宣言の無い型は構築子ゼロで Coverage
  を自明に満たす。よって Coverage は**実装可能**。`matchedTy` の head former から構築子集合を引き、各 clause が
  一般節 `c $..$`(pp = 全 hole の PPInductivePat)を持つ構築子を集計、欠けを `MatcherCoverageWarning` で報告
  (`Infer.hs` の `IMatcherExpr`、`matcherTypeHead`/`ctorResultHead`/`generalClauseCtor`)。
  - **真の障害は stdlib の意図的な部分マッチャー**(Σ_P の開閉ではない)。粗い `MathValue` 型に16構築子を持たせ、
    その上に専門化マッチャー(`poly`/`frac`/`multExpr`)を定義しているため、各々は一部の構築子しか handle しない。
    例: `poly : Matcher MathValue` は `/`・`^`・`frac` の一般節を欠く(分数・べきは CAS タワーの別マッチャーが扱う)。
    他に `multiset`→`*:`、`assocMultiset`→`*:`/`++`/`::`、`String`→`regex`/`regexCg`、`Matrix`→`matCons` 欠如。
  - hard error にすると これら stdlib マッチャーを拒否するため、**`cfgCoverageWarnings`（CLI `--matcher-consistency-warnings`）
    でゲートした opt-in warning**(デフォルト OFF)とした。デフォルトでは無音で `cabal test` の warning を汚染しない。
    論文に忠実化するには (a) 各部分マッチャーに全構築子の一般節を追加、または (b) CAS 型を細分化(Poly/Frac/Term を
    別型に)する stdlib 改修が必要。
- **未実装**: decomposition body 型(1b)・refinement(4) の強制。

### E structural equality ≡
論文の `≡` は**既存の組み込み項等価性**(Egison の値等価、`something`/値パターンの実行時比較)であり、
型システムはこれを変更しない。実装すべき差分なし。

## Stage 4 総括（論文との一致状況）
| 規則/性質 | 状態 |
|---|---|
| `Matcher τ` / `MatcherSlot τ_s τ_t` 区別・糖衣 | ✅ |
| COERCE-MATCHER-TO-SLOT 二重検査 | ✅ |
| COERCE-SLOT-TUPLE / TUPLE-MATCHER | ✅ |
| T-MATCHALL/T-MATCH 構造的許容性(独立 τ_p、4×2 マトリクス、ネスト、slot-tuple) | ✅(本体内 match-site も検査。保留撤去済) |
| stdlib コンストラクタの slot 型 param | ✅ |
| CAS ground 同値 | ✅ |
| PAT-OR 出力文脈一致 | ✅ |
| Matcher Consistency catch-all (4.2(2)) | ✅ |
| T-MATCHER PP-Con next-matcher 許容性(`weird`) | ⚠️ opt-in warning（`--matcher-consistency-warnings`）。`weird` を捕捉。hard error 化は CAS 非準拠で stdlib を拒否 |
| Coverage (4.2(3)) | ⚠️ opt-in warning 診断（`--matcher-consistency-warnings`）。hard error 化は stdlib の部分マッチャーを拒否 |
| refinement (4.2(4)) / decomposition body (4.2(1b)) | ❌（未強制、文書化） |
| structural equality ≡ | 変更不要（既存等価を使用） |

**根本制約(B・Coverage・A本体内保留は別々の原因)**:

- **B = stdlib CAS の非準拠のみ(rigid 変数とも `::` hole 未解決とも無関係)**。当初の説明は三度誤った
  (①再帰未解決 ②`::` hole 未解決 ③`weird` 捕捉不可)。全て誤りで、正しくは: 再帰参照も `::` の `[a]` hole も
  正しく解決され、型ベース検査は `weird` の `something@[a]` を捕捉する。唯一の制約は CAS マッチャーが
  `something`/`m` を具体型 hole で使う非準拠コードで、hard error 化すると stdlib が壊れる(gap C/Coverage と
  同根)。よって opt-in warning として実装済み。

- **A の本体内 match-site = 検査される(保留撤去済)。明示 self-match-site 再帰の false-positive も解決済み**。
  以前はマッチャー本体内の match-site 検査を `inferInMatcherBody` で保留していたが、**撤去した**(ユーザ要求
  「きちんと型検査」)。撤去後:
  - **作法的な再帰は通る**: `algebraicDataMatcher`(再帰型 `Nat = Zero | Suc Nat` でも strict mode で 0 type-errors、
    実測)と、手書きでも next-matcher スロット経由の再帰(`suc nat`, `$ :: $ as (m, list m)`)。スロット経路は
    **柔軟な unification** で self-ref の fresh inner を解決する。
  - **不健全節は正しく拒否**: 本体内の `match v as integer with $x::$xs` / `something @ cons`(以前は素通り)。
  - **回帰ゼロ(厳密検証)**: `-t` permissive のため exit code でなく出力の `Type error:` を grep。保留あり/なしで
    mini-test・math・`cabal test` が完全一致(上記 A 完了節)。
  - **明示 self-match-site 再帰も解決(`concretizeDeclaredTypes`)**: concrete 型を `match v as self` で再帰する手書き
    マッチャー(例 `def nat : Matcher Nat := matcher | suc $ as (nat) with | $tgt -> match tgt as nat with | suc $n -> [n] …`)
    は以前誤拒否されていた。根本原因は `typeExprToType (TEVar name) = TVar (TyVar name)` が bare 型名を大文字でも型変数化し、
    `Matcher Nat` が `∀Nat. Matcher Nat` に一般化され self-ref が fresh inner `t4` に instantiate されること。
    **修正**は `EnvBuilder.hs` の `concretizeDeclaredTypes`: このバッチの `inductive` 宣言名を集め、`DefineWithType` 署名内の
    それら**宣言済み型名 `TVar` を `TInductive` に補正**してから一般化する。`nat` は `Matcher Nat` concrete で格納され通る。
    宣言済み型のみ対象なので `something`(真の多相変数)や未宣言名(旧名 `MathExpr` 等)は不変=健全性・既存挙動を維持。
    `nat` は strict でも accept、`nat@::`/`something@cons` 等は依然 REJECT、フル回帰 0。詳細は本書 A 完了節参照。
    （未カバー: 型を別ロード単位で宣言しその型のマッチャーを別ファイルで書く稀ケース。同一ファイルなら全てカバー。）
  - 補足(理論): この体系は HM + subsumption 的な片方向検査 `⊑`(順序依存・matcher rigid)で、純 HM ではない。
    `⊑` の rigid 性は `something` を具体パターンで正しく拒否するために必要だが、再帰 self-ref の未解決 inner も
    同様に拒否してしまう — 論文はこれを「再帰マッチャーは宣言型がスコープに入る」前提で回避する。`concretizeDeclaredTypes` は
    まさにその前提(宣言型を self-ref のスコープに concrete で入れる)を実装したもの。`multiset` 等の parameterized 型
    (`Matcher [a]`)を matchする手書きマッチャーは、パターン型 `[α]` に変数葉があり `⊑` がそれを吸収するため従来から通る。

- **Coverage = stdlib の部分マッチャー(rigid 変数とは無関係)**。当初「Σ_P が開いている」としたのは誤りで、
  `inductive pattern` 宣言により Σ_P は**閉じており列挙可能**(Coverage は実装済み・opt-in warning)。真の障害は、
  stdlib が粗い `MathValue` 型上に**意図的な部分マッチャー**(poly/frac 等が一部構築子のみ handle)を定義して
  いること。hard error 化にはこれら stdlib マッチャーへの全構築子一般節の追加、または CAS 型の細分化が必要。

## 検証手順（CLAUDE.md 準拠）
各 stage 後に `gtimeout 590 cabal build`。意味論変更後は `mini-test/` に小テストを追加し
`gtimeout 60 cabal run egison -- -t mini-test/NN-matcher-slot-*.egi`。最終的に代表サンプルと
`cabal test` で回帰確認。自動 git commit はしない。
