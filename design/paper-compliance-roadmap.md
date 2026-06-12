# λ_P 論文準拠の残課題 (roadmap)

論文: *A Calculus and Type System for Ad-hoc Polymorphic Pattern Matching on Non-free Data Types* (λ_P)。

本書は「論文の型システムを Egison 上に**全規則を強制する健全な検査器**として完成させる」ために残っている課題をまとめる。
完了済みの実装内容と詳細な根拠は [`design/matcher-slot.md`](./matcher-slot.md) を参照。

## 0. 現状サマリ

**忠実に実装・実証済み(論文の中核)**:
- `Matcher τ` / `MatcherSlot τ_s τ_t` の区別・糖衣
- COERCE-MATCHER-TO-SLOT 二重検査 / COERCE-SLOT-TUPLE / TUPLE-MATCHER
- T-MATCHALL/T-MATCH 構造的許容性(独立 τ_p、§B.2 の 4×2 マトリクス、ネスト、slot-tuple、**本体内 match-site**、
  **明示 self-match-site 再帰** = `concretizeDeclaredTypes`)
- PAT-OR 出力文脈一致 / Matcher Consistency catch-all (Def 4.2(2)) / structural equality ≡

**未達(本書の対象)**: Matcher Consistency (Def 4.2) の hard-error 強制、PAT-VAR 線形性、制約付きマッチャーの接地、
クロスバッチの宣言型解決、形式的検証。**最大の理由は「既存 stdlib 自身が論文非準拠」**であること。

これは**実装＋代表ケースの経験的テスト**であり、形式的健全性証明や規則ごとの網羅的適合テストは未整備。

---

## 1. 課題一覧

### 課題 A(最重要・enabler): stdlib の `something`@具体 hole の棚卸しと具体マッチャー化 — ✅ **完了(2026-06)**

> **完了**: (A-1) `lib/math/expression.egi` の `symbol $ $ as (something, …)` を **`string`** に置換(String hole)。
> (A-2) `apply_n` の**関数型 hole**(`MathValue -> … -> MathValue`)は PP-Con から **exempt**(関数は構造分解不能、
> `something` が唯一妥当)— `Infer.hs` の PP-Con 検査に `TFun` 除外を追加。結果、stdlib の `something`@具体 警告は 0
> (mini-test/test/math 全件で確認)。これにより課題 C(hard error 化)が可能になった。

**概要**: 論文は「具体位置(構築子 hole / 具体値パターン)には多相 `something` でなく具体マッチャーを使う」ことを要求
(T-MATCHER / Coverage の前提)。現状の stdlib は `something` を具体型の hole に使う非準拠コードを含む。
これを潰さない限り、課題 B/C を hard error 化すると stdlib が壊れる。**よって本課題が B/C の前提**。

**現状インベントリ**(`--matcher-consistency-warnings` と grep で確認):
- `lib/math/expression.egi`(CAS マッチャー)に集中、約 20–25 箇所。代表:
  - `symbol $ $ as (something, list indexExpr)` — `symbol :: String [IndexExpr]` の **String hole に `something`**。
  - `apply1 $ $ as (something, mathValue)` … `apply4 …` — `apply_n :: (MathValue->…->MathValue) MathValue…` の
    **関数 hole に `something`**。

**置換可否(per-case 判断が要る)**:
- **String hole(`symbol` の第1引数)** → 具体マッチャー `string` で置換可能。クリーンに準拠化できる。
- **関数 hole(`apply_n` の第1引数)** → 関数は構造マッチできない(束縛するだけ)ため、`something` が妥当な可能性が高い。
  論文規則を「関数型 hole は `something` 許容」と精緻化するか、専用の関数マッチャーを用意するか要検討。
- 参考: 本リポジトリのテストで `assocMultiset something` を `assocMultiset integer`(`= eq`, CAS で Integer≈MathValue)
  に置換して型 clean 化した実例あり(`test/lib/core/assoc.egi`)。同じ要領で「適切な具体マッチャー」を選ぶ。

**作業**: (1) `--matcher-consistency-warnings` で全 `something`@具体 を洗い出す → (2) 各箇所が具体マッチャーで
置換可能か判断(String/数値型 hole は可、関数 hole は要検討)→ (3) 置換 → (4) 数式サンプル全件で warning/error/
slowdown が出ないことを確認(`CLAUDE.md` の検査手順)。

**優先度**: 高(B/C の enabler)。

---

### 課題 B: Coverage (Def 4.2(3)) の hard error 化 — **方針: opt-in warning のまま据え置き(2026-06 ユーザ判断)**

> **据え置き決定**: 部分マッチャーは stdlib の意図的な設計であり、hard error 化は割に合わないため、
> **opt-in warning のまま**とする。以下は将来再検討する場合の参考情報。

**現状**: opt-in warning(`--matcher-consistency-warnings`、`MatcherCoverageWarning`)。実装は
`Infer.hs` の `IMatcherExpr`(`matcherTypeHead`/`ctorResultHead`/`generalClauseCtor`)。

**ブロッカー**: stdlib の**意図的な部分マッチャー**が一般節を欠く:
- `multiset` → `*:` 欠如、`sortedList` → `*:`/`++` 欠如、`assocMultiset` → `*:`/`++`/`::` の一部欠如、
  `String` → `regex`/`regexCg` 欠如、`Matrix` → `matCons` 欠如。
  (粗い `MathValue` 型上に専門化マッチャー `poly`/`frac` 等を載せているため、各々が一部構築子のみ handle)

**作業**: いずれか:
- (a) 各部分マッチャーに欠落構築子の一般節を追加(完全被覆にする)、または
- (b) 論文の Coverage を「意図的部分マッチャー」に対して緩和(宣言で `partial` を明示する等の言語拡張)。
- 完了後に warning → hard error 化(`cfgMatcherConsistencyWarnings` のゲートを外す/別フラグに)。

**優先度**: 中。課題 A とは独立(部分マッチャー対応が主)だが同根。

---

### 課題 C: PP-Con next-matcher 許容性 (T-MATCHER, `weird` 例) の hard error 化 — ✅ **完了(2026-06)**

> **完了**: 課題 A の準拠化後、`Infer.hs` の PP-Con 検査を **ungate＋`throwError`**(`TypeMismatch`)に変更し、
> 構築子頭 hole の literal `something` を**ハードエラー**化(`TVar`/`TFun` hole は exempt)。`weird` は拒否、実コードは
> 回帰ゼロ(mini-test 0 / math 0 / cabal test 21/21・Type error 0・Warning 0)。`MatcherNextMatcherWarning`(旧 warning)は
> 未使用化(整理は任意)。Coverage(課題 B)は opt-in warning のまま据え置き。

**現状**: opt-in warning(`MatcherNextMatcherWarning`)。マッチャー定義の構築子頭 hole にリテラル `something`
(`IConstantExpr SomethingExpr`)が来たら警告。論文の看板例 `weird`(`something @ [a]`)を捕捉する。

**ブロッカー**: **課題 A** の stdlib `something`@具体(同じものを構文的に警告しているため、hard error 化すると
stdlib が壊れる)。

**作業**: 課題 A 完了後、warning → hard error 化。

**優先度**: 中(課題 A に従属)。

---

### 課題 D: refinement (Def 4.2(4)) の強制 ／ decomposition body 型 (Def 4.2(1b)) — **(1b) は既に強制済み(2026-06 確認)**

> **(1b) 完了済み(当初の「未実装」は誤り)**: `inferDataClauseWithCheck`(`Infer.hs`)が各節 body の返り型を
> `[<next-matcher の hole 型タプル>]` と単一化するため、body が hole 型と不一致なタプルを返すマッチャーは型エラーになる
> (実測: `[(x,x)]` を hole 型 `(integer,[integer])` に返すと「Expected Integer, Actual [Integer]」で拒否)。

**残**: **refinement (Def 4.2(4))** の強制のみ。

**ブロッカー / 作業**: 論文本文がリポジトリに無く Def 4.2(4) の厳密な条文が未確認。まず論文で (4) を確定 →
各構築子に対する節集合の refine 関係(値パターン節 vs 一般節の整合性)の検査を実装。

**優先度**: 低(論文定義確定が前提)。

---

### 課題 E: PAT-VAR 線形性 (`$x :: $x` の禁止)

**現状**: 未強制(監査で発見、`matcher-slot.md` 残差(3))。同一パターン内で同じ変数を二度束縛する非線形パターンを
許してしまう。

**作業**: `inferIPattern` で、1パターン内の束縛変数集合に重複が無いことを検査(重複なら型エラー)。PAT-VAR の
線形性条件の実装。

**優先度**: 低(局所的・小規模)。

---

### 課題 F: 制約付き多相マッチャー (`eq : {Eq a} => Matcher a`) の接地 — ✅ **解決(2026-06、T-MATCHALL の τ_p/τ_t 精緻化による)**

> **解決**: 当初は「制約付き変数を辞書経由で接地する」アプローチを想定したが、より一般的・本質的な解として
> **T-MATCHALL の τ_p/τ_t 分離**(ユーザ提案)を実装した。`checkMatcherAdmissibility` で:
> - **τ_p = 構築子スケルトン**(`mapValuePatsToFreshVars` で値/述語パターンを fresh-var hole 化)→ 片方向 `⊑`
> - **τ_t = 値パターン込みの full 型**(ターゲットと unify)
>
> 値パターン `#e` は構造等価 `≡`(全マッチャーが持つ)で照合されるため**構造分解を要求しない**=τ_p に入れない。
> これにより `multiset eq with #1` も `something with #1` も受理、`something with $x::$xs`(構築子)は依然拒否。
> `eq` に限らず全マッチャーの値パターン位置の過剰拒否が一括解消(制約接地より一般的)。実装は `Infer.hs` の
> `mapValuePatsToFreshVars` + `checkMatcherAdmissibility`。回帰ゼロ(mini-test 0 / math 0 / cabal test 21/21)。
> 詳細は `design/matcher-slot.md`「T-MATCHALL の τ_p/τ_t 精緻化」。

---

### 課題 G: `concretizeDeclaredTypes` のクロスバッチ対応 — ✅ **完了(2026-06)**

> **完了**: `buildEnvironments`(`EnvBuilder.hs`)で、このバッチの `inductive` 宣言名に加え、**既に登録済みの
> constructor env(`getConstructorEnv` → `ctorTypeName`)から prior バッチの型名も補充**して `declaredTypes` に union。
> `evalTopExpr` は top-expr ごとに別バッチなので、**REPL/逐次評価で `inductive Nat` の後に別入力で
> `def nat : Matcher Nat := <明示 self-match-site>` を打つケース**でも Nat が concrete 化されて通る。
> `-l` ロードや単一ファイルは `expandLoads` で同一バッチになるため元々カバー済み(G はそこでは no-op だが正しい)。
> 回帰ゼロで確認。

**(参考)当初の現状認識**: 明示 self-match-site 再帰の修正(`concretizeDeclaredTypes`)は当初**同一バッチ内**の `inductive`
型のみ concrete 化していたため、型とマッチャーが別バッチ(逐次評価)だと未対応だった。`getConstructorEnv` 経由で
prior 宣言型を補うことで解消(`buildEnvironments` 内のみの小改修、signature 変更なし)。

**優先度**: 低(稀ケース。回避策=同一ファイルに書く)。

---

### 課題 I: パターン関数の構造シグネチャ + パラメータ線形性 (PATFUN-DEF / PAT-APP) — ✅ **完了(2026-06、レビュー M1/M2 対応)**

> **背景**: 論文レビューで Type Safety への反例が発見された。旧 PAT-APP は適用全体の構造インデックスを
> fresh α とし(引数の σ_i も捨てる)、パターン関数適用が match site に構造的要求を一切課さなかった:
> `matchAll [1,1] as something with pair $x [] -> x` が well-typed なのに実行時 stuck
> (実装でも「something can only match with a pattern variable」の実行時エラーを再現)。
> さらに実装の旧 `IPApplyPat` は**ターゲット側も未検査**で、`seqp #1 _ as multiset tile`(論文 §B.1.2 の
> 「型エラーになる」例)が素通りして silent fail していた。
>
> **実装(2026-06)**:
> - `IPatternFunctionDecl`: パラメータごとに fresh 構造インデックス β_i を発行(`inferPatfunParamTaup` 経由で
>   `IVarPat` が返す)。本体推論中に `taupCombine`/`taupFromCtor` が局所的に解いて捨てる構造方程式を
>   `inferPatfunTaupEqs` に記録し、定義末で**一括再解決**して構造シグネチャ
>   `β_1 -> … -> β_k -> τ_p_body` に適用(unifier の変数の向きで β リンクが落ちる問題への対策)。
>   全自由変数で全称化し `inferPatternFuncStructEnv`(EvalState 経由でバッチ間永続)に登録。
> - `IPApplyPat`: (1) ターゲット側 = 関数型を `parg_1 -> … -> parg_k -> expectedType` と unify してから
>   引数を解決済み型に対し推論(τ_t のトップダウン・コヒーレント計算を維持)。(2) 構造側 = 構造シグネチャを
>   fresh インスタンス化し、引数の τ_p を β_i と unify、`τ_p_body` のインスタンスを適用の τ_p として返す
>   (IInductivePat = PAT-CON と同じ装置)。シグネチャ未登録時は fresh にフォールバック。
> - **パラメータ線形性(レビュー M2)**: 各パラメータは本体中に「ちょうど1回・宣言順・分岐(or/loop/not/forall)外」
>   で出現しなければならない。違反は専用型エラー(`PatternFunctionLinearityError` /
>   `PatternFunctionParamUnderBranchError`)。unused(束縛欠落)・重複(束縛曖昧)・順序違反(Δ-threading 破壊)・
>   分岐下(パスにより 0 回/複数回展開)の全モードを静的に遮断。
>
> **検証**: 反例 4 種+線形性違反 4 種が型エラー化、受理側(`pair $x []` × multiset integer、`idp $x` × something、
> `pair (num $s $n) []` × multiset tile)は従来通り受理・正常動作。`pair (num $s $n) []` × multiset something は
> `MatcherSlot [Tile] [Tile]` で拒否(構造伝播がネスト引数まで届く)。mahjong -t クリーン、mini-test 全 0 fail、
> math サンプル全 ok(slowdown なし)、cabal test 21/21。`mini-test/120-patfun-struct-index.egi` に正例を追加
> (拒否例はファイル内コメントに列挙)。
>
> **論文側**: PATFUN-DEF(β_i・構造 unifier U・線形性側条件)/ PAT-APP(τ_p^f⟨σ̄⟩ ▷ τ⟨ρ̄⟩)へ改訂、
> Σ_F は双対スキーム `∀ᾱ. Pattern (Uβ_i ▷ τ_i) → … → Pattern (Uτ_p^f ▷ τ)` を記録(2026-06 改訂)。

---

### 課題 J: Matcher 型の rigid 化(Matcher 同士の unification 禁止) — ✅ **完了(2026-06)**

> **背景(ユーザ発見の健全性穴)**: `matchAll [something, list integer] as list something with $m :: _ ->
> matchAll [1, 2] as m with $x :: _ -> x` が well-typed なのに実行時 stuck。リスト要素の単一化で
> `something : Matcher β` の β が `[Integer]` に**具体化**され、`$m` 経由の内側 match site の構造検査が
> 「嘘の型」 `Matcher [Integer]` を信じてしまう(構造能力は値の定義で固定なのに、式レベル単一化で型だけ
> 特殊化できてしまう)。
>
> **対策(ユーザ指定)**: `unifyG (TMatcher t1) (TMatcher t2)` を **t1 == t2 のときのみ成功**(それ以外は
> 専用エラー `MatcherRigidity`)に変更。var↦var の緩和も検討したが、`[something, eq]` で変数を共有した後に
> 片方の使用点の coercion target unify が共有変数を具体化する経路で同じ嘘が再現するため不採用(全面禁止が正)。
>
> **例外は1経路のみ**: `def x : Matcher τ := matcher ...`(注釈付き matcher **literal**)の注釈照合 =
> T-MATCHER の checking モード(literal の能力は宣言型の clause 検査で導出される)。実装は IDefine/IDefineMany の
> 署名分岐で `rhsCore expr`(λ ラップ・algebraicDataMatcher の letrec ラップを剥がす)が `IMatcherExpr` のとき
> `unifyMatcherDefType`(TFun スパインを下り、結果位置の Matcher パラメータのみ通常 unify)を使う。
> パラメータ付き matcher 定義(R.multiset 等の再帰、math の CAS 連携 mathValue 系)はこの一般化が必須。
>
> **内部配管の修正**: 「`Matcher fresh` と unify して inner を取り出す」抽出イディオム(matcher 節の
> next-matcher 正規化・match/matchAll の matcher 型正規化、計4箇所)は TMatcher×TMatcher を踏むため、
> `bindMatcherInner`(既に Matcher 型なら直接 destructure して fresh 側だけ束縛)に置換。
>
> **stdlib 移行**:
> - `def bool/char/integer/float : Matcher T := eq` → eq の本体を各具体型で**インライン literal 化**
>   (#$val arm は型クラスの `==` でなく**組み込み等価 `=`** を使用 — sortedList/assocMultiset の #$val arm と
>   同じ慣行。プリミティブ型の instance は `(==) x y := x = y` なので意味同一。`==` のままだと concrete-constraint
>   の typeclass 展開経路で実行時に `Expected CASData, but found: "eq"` が出る既存の落とし穴がある)。
>   ※ユーザ案の「注釈なしエイリアス `def integer := eq`」は試行の結果**不採用**: eq の多相スキームのままだと
>   **具体型 hole**(`mod` の `as (integer, …)`・math・`MatcherSlot Integer Integer` 引数)で bare 変数 matcher
>   として拒否され(something@具体 hole と同じ規則 = 課題 A の規律)、mini-test 約40件+CAS が崩壊する。
>   インライン literal は「rigidity + 特殊化エイリアス禁止」を満たしつつ `integer : Matcher Integer` の
>   具体性を保つ唯一の形。
> - matcher を受け取る関数引数は slot 型に統一: `eqAs`・collection.egi の *As 系 12 関数・`AC.intersectAs` の
>   `(m: Matcher a)` → `(m: MatcherSlot a a)`(値は呼び出し点で COERCE-MATCHER-TO-SLOT)。mini-test 63/64 も同様。
> - `def termExpr/symbol/mathExpr : Matcher MathValue := mathValue` は**同型エイリアス**(t1 == t2)なので無変更で通る。
> - sample/: tree/graph/chopsticks/bipartite-graph の Matcher 引数を slot 化、five-color/salesman{,2}/cdcl/
>   gsp-mining の poly-RHS エイリアス注釈を撤去(`def price := integer` 等)。
>
> **テスト**: `test/type-error/50-matcher-collection-hetero.egi`(冒頭の反例)、`51-matcher-alias-specialize.egi`
> (`def integer2 : Matcher Integer := eq`)。全回帰(type-error 23件全拒否 / mini-test / math / mahjong /
> cabal test 21/21)はこの項の完了時点で green。

---

### 課題 K: `def integer := eq`(多相エイリアス)を阻む2つの問題 — (1) ✅ **修正済み** / (2) ✅ **最終決定: 厳格化+インライン literal**(2026-06)

> **最終決定(2026-06-12、ユーザ判断)**: 統一免除案(bare-変数 matcher × パターン構築子フリー head の
> 一様免除)は **ad-hoc であるため撤回**。`def integer : Matcher Integer := eq` は型エラーのままでよく、
> primitive matcher は **eq 本体のインライン literal**(T-MATCHER checking モード)で定義する。
>
> **撤回内容**(実装から完全除去): `exemptibleMatcherHead` 述語・`classEnvPatternHeads`(Env)、
> 束縛 cast(IDefine/IDefineMany)、slot coercion の免除(Unify)、hole 検査の免除 arm(eager/遅延)、
> interlock(`matcherExemptHeads`、EnvBuilder 検査、EvalState/Eval/InferState 配管)。
> **維持**: hole の遅延検査(`flushDeferredHoleChecks` — 注釈解決後の最終 hole 型で Def 4.2(1a) を判定、
> = 旧(b)厳格化)・**関数型 hole の許容**(TFun arm — 関数型は宣言文法上パターン構築子を持ち得ないため
> ad-hoc でない。論文 Def 4.2(1a) に明文化)・rigidity 本体・literal checking 例外(rhsCore/
> unifyMatcherDefType)・系統1の修正(TypeClassExpand)・シグネチャ完全性検査。
> **「免除なし+厳格遅延検査」の組合せ**で stdlib ロードは型エラーゼロ、全回帰 green を実証。
> テスト: 51(something→構造型 cast 拒否)/ **53 を `53-matcher-alias-specialize.egi`(eq→Matcher Integer
> cast 拒否)に差し替え** / 54(後確定 hole の something 拒否)/ mini-test/123(免除 accept 集)は削除。
> 論文: §4.6 を「Matcher Rigidity」(免除段落を削除、インライン literal の帰結文を復元)、
> Fig.4 キャプション・Def 4.2(1a)(関数型 hole の半文+resolved-hole-types 評価を明記)・
> App C(Step 3(b)(i))・App H・App I・App B.2.5・Conclusion から免除を除去。
>
> 以下は経緯の記録(統一免除案の設計と一時実装):

> **(2) の最終解決(2026-06-11、ユーザ決定 = 統一免除案)**: rigidity・slot・hole・束縛の全経路を
> 論文の ⊑ どおり厳格にしつつ、**「bare-変数 matcher 値 × パターン構築子フリーな head」だけを一様免除**する。
>
> - **免除述語** `exemptibleMatcherHead`(Env.hs): 組み込み基底型(Integer/Bool/Char/String/Float)で
>   **`inductive pattern` 宣言が無いもの**、および関数型。宣言の有無は `classEnvPatternHeads`
>   (EnvBuilder が PatternInductiveDecl 登録時に記録)で動的判定 — 宣言禁止でなく
>   「**宣言がある基底型は免除しない**」(ユーザの refinement)。
>   ※ stdlib では String は宣言あり(s.cons 等)= 免除不可、Integer は宣言なし
>   (Peano `o`/`s` は deprecated.egi にあり**デフォルト未ロード**)= 免除可。
> - **適用箇所(全consumer position で一様)**:
>   ① 束縛 cast `def x : Matcher τ := e`(IDefine/IDefineMany — bare-変数 RHS × 免除可能 τ で内側 unify)
>   ② slot coercion(Unify.coerceMatcherToSlot — 構造半分を免除、target 半分は通常どおり)
>   ③ matcher 定義の hole(eager 構文検査 + **遅延検査** `flushDeferredHoleChecks`)。
> - **hole の遅延検査(= 以前の (b) 厳格化)**: hole の target 型は囲む定義の注釈で初めて確定し得るため、
>   節推論時に (holeTy, 成分形状) を記録し、トップ式の最終 subst 適用後に判定。形状分類は
>   **成分の TIExpr ノード+環境スキーム**で行う(λパラメータは bindMatcherInner で `Matcher 変数` 化する
>   ため型だけでは eq 参照と区別不能 — **∀量化済み bare-Matcher スキームの変数参照と something リテラルのみ**
>   bare 分類、未解決の適用結果は skip(callee 側で検査済み)、slot は HCSlot、構造あり値は HCShape =
>   fresh-leaves τ_p との unify 判定(CAS タワー対応のため matchOneWay でなく unify、コピーは freshen 済みで
>   binding-independent)。catch-all `$` pp は PP-Hole(構造インデックスは常に fresh 変数)なので記録しない。
> - **interlock**: 免除を実際に使った head を記録(InferState.inferMatcherExemptHeads ⇄ EvalState 永続)し、
>   その head への**後出しの `inductive pattern` 宣言をロード時エラー**に(EnvBuilder)。同一ファイル内は
>   EnvBuilder が宣言を先に登録するため自然に「宣言があれば cast 拒否」になる。免除の正当化
>   (「構築子パターンは届かない」)が恒久化される。
> - **stdlib**: base.egi を **`def integer : Matcher Integer := eq`**(bool/char/float 同様)に回帰 —
>   ユーザが当初諦めた形がそのまま成立。インライン literal は撤去。
> - **テスト**: reject = 51(構造 head への cast)/ 53(宣言×免除の相互排他 — stdlib alias が Integer を
>   免除済みのため interlock も発火)/ 54(注釈で後確定する構造 hole への something、遅延検査)。
>   accept = mini-test/123(cast・slot・hole の免除一式+alias 合成)。40(構築子頭 hole への something)・
>   50(式レベル rigidity)は従来どおり拒否。
> - **論文**: §4 rigidity 段落に「bare-variable matcher exemption」段落を新設(免除述語・束縛 cast・
>   interlock・principality 無影響)、App I を `def integer : Matcher Integer := eq` の記述に復元。
>
> 旧記録(参考):

> 課題 J で「alias 不採用」とした根拠を最小化・精密化した。破綻は**独立な2系統**で、片方は実装バグ。
>
> **(1) 実行時破綻 = 型クラス展開(TypeClassExpand / runtime dispatch)の実装バグ(型エラーは一切出ない)**。
> 最小再現(base.egi の 4 行を `def integer := eq` 等の alias に変えた状態で、**2行**):
> ```
> declare symbol x
> x + x
> -- => Evaluation error: Expected CASData, but found: "ge"
> --    (stack: autoRule.12 → mathNormalize → plusForMathValue → addSemigroupMathValuePlus)
> ```
> 注意: `def t := x + x` だと「通った」ように見えるが、これは**遅延評価で未評価なだけ**
> (`t` を強制すれば同じエラー)。シンボリック加算の強制評価が全滅する。
> **"ge" = `sanitizeMethodName ">=" `(Types.hs:342)**、つまり型クラスメソッド `>=` の内部名文字列が
> **値として** CAS に流入している。ユーザコードに `>=` は無い — `>=` の所在は **auto rule 本体のみ**
> (normalize.egi:76 `if n >= 2`(`(x^y)^n` 規則)、:90 `k >= n`(rt 規則))。
> 経路: alias 化で `integer` が `{Eq a} ⇒ Matcher a` の**制約付き値**になる(制約付き定義は
> `addDictionaryParametersT` で**辞書引数がλの先頭に前置**される)→ `integer` を本体で使う
> `assocMultiset`(`match tgt as list (a, integer)`)以下、mathValue 系 matcher・declare rule auto の
> 生成コードの型付け/展開が変わる → rule 本体の `>=` の展開(静的解決 or runtime dispatch)が崩れ、
> sanitize 済みメソッド名が値位置に漏れ、autoRule 列の次段(autoRule.12)が CAS 値の代わりに受け取る。
> インライン literal 内で `==` を使うと出る `"eq"` 文字列エラーと同族(課題 J 参照)。
>
> **✅ 修正済み(2026-06-11)**。原因は単一でなく**4層の複合**だった:
> 1. **Infer**: `applySubstToTIExprWithClassEnv` の TIMatcherExpr ケースが **arm(data-clause 本体)に
>    代入を適用していなかった**(next-matcher のみ)→ arm も走査するよう修正。
> 2. **Infer**: `inferPatternDef` が arm を**2回推論**し、TIPatternDef に格納する2回目の TIExpr の
>    subst を `(targetTI, _)` で**破棄**。ノードの制約変数が孤児化(最終代入で解決不能)し、重複制約
>    `{Eq2 param24, Eq2 a14}` がスキームに残り辞書パラメータ名も衝突。→ `inferDataClauseWithCheck` が
>    TI も返す形に統合し、2回目の推論を削除。
> 3. **Infer**: IDefine / IDefineMany の**署名なし分岐**が `applySubstToTIExprM` /
>    `resolveConstraintsInTIExpr` を呼ばず生の exprTI を格納(署名あり分岐と非対称)。`declare rule auto`
>    は IDefineMany 経由なので autoRule 本体がこの穴を踏む。→ 署名あり分岐と同じ後処理を追加。
> 4. **TypeClassExpand**: それでも残る「**未束縛 `dict_*` 参照**」(シグネチャに必要な {Eq a} が欠けた
>    定義の本体で発生 — countAs / includeAs / fastUnique / ε′ は**インライン stdlib でも潜在的に壊れて
>    いた**)への安全網が皆無だった。→ **`fixUnboundDictRefs` 後処理パス**を新設(TypedDesugar の
>    `addDictionaryParametersT` 直後に適用)。未束縛参照は**2形状**あり両方に対処:
>    - **メソッドアクセス形** `(dict_C[__super_X…]["m"]) args` → `TIRuntimeDispatch`(第1引数の
>      実行時型でインスタンス解決)に書き換え。**パターン内の式**も走査する(真の漏出点は assoc.egi の
>      arm 内・**述語パターン `?(\n -> n >= 2)` の `>=`** だった — 式走査(mapTIExprChildren)は
>      パターンに降りないため、TIMatchExpr/TIMatchAllExpr の節パターンを TIValuePat/TIPredPat/
>      TIPApplyPat/TILoopRange 込みで処理)。
>    - **辞書引数形** `(sort dict_Ord) xs`(制約付き callee への辞書「値」渡し)→ 未束縛 `dict_C` 自体を
>      **合成辞書**(全メソッドが `TIRuntimeDispatch` する hash、superclass エントリは再帰合成、
>      0-arity メソッドは実行時ディスパッチ不能なので省略)に置換。遅延評価のため callee が辞書を
>      強制しない場合(eqAs を呼ぶ countAs 等)は偶然動いていたが、強制する callee(sort の compare を
>      呼ぶ fastUnique → `Expected CASData, but found: "compare"`)は実際に壊れていた。
>
> 副産物の修正: `--dump-tc` を math で落としていた prettyTypeDoc の CAS 型ケース欠落、matcher の
> **arm が表示されない**ダンプ(原因究明を阻んでいた)、`applySubstsToType`(TypeClassExpand)の
> CAS 型ケース欠落(非網羅クラッシュ)。回帰テスト = `mini-test/121-matcher-arm-typeclass.egi`
> (A: 具体注釈 / B: 注釈なし / C: 制約注釈 の3形状 × arm 内 `==`、+ 述語パターン内 `>=`)。
> **検証**: alias 状態で `x + x` → `2 * x`、2-math-basic / mahjong / 全 autoRule 単体 ok(系統1は
> alias 下でも完全解消)。本番(インライン)状態の全回帰も green。
> **追補(2026-06-11、ユーザ判断で厳格化を採用)**: 「本体が要求する制約がシグネチャに無い定義」は
> **型エラー**にした(`MissingSignatureConstraint`、Error.hs)。実装 = `checkResidualConstraints`
> (Infer.hs): IDefine / IDefineMany の**署名あり分岐**で、本体検査後の残存制約のうち
> 「型変数を含み、かつシグネチャの型変数に触れるもの」が、(superclass 展開済み)シグネチャ制約に
> 含意されなければ拒否。判定前に**インスタンス文脈還元**を行う(`Eq (Tensor t)` は
> `instance {Eq a} Eq (Tensor a)` の文脈 `{Eq t}` に置換して再判定、インスタンスの無い Tensor は
> 要素型へ unwrap — これをしないと生成インスタンスメソッド eqTensorNeq / eqCollectionEq /
> ordCollectionCompare / tensor の `.` が誤検知される)。インスタンスマッチは `matchOneWay`
> (Unify.hs、export 追加)による一方向一致。署名なし定義は従来どおり制約を自動 generalize(HM 的)。
>
> このチェックが stdlib から検出した**本物のバグ3件**を修正:
> - `set` の全体値パターン arm が Eq 版 `unique` を呼んでいた → **`uniqueAs m`**(matcher 基準の
>   set 等価)に修正。論文の「コンテナ matcher は Eq を伝播しない」設計と一致(unique/uniqueAs は
>   collection.egi 内で set より前に移動)。
> - `includeAs` が Eq 版 `deleteFirst` を呼んでいた → **`deleteFirstAs m`** に修正。
> - `fastUnique {Eq a}` → **`{Ord a}`**(本体の sort が Ord を要求。Ord extends Eq)。
> - あわせて `eqAs` の `{Eq a}` を撤去(本体は matcher m が等価判定を行い、辞書は未使用の遺物。
>   これで countAs / frequencyAs 等への波及も消滅)。
> テスト: reject = `test/type-error/52-missing-signature-constraint.egi`、
> accept = `mini-test/122-signature-constraints.egi`(制約宣言 / インスタンス還元 / set の
> matcher 基準 set 等価 / fastUnique)。
> `fixUnboundDictRefs` 安全網は**推論の取りこぼし向けの防御**として存続(署名欠落は今後ここに
> 到達する前に型エラーになる)。
>
> **付記: パラメータ付き algebraicDataMatcher の正しい形(2026-06-11)**。bipartite-graph.egi の
> 調査で確定: ①データ宣言 `inductive Edge a b := Edge a b` ②**パターン宣言**
> `inductive pattern Edge a b := edge a b` ③slot 注釈付き def
> (`def edge {a,b,c,d} (a: MatcherSlot b b) (c: MatcherSlot d d) : Matcher (Edge b d) := algebraicDataMatcher | edge a c`)
> の3点セットで型クリーン+実行正常。パターン宣言が無いと、推論が未宣言パターン構築子に
> `TInductive "<パターン名>"`(小文字)の構造型を捏造し、生成 main 節
> (`match (val, tgt) as (m', m')`)の照合で実データ型と衝突する(rigidity 表示になる場合もあるが
> 根本は宣言欠落)。また課題 J 期に「結果注釈の撤去で bipartite を修復」と記録したのは誤り
> (注釈なし typed-params は parse エラーで、検査の grep がそれを見落としていた — 注釈は復元済み)。
> tree.egi は同レシピで前進するが、`$ :: $`/`$ ++ $` 節(リスト用パターン構築子の Tree への
> v3 的流用)が残課題。
>
> **(2) 静的拒否 = 仕様どおりだが hole 経路と不整合**。最小再現(stdlib 無改変):
> ```
> def integer2 := eq
> def acceptIntSlot (m: MatcherSlot Integer Integer) : Bool := True
> def t := acceptIntSlot integer2   -- 型エラー(coerceMatcherToSlot: Integer ⊑ に bare 変数)
> ```
> これは論文の ⊑(matchOneWay: τ_p=Integer は変数を持たないので bare 変数 matcher を拒否)どおり。
> **しかし** 同じ「消費位置」概念の **hole 経路は同じ matcher を受理して実行も正常**:
> ```
> def integer2 := eq
> inductive Wrap := W Integer
> inductive pattern Wrap := w Integer
> def wrap : Matcher Wrap :=
>   matcher
>     | w $ as integer2 with        -- 具体 Integer hole に多相 matcher → 受理される
>       | W $n -> [n]
>       | _ -> []
>     | $ as something with
>       | $tgt -> [tgt]
> def t := matchAll (W 5) as wrap with w #5 -> True   -- 実行も [True]
> ```
> hole 側は意図的に**構文的 literal-`something` 検査 + target unify**(Infer.hs `inferPatternDef` の
> コメント参照)で論文より緩く、slot 側(matchOneWay)は論文どおり厳格 — 二経路の判定が食い違っている。
> 揃え方は設計判断: (a) slot 側を「パターン構築子宣言を持たない具体型 head に対する bare 変数 matcher は
> 許可(target unify で inner を確定)」に緩める(hole 側と同じ理屈、`def integer := eq` が viable になる)、
> (b) hole 側を論文どおり厳格化(stdlib の something@関数型 hole 例外などの再整理が必要)。
> (1) の修正と (a) を併せれば alias 方式に移行できる見込み。

---

### 課題 H: 形式的検証(適合テストスイート + 健全性)

**現状**: 個別ケースの経験的テストは多数(§B.2 の 4×2 マトリクス、COERCE、PAT-OR 等)あるが、**網羅的ではない**。
健全性(progress/preservation 相当)の機械的・論証的検証も無い。

> **進捗(2026-06)**: **reject 側の適合テストスイートを `test/type-error/` に新設**(21 ケース、1ファイル1ケース、
> 各ファイル先頭に対応規則と期待エラーを記載、README にスイープ手順)。カバー: match-site dual check
> (Case 2/3・ネスト構築子・タプル・ターゲット不一致)、PAT-APP の構造/ターゲット両側(レビュー M1 反例)、
> PATFUN-DEF 線形性 4 モード(M2)、PAT-VALUE/非線形/PAT-OR、Def 4.2(1a)/PP-Con(`weird`)、本体内 match-site、
> PAT-TUPLE arity。全件「意図したエラーで」拒否されることを検証済み。accept 側の対は `mini-test/`
> (例: `120-patfun-struct-index.egi`)。残: 規則網羅率の拡大(COERCE-SLOT-TUPLE、Def 4.2(2)(3) 等)と健全性論証。

**作業**:
- 論文の各規則(PAT-*、T-*、COERCE-*、Def 4.2 各項)に 1 対 1 対応する **accept/reject 適合テストスイート**を
  `mini-test/`(accept)/ `test/type-error/`(reject)に整備。
- 型システムの健全性(well-typed なら実行時に stuck しない)の議論・反例探索。

**優先度**: 「検証」を名乗るには重要だが、機能実装(A–F)の後。

---

## 2. 優先順位と依存関係

```
課題 A (stdlib something@具体 棚卸し)  ──→  課題 C (PP-Con hard error 化)
課題 B (Coverage hard error 化)          [A と同根だが部分マッチャー対応が主]
課題 D (refinement / decomposition body)  [独立]
課題 E (PAT-VAR 線形性)                    [独立・小]
課題 F (制約付きマッチャー接地)            [独立]
課題 G (クロスバッチ宣言型)                [独立・小]
課題 H (適合テスト + 健全性)              [A–F の後]
```

**推奨着手順**: A(棚卸し)→ B/C(hard error 化)→ F → D → E/G → H。
A→B→C で論文準拠度が最も大きく前進する(Matcher Consistency の opt-in warning を hard error に格上げできる)。

## 3. 参考(コード位置)

- 構造的許容性 / COERCE: `hs-src/Language/Egison/Type/Infer.hs`(`checkMatcherAdmissibility`)、
  `hs-src/Language/Egison/Type/Unify.hs`(`coerceMatcherToSlot`/`coerceSlotTuple`)
- Matcher Consistency / 警告: `Infer.hs` の `IMatcherExpr`・`inferPatternDef`、
  `Type/Error.hs`(`MatcherCoverageWarning`/`MatcherNextMatcherWarning`)、
  フラグ `--matcher-consistency-warnings`(`CmdOptions.hs`/`Eval.hs` の `cfgMatcherConsistencyWarnings`)
- 宣言型 concrete 化: `hs-src/Language/Egison/EnvBuilder.hs`(`concretizeDeclaredTypes`)、
  型変換 `Type/Types.hs`(`typeExprToType`)
- 非準拠 stdlib: `lib/math/expression.egi`(`something`@具体)、`lib/core/collection.egi`(`multiset`/`sortedList`)、
  `lib/core/assoc.egi`(`assocMultiset`)
- 完了済み実装の詳細: [`design/matcher-slot.md`](./matcher-slot.md)
