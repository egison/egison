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
