# Egison 用語集

Egison の論文（英語版・日本語版），Lean による機械化（`~/PL/type-pm-mech`），Egison インタプリタの
コメントと利用者向けメッセージで使う用語の一覧である．ここに載せた語を使い，§7 の「使わない語」は使わない．

## 決め方と使い方

- パターンマッチの語は，APLAS 2018 論文（`~/PL/pm-paper/egison.tex`）で定義された語を使う．
  そこにない語は Programming 2020 論文（`~/PL/pmo-paper3/main.tex`）の語を使う．
- 日本語訳は `~/PL/pm-paper/ja/main.tex` の訳語に合わせる．日本語版では，重要語の初出で「訳語（英語）」を併記する．
- 専門用語は初出で定義してから使う．定義していない略称，開発中のコード名，作業単位の名前を
  論文・ドキュメント・ユーザーへの報告に持ち込まない．
- 用語を変えたら，このファイルと，`~/PL/type-pm-paper` の `CLAUDE.md`・`AGENTS.md`・`example-correspondence.md` を合わせて更新する．
- 決定の経緯: 2026-10-01 に論文の用語を統一した（ユーザー決定）．2026-10-03 に「添字（indices）」などの言い換えと，
  最汎・全域・作業リストの定義を加え，matcher tuple の日本語を「マッチャーのタプル」に統一した．

## 1. パターンマッチ

| 英語 | 日本語 | 意味・注意 |
| --- | --- | --- |
| matcher | マッチャー | パターンの解釈を与える第一級の値． |
| matcher expression | matcher式 | `matcher` で始まる式．matcher literal とは書かない． |
| matcher definition | マッチャー定義 | matcher式とそのマッチャー節の全体． |
| matcher clause | マッチャー節 | `pp as e with dataClauses` の形の節． |
| primitive-pattern pattern | プリミティブパターンパターン | マッチャー節の左辺．ハイフン付きで書く．メタ変数は pp． |
| pattern hole | パターンホール | プリミティブパターンパターン中の `$`．ネクストパターンを受け取る位置． |
| value-pattern pattern | バリューパターンパターン | `#$val`．利用者のバリューパターンの値を束縛する． |
| next pattern / next matcher / next targets | ネクストパターン／ネクストマッチャー／ネクストターゲット | パターンホールごとに次の照合に渡すもの． |
| next-matcher expression | ネクストマッチャー式 | マッチャー節の `as` の後の式．構文上の制限はなく，型だけを要求する． |
| primitive-data-match clause | プリミティブデータマッチ節 | ターゲットを分解してネクストターゲットのリストを返す節． |
| primitive-data pattern | プリミティブデータパターン | プリミティブデータマッチ節の左辺．メタ変数は dp． |
| catch-all clause | キャッチオール節 | 最後に置く `$ as something with ...` の節． |
| match clause | マッチ節 | `match`・`matchAll` の各節． |
| target | ターゲット | 照合される値． |
| pattern constructor | パターンコンストラクタ | パターン族に属するコンストラクタ（nil，cons，join など）． |
| data constructor | データコンストラクタ | 値を作るコンストラクタ． |
| pattern variable / wildcard / value pattern | パターン変数／ワイルドカード／バリューパターン | `$x`／`_`／`#e`． |
| generic pattern | 汎用パターン | パターンコンストラクタを選ばないパターン．generic はこの語にだけ使い，定理などの「一般の」は general と書く． |
| and-pattern / or-pattern / not pattern | andパターン／orパターン／否定パターン | |
| alternatives | 選択肢 | orパターンの枝．branch は探索の枝にだけ使う． |
| conjuncts | 連言肢 | andパターンの両側． |
| non-linear pattern | 非線形パターン | 先に束縛した変数を後のバリューパターンで参照するパターン． |
| non-free data type | 非自由データ型 | multiset や set のように，同じ値に複数の表し方があるデータ型． |
| loop pattern / sequential pattern / pattern function | ループパターン／逐次パターン／パターン関数 | |
| matching atom / matching state | マッチングアトム／マッチングステート | 照合の途中状態． |
| matching result | マッチ結果 | |
| successful matching result | 成功するマッチ結果 | 探索木の経路で区別する．occurrence とは書かない． |
| search tree | 探索木 | マッチングステートを節点とする木． |
| binary reduction tree | 二分簡約木 | 節点はステートのリスト．左の子は先頭のステートの後続のリスト，右の子は残りのステートのリスト．単に reduction tree（簡約木）とも書く． |
| fair enumeration | 公平な列挙 | 可算無限個のマッチ結果をすべて有限の位置に出す列挙． |
| pattern-match-oriented programming | パターンマッチ指向プログラミング | |

## 2. 型システム

| 英語 | 日本語 | 意味・注意 |
| --- | --- | --- |
| capability | 能力 | κ．マッチャーが解釈できるパターン族を表す． |
| Any | Any | パターンコンストラクタを一つも許さない能力．`something` の能力． |
| target type | ターゲット型 | τ．マッチャーが調べる値の型． |
| matcher type | マッチャー型 | `Matcher κ τ`．κ と τ は「能力とターゲット型」と呼ぶ（「添字」とは書かない）． |
| pattern type | パターン型 | ⟨κ, τ, Δ⟩．パターンが要求する能力とターゲット型と，導入する束縛． |
| binding list | 束縛リスト | Δ．相異なる束縛 x : τ の順序付きリスト． |
| type context / initial type context | 型文脈／初期型文脈 | Γ／Γ₀．実行時の ρ だけを environment（環境）と呼ぶ． |
| closed program | 閉じたプログラム | 自由変数が Γ₀ の名前だけの式． |
| scheme / instance | スキーム／インスタンス | σ／inst(σ)．型スキームとも書く． |
| sort | ソート | 型変数と能力変数の二種類．説明なしで使わない． |
| normalization / normal form | 正規化／正規形 | `normalize`． |
| definitional equality | 定義的等式 | `Matcher (κ₁,…,κₙ) (τ₁,…,τₙ) ≡ (Matcher κ₁ τ₁, …, Matcher κₙ τₙ)`（n ≥ 2）． |
| requirement pair | 要求対 | κ ⊣ τ．フィールドや結果が要求する能力とターゲット型の対． |
| pattern family | パターン族 | 宣言された族はこれだけ．データ側は data type． |
| data type | データ型 | |
| capability constructor | 能力コンストラクタ | 各パターン族が導入する能力の構成子（リストの族では [κ]）． |
| signature | シグネチャ | Σ．すべての宣言の一覧（データ型とデータコンストラクタ，パターン族とパターンコンストラクタ，各コンストラクタのスキーム）．この意味だけに使い，一つのコンストラクタの型は scheme（スキーム），型注釈は type annotation（型注釈）と書く． |
| declaration conditions | 宣言条件 | 宣言（シグネチャ）に課す条件．整形式の条件（論文の §3.1）と実行時安全性のための条件（§4.2）からなる． |
| well formed (signature) | 整形式（なシグネチャ） | 宣言が通常の有効範囲と引数の個数の条件を満たすこと（§3.1）．宣言的型付けと型推論が前提とする． |
| conditions for runtime safety | 実行時安全性のための条件 | 実行時の結果だけが仮定する宣言の条件（§4.2．組み込みのリスト・真偽値・unit 型が組み込みの値だけを持つこと，スキームの結果が量化変数を定めること．データコンストラクタでは量化変数が結果の直接の引数，パターンコンストラクタでは量化した型変数がターゲット型に現れる）． |
| type annotation | 型注釈 | プログラマが書く型．signature とは書かない． |
| pattern declaration | パターン宣言 | `inductive pattern` 宣言．frozen pattern signature とは書かない． |
| root capability | 根の能力 | o．プリミティブパターンパターンの根のコンストラクタから得る能力． |
| expected capability | 期待能力 | b．根では none（根の期待能力）． |
| matcher tuple / tuple of matchers | マッチャーのタプル | 通常のタプルの型付けを使う．第一級の値として扱うものは first-class matcher tuple（第一級のマッチャーのタプル）．日本語では「マッチャータプル」とは書かない． |
| forced matcher tuple | 強制済みのマッチャーのタプル | 実行時にネクストマッチャーを強制した構造．第一級のマッチャーのタプルと区別する． |
| tuple | タプル | product（積）とは書かない． |
| type agreement | 型の一致 | |
| declarative typing | 宣言的型付け | |
| static conditions | 静的条件 | StaticChecks が表す条件（形成条件，網羅性，ValueBeforeHole）． |
| formation conditions / coverage | 形成条件／網羅性 | |

## 3. 等式と単一化

| 英語 | 日本語 | 意味・注意 |
| --- | --- | --- |
| equality constraints | 等式制約 | 概念として使う． |
| equation generation | 等式生成 | 規則と節の名前（Equation Generation，Equation-generation rules）． |
| substitution | 代入 | S．型変数を型へ，能力変数を能力へ写す． |
| solution | 解 | 等式 E のすべての等式を満たす代入． |
| most general | 最汎 | E の解であって，他のすべての解がこの解にさらに代入して得られるもの（型の一致は正規化後に判定する）． |
| most general unifier | 最汎単一化子 | mgu(E)．最汎な解（代入）．手続きの単一化器と区別する． |
| unifier | 単一化器 | 等式を解く手続き．solver は SAT solver にだけ使う． |
| total | 全域 | どの入力でも停止し，解または失敗を返すこと．complete（完全性）と区別する． |
| worklist | 作業リスト | まだ処理していない等式のリスト． |
| fresh variable | 新鮮な変数 | `freshInst` はスキームの量化変数を新鮮な変数で置き換える． |
| equate | 等置する | |
| occurs check | 出現検査 | |
| expansion / distribution | 展開／分配 | マッチャー型とタプル型の等式を成分ごとの等式に置き換えること／能力とターゲット型がすでに同じ要素数のタプルのマッチャー型を分けること． |
| principal type / principality | 主要型／主要型性 | |
| generalization | 一般化 | let での Gen． |
| monomorphic / polymorphic | 単相／多相 | |
| soundness / completeness | 健全性／完全性 | complete は完全性の意味だけに使う．「全体」の意味は whole／full． |

## 4. 評価と実行時

| 英語 | 日本語 | 意味・注意 |
| --- | --- | --- |
| fuel | 燃料 | 評価の上限．単一化の step bound は別の語として残す． |
| runtime safety | 実行時安全性 | 型の付いたプログラムが動的型エラーを起こさないこと．「実行安全性」とは書かない． |
| dynamic type error | 動的型エラー | 実行時の型エラー．stuck は Lean の評価器の結果名としてだけ書く．shape error は使わない． |
| delayed computation | 遅延計算 | suspension とは書かない． |
| evaluated result | 評価結果 | cache／cached とは書かない． |
| weak-head value | 弱頭値 | |
| call-by-need evaluation | 必要呼び評価 | |
| the Egison interpreter | Egisonインタプリタ | production とは書かない． |
| type checker / strict mode | 型検査器／厳格モード | 厳格モードは `--type-check-strict`． |

## 5. 機械化（Lean）

| 英語 | 日本語 | 意味・注意 |
| --- | --- | --- |
| the formalized type system | 形式化した型システム | |
| Lean's inference function `infer` | Lean の推論関数 `infer` | |
| numbers that name type variables and capability variables | 型変数と能力変数を表す番号 | `Supply` の上限の対象．「添字（indices）」とは書かない． |
| de Bruijn indices | 束縛位置を表す番号 | 局所変数の表現． |
| position | 位置 | ソース列の要素の位置．「添字（index）」とは書かない． |

Lean の識別子は論文の語に合わせ，長い語は論文のメタ変数の略記を使う（2026-10-01 決定）．

| 旧名 | 新名 |
| --- | --- |
| header（`HeaderTyping`，`firstHeader`） | `PPat`（`PPatTyping`，`firstClause`） |
| data pattern（`DataPatternTyping`） | `DPat`（`DPatTyping`） |
| `Arm` | `DataClause` |
| `Clause`（`Clause.general`） | `MatcherClause`（`MatcherClause.mk`） |
| `PPat.capture`，`captureDiscipline` | `PPat.value`（束縛は `valueBindings`），`valueBeforeHole` |
| `Expr.matcherLit` | `Expr.matcher` |
| product／prod（`Ty.product`） | tuple（`Ty.mkTuple`） |
| `Result.stuck`（`*_never_stuck`） | `dynamicTypeError`（`*_no_dynamic_type_error`） |
| `Suspension`，`cached` | `DelayedComputation`，`evaluated` |
| occurrence（`MatchingOccurrence`） | path result（`MatchingPathResult`） |
| `Cursor` | `LazyStream` |
| `Bundle` | `MatcherTuple` |
| `DataFormer`／`PatternFormer` | `DataType`／`PatternFamily` |
| `Dual`（`DualScheme`） | `RequirementPair`（`PatternCtorScheme`） |
| `ObservationalEq` | `NormalizedEq` |
| fallback | else |

静的条件は真偽値の関数 `staticChecks` と命題 `StaticConditions` の二つで表し，`staticConditions_iff_staticChecks` が両者の同値を示す．

## 6. Egison インタプリタ（Haskell）の識別子

識別子，コメント，利用者向けメッセージまで同じ語に揃える（2026-10-01 決定）．CLI のオプション名は外部仕様なので変えない．

| 旧名 | 新名 |
| --- | --- |
| `Dual` | `RequirementPair`（`requirementCapability`／`requirementTarget`） |
| `DualScheme` | `PatFuncScheme`（`patFuncCapBinders`／`patFuncTyBinders`／`patFuncParams`／`patFuncResult`） |
| `TypeFormer` | `DataType`（`DataTypeId`／`dataTypeOf`／`mkDataType`） |
| capability former | `CapabilityConstructor…` |
| primitive-pattern pattern の header | `PPat`（`inferPPat`） |
| arm | `PD…`／`dataClauses`（`dataClausesExhaustive`，`MatcherDataClausesNotExhaustive`） |
| `MatchCapturedValuePatScope` | `MatchValuePatternScope` |
| `…MatcherProducts` | `…MatcherTuples` |
| match の fallback | `matchElse…` |
| Unify.hs の producer／consumer | `rigid`／`quantified` |
| 生成型変数名の接頭辞 headerCap／dualCap | ppatCap／requirementCap |

## 7. 使わない語と言い換え

| 使わない語 | 代わりに使う語 |
| --- | --- |
| indices／添字（マッチャー型の κ と τ） | capability and target type（能力とターゲット型） |
| indices／添字（型変数・能力変数の番号） | numbers that name type variables and capability variables（型変数と能力変数を表す番号） |
| binding indices | de Bruijn indices（束縛位置を表す番号） |
| index／添字（列の要素の位置） | position（位置） |
| product／積 | tuple（タプル） |
| matcher literal | matcher expression（matcher式） |
| production（実装を指す語） | the Egison interpreter（Egisonインタプリタ） |
| public | 使わない |
| interface | 「パターンがマッチャーに要求するもの」「マッチャーが提供するもの」と言い換える |
| solver（SAT solver 以外） | unifier（単一化器） |
| stuck（Lean の結果名以外），shape error | dynamic type error（動的型エラー） |
| occurrence（マッチ結果の意味） | successful matching result（成功するマッチ結果）．occurrence は要素の出現などの普通の意味だけ． |
| structural constructor／structural position | pattern constructor／constructor position．structural は値の構造比較だけに使う． |
| complete（全体の意味） | whole／full（whole match expressions など） |
| branch（orパターンの枝） | alternatives（選択肢） |
| observed type（単一化） | 解を適用する結果型 |
| header | primitive-pattern pattern |
| arm | primitive-data-match clause |
| capture | value-pattern pattern |
| matcher bundle | forced matcher tuple（強制済みのマッチャーのタプル） |
| マッチャータプル（日本語） | マッチャーのタプル |
| cursor | lazy stream |
| former（data former／pattern former） | data type／pattern family |
| dual | requirement pair（要求対） |
| pattern-function dual，dual scheme | pattern-function type，pattern-function scheme（パターン関数スキーム） |
| suspension | delayed computation（遅延計算） |
| cache／cached | evaluated result（評価結果） |
| frozen signature，frozen pattern signature | pattern declaration（パターン宣言） |
| signature（一つのコンストラクタの型の意味） | scheme（スキーム） |
| signature（型注釈の意味） | type annotation（型注釈） |
| constructor signature | signature（シグネチャ） |
| 実行安全性 | 実行時安全性 |
| callback | decomposition function |
| payload | 使わない |
| 開発コード名（milestone 5.x，Paper 1，mech4，theorem axis，artifact 5.x，D14 など） | 内容で述べる．先行研究は「Egi と Nishiwaki~\cite{...}」のように内容と引用で指す |

## 8. 規則名と操作名

- 規則名: T-PPHole，T-PPValue，T-DataClause，T-MatcherClause，T-Matcher，T-MatchClause，T-PTuple，P-Tuple，U-Matcher-Tuple
  （宣言的規則は T-，等式生成規則は G-・P-，単一化は U- で始める）．
- 操作名・述語名: firstClause，firstDataClause，nextTargets，Next，Targets，ValueBeforeHole，CapOK，EvidenceOK，collectSome，
  StaticChecks，CapEq，Evidence，freshInst，mgu，substitutionEquations，normalize，inst，Gen．
- 規則名・数式・コード・`\cite`／`\label` のキーは日本語版でも変えない．
