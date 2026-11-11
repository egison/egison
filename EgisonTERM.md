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
- 日本語で matching は「パターンマッチ」と書き，「照合」とは書かない．条件を調べるだけでなく，
  成功時にパターン変数へ値を束縛する処理であることを初出で説明する．実際の宣言と掲載内容を比べる作業は「一致確認」と書く（2026-10-12）．
- 決定の経緯: 2026-10-01 に論文の用語を統一した（ユーザー決定）．2026-10-03 に「添字（indices）」などの言い換えと，
  最汎・全域・作業リストの定義を加え，matcher tuple の日本語を「マッチャーのタプル」に統一した．
- テンソル論文（`~/PL/type-tensor-paper`）の用語は §9 にまとめ，使わない語は §7 の二つ目の表に置く．2026-11-10 に統一した（ユーザー決定）．

## 1. パターンマッチ

| 英語 | 日本語 | 意味・注意 |
| --- | --- | --- |
| pattern matching / matching | パターンマッチ | ターゲットがパターンの条件を満たすか調べ，成功した場合にパターン変数へ値を束縛する処理．Egisonでは一つのパターンとターゲットに対して複数の束縛が得られることがある． |
| matcher | マッチャー | パターンの解釈を与える第一級の値． |
| matcher expression | matcher式 | `matcher` で始まる式．matcher literal とは書かない． |
| matcher definition | マッチャー定義 | matcher式とそのマッチャー節の全体． |
| matcher clause | マッチャー節 | `pp as e with dataClauses` の形の節． |
| primitive-pattern pattern | プリミティブパターンパターン | マッチャー節の左辺．ハイフン付きで書く．メタ変数は pp． |
| pattern hole | パターンホール | プリミティブパターンパターン中の `$`．ネクストパターンを受け取る位置．根にあってもフィールドにあってもどのパターンも受け付ける（根のパターンホールがコンストラクタパターンを除くという規則はない．2026-10-06）． |
| value-pattern pattern | バリューパターンパターン | `#$val`．利用者のバリューパターンの値を束縛する． |
| next pattern / next matcher / next targets | ネクストパターン／ネクストマッチャー／ネクストターゲット | パターンホールごとに次のパターンマッチに渡すもの． |
| next-matcher expression | ネクストマッチャー式 | マッチャー節の `as` の後の式．構文上の制限はなく，型だけを要求する． |
| primitive-data-match clause | プリミティブデータマッチ節 | ターゲットを分解してネクストターゲットのリストを返す節． |
| primitive-data pattern | プリミティブデータパターン | プリミティブデータマッチ節の左辺．メタ変数は dp． |
| catch-all clause | キャッチオール節 | 最後に置く `$ as something with ...` の節．最後にあるので，それより前のマッチャー節が受け付けなかったパターン（パターン変数やワイルドカードなど）だけを受け取る． |
| general matcher clause | 一般のマッチャー節 | プリミティブパターンパターンの根がコンストラクタで，その全フィールドがパターンホールであるマッチャー節（リストの族では `[]`，`$ :: $`，`$ ++ $`）．根にそのコンストラクタを持つすべてのパターンを受け付ける．網羅性の条件は，宣言された各コンストラクタにこの節を要求する． |
| specialized matcher clause | 特殊化したマッチャー節 | 根がコンストラクタで，一般のマッチャー節でないマッチャー節（`$ :: _`，`#$val :: $`，`_ ++ $ :: _` など）．一般のマッチャー節より前に置き，特定の形のパターンだけを受け付ける．網羅性の条件には数えない． |
| match clause | マッチ節 | `match`・`matchAll` の各節． |
| target | ターゲット | パターンマッチの対象となる値． |
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
| matching atom / matching state | マッチングアトム／マッチングステート | パターンマッチの途中状態．アトムはパターン，マッチャー，ターゲットの組，ステートは未処理のアトムと環境と既に得た束縛を持つ． |
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
| `(Any for T)` | `(Any for T)` | パターンコンストラクタのフィールドで，ターゲット型 T の上の能力 Any を表す．フィールドには能力を書き，ターゲット型は能力から補うが，Any だけはターゲット型を定めないので `for` で型を添える．型だけのフィールド（`even Integer`）は書けない（2026-10-06）． |
| target type | ターゲット型 | τ．マッチャーが調べる値の型． |
| matcher type | マッチャー型 | `Matcher κ τ`．κ と τ は「能力とターゲット型」と呼ぶ（「添字」とは書かない）． |
| pattern type | パターン型 | ⟨κ, τ, Δ⟩．パターンが要求する能力とターゲット型と，導入する束縛． |
| binding list | 束縛リスト | Δ．相異なる束縛 x : τ の順序付きリスト． |
| type context / initial type context | 型文脈／初期型文脈 | Γ／Γ₀．実行時の ρ だけを environment（環境）と呼ぶ． |
| closed program | 閉じたプログラム | 宣言列と本体式からなる P = (d̄, e) のうち，本体 e の自由変数が Γ₀ の名前に限られるもの．型推論は宣言を検査して得た Σ の下で本体 e を扱う． |
| scheme / instance | スキーム／インスタンス | σ／inst(σ)．型スキームとも書く． |
| sort | ソート | 型変数と能力変数の二種類．説明なしで使わない． |
| normalization / normal form | 正規化／正規形 | `normalize`． |
| definitional equality | 定義的等式 | `Matcher (κ₁,…,κₙ) (τ₁,…,τₙ) ≡ (Matcher κ₁ τ₁, …, Matcher κₙ τₙ)`（n ≥ 2）． |
| requirement pair | 要求対 | κ ⊣ τ．フィールドや結果が要求する能力とターゲット型の対． |
| pattern family | パターン族 | 宣言された族はこれだけ．データ側は data type． |
| data type | データ型 | |
| capability constructor | 能力コンストラクタ | 各パターン族が導入する能力の構成子（リストの族では [κ]）． |
| signature | シグネチャ | Σ．すべての宣言の一覧（データ型とデータコンストラクタ，パターン族とパターンコンストラクタ，各コンストラクタのスキーム）．この意味だけに使い，一つのコンストラクタの型は scheme（スキーム），型注釈は type annotation（型注釈）と書く． |
| declaration conditions | 宣言条件 | 宣言（シグネチャ）に課す条件．整形式の条件と実行時安全性のための追加条件を，ともに論文の §3.1 で説明する． |
| well formed (signature) | 整形式（なシグネチャ） | 宣言が通常の有効範囲と引数の個数の条件を満たすこと（§3.1）．宣言的型付けと型推論が前提とする． |
| conditions for runtime safety | 実行時安全性のための条件 | 実行時の結果だけが仮定する追加の宣言条件（§3.1．組み込みのリスト・真偽値・unit 型が組み込みの値だけを持つこと，スキームの結果が量化変数を定めること．データコンストラクタでは量化変数が結果の直接の引数，パターンコンストラクタでは量化した型変数がターゲット型に現れる）． |
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
| progress | 進行性 | 実行が完了していなければ，次の実行ステップが存在すること．「前進」とは書かない． |
| dynamic type error | 動的型エラー | 実行時の型エラー．stuck は Lean の評価器の結果名としてだけ書く．動的型エラーの意味で shape error は使わない（テンソル論文の形状エラーは §9.4）． |
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
| AI coding assistant | AI のコーディング支援ツール | Lean の証明を書いた Claude（Anthropic）と Codex（OpenAI）．論文では §4 の冒頭の段落の末尾で述べる． |

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
| stuck（Lean の結果名以外），shape error（動的型エラーの意味） | dynamic type error（動的型エラー）．テンソル論文の shape error（形状エラー）は §9.4 の別の語 |
| occurrence（マッチ結果の意味） | successful matching result（成功するマッチ結果）．occurrence は要素の出現などの普通の意味だけ． |
| structural constructor／structural position | pattern constructor／constructor position．structural は値の構造比較だけに使う． |
| complete（全体の意味） | whole／full（whole match expressions など） |
| branch（orパターンの枝） | alternatives（選択肢） |
| observed type（単一化） | 解を適用する結果型 |
| header | primitive-pattern pattern |
| arm | primitive-data-match clause |
| Int（組み込みの整数型．論文の型の表記） | Integer（Egison の型名に合わせる．Lean の識別子 `Ty.int` はそのまま．規則名 T-Int・G-Int は整数リテラルの規則の名前なので残す．2026-10-07） |
| essential clause（一般のマッチャー節の意味） | general matcher clause（一般のマッチャー節）．essential は序論の「本質的なループ」と紛れる |
| refinement clause（特殊化したマッチャー節の意味） | specialized matcher clause（特殊化したマッチャー節）．refinement は refinement type と紛れ，結果の多重性を変える節もあるので不正確 |
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
| corpus／コーパス（評価・検査に使うプログラムの集合） | programs used for evaluation（評価対象のプログラム集合）／programs checked（検査対象のプログラム群） |
| callback（パターンマッチの分解を行う関数を指す語として） | decomposition function |
| payload | 使わない |
| 開発コード名（milestone 5.x，Paper 1，mech4，theorem axis，artifact 5.x，D14 など） | 内容で述べる．先行研究は「Egi と Nishiwaki~\cite{...}」のように内容と引用で指す |

テンソル論文（§9）で使わない語と言い換え（2026-11-10）．上の表の public，solver，「全体」の意味の complete なども同論文に適用する．

| 使わない語 | 代わりに使う語 |
| --- | --- |
| 独立した規則・導出・判断，independent（型推論の手続きを参照しないという意味） | declarative typing rules（宣言的な型付け規則）．数値計算や具体化の「独立」は普通の意味で使ってよい |
| 候補，candidate（一つの式への型の与え方の意味） | typing（型付け）／derivation（導出） |
| 候補群，変換候補，本体の候補，通常の候補，追加の候補，テンプレート，雛形，member，native，alternative，family，template | definition candidates（定義の候補），original body（元の本体），body after insertion（挿入後の本体） |
| コールバック，渡された関数，高階引数，関数引数，callback，supplied function | function argument（引数として渡す関数，渡す関数） |
| 変換関数，adapted function | converted function（変換後の関数） |
| 変換（総称の意味），自動変換，テンソルの変換，変換結果，adapt，transformation | insertion（挿入），挿入後の式．「変換」は「渡す関数の変換」にだけ使う |
| 写像，暗黙の写像，写像する，map（挿入する操作を指す名詞・動詞） | `tensorMap` の挿入，成分ごとに適用する（apply componentwise），持ち上げ（lifting） |
| 受入れ，受け入れ，昇格，admission（階数 0 の意味） | rank-0 inclusion（階数 0 の包含） |
| seed | origin（起点） |
| supported，grounded，テンソルの要求（用語として），demand | justified／justification（根拠のある／根拠） |
| 固定されたテンソル仮引数，fixed tensor parameter | parameter of tensor type（テンソル型の仮引数） |
| 一般的な導出，generic derivation | general derivation（一般の導出） |
| 保存型，保存した型，内部の型，注釈（推論の内部データの意味），saved types | types of subexpressions（部分式の型）．「保存」は型の保存と保存性にだけ使う |
| 報告型，候補型，報告する，reported type | inferred type（推論した型），「推論する」「返す」 |
| 優先型，最優先の型 | preferred type（優先される型） |
| 生成項，生成した項，生成式，generated term | generated expression（生成した式） |
| 最大の連続適用，適用全体，適用の列，spine | application chain（連続適用） |
| 直接適用，階数 0 の受入れ，成分ごとの適用，コールバック（適用モードの名前として） | 通常，階数 0，持ち上げ，関数の変換 |
| 優先規則，優先関係，選択規則 | priority order（優先順位） |
| 補完，completion（挿入の意味．「補完済みの定義」など） | insertion（挿入）．「補完」は添字補完にだけ使う |
| 公開入口，公開推論，public entry point | combined inference（統合型推論） |

## 8. 規則名と操作名

- 規則名: T-PPHole，T-PPValue，T-DataClause，T-MatcherClause，T-Matcher，T-MatchClause，T-PTuple，P-Tuple，U-Matcher-Tuple
  （宣言的規則は T-，等式生成規則は G-・P-，単一化は U- で始める）．
- 操作名・述語名: firstClause，firstDataClause，nextTargets，Next，Targets，ValueBeforeHole，CapOK，EvidenceOK，collectSome，
  StaticChecks，CapEq，Evidence，freshInst，mgu，substitutionEquations，normalize，inst，Gen．
- 規則名・数式・コード・`\cite`／`\label` のキーは日本語版でも変えない．

## 9. テンソルの型推論と評価

テンソル論文（`~/PL/type-tensor-paper`）の本文・要旨・図表では，操作の名前と，何を保証するかを具体的に述べる．
2026-11-10 に，同論文の用語を以下のとおり統一した（ユーザー決定）．一つの概念に一つの語を使い，初出で定義する．
同論文で使わない語は §7 の二つ目の表にまとめる．Lean（`~/PL/type-tensor-mech`）と Egison インタプリタの識別子は変えず，
`PublicInference` などの成果物の実名は，付録の対応表ではそのまま使う．

2026-11-11 の設計整理を反映する．新しい規則の正本は [テンソルの設計仕様](design/type-tensor-simple.md) であり，印を使う型推論は未実装である．「定義の候補」「展開したソース」「優先される型」「優先順位」は，移行前の論文・Lean・実装との対応を説明するときに限って使う．それらの識別子が残っていることを，新設計の規則と混同しない．

### 9.1 型推論と挿入

| 英語 | 日本語 | 意味・注意 |
| --- | --- | --- |
| insertion | 挿入 | `tensorMap`・`tensorMap2` の呼び出しを式に加えること．適用を成分ごとの適用にする場合（持ち上げ）と，引数として渡す関数を包む場合（渡す関数の変換）があり，定義の本体の中に加える場合も含めて，その総称として使う．「変換」は総称に使わない． |
| inference without insertion | 挿入なし型推論 | 入力式に挿入を行わずに型を求める．利用者が明示した `tensorMap` は使える．通常の Hindley–Milner 型推論とは区別する． |
| inference with automatic insertion | 自動挿入型推論 | 挿入を許し，型と実行する式を求める．必ず何かを挿入するという意味ではない． |
| combined inference | 統合型推論 | 挿入なし型推論と自動挿入型推論の扱いを合わせた利用者向けの処理．新設計ではテンソル型の位置から決まる挿入を認め，それ以外では入力全体の挿入なし型付けを保つ．Lean の現行の入口は `Algebraic.inferAndInsert`． |
| lifting | 持ち上げ | スカラー型の仮引数にテンソルを渡す適用を，成分ごとの適用として型付けすること．`tensorMap`・`tensorMap2` の挿入で実現する．適用モードと持ち上げ集合の名前にも使う． |
| function argument | 引数として渡す関数 | 別の関数に引数として渡す関数（`foldl (+) 0 xs` の `(+)`）．初出でこう書き，二度目から「渡す関数」と書く． |
| conversion of a function argument | 渡す関数の変換 | 実際の関数を高階関数へ渡す箇所で，テンソルを受け取って成分ごとに適用する関数に包む挿入（`(+)` を `\u v -> tensorMap2 (+) u v` にする）．その使用の制約から一度だけ決め，受け取り側の本体では同じ関数と型を共有する．置き換えた関数は converted function（変換後の関数）と書く． |
| rank-0 inclusion | 階数 0 の包含 | スカラー型 κ の値を，値も式も変えずに `Tensor κ` 型の値（階数 0 のテンソル）として扱うこと． |
| generated expression | 生成した式 | 挿入の結果として実行する式．「項」とは書かない．挿入の前後を対比するときは「挿入前の式」「挿入後の式」と書いてよい． |
| scalar function / tensor function | スカラー関数／テンソル関数 | スカラー関数は整数などの個々の成分の値を受け取り，テンソルへの適用では持ち上げる．テンソル関数はテンソル全体を受け取る．型と印の決定後の仮引数型で区別する．新設計の積 `.` は一つのテンソル入力の型と本体を持ち，スカラーは階数 0 として受け取る． |
| 未定 | 印 | 型の位置がスカラーのままか `Tensor` 型になるかを表す二値の変数．`τ*p` は印 `p` を付けた型で，`p∨q` はどちらかがテンソルならテンソルになる．英語名と論文の記法は未決定． |
| 未定 | 印を消した型 | すべての印をスカラーにした型．固定された `Tensor` は取り除かない． |
| 未定 | 印の値ごとの本体 | 一度型付けした定義から，決定した印に従って挿入を生成した本体．使用箇所で本体を推論し直して選ぶ定義の候補とは区別する． |

### 9.2 型付け規則と導出

| 英語 | 日本語 | 意味・注意 |
| --- | --- | --- |
| declarative typing rules | 宣言的な型付け規則 | 型推論の手続きを参照せずに，式にどの型と挿入を認めるかを定める規則．論文 1 の宣言的型付け（§2）と同じ語を使い，「独立した規則」とは書かない． |
| source typing / core typing | ソースの型付け／コアの型付け | ソースの型付けは，挿入を認める位置を定める宣言的な型付け規則．コアの型付けは，`tensorMap` が明示された式（挿入前後の式）と実行時の値の型付け Γ ⊢ e : τ． |
| typing / derivation | 型付け／導出 | 一つの式への型と挿入の与え方．例で説明する節では「型付け」，規則の節では「導出」と書く．一つの式に複数ありうる．これを「候補」とは呼ばない． |
| general derivation | 一般の導出 | 適用モードなどの選択を固定して，型の等式を最汎に解いた導出（Γ ⊢g e : τ ▷ H）．generic は汎用パターン（§1）にだけ使うので，generic derivation とは書かない． |
| definition candidates | 定義の候補 | 移行前の方式が保持する元の本体と挿入後の本体．使用箇所ごとに一つを選ぶ．新設計ではこの選択を行わない．テンソルの旧方式の説明では「候補」をこの意味に限る． |
| expanded source | 展開したソース | 移行前の規則で，定義の各参照の候補と渡す関数の変換を書き込んだ式（e ⇝ q の q）．新設計の生成する式と無条件に同一視しない． |
| types of subexpressions | 部分式の型 | 導出が式の各部分式に与えた型の列（H，A）．「保存型」「注釈」とは書かない．型注釈は利用者が書く型だけを指す（§2）． |
| inferred type | 推論した型 | 統合型推論が返す型．「報告型」「報告する」とは書かない． |
| preferred type | 優先される型 | 移行前の規則で選んだ導出の型の具体化（述語 Preferred）． |
| priority order | 優先順位 | 移行前の規則で複数の導出から一つを選ぶ順序．新設計では，型と印の制約を解く規則に置き換える． |
| origin | 起点 | ソースに書かれたテンソル操作（テンソル構築式，`tensorMap`・`tensorMap2`，添字参照，`contractWith` など）が `Tensor` 型にする，型の位置． |
| justification / justified | 根拠／根拠のある | 型のある位置を `Tensor` 型にするとき，起点からその位置まで，型の等式と定義の使用をたどって届くこと．その条件を「根拠の条件」と呼び，論文の述語は Justified と書く（Lean の `supported_iff` などはそのまま）．「テンソルの要求」は用語にせず，「`Tensor` 型を要求する」と動詞で書く． |
| parameter of tensor type | テンソル型の仮引数 | 型が `Tensor` 型に決まっている仮引数（`contractWith` の第二引数など）． |
| 未定 | テンソル型の位置 | `Tensor` 型の仮引数，テンソル構築式の成分，添字参照の対象など，テンソル型を要求する位置．型が未確定の入力へ要求を伝える起点になる． |

### 9.3 適用

| 英語 | 日本語 | 意味・注意 |
| --- | --- | --- |
| application chain | 連続適用 | 関数に引数を続けて適用した式 f e₁ … eₙ で，外側にさらに適用が続かないもの．印のないスカラー仮引数への持ち上げは，全体の制約を解いて決める．高階関数の本体内の仮引数の各適用に，独立した挿入を選ぶ単位ではない．spine とは書かない（`liftSpine` はそのまま使う）． |
| application mode | 適用モード | 連続適用の各引数の扱い．ordinary（通常），rank 0（階数 0），lifting（持ち上げ），function conversion（関数の変換）の四つ． |

### 9.4 テンソルの式，操作，添字，評価

| 英語 | 日本語 | 意味・注意 |
| --- | --- | --- |
| tensor construction | テンソル構築式 | 成分となる式を並べてテンソルを作る式．Egison の `[e1,...,ek]` はリストを表す．初出で記法を説明する．literal／リテラルとだけ書かない． |
| `tensorMap` / `tensorMap2` | 操作名をそのまま使う | テンソルのスカラー成分ごとに関数を適用する操作と，その二引数版．プログラマが書いた呼び出しと挿入した呼び出しを区別する場合も，操作名を述べる．操作の動作は「成分ごとに適用する（apply componentwise）」と書き，名詞・動詞の「写像」は使わない． |
| `Integer` | 整数型 | テンソル論文の数値型．任意精度の整数に対する加算・減算・乗算を扱う．Leanでは `Int` で表す． |
| symbolic index | シンボリックな添字 | 軸に付ける名前と，上付き・下付き・上下両方の印の組．添字を指して「ラベル」「軸ラベル」「シンボル」とは書かない． |
| numeric index | 数値添字 | テンソルの一つの軸に沿って成分を選ぶ番号．ソースの添字は1から数える．シンボリックな添字は軸に名前を付ける．数値添字にも上下の印を付けられるが，成分選択はその印によらない．幾何学の座標とは区別する． |
| index tuple | 添字の組 | 各軸における整数の位置を並べたもの．配列の内部表現では0から数えることを明記する． |
| index completion | 添字補完 | 省略した添字に名前を補うこと．uniform completion（一様補完）は対応する位置で名前を共有し，disjoint completion（非共有補完）は互いに異なる名前を与える．「補完」はこの意味だけに使う． |
| typing of evaluation results | 評価結果の型付け | 値呼びの大ステップ意味論で，$e : t$ かつ $e \Downarrow v$ ならば $v : t$ となる性質．結果は値または定義済みの実行時エラー．大ステップ評価の結果についての保証であり，小ステップの型保存・進行性とは区別する． |
| termination, array bounds, and fresh index names | 停止性，配列参照の範囲，添字名の衝突回避 | 添字処理の実装についての個別の保証を述べる．階数や数値添字の範囲を型で管理する保証と混同しない．「安全性」とだけ書かない． |
| shape error | 形状エラー | 形状や添字列が合わないテンソルの構築・演算が返す実行時エラー．論文 1 で動的型エラーの言い換えとして使わないと決めた shape error（§7）とは別の概念であり，テンソル論文ではこの意味で使う． |
