# Matcher capability 実装契約

> **状態（2026-08-27）**
> 本文書は、Egison 本体に実装した TypePM matcher capability の契約と、その保証範囲を記録する。
> 形式設計の正本は
> [type-pm-mech3](../../type-pm-mech3/README.md)
> を参照すること。二添字導入前の一-sort matcher 設計は現行契約ではなく、
> 本文書と `type-pm-mech3` へ統合済みである。

## 1. 実装範囲の要約

| 項目 | 現在の状態 | 保証範囲 |
|---|---|---|
| core に対する保守的拡張 | 実装済み | core の構文・型・署名を使い、core の静的な前提を満たすプログラムは同期済み TypePM 規則だけで型付けし、core の型等式の失敗を別の matcher solver で救済しない |
| 二 sort・二添字 | 実装済み | `Matcher κ τ` と `MatcherSlot κ τ` を使用し、capability と通常型を別 sort として扱う |
| D1 ShapeCap | 実装済み | pattern constructor の署名から evidence と observability を計算し、節間を exact merge する |
| Coverage | D1 から独立して実装済み | primitive pattern の網羅性は opt-in warning のままとし、ShapeCap を弱めない |
| D3 代入・一般化 | core fragment 実装済み | capability 変数と通常型変数を別々に代入、一般化、具体化し、top-level definition と pattern function の明示 scheme を両 sort とも rigid に検査する |
| 再帰値 | 実装済み | 再帰変数へ A/R 用途制約を持つ単相 placeholder を与え、本体の推論型と最後に単一化する。再帰値の根を lambda／matcher literal に制限し、self の名前や別名関係は追跡しない |
| D5-core `CapTargetOK` | 実装済み | 固定された構文的 type former と文脈仮定だけで検査する |
| D5-CAS | 未実装 | 標準ライブラリを動かすための legacy compatibility boundary のみ存在する |

ここで保守的拡張とは、core の静的な前提を満たす既存プログラムの型と代入を Egison
固有の規則が変更しないという意味である。Egison 固有の型付け規則は、通常型
`TAny`、CAS／Tensor 型、legacy CAS view、core 外の pattern 構文、または Coverage
などの core の表層上の前提を緩和することが明示的に診断された場合だけ選択する。
core の型等式や matcher／slot 判定が失敗したという事実だけでは拡張規則を選択しない。

したがって、現在の実装は **非 legacy 経路に TypePM core の検査済み fragment を持つ**
が、形式証明済みまたは certified な fragment を持つという主張ではない。型検査器
または標準ライブラリ全体も certified とみなせる状態ではない。
特に D5-CAS を実装済みとは扱わない。

## 2. 型の契約：二 sort・二添字

内部型は次の二つを区別する。

```haskell
TMatcher     Capability Type
TMatcherSlot Capability Type
```

- `κ : Capability` は、そのマッチャーで使用できる構造化パターンの形を表す。
- `τ : Type` は、そのマッチャーが照合する target の通常型を表す。
- surface syntax でも `Matcher κ τ` と `MatcherSlot κ τ` の両添字を必須とする。
  一添字表記への互換糖衣は設けない。
- capability 変数と通常型変数は別 namespace、別 sort である。
- 通常型側の特殊化は capability を強化しない。例えば
  `Matcher Any a` の `a` が `[Integer]` に特殊化されても、結果は
  `Matcher Any [Integer]` であり、`Matcher [Any] [Integer]` にはならない。
- capability sort の `Any` と、通常型側の gradual escape hatch `TAny` は別物である。
  match site では通常型 `TAny` を structured capability の witness として使用しない。
  constructor capability、rigid skolem、またはそれらを含む tuple が要求される場合は
  型エラーとし、literal consumer capability `Any` と flexible capability variable の
  unconstraining な要求だけを許す。
- match-site pattern から capability skeleton を導出できない場合も、fresh な
  unconstraining slot へ弱めず型エラーにする。

実装箇所は `Type/Types.hs`、surface AST は `AST.hs`、構文解析は
`Parser/NonS.hs` にある。

### 2.1 producer-stable capability matching

`Matcher κ_p τ_p` を `MatcherSlot κ_c τ_c` へ渡す片方向判定では、
`matchCapability κ_p κ_c` を producer-first の順で呼ぶ。代入の定義域は、判定開始時の

```text
fcv(κ_c) \ fcv(κ_p)
```

に固定する。従って consumer だけに属する flexible capability variable は producer
の構造へ解けるが、producer の変数、および producer と consumer が共有する変数は
解けない。分解途中で producer の変数が consumer 位置へ現れても定義域へ追加しない。
反復 consumer 変数は同じ代入で整合しなければならず、occurs check も行う。

この判定は `Type/Unify.hs` の対称な `unifyCapability` とは別である。前者は
producer-to-slot coercion と ordinary type の `matchOneWay` 内の nested matcher
比較にだけ使い、後者は二つの producer 型または二つの slot 型の等式制約に使う。
`coerceMatcherToSlot` は capability の片方向代入を先に求め、その代入を constraints
と両 target 型へ適用してから通常型を unify し、二つの代入を順序どおり compose
する。

capability `Any` は ground constructor であり、変数や「未解決」を表さない。その
wildcard 性は片方向判定の **元の consumer 構文に literal `Any` がある位置だけ**に
限定する。従って次を満たす。

- `matchCapability κ_p Any` は、well-formed な任意の producer `κ_p` を受理する。
- producer 側の `Any` は structured consumer を満たさない。
- 対称な `unifyCapability` と evidence の exact merge では `Any` は rigid であり、
  `Any` と一致するのは `Any` だけである。
- consumer 変数 `κ` を producer `Any` へ解いた後の `κ` の再出現は、保存済みの
  `Any` と strict に一致しなければならない。代入後に現れた `Any` を literal wildcard
  とみなしてはならない。例えば `Prod[κ, κ]` を `Prod[Any, K]` で満たす判定は、
  `K = Any` でない限り失敗する。

実装は worklist に元の consumer node を保持し、wildcard 判定にはその node を、
既束縛変数の一致判定には累積代入後の capability を使用する。この provenance と
共有 binding environment は product-slot の aggregate 検証、通常型内の nested
`Matcher`／`MatcherSlot`、multi-parameter の片方向照合でも維持する。

### 2.2 capability name／kind elaboration

surface annotation に現れる constructor capability は、型推論より前に凍結した
type-former signature と照合する。

- 未宣言の capability former は拒否する。
- former の宣言 arity と capability 引数の個数が一致しなければ拒否する。
- `declare cas-type` の transparent alias は capability head として拒否する。
- `List`／`Collection` や `Vector`／`Tensor` のように同じ canonical ID へ写る
  builtin と user inductive の衝突は、arity が同じでも拒否する。
- top-level signature だけでなく、局所型注釈、typed lambda、class／instance、
  pattern function、matcher clause 内の式も、surface annotation に書かれた
  capability former の名前と arity については再帰的に検査する。

この検査は `EnvBuilder.hs` にあり、失敗は `Capability kind error` として報告する。
なお、signature の永続化は現在 constructor environment から復元しているため、
constructor を一つも持たない inductive type を別 load unit から capability head
として参照する場合は、専用の type-former signature を EvalState に保持する追加実装が
必要である。また、nominal `declare cas-quotient` 名は alias environment にだけ保存
され、現在の former signature には登録されないため、capability head としては
unknown former になる。これは D5-CAS と合わせて解消すべき未実装境界である。

## 3. D1：evidence、observability、exact merge

### 3.1 evidence の情報源

ShapeCap の推論は、matcher clause が使用する **pattern constructor の署名**を
evidence とする。pattern-function 環境、結果型注釈、match-site からの需要、
再帰呼出しそのものを evidence にはしない。

内部では概念的に次の状態を区別する。

- `Unseen`：その位置を確定する genuine evidence がまだない。
- `Known κ`：capability が確定している。
- constructor／tuple evidence：子位置ごとの evidence を保持する。

constructor 頭または tuple 頭の clause は evidence を供給する。これは general
clause だけでなく refinement clause も合成対象であり、同じ exact merge 規則に
従う。一方、最上位の bare hole、wildcard、value pattern などは `Any` の evidence
ではなく `Unseen` である。

この区別により、結果注釈や再帰的な自己需要だけで構造 capability を捏造できない。

### 3.2 observability

型パラメータが pattern constructor の field 署名を通じて観測可能かを、
宣言された pattern constructor 群について最小不動点で計算する。

- observable な位置が最終的に `Unseen` のままなら型エラーにする。
- unobservable、すなわち phantom な位置だけは ground capability `Any` に確定してよい。
- function、effect、matcher、通常型 `TAny`、未宣言型、CAS view は観測の barrier とする。

例えば、`Maybe a` に `Nothing` 相当の、`a` を確定しない clause しかない場合でも、
別の pattern constructor の field から `a` が observable なら、その `a` を
`Any` で埋めず型エラーにする。

### 3.3 複数 clause の合成

同じ result slot に届く複数の evidence は exact merge する。

- `Unseen` と `Known κ` の合成結果は `Known κ`。
- 同一構造の evidence は子位置を再帰的に合成する。
- 異なる constructor head、arity、または異なる既知 capability は型エラーにする。
- 通常の型単一化のように一方を他方へ特殊化したり、上限を取ったりしない。

これにより、複数 general clauses の capability が異なる場合も、どちらか一方へ
暗黙に寄せず拒否する。D1 の実装は `Type/Capability.hs`、matcher clause からの
呼出しは `Type/Infer.hs` にある。

## 4. Coverage は ShapeCap から独立

primitive pattern の網羅性は、従来どおり
`--matcher-consistency-warnings` を指定したときの warning とする。Coverage と
ShapeCap は別の判断であり、次のように扱う。

- ある型の pattern constructor が一つでも genuine evidence として使われれば、
  Coverage が partial でも、その構造 capability を保持できる。
- constructor をすべて列挙していないことを理由に capability を `Any` にしない。
- Coverage warning の有無を ShapeCap evidence に加えない。
- catch-all clause の順序や data arm の整合性など、既存の hard error は warning
  へ弱めない。

例えば `Left` と `Right` を持つ型について `Left` の pattern clause だけを定義した
matcher は、Coverage warning の対象になり得る一方、`Left` から推論された構造
capability を持てる。この近似は意図した設計判断である。

## 5. D3：capability と通常型の代入・一般化

`Subst` は通常型変数用と capability 変数用の map を別々に持つ。

- 通常型代入 `TyVar ↦ Type` は target を特殊化するが、matcher の capability 添字を
  書き換えない。
- capability 代入 `CapVar ↦ Capability` は capability 位置へ明示的に適用する。
- free-variable 計算、scheme の量化、instantiate は二 sort を別々に処理する。
- 明示 scheme の capability binder は `CapSkolem`、通常型 binder は `TSkolem`
  として具体化し、どちらも rigid に検査する。通常型の flexible meta は
  `TSkolem` へ解けるが、`TSkolem` 自身を代入の定義域にはできない。
- fresh skolem は surface parser が生成できない `$skc`／`$skt` namespace と共有
  counter を使う。検査前から環境に自由な meta へ skolem が流出する場合は
  `AnnotationSkolemEscape` として拒否する。
- 検査に成功した typed tree、constraints、substitution は declared binder へ
  deskolemize してから annotation boundary の外へ返す。従って dictionary
  elaboration、後続の `IDefineMany` 要素、保存される scheme に skolem は残らない。
- pattern function の `DualScheme` を一般化するときは、周囲の環境に自由でない
  capability 変数の出現数を、全 argument/result dual の capability と target 内に
  入れ子になった matcher capability を含めて数える。ちょうど一回だけ現れる変数は
  相関を表さないため `Any` へ canonicalize し、二回以上現れる変数だけを量化して
  argument 間・argument/result 間の共有を保存する。ambient な変数は従来どおり自由な
  まま残す。
- pattern variable、wildcard、value/predicate pattern の局所推論では引き続き fresh
  capability variable を割り当てる。`Any` への canonicalization は完成した
  `DualScheme` の definition-side generalization 境界でだけ行い、推論途中の異なる
  leaf を同一視しない。

したがって、通常型の Algorithm W と同じ「制約を集め、代入を解き、環境に自由でない
変数を一般化する」という骨格を共有しつつ、両 sort の代入が混線しない。適用結果の
target 型へ通常型代入を反映しても、それだけで capability witness が生成されることは
ない。

実装箇所は `Type/Types.hs`、`Type/Subst.hs`、`Type/Env.hs`、および annotation
boundary を管理する `Type/Infer.hs` である。

この annotation 実装の範囲は、top-level definition（`IDefine`／
`IDefineMany`）と pattern-function declaration、およびそれらの本体に現れる
nested expression annotation である。次の局所 annotation は `Desugar.hs` で型情報を
消去しており、rigid binder を持つ局所 scheme checking としては未実装である。

- `TypedLambdaExpr`
- `TypedMemoizedLambdaExpr`
- `BindWithType`

また、pattern function の ordinary type variable は surface の `{...}` に明示された
ものだけを量化する。parameter／result annotation にだけ現れる未宣言 ordinary
variable を拒否する general well-scopedness check は未実装であり、inductive、
pattern-inductive、class signature にも同種の前段検査が必要である。
type class は formal TypePM core の対象外であり、constraint のみに現れる変数の閉包、
capability variable／skolem のみに依存する residual constraint の検査、
pattern function の constraint syntax も未実装境界である。

## 6. D5-core：構文的 `CapTargetOK`

`CapTargetOK` は、capability が target 型に対して構造的に妥当かを確認する
文脈相対の判定である。現在の core 判定は次の契約に固定する。

- actual matcher／slot から得た capability-target の仮定対は受理する。
- ground capability `Any` は任意の target に対して受理する。
- capability 変数または skolem は、それだけでは無条件に受理しない。
- tuple と constructor capability は、canonical な type former と arity が一致する
  ときだけ、子位置を再帰的に検査する。
- canonicalization は固定された構文的 synonym に限る。
  現在は `List → Collection`、`Vector/Matrix/DiffForm → Tensor` を扱う。

この判定は `groundEquiv`、CAS の同値・subtyping、type class、tensor の意味論的
正規化を capability equality として使用しない。実装箇所は
`Type/Types.hs` の type former 抽出と `Type/Capability.hs` の
`capTargetOK` である。

これは D5-core の実行時 checker fragment の実装であり、形式化で必要となる
import 時の署名環境の凍結や coupled substitution lemma の証明まで完了した、という
主張ではない。

## 7. 再帰値：A/R 用途制約と根の制限

再帰定義では、再帰変数へ fresh な結果用型変数 `ρᴿ` を与え、同じ変数を本体の
型と最後に単一化する。結果用型変数（R 変数）は `ResultOK`、すなわち関数の返り値
として妥当な型だけへ置換できる。仮引数用型変数（A 変数）は通常の `TypeOK` な型へ
置換できる。これにより `Slot` は関数の仮引数部分だけに現れ、再帰本体の結果には
現れない。

matcher literal の next-matcher 位置で未解決の R 変数が現れた場合、その変数は
`Matcher` へ精密化される。ここで得た capability は slot 適合性の検査には使うが、
現在構築中の結果形に対する独立した evidence には数えない。重要なのは、この判定が
式の名前や構文上の head ではなく、現在の型変数の A/R 用途だけに基づく点である。
したがって、次は同じ規則で処理される。

- `self` の直接利用。
- `let next := self in next` のような局所 alias。
- `identity self` のような通常の関数適用。

推論器は next-matcher 式の名前、構文上の head、局所 alias、通常の関数適用、
load unit をまたぐ値依存を capability evidence として追跡しない。再帰に関して残す
構文制限は、実際に循環する定義の根を lambda または matcher literal に限る規則だけ
である。再帰グループの循環検査はこの根制限のためだけに行い、matcher capability の
evidence や型変数の単一化には使わない。

## 8. D5-CAS：legacy compatibility boundary

現在の pattern type 環境は pattern 名から一つの `TypeScheme` への map であり、
target/view ごとの virtual pattern signature や、runtime extraction がその署名を
保存することを示す certificate を持たない。そのため、形式設計が要求する D5-CAS
はまだ実装できていない。

一方、標準ライブラリの `mathValue`、`factor`、`term` などは、nullary な
`MathValue`、`PolyExpr`、`TermExpr`、`SymbolExpr`、`IndexExpr` の下で宣言された
pattern constructor を view として使用する。この既存コードを動かすため、現在の
型推論器には限定的な legacy allowlist がある。

legacy root の検出は通常の evidence finalization より先に行われる。この branch は
対応する deferred structural hole を legacy 扱いにし、最終的な `CapTargetOK` 検査も
省略する。そのため受理結果には core の `CapTargetOK` certificate がない。legacy
判定は局所的な boolean であり、`TypeScheme`、typed expression、runtime state に
certificate として保存されない。したがって、

- legacy CAS の受理は ShapeCap evidence でも証明書でもない。
- legacy CAS を通過したプログラムを certified と呼ばない。
- `groundEquiv` を capability equality や D5-CAS の代用品にしない。
- alias や application をまたいで D5-CAS の保証が保存されるとは主張しない。

full D5-CAS には、少なくとも target-indexed virtual pattern signatures、
view-qualified constructor identity、kind-aware projection、および runtime extraction
preservation certificate が必要である。詳細は
[type-pm-mech3 の D5-CAS](../../type-pm-mech3/README.md)
に従う。

また、`casQuotientCast` は現在、公開型では任意の型間の cast、runtime では identity
として扱われる。これも D5-CAS certificate ではなく、現状で whole-program の
certified mode を主張できない理由の一つである。

## 9. certification claim の境界

現在、非 legacy の TypePM core 経路については、上記の D1、D3、A/R 用途制約による再帰値、
D5-core の契約を checker が検査する。ただし、この実装上の検査を形式証明済みの
certification と呼ばず、次の理由から Egison 全体の certification も主張しない。

core の等式制約については、公開の production entry point と同期済み core entry
point の受理・拒否・代入が一致することを回帰テストで固定する。これは保守的拡張の
実装契約を検査するものである。Haskell 実装全体と Lean 実装の翻訳・対応定理は
本プロジェクトの明示的な非目標とし、将来課題にも含めない。

- D5-CAS は legacy compatibility boundary に留まる。
- 回帰テストは同期済み Haskell entry point の実装契約を検査するが、Lean との
  対応証明または proof certificate ではない。
- 通常型 `TAny` の structured-duty 拒否は gradual typing の抜け道を一つ閉じるが、
  certified execution mode を与えるものではない。
- default の core library 読込みには legacy CAS 利用箇所が含まれる。
- default の permissive evaluator は型エラーを表示した後に untyped evaluation へ
  フォールバックできる。`--type-check-strict` は型エラー時に評価前で停止するが、
  matcher capability 専用の certificate や taint の追跡を持つ certified execution mode ではない。

したがって、通常の runtime 回帰テストが通ることと、TypePM の検査済み fragment に
属することは別の主張である。

## 10. コード対応表

| 契約 | 主な実装箇所 |
|---|---|
| 二 sort・二添字、type former | `hs-src/Language/Egison/Type/Types.hs` |
| surface type syntax | `hs-src/Language/Egison/AST.hs`, `hs-src/Language/Egison/Parser/NonS.hs` |
| capability 代入と通常型代入 | `hs-src/Language/Egison/Type/Subst.hs` |
| 二 sort の一般化・instantiate・skolemize | `hs-src/Language/Egison/Type/Env.hs` |
| core の等式、producer-stable capability matching、明示 slot 判定 | `hs-src/Language/Egison/Type/Unify.hs` |
| core と拡張規則の正の証拠に基づく dispatch | `hs-src/Language/Egison/Type/Infer.hs` |
| annotation skolem の no-escape／deskolemize | `hs-src/Language/Egison/Type/Infer.hs` |
| capability name／kind elaboration | `hs-src/Language/Egison/EnvBuilder.hs` |
| D1 evidence、observability、exact merge、D5-core | `hs-src/Language/Egison/Type/Capability.hs` |
| matcher 推論、A/R 用途制約による再帰 component、Coverage warning、通常型 `TAny` guard、legacy 境界 | `hs-src/Language/Egison/Type/Infer.hs` |
| legacy CAS を利用する標準 matcher | `lib/math/expression.egi` |

## 11. テスト入口

### 11.1 自動テスト

repository root で次を実行する。

```sh
cabal test test
```

`test/Test.hs` の `matcherCapabilityTests` は、通常型／capability の別代入、nested
capability 代入、producer-stable な片方向 capability matching、literal consumer
`Any` の wildcard、対称単一化での `Any` の rigidity、`Any` へ束縛された反復 consumer
変数の strict な再利用、shared-variable 拒否、evidence の exact identity、observability の最小不動点、
projection、文脈相対 `CapTargetOK`、malformed capability arity の内部拒否を検査する。
さらに pipeline 回帰は、strict mode の評価前停止と失敗した inference state の
非永続化を検査する。TypePM 選択テストには、
CAS bridge 名だけを inert stub で与えて選択した core 9 ライブラリと
`matcher-capability.egi` と `ar-recursive-matcher-strict.egi` を strict mode で読み込む回帰、
および過剰一般な通常型 annotation
を `TSkolem` 不一致、過剰一般な capability annotation を capability skolem
（診断上は `$skc`）との不一致として strict mode で拒否する回帰も含む。
`patternFunctionDualSchemeTests` は、一回だけ現れる非 ambient capability variable が
`Any` へ確定し、一回だけ現れる ambient variable は自由なまま残り、二回以上現れる
variable は量化された共有として残ることを検査する。
この選択ライブラリ回帰は `MathValue` stub、
type class、通常型 `TAny`、既存の CAS／Tensor 寛容単一化を含むため、formal non-CAS core を
隔離した試験ではない。

同じ test suite は `test/lib/**/*.egi` を自動発見するため、
`test/lib/core/matcher-capability.egi` も実行される。ここでは target 特殊化による
capability 非強化、関数結果での保存、list／nested capability、A/R 用途制約による再帰、
Coverage と独立な partial matcher、flexible capability projection、closed nested
constructor、通常関数適用または局所 alias を介した recursive self に加え、正しい
通常型 rigid annotation と type-class constraint の dictionary passing を受理側から
検査する。

通常の自動発見 language-level test は `defaultOption` で全 core libraries を
読み込むため legacy D5-CAS view 経路も含む。上記の strict 選択ライブラリ回帰は
その view library を読み込まないが、前述の stub と非 core 機能を含む。どちらも
proof-producing certificate を生成するわけではないため、test suite 全体を
certified-mode test とは呼ばない。

### 11.2 reject テスト

reject suite は `cabal test test` には含まれず、手動実行である。正本の実行方法は
`test/type-error/README.md` にある。`-t` は型エラー後も untyped evaluation へ
フォールバックして exit status 0 になり得るため、終了コードではなく出力中の
`Type error:` を検査する。

```sh
fail=0
for f in test/type-error/*.egi; do
  o=$(gtimeout -k 10 60 cabal run -v0 egison -- -t "$f" 2>&1)
  echo "$o" | grep -q "Type error:" || { echo "MISSING ERROR: $f"; fail=1; }
  echo "$o" | grep -q "Parse error" && { echo "PARSE ERROR: $f"; fail=1; }
done
[ $fail -eq 0 ] && echo "all rejected as expected"
```

matcher capability 固有の reject ケースは次のとおりである。

- `67-target-specialization-cons.egi`：target 特殊化で capability を強化しない。
- `68-unseen-observable-parameter.egi`：observable な `Unseen` を注釈で埋めない。
- `69-exact-clause-mismatch.egi`：clause evidence の不一致を拒否する。
- `72-capability-unknown-former.egi`：未宣言 capability former を拒否する。
- `73-capability-arity-mismatch.egi`：capability former の arity 不一致を拒否する。
- `74-capability-alias-head.egi`：transparent alias head を拒否する。
- `75-local-capability-unknown-former.egi`：局所注釈からの name check bypass を拒否する。
- `80-capability-builtin-collision.egi`：builtin と同じ canonical former を持つ
  user inductive を拒否する。
- `82-any-cannot-witness-capability.egi`：通常型 `TAny` で structured match-site
  capability を満たさない。
- `83-ordinary-annotation-rigidity.egi`：通常型 binder を本体から特殊化して
  `forall a. a -> a` を捏造する過剰一般 annotation を拒否する。
- `84-nested-annotation-rigidity.egi`：top-level の通常型 rigid binder を
  nested annotation でも共有する。
- `85-pattern-function-annotation-rigidity.egi`：pattern function の通常型
  binder を rigid に検査する。
- `86-pattern-function-nested-annotation-rigidity.egi`：pattern-function body の
  nested annotation でも同じ rigid binder を共有する。
- `87-capability-annotation-rigidity.egi`：明示 matcher scheme の capability
  binder を `CapSkolem` として rigid に検査する。

D5-CAS の certified-mode test はまだ存在しない。D5-CAS の metadata、certificate、
checker integration とともに追加する必要がある。

## 12. 残作業

未実装または formal core の外に残す項目は次である。

1. **former signature の永続化**：constructor の有無に依存しない専用 metadata を
   `EvalState` に保持し、load unit をまたぐ空 inductive と nominal quotient の扱いを
   明示する。
2. **D5-CAS**：target-indexed virtual pattern signatures と preservation
   certificate を導入し、legacy allowlist を certified 経路で置き換える。
3. **局所 annotation semantics**：`TypedLambdaExpr`、
   `TypedMemoizedLambdaExpr`、`BindWithType` を型付き IR に保持し、局所 scheme
   checking、rigid binder、no-escape を実装する。
4. **declaration well-scopedness**：pattern function、inductive、
   pattern-inductive、class signature の未宣言 ordinary type variable を拒否し、
   constraint-only variable も含め scheme closure を検査する。type class を core
   証明に含めない間も、この実装境界を明示する。
5. **Coverage／module integration**：ordinary warning と covered/strict mode、
   module validation、raw declaration validator、標準ライブラリ全体の移行を行う。

その後に、選択した strict fragment だけを受理・実行し、legacy bypass、
`casQuotientCast`、型エラー後の untyped fallback を明示的に排除または追跡する
whole-program mode を設計できる。この mode も Haskell–Lean 対応定理や形式的な
certification を主張しない。
