# P2 matcher capability 実装契約

> **状態（2026-07-31）**
> 本文書は、Egison 本体に実装した P2 の契約と、その保証範囲を記録する。
> 形式設計の正本は
> [type-pm-mech の P2 設計文書](../../type-pm-mech/problem/matcher-capability-instantiation.md)
> であり、課題全体の索引は
> [type-pm-mech の problem 一覧](../../type-pm-mech/problem/README.md)
> を参照すること。`design/matcher-slot.md` は P2 以前の実装史であり、現行契約の
> 正本ではない。

## 1. 実装範囲の要約

| 項目 | 現在の状態 | 保証範囲 |
|---|---|---|
| 二 sort・二添字 | 実装済み | `Matcher κ τ` と `MatcherSlot κ τ` を使用し、capability と通常型を別 sort として扱う |
| D1 ShapeCap | 実装済み | pattern constructor の署名から evidence と observability を計算し、節間を exact merge する |
| Coverage | D1 から独立して実装済み | primitive pattern の網羅性は opt-in warning のままとし、ShapeCap を弱めない |
| D3 代入・一般化 | 実装済み | capability 変数と通常型変数を別々に代入、一般化、具体化する |
| D4 再帰 flow | 一部実装 | scoped な name-level dependency と singleton direct-self を扱い、未解決／循環 summary を load unit 間で保持するが、一般の producer/path 方程式は未統合 |
| D5-core `CapTargetOK` | 実装済み | 固定された構文的 type former と文脈仮定だけで検査する |
| D5-CAS | 未実装 | 標準ライブラリを動かすための legacy compatibility boundary のみ存在する |

したがって、現在の実装は **非 legacy 経路に P2-core の検査済み fragment を持つ**
が、形式証明済みまたは certified な fragment を持つという主張ではない。型検査器
または標準ライブラリ全体も certified とみなせる状態ではない。
特に full D4 と D5-CAS を実装済みとは扱わない。

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
  `Matcher none a` の `a` が `[Integer]` に特殊化されても、結果は
  `Matcher none [Integer]` であり、`Matcher [none] [Integer]` にはならない。
- match site では、通常値の gradual escape hatch である `Any` を structured
  capability の witness として使用しない。constructor capability、rigid skolem、
  またはそれらを含む tuple が要求される場合は型エラーとし、`none` と flexible
  capability variable の unconstraining な要求だけを許す。
- match-site pattern から capability skeleton を導出できない場合も、fresh な
  unconstraining slot へ弱めず型エラーにする。

実装箇所は `Type/Types.hs`、surface AST は `AST.hs`、構文解析は
`Parser/NonS.hs` にある。

### 2.1 capability name／kind elaboration

surface annotation に現れる constructor capability は、型推論より前に凍結した
type-former signature と照合する。

- 未宣言の capability former は拒否する。
- former の宣言 arity と capability 引数の個数が一致しなければ拒否する。
- `declare cas-type` の transparent alias は capability head として拒否する。
- `List`／`Collection` や `Vector`／`Tensor` のように同じ canonical ID へ写る
  builtin と user inductive の衝突は、arity が同じでも拒否する。
- top-level signature だけでなく、局所型注釈、typed lambda、class／instance、
  pattern function、matcher clause 内の式を再帰的に検査する。

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
従う。一方、最上位の bare hole、wildcard、value pattern などは `none` の evidence
ではなく `Unseen` である。

この区別により、結果注釈や再帰的な自己需要だけで構造 capability を捏造できない。

### 3.2 observability

型パラメータが pattern constructor の field 署名を通じて観測可能かを、
宣言された pattern constructor 群について最小不動点で計算する。

- observable な位置が最終的に `Unseen` のままなら型エラーにする。
- unobservable、すなわち phantom な位置だけは `none` に確定してよい。
- function、effect、matcher、`Any`、未宣言型、CAS view は観測の barrier とする。

例えば、`Maybe a` に `Nothing` 相当の、`a` を確定しない clause しかない場合でも、
別の pattern constructor の field から `a` が observable なら、その `a` を
`none` で埋めず型エラーにする。

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
- constructor をすべて列挙していないことを理由に capability を `none` にしない。
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
- 明示的な matcher 型注釈の capability は rigid な capability skolem として扱う。

したがって、通常型の Algorithm W と同じ「制約を集め、代入を解き、環境に自由でない
変数を一般化する」という骨格を共有しつつ、両 sort の代入が混線しない。適用結果の
target 型へ通常型代入を反映しても、それだけで capability witness が生成されることは
ない。

実装箇所は `Type/Subst.hs` と `Type/Env.hs` である。

## 6. D5-core：構文的 `CapTargetOK`

`CapTargetOK` は、capability が target 型に対して構造的に妥当かを確認する
文脈相対の判定である。現在の core 判定は次の契約に固定する。

- actual matcher／slot から得た capability-target の仮定対は受理する。
- `none` は任意の target に対して受理する。
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

## 7. D4：現在扱える再帰 flow

### 7.1 型推論器に統合済みの範囲

現在の `Type/Infer.hs` は producer を次の三つに分類する。

1. 再帰参照を含まない producer。
2. singleton direct-self producer。
3. それ以外の、追加の flow 方程式を必要とする producer。

singleton direct-self は、next matcher の head を構文的に追った結果が同じ binder
になる範囲である。bare variable に限らず、その binder を head とする application
や reshape を含む。ただし direct-self edge 自体は evidence でも
`CapTargetOK` の仮定でもない。非再帰 clause などから得た genuine evidence が別に
必要であり、自己再帰や結果注釈だけで `Unseen` を確定できない。

この direct-self fragment は P2-core の検査済み fragment に含める。transform、
alias、相互再帰、higher-order 経由など、producer/path 方程式が必要な非 legacy
flow は現在 fail-closed で拒否する。

producer provenance は、単一の matcher literal 内だけでなく、局所 alias／closure、
同じ load unit の先行 top-level 定義、および以前の load unit から引き継いだ
top-level summary を通して追跡する。

- load unit 全体の定義名を先に集め、未確定の forward producer を通常の外部 producer
  と区別する。
- 完了済み top-level alias／closure の依存関係を推移的に要約し、alias-only の閉じた
  SCC は、全要素が構文上は完了した後も `Known` evidence にしない。
- 非空の unresolved／cyclic summary は `EvalState` に保存し、次の load unit へ
  引き継ぐ。現在の load unit による再定義は以前の summary を shadow する。
- 型検査に成功した matcher literal は producer boundary とする。その clause body の
  任意の自由変数を matcher value の flow と混同しない。
- 局所環境と speculative constraint state は成功時・失敗時の両方で復元し、top-level
  inference state はその項目全体が成功した場合だけ永続化する。

これらは、未統合の一般 solver を使わずに既知の alias／closure 隠蔽と load-unit
境界のすり抜けを fail-closed にするための name-level provenance 近似である。
summary 自体は capability evidence や certificate ではない。summary がない外部
producer はこの近似の下では `Known` と扱われ、保持範囲も同じ `EvalState` を使う
evaluator session 内に限られる。`TypeScheme`、typed AST、serialized module artifact、
process restart には保存されない。相互再帰を一般に解けるようにするものでもない。

### 7.2 純粋 `ShapeSolver` の位置づけ

`Type/ShapeSolver.hs` には、producer/node 方程式、参照、constructor projection、
SCC、exact merge、expansive cycle の拒否を扱う純粋 solver がある。また、この solver
単体の回帰テストも存在する。

しかし、現在この module は `Type/Infer.hs` から呼ばれていない。したがって、
solver の存在をもって相互再帰や一般の producer/path flow が言語実装へ統合済みとは
みなさない。full D4 の残作業は、型推論時に producer ID と path 方程式を収集し、
この solver の解を D1 finalization と `CapTargetOK` へ接続することである。
現在の name set だけでは projection／constructor edge を表現できないため、solver
なら受理できる可能性がある transform、mutual flow、higher-order flow も保守的に
拒否する。

## 8. D5-CAS：legacy compatibility boundary

現在の pattern type 環境は pattern 名から一つの `TypeScheme` への map であり、
target/view ごとの virtual pattern signature や、runtime extraction がその署名を
保存することを示す certificate を持たない。そのため、形式設計が要求する D5-CAS
はまだ実装できていない。

一方、標準ライブラリの `mathValue`、`factor`、`term` などは、nullary な
`MathValue`、`PolyExpr`、`TermExpr`、`SymbolExpr`、`IndexExpr` の下で宣言された
pattern constructor を view として使用する。この既存コードを動かすため、現在の
型推論器には限定的な legacy allowlist がある。

legacy root の検出は unsupported-flow rejection と通常の evidence finalization
より先に行われる。この branch は対応する deferred structural hole を legacy 扱いに
し、最終的な `CapTargetOK` 検査も省略する。そのため、対象となる matcher literal
または nested leaf は従来の recursive／alias flow を保持し得る。legacy 判定は局所的な
boolean であり、`TypeScheme`、typed expression、runtime state に certificate として
保存されない。producer summary の永続化や matcher literal を summary boundary とする
処理も、この経路を D4／D5 certificate に変えない。したがって、

- legacy CAS の受理は ShapeCap evidence でも証明書でもない。
- legacy CAS を通過したプログラムを certified と呼ばない。
- `groundEquiv` を capability equality や D5-CAS の代用品にしない。
- alias や application をまたいで D5-CAS の保証が保存されるとは主張しない。

full D5-CAS には、少なくとも target-indexed virtual pattern signatures、
view-qualified constructor identity、kind-aware projection、および runtime extraction
preservation certificate が必要である。詳細は
[P2 設計文書の D5-CAS](../../type-pm-mech/problem/matcher-capability-instantiation.md)
に従う。

また、`casQuotientCast` は現在、公開型では任意の型間の cast、runtime では identity
として扱われる。これも D5-CAS certificate ではなく、現状で whole-program の
certified mode を主張できない理由の一つである。

## 9. certification claim の境界

現在、非 legacy の P2-core 経路については、上記の D1、D3、direct-self D4 fragment、
D5-core の契約を checker が検査する。ただし、この実装上の検査を形式証明済みの
certification と呼ばず、次の理由から Egison 全体の certification も主張しない。

- full D4 は型推論器へ未統合である。
- D5-CAS は legacy compatibility boundary に留まる。
- 永続化する producer summary は name-level の fail-closed metadata であり、
  capability evidence や形式的 certificate ではない。また legacy bypass を修復しない。
- `Any` の structured-duty 拒否は gradual typing の抜け道を一つ閉じるが、
  certified execution mode を与えるものではない。
- default の core library 読込みには legacy CAS 利用箇所が含まれる。
- default の permissive evaluator は型エラーを表示した後に untyped evaluation へ
  フォールバックできる。`--type-check-strict` は型エラー時に評価前で停止するが、
  P2 専用の certificate や taint の追跡を持つ certified execution mode ではない。

したがって、通常の runtime 回帰テストが通ることと、P2 の検査済み fragment に
属することは別の主張である。

## 10. コード対応表

| 契約 | 主な実装箇所 |
|---|---|
| 二 sort・二添字、type former | `hs-src/Language/Egison/Type/Types.hs` |
| surface type syntax | `hs-src/Language/Egison/AST.hs`, `hs-src/Language/Egison/Parser/NonS.hs` |
| capability 代入と通常型代入 | `hs-src/Language/Egison/Type/Subst.hs` |
| 二 sort の一般化・instantiate・skolemize | `hs-src/Language/Egison/Type/Env.hs` |
| capability name／kind elaboration | `hs-src/Language/Egison/EnvBuilder.hs` |
| D1 evidence、observability、exact merge、D5-core | `hs-src/Language/Egison/Type/Capability.hs` |
| matcher 推論、Coverage warning、scoped provenance、direct-self、`Any` guard、legacy 境界 | `hs-src/Language/Egison/Type/Infer.hs` |
| load-unit 間 producer summary、成功時だけの state 永続化 | `hs-src/Language/Egison/Type/Infer.hs` (`batchForwardProducerDependencies`), `hs-src/Language/Egison/Eval.hs`, `hs-src/Language/Egison/EvalState.hs` |
| 未統合の D4 純粋 solver | `hs-src/Language/Egison/Type/ShapeSolver.hs` |
| legacy CAS を利用する標準 matcher | `lib/math/expression.egi` |

## 11. テスト入口

### 11.1 自動テスト

repository root で次を実行する。

```sh
cabal test test
```

`test/Test.hs` の `p2CapabilityTests` は、通常型／capability の別代入、nested
capability 代入、evidence の exact identity、observability の最小不動点、
projection、文脈相対 `CapTargetOK`、純粋 `ShapeSolver` の SCC・exact mismatch・
expansive cycle、malformed capability arity の内部拒否を検査する。さらに pipeline
回帰は、strict mode の評価前停止、失敗した inference state の非永続化、および
load unit をまたぐ producer SCC summary の保持を検査する。

同じ test suite は `test/lib/**/*.egi` を自動発見するため、
`test/lib/core/p2-capability.egi` も実行される。ここでは target 特殊化による
capability 非強化、関数結果での保存、list／nested capability、direct-self 再帰、
Coverage と独立な partial matcher、flexible capability projection、closed nested
constructor を受理側から検査する。

ただし language-level test は `defaultOption` で全 core libraries を読み込むため、
legacy CAS boundary も含む。従って、この test suite 全体を certified-mode test
とは呼ばない。

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

P2 固有の reject ケースは次のとおりである。

- `67-p2-target-specialization-cons.egi`：target 特殊化で capability を強化しない。
- `68-p2-unseen-observable-parameter.egi`：observable な `Unseen` を注釈で埋めない。
- `69-p2-exact-clause-mismatch.egi`：clause evidence の不一致を拒否する。
- `70-p2-recursive-annotation-is-not-evidence.egi`：自己需要と結果注釈を evidence にしない。
- `71-p2-recursive-transform-requires-flow.egi`：未統合の再帰 flow を fail-closed にする。
- `72-p2-capability-unknown-former.egi`：未宣言 capability former を拒否する。
- `73-p2-capability-arity-mismatch.egi`：capability former の arity 不一致を拒否する。
- `74-p2-capability-alias-head.egi`：transparent alias head を拒否する。
- `75-p2-local-capability-unknown-former.egi`：局所注釈からの name check bypass を拒否する。
- `76-p2-recursive-alias-hiding.egi`：local alias に隠れた recursive producer を拒否する。
- `77-p2-forward-producer-is-unseen.egi`：未確定の forward producer を evidence にしない。
- `78-p2-forward-alias-cycle.egi`：top-level alias に隠れた producer cycle を拒否する。
- `79-p2-recursive-closure-hiding.egi`：closure に capture された recursive producer を
  evidence にしない。
- `80-p2-capability-builtin-collision.egi`：builtin と同じ canonical former を持つ
  user inductive を拒否する。
- `81-p2-completed-alias-cycle.egi`：完了済み alias-only SCC を evidence にしない。
- `82-p2-any-cannot-witness-capability.egi`：`Any` で structured match-site
  capability を満たさない。

D5-CAS の certified-mode test はまだ存在しない。D5-CAS の metadata、certificate、
checker integration とともに追加する必要がある。

## 12. 残作業

優先度の高い未完了項目は次の三つである。

1. **former signature の永続化**：constructor の有無に依存しない専用 metadata を
   `EvalState` に保持し、load unit をまたぐ空 inductive と nominal quotient の扱いを
   明示する。
2. **full D4 integration**：producer/path 方程式を `Type/Infer.hs` で生成し、
   name-level summary と matcher-literal boundary を producer ID／path equation へ
   置き換え、`Type/ShapeSolver.hs` の解を evidence finalization と `CapTargetOK` へ
   接続する。evaluator session 外へ依存情報を持ち出す場合は typed／module artifact
   上の永続表現も定義する。
3. **D5-CAS**：target-indexed virtual pattern signatures と preservation
   certificate を導入し、legacy allowlist を certified 経路で置き換える。

その後に、certification 条件を満たす fragment だけを受理・実行し、legacy bypass、
`casQuotientCast`、型エラー後の untyped fallback を明示的に排除または taint として
追跡する whole-program mode を設計する。
