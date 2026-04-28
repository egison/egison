# Egison CAS 型システム 実装状況と発見された詰まり

このドキュメントは [type-cas.md](./type-cas.md) の設計に対する**実装時点での状態**と、**実装中に実際に発見された詰まり・妥協点**をまとめる。設計時点で予測された課題は [type-cas-issues.md](./type-cas-issues.md) に書かれているのに対し、こちらは「実装してみて分かったこと」を記録する。

最終更新: 2026-04-28

---

## 目次

1. [現状サマリー (実装済みフェーズ)](#1-現状サマリー-実装済みフェーズ)
2. [真の技術的詰まり](#2-真の技術的詰まり)
   - [2.1 Multi-param 型クラス dispatch](#21-multi-param-型クラス-dispatch)
   - [2.2 `partialDiff` を `chainPartialDiff` に委譲する統合](#22-partialdiff-を-chainpartialdiff-に委譲する統合)
   - [2.3 `casNormalize` への auto-rule 自動適用](#23-casnormalize-への-auto-rule-自動適用)
3. [破壊的変更を要する保留項目](#3-破壊的変更を要する保留項目)
4. [設計と実装の乖離 (妥協点)](#4-設計と実装の乖離-妥協点)
5. [細かい摩擦点](#5-細かい摩擦点)
6. [詰まりではないが時間が必要な項目](#6-詰まりではないが時間が必要な項目)
7. [詰まり項目を解く道筋](#7-詰まり項目を解く道筋)

---

## 1. 現状サマリー (実装済みフェーズ)

| Phase | 内容 | 状態 | 関連 mini-test |
|---|---|---|---|
| 1 | `CASValue` 基盤 | ✅ 完了 (既存) | - |
| 1.5 | `SymbolSet` の `TypeAtom` 化 | ✅ 完了 (`Apply1〜4 → ApplyN` は実装しない方針) | 67, 69, 80 |
| 2 | 型システムへの統合 | ✅ 完了 | 75 |
| 3 | `ScalarData → CASValue` 置換 | ✅ 完了 (既存) | - |
| 4 | プリミティブパターン | ✅ 完了 (既存) | - |
| 5 | パラメトリックマッチャー | ✅ 完了 | 60, 62, 63 |
| 5.5 | Embed/Coerce | ✅ Multi-param dispatch で完成 (`Coerce MathValue (Frac Integer)` 等が結果型ディスパッチで動く) | 73, 75, 77, 78, 81, 104 |
| 6.1-6.2 | `expandAll`/`substitute` | ✅ 既存実装で動作 | 65 |
| 6.3 | `∂/∂` (Differentiable) | ✅ 完了 (`partialDiff` が `chainPartialDiff` 経由で declared derivative を自動 dispatch) | 74, 76, 91, 93, 94, 103 |
| 6.3 part 5 | `declare mathfunc` | ✅ 完了 | 90, 91 |
| 6.3 part 6 | `chainPartialDiff` ディスパッチャ | ✅ 完了 | 93, 94 |
| 6.5 | `Sd` (積分) | ❌ pre-existing バグ ([5.7](#57-sd-integration-の-lib-バグ) 参照) | - |
| 7.4 | `declare rule` 構文 | ✅ 完了 | 70, 83 |
| 7.5 | rule 適用エンジン (literal LHS) | ✅ 完了 (`applyTermRule`、factor containment 付き) | 87, 89 |
| 7.5 | rule 適用エンジン (pattern LHS、`$x`/`#x`) | ✅ 完了 (Phase A) | 99, 100 |
| 7.5+ | pattern LHS の sub-expression 再帰 (Phase A.5) | ✅ 完了 (`mapPolyAll`/`mapTermAll`/`mapFracAll`) | 101, 102 |
| 7.5 | rule combinator (`applyRules` / `iterateRules`) | ✅ 完了 | 89 |
| 7.5 | **auto-rule 自動適用 (mathNormalize 再生成)** | ✅ 完了 | 95, 96 |
| 7.5+ | **term-level recursive + factor-containment 適用** | ✅ 完了 (built-in `casRewriteI` 等が無くても算術恒等式が動く) | 97, 98 |
| 7.6 | `simplify ... using ...` | ✅ literal LHS で完了 | 85 |
| 8 | 観察型 `typeOf` / `inspect` / `differentialClosed` | ✅ 完了 (REPL 統合は別途) | 68, 71 |
| - | multi-param TC dispatch (Coerce 等) | ✅ 完了 ([2.1](#21-multi-param-型クラス-dispatch--完了-2026-04) 参照) | 72, 104 |

凡例: ✅ 設計通り完了 / 🟡 動作するが妥協あり / ❌ 未実装または既知のバグ

---

## 2. 真の技術的詰まり

実装時に「**設計通りに動かす**ためには大きな改修が必要」と判断した項目。

### 2.1 Multi-param 型クラス dispatch ✅ 完了 (2026-04)

**設計意図**: `class Embed a b where embed :: a -> b` や `class Coerce a b where coerce :: a -> b` のような複数型引数のクラスを定義し、結果型 `b` で適切な instance を選ぶ。

**実装** (2026-04):

1. **`Constraint` を `[Type]` に拡張** (`Type/Types.hs`):
   - `data Constraint = Constraint { constraintClass :: String, constraintTypes :: [Type] }`
   - `constraintType :: Constraint -> Type` を後方互換アクセサとして提供 (= `head . constraintTypes`)
   - 影響範囲: 9 ファイル、約 100 箇所のパターンマッチ・構築点を機械的に更新

2. **`findMatchingInstanceForTypes :: [Type] -> [InstanceInfo] -> Maybe InstanceInfo`** (`Type/Instance.hs`):
   - 各 instance の全 instTypes と target を pairwise に unifyStrict
   - 全 type が unify する instance のみ選ぶ

3. **EnvBuilder で全 class type vars を constraint に積む** (`EnvBuilder.hs:registerClassMethod`):
   - `class Coerce a b where coerce (x: a) : b` から `forall a b. Coerce a b => a -> b` を生成
   - 旧: 第一型のみ (`forall a. Coerce a => ...`)

4. **Desugar で multi-type instance に unique な dictionary 名を生成** (`Desugar.hs`):
   - `Coerce MathValue Integer` → `coerceMathValueInteger`
   - `Coerce MathValue (Frac Integer)` → `coerceMathValueFracInteger`
   - 旧: 第一型のみで命名 → 同一名で衝突していた

5. **TypeClassExpand のディスパッチを多型対応** (`Type/TypeClassExpand.hs`):
   - `resolveDictionaryForConstraint`、`resolveDictionaryArgWithDepth`、`tryResolveMethodCall` 等で **常に** `findMatchingInstanceForTypes` を使用 (single/multi 経路を統合)
   - 旧 `findMatchingInstanceForType` への呼び出しは Type/Infer.hs の Tensor 関連の局所コード以外撤去 (head-only 検索は multi-param で誤った instance を返す危険があるため)

**動作確認** (`mini-test/104-multiparam-coerce.egi`):
```egison
instance Coerce MathValue Integer where ...
instance Coerce MathValue (Frac Integer) where ...

def n : Integer       := coerce (1 + 2)        -- 3 (Integer instance)
def f : Frac Integer  := coerce (1 / 2)        -- 1/2 (Frac instance)
```

`81-coerce-class.egi` の per-target 関数による回避策はもう不要 (旧テストとして残置)。

### 2.2 `partialDiff` を `chainPartialDiff` に委譲する統合 ✅ 完了 (2026-04)

**設計意図**: `Differentiable` 型クラスの `partialDiff` インスタンスが、`declare derivative` で登録された関数を自動的に拾う。

**経緯**:
- 当初 `partialDiff f x := chainPartialDiff f x` に書くと runtime エラー `Expected CASData, but found: "partialDiff"` が発生していた
- 同根のバグ (`f 3 + f 4` の `"plus"` エラー) を調査して、`declare mathfunc` が型環境にシグネチャを登録していなかったことが原因と判明
- EnvBuilder で mathfunc に `MathValue -> MathValue` 型シグネチャを登録するよう修正
- これにより `partialDiff` instance も `chainPartialDiff` 委譲で正しく動作するようになった

**実装** (2026-04):
- `hs-src/Language/Egison/EnvBuilder.hs`: `DeclareMathFunc` を `processTopExpr` で型環境に登録 (`f : MathValue -> MathValue`)
- `lib/math/analysis/derivative.egi`: 全 `Differentiable` instance が `chainPartialDiff` 経由に
- 動作: `partialDiff (f x) x = 2 * x` (declared derivative `f' = 2*z` を自動 dispatch)
- テスト: `mini-test/103-partialdiff-auto-dispatch.egi`

### 2.3 `casNormalize` への auto-rule 自動適用 (✅ 完了 — 2026-04-28)

**設計意図**: `declare rule auto term i^2 = -1` で登録した規則が `casPlus`/`casMult` 等の演算過程で自動的に適用される (Maxima の `tellsimp` 相当)。

**当初の認識** (詰まり項目として):
- `casNormalize :: CASValue -> CASValue` を Haskell で `EvalM` 化する必要があり、大規模リファクタとなる
- Sweet Egison の `[mc| ... |]` は TH ベースで runtime コンパイルできない

**新方針 (2026-04-28)**: **`mathNormalize` 再生成による Egison-level 解決** ✅ 実装完了

詳細は § 7.3 (改訂版) を参照。要点:

- desugar が `declare rule auto` を見るたびに lib の `mathNormalize` を上書きする `def` を emit
- 上書き版は `iterateRules [autoRule.0, ..., autoRule.N] (mathNormalizeBuiltin v)` を返す
- 既存の `+`/`*` for MathValue は既に `mathNormalize` を経由する → user rule が自動発火
- `recursiveBindAll` (Eval.hs) が最新版 `mathNormalize` を全箇所で使えるようにする
- パターンマッチは Egison match engine で実行 → Sweet Egison の TH 制約を回避
- 動作確認: `mini-test/95-auto-rule-application.egi` で `p*p = -1`、`q*q*q = 1` が自動正規化されることを実証

**実装上のキーポイント (見つかった落とし穴)**:
- **落とし穴 1 (無限ループ)**: 当初 `unNormalizeOps` が `^` を `^'` (lib-level un-normalized power) に置換していたが、`^'` の内部は `take`/`foldl` を使い、これが `n - 1` で MathValue の `-` を呼び、結果として `mathNormalize` を呼ぶ無限ループに陥った
- 解決 (1): 演算子をプリミティブ (`i.+`, `i.-`, `i.*`, `i./`) に直接展開。`^` は `i.power` だと整数しか扱えないので、リテラル正整数の場合のみ反復した `i.*` に展開 (例: `p^3` → `i.* p (i.* p p)`)。symbolic CAS 値も扱える
- **落とし穴 2 (変数シャドウイング)**: rule lambda の引数名を `v` にしていたが、user が `declare rule auto term v^2 = 0` のように `v` という名前の symbol を使うと、LHS の `v` が lambda 引数を参照してしまい (= 入力値そのもの)、rule が決して match しなくなる
- 解決 (2): lambda 引数を `fresh` で生成した `__rule_input.<n>` 形式の名前にして user symbol との衝突を回避 (`mini-test/96-auto-rule-workflow.egi` で `u/v/w` 各 symbol について auto rule が動くことを実証)

**Phase 7.2 の意図的単純化**:
- `casNormalize :: CASValue -> CASValue` は **そのまま (純粋)**
- `Math/CAS.hs` への変更不要
- Sweet Egison でユーザー rule をコンパイルする必要なし
- パフォーマンスは「頻出 rule を Sweet Egison 実装に昇格」で対応 (二層アーキテクチャ)

**追加実装 (Phase B、2026-04-28)**: term-level recursive 適用 + factor-containment 判定

`applyTermRule` プリミティブを `Primitives.hs` に追加。`term` ルールはこのプリミティブを介して**各項のモノミアルに対して再帰的に**適用される:

```haskell
applyTermRule :: CASValue -> CASValue -> CASValue -> CASValue
applyTermRule lhsValue rhsValue input =
  -- For each term in `input`:
  --   If the term's monomial *contains* lhsValue's monomial as a factor,
  --   replace that factor with rhsValue (multiplying by coefficient and remainder)
  --   Otherwise, keep the term
```

特徴:
- **factor containment**: LHS `u^2` は `u^2`、`u^3`、`u^4`、`u^k` (任意の k≥2) すべての項にマッチ
- **iterateRules による反復**: `u^4` → `(u^2)^2` → `-u^2` → `1` のように固定小数点まで適用
- **multi-symbol monomials**: `u*v` のような複合モノミアルにも factor として認識

これにより、**built-in の `casRewriteI`/`casRewriteW` 等が無くても**、user 宣言の rule のみで `(1+u)*(1-u) = 2` などの代数的恒等式が機能する (`mini-test/97-rule-recursion.egi`, `mini-test/98-user-rules-no-builtin.egi`)。

**残課題**:
- LHS が constant-only (例: `term 7 = 42`) の場合、term-level recursive 適用ではマッチしない (poly-level rule を使う必要がある)
- pattern variable LHS が動作するのは **top-level match のみ**。`dbl 7 + 0` (top-level Apply1) はマッチするが、 `dbl 3 + dbl 4` のような sub-expression にはマッチしない (within-term/within-poly recursion は次の Phase で対応)

**設計書 (旧) (Phase 7.2) は次のような変更を要求していた**:

```haskell
-- 今:
casNormalize :: CASValue -> CASValue

-- 設計:
casNormalizeWithRules :: ReductionEnv -> CASValue -> EvalM CASValue
```

これに伴い、`casPlus`/`casMult`/`casDivide`、`Math/Rewrite.hs`、`Primitives/Arith.hs`、`Core.hs` の評価パスすべてが `EvalM` 経由になる。実装範囲が広い。

**回避策(現状)**: ユーザーが `applyRules [rule.iSq] expr` のように **named rule を手動で適用**する。Auto rule の自動適用は無い。

**真の修正**: § 7.3 を参照。

---

## 3. 破壊的変更を要する保留項目

### 3.1 Pattern variable を含む rule LHS ✅ Phase A + Phase A.5 実装済

**設計意図**:
```egison
declare rule trig_pythagorean poly (sin $x)^2 + (cos #x)^2 = 1
declare rule auto term (sqrt $y)^2 = y
```

`$x` (パターン変数バインディング) と `#x` (値参照) を LHS に書ける。

**実装** (Phase A, 2026-04):
- AST 変更: `DeclareRule (Maybe String) RuleLevel Pattern Expr` (旧: `Expr Expr`)
- 専用 LHS parser `ruleLhsPattern` (`hs-src/Language/Egison/Parser/NonS.hs`):
  - **auto-quoting**: bare lowercase ident と integer literal は `ValuePat` に自動変換 — `i^2 = -1` のような既存構文がそのまま動く
  - `$x` → `PatVar`, `#expr` → `ValuePat`
  - `apply1`/`apply2`/`term`/`frac`/`poly`/`plus`/`mult`/`symbol`/`quote`/`func`/`sub`/`sup`/`user` (mathExpr matcher constructors) は `InductivePat` として扱う
  - juxtaposition `f $x` は `PApplyPat` (関数呼び出しパターン)
- desugar 分岐 (`hs-src/Language/Egison/Desugar.hs`):
  - `patternHasPatVar`: PatVar の有無で path を選択
  - **literal path** (PatVar なし): `patternToLiteralExpr` で Pattern → Expr 変換し、既存の `applyTermRule` ルートを使用
  - **pattern path** (PatVar あり): `translateToMatcherPattern` で `f $x` → `apply1 #f $x` に変換し、`\v -> match v as <matcher> with | <pat> -> <rhs> | _ -> v` を生成。
- 動作例 (`mini-test/99-rule-pattern-vars.egi`, `mini-test/100-auto-rule-patvars.egi`):
  - `declare rule extractSin poly sin $x = x`
  - `declare rule sameArgs poly sin (cos $a) + cos (sin #a) = a`
  - `declare rule auto poly dbl $x = x + x` (auto rule with PatVar)

**実装** (Phase A.5: sub-expression recursion, 2026-04):
- Haskell プリミティブ追加 (`hs-src/Language/Egison/Primitives.hs`):
  - `mapPolyAll : (MathValue -> MathValue) -> MathValue -> MathValue`
  - `mapTermAll : (MathValue -> MathValue) -> MathValue -> MathValue`
  - `mapFracAll : (MathValue -> MathValue) -> MathValue -> MathValue`
  - 各プリミティブは MathValue ツリーをボトムアップで走査し、`Apply1-4`/`Quote`/`FunctionData` の引数 (内側 MathValue) に再帰下降。各ノードで rule を fixpoint まで適用 (`applyRuleFix`)。
  - `descendCASNoCoef`: 構造再帰のヘルパー。term の coefficient slot は内部表現なので走査対象外 (= rule は適用しない)。Apply args/Quote/Function args/Symbol indices などユーザー可視の MathValue 子要素のみ再帰。
  - 命名注: lib に既存の `mapPoly`/`mapTerm`/`mapFrac` (frac の n/d split 系) と区別するため、新プリミティブは `*All` サフィックス付き。
- desugar の pattern-LHS path 更新: 生成 lambda が `\v -> mapXAll <oneStep> v` の形になり、サブ式まで rule が浸透。
- `-` 演算子を `reservedPatternOp` に追加 (parser 受理のみ、matcher の `$ - $` clause は将来対応)。
- 動作例 (`mini-test/101-pattern-rule-subexpr.egi`):
  - `dbl (dbl 3) → 12` (cascading reduction)
  - `f (dbl 3) → f 6` (recursion into Apply1 arg)
  - `x + f (dbl 5) → x + f 10`

**残課題**:
- `(sqrt $a)^2 = a` のような LHS は ✅ 動作する (mathValue matcher に `$ ^ $` clause を追加済)
- `1-u` 等の `-` パターン: parser は受理するが、対応する `$ - $` matcher clause が無いため runtime で fail する。 必要なら lib 側で追加可能。
- 名前衝突回避のため新プリミティブが `mapTermAll` 等になった (lib 既存の `mapTerm` は frac の numerator/denominator を分けて map する別関数)。将来的に lib の同名関数を整理して `mapTerm`/`mapPoly`/`mapFrac` に戻すことも検討可能。

### 3.2 `Apply1〜Apply4` → `ApplyN` (実装しない方針)

**設計意図**: `SymbolExpr` の `Apply1 a` ... `Apply4 a b c d` を `ApplyN MathFuncRef [CASValue]` に一般化。任意 arity の math function を表現可能に。

**判断 (2026-04)**: **実装しない**。現状の Apply1〜4 で実用的な arity (4変数まで) はカバーでき、5変数以上の math function は実用上ほぼ無い。一般化のための広範囲リファクタ (Layer 1/2 のパターン、各 Apply ケース、`Differentiable Factor` dispatch、`extractSymbolExpr` 等のヘルパー) のコストに対するベネフィットが小さい。

---

## 4. 設計と実装の乖離 (妥協点)

動作はするが、設計書通りの実装ではない箇所。

### 4.1 CAS subtype unification による Embed elaboration の代替

**設計**: `Embed` 型クラスを base にして、型不一致時に AST に `embed expr` 呼び出しを elaboration で挿入する。

**実装**: unifier (`Type/Unify.hs`) に `MathValue ↔ {Factor, Frac _, Poly _ _}` の双方向ユニフィケーション規則を追加した。型エラーは消えるが、AST に `embed` 呼び出しは現れない。

**問題ではない理由**: CAS 型は runtime 表現が同じ `CASValue` なので、明示的な `embed` を挿入しても runtime 動作に差は出ない。Embed 型クラス自体は lib に存在し、ユーザーが手動で `embed x` と書ける。

**設計通りの実装が必要になるケース**:
- 非 CAS 型同士の `embed` (例: `Maybe Int` への wrap)
- elaboration が必要な意味論を持つ Embed (現状は identity-only body)

### 4.2 `chainPartialDiff` の前方参照 ✅ 修正済 (2026-04)

**経緯**: かつて各 `declare derivative <name>` の desugar が、EnvBuilder が事前収集した「全 derivative names」を参照して `chainPartialDiff` を再定義していたため、最初の宣言時点でまだ未定義の `deriv.<later>` を参照し warning が出ていた。

**修正**: `derivativesDesugared` フィールドを `EvalState` に追加し、desugar が一つ宣言を処理するたびに append。`chainPartialDiff` の再定義は「現在まで desugared 済み」の名前のみ参照するようになった。

実装: `hs-src/Language/Egison/EvalState.hs`, `hs-src/Language/Egison/Desugar.hs`。動作上の semantics は変わらず (最終的な `chainPartialDiff` は最後に上書きされるので全 derivative を含む)、warning だけが消える。

### 4.3 名前付き rule のみが auto-apply 可能

**設計**: `declare rule auto ...` は `casNormalize` で自動適用される。

**実装**: auto rules は `autoRule.<freshN>` というユニークな名前で desugar されるが、ユーザーは fresh-id を予測できないので呼べない。実用上は **named rule** (`declare rule <name> ...`) を使う運用。

---

## 5. 細かい摩擦点

実装中に遭遇した、細かいが時間を消費した問題群。

### 5.1 Egison 識別子に `_` が使えない

`identChar = alphaNumChar` のため、設計の `trig_pythagorean` のような名前は parse できない。`trigPythagorean` (camelCase) を推奨。

**影響**: design 内の rule 名を実装時に書き換える必要があった。`design/type-cas.md` Step 7.4 に注記済。

### 5.2 `$y` の expression context での silent parse

`def y := $a` は parse error にならず、何か奇妙な解釈をする。デバッグで時間を消費した。pattern variable のような identifier-postfix を expression として書いた場合の挙動が直感的でない。

### 5.3 関数値の比較メカニズム

`apply1 #sin $g` パターンが動くのは、CAS 内部の `extractFunctionObjectCAS` が `QuoteFunction` ラッパーを unwrap してくれるから。仕組みを理解するのに時間がかかった。

```haskell
extractFunctionObjectCAS :: CASValue -> WHNFData
extractFunctionObjectCAS (CAS.CASFactor (CAS.QuoteFunction funcWHNF)) = funcWHNF
extractFunctionObjectCAS (CAS.CASPoly [CAS.CASTerm (CAS.CASInteger 1) [(CAS.QuoteFunction funcWHNF, 1)]]) = funcWHNF
extractFunctionObjectCAS cv = Value (CASData cv)
```

### 5.4 `∂/∂'` のループ on unknown apply1

`∂/∂'` の hardcoded ケース (`apply1 #sin`/#cos/#log/...) のいずれにもマッチしない user mathfunc apply1 は、後続の `term`/`poly` ケースに落ちて **無限ループ**する。

具体的には `(g x) = CASPoly [CASTerm 1 [(Apply1 g_val x_val, 1)]]` が `#1 * $fx ^ $n` パターンに 1 度マッチし、再帰呼び出しで同じ値に戻り、無限ループ。

**回避**: `chainPartialDiff` の再帰アームでは `partialDiff` ではなく `chainPartialDiff` を呼ぶことで、user mathfunc がディスパッチで吸収される。`mini-test/93-chain-partial-diff.egi` のネスト合成テスト (`f (g x)`) で実証。

### 5.5 EnvBuilder と Desugar の処理順

EnvBuilder は **すべての TopExpr** を先に処理して `EnvBuildResult` (= `EvalState`) を埋める。その後 Desugar が **expr 単位で** 走る。

`getDerivativeRuleNames` は desugar 時には **すべての declare derivative** が見える状態。各 declare の desugar が「自分まで」のリストを構築するには、別途 counter で管理する必要がある(現状は「すべて」を毎回出す方針で、recursiveBindAll の再帰束縛に頼っている)。

### 5.6 Cabal キャッシュ

度々 `cabal clean` してフルリビルドしないと変更が反映されないことがあった。`.dyn_o` のタイムスタンプを確認すると `cabal build` が "Up to date" と判断するが、コードは更新されていなかった。Haskell コードの depends 解決のなにか。

**緩和策**: 大きな変更の後は `cabal clean` を念のため実行する。

### 5.7 Sd integration の lib バグ

`lib/math/analysis/integral.egi` の `Sd` 関数内のパターン `~f ~y` が type error を起こす:

```
Cannot unify types:
  Expected: MathValue -> MathValue (TFun TMathValue TMathValue)
  Actual:   MathValue (TMathValue)
```

`~f` を関数値として使う想定だが、`~` は VarPat (値参照) でしかなく、関数として使うには別途 lookup が必要。**これは pre-existing で本セッションの変更とは無関係**。

### 5.8 LHS 中の `=` 演算子

`declare rule auto term i^2 = -1` の LHS と RHS の境界 `=` は `expr` parser では一般的な比較演算子としても認識される。LHS と RHS を分けるため、parser はまず `exprWithoutWhere` で `i^2 = -1` 全体を `InfixExpr "=" (i^2) (-1)` として parse し、後から `extractRuleSides` で分解する。

```haskell
extractRuleSides :: Expr -> Maybe (Expr, Expr)
extractRuleSides (InfixExpr op lhs rhs) | repr op == "=" = Just (lhs, rhs)
extractRuleSides _ = Nothing
```

Workaround として動作するが直感的でない。

---

## 6. 詰まりではないが時間が必要な項目

詰まりは無いが、未着手または部分的にしか実装していない項目。

| 項目 | 内容 | 着手見込み |
|---|---|---|
| Phase 9 declare-key 機構 | `declare-key derivative` 等の汎用宣言キー仕組み | 後回し |
| `inspect` の REPL 統合 | REPL で式評価時に静的型 + 観察型を自動表示 | UI 寄り、別 issue 化候補 |
| 型注釈提案機能 | 観察型をコピペ可能な形で出力 (`suggest:` ラベル付け) | UI 寄り |
| `frac (poly (frac integer))` 等の深いネスト型テスト | 動作はするが専用テストが無い | 軽量 |
| Pattern function 等の pattern matcher 内での CAS 型の扱い | 部分的 | 別途必要に応じて |

---

## 7. 詰まり項目を解く道筋

§ 2 で挙げた真の詰まり項目を解くための具体的な道筋。

### 7.1 Multi-param TC dispatch の改修 ✅ 完了 (2026-04)

§2.1 を参照。Step 1〜5 すべて実装済:
1. ✅ `findMatchingInstanceForTypes` (Type/Instance.hs)
2. ✅ `Constraint` の `[Type]` 化 (Type/Types.hs ほか 9 ファイル)
3. ✅ EnvBuilder が全 class type vars を constraint に乗せる
4. ✅ Desugar が unique な dictionary 名を生成
5. ✅ TypeClassExpand が multi-type 経路でディスパッチ

未対応: 結果型未確定の場合の ambiguity check (Haskell の `Ambiguous type variable` 相当)。現状は最初に見つかった instance が選ばれる。実用上は型注釈で曖昧性を解消できるので問題は出にくい。

**設計上の選択肢**:
- (A) 結果型未確定の場合はエラー (Haskell の `Ambiguous type variable` 相当)
- (B) Functional dependencies (`class Coerce a b | a -> b`) を導入して a から b が決定するルールを書ける

実装上は (A) の方が小さい。

### 7.2 `partialDiff` と `chainPartialDiff` の統合

**スコープ**: `TypeClassExpand.hs` のメソッド辞書生成と、user による `chainPartialDiff` 再定義のシャドウイングがどう衝突するかを精査。

**仮説**:
- 型クラス・メソッド `partialDiff` は内部的に `dictPartialDiff_<TypeName>` のような辞書経由で呼ばれる
- インスタンス body の `chainPartialDiff` 参照は、user の最新 `chainPartialDiff` 定義を resolve するべきだが、何かが先回りして「文字列 `"partialDiff"`」に解決されてしまう

**修正候補**:
- 辞書生成時の名前 mangling を確認
- `IDefineMany` (chainPartialDiff の再定義) と type-class instance binding の評価順を確認
- 必要なら chainPartialDiff の生成名を `chainPartialDiff_<n>` のように バージョン管理し、最新版を直接参照する

**影響範囲**: 中程度。デバッグできれば局所的な修正で済む見込み。

### 7.3 `casNormalize` への auto-rule 自動適用 (改訂版 — 2026-04-28)

**新方針**: Egison-level `mathNormalize` の動的再生成。Haskell 側 (`Math/CAS.hs`) は変更しない。

#### 二層アーキテクチャ

```
arithmetic op (+, *, /, ^)  [lib/math/common/arithmetic.egi]
   ↓
mathNormalize  ← desugar が auto rule を含めて再定義する
   ↓
mathNormalizeBuiltin  ← lib の既存処理 (sin/cos/rtu の rewrite 含む)
   ↓
symbolNormalize (primitive)
   ↓
casRewriteSymbol  [Math/Rewrite.hs, Sweet Egison]
   ↓        ← ⭐ 頻出 rule を Sweet Egison で実装する場所 (Phase C)
casNormalize  [Math/CAS.hs, 構造的正規化]
```

| 層 | 場所 | 速度 | 柔軟性 |
|---|---|---|---|
| **Hot path (built-in)** | `Math/Rewrite.hs` の Sweet Egison rule | 速い (compiled) | 低 |
| **Cold path (user)** | desugar emit の Egison `match` | 遅い (interpreted) | 高 |

両層は冪等な rewrite step として独立に動作する。Built-in に無い rule は user 層で
処理され、頻出になれば maintainer が Sweet Egison 実装に**昇格**させる
(TeX/LaTeX のマクロ → primitive 化に類似)。

#### 実装

**Step 1**: `lib/math/normalize.egi` に `mathNormalizeBuiltin` を追加 (既存の `mathNormalize` 定義のエイリアス):

```egison
def mathNormalizeBuiltin (x: MathValue) : MathValue := <既存の mathNormalize 本体>
def mathNormalize (x: MathValue) : MathValue := mathNormalizeBuiltin x
```

**Step 2**: Desugar が `declare rule auto` を見るたびに `mathNormalize` を上書きする `def` を emit:

```egison
-- declare rule auto term i^2 = -1 が処理される
-- ↓
def autoRule.0 := \v -> if v = i^2 then -1 else v
def mathNormalize := \v -> iterateRules [autoRule.0] (mathNormalizeBuiltin v)
```

**Step 3**: 次の auto rule 宣言があれば、`mathNormalize` をさらに上書き:

```egison
-- declare rule auto term w^3 = 1 が追加される
-- ↓
def autoRule.1 := \v -> if v = w^3 then 1 else v
def mathNormalize := \v -> iterateRules [autoRule.0, autoRule.1] (mathNormalizeBuiltin v)
```

**Step 4**: `recursiveBindAll` (Eval.hs Phase 8) が最新版 `mathNormalize` を全箇所で使えるようにする (相互再帰束縛)。`+`/`*` for MathValue は既に `mathNormalize` を経由するので、user rule が自動発火する。

#### 必要な変更箇所

- **`lib/math/normalize.egi`**: `mathNormalizeBuiltin` を導入
- **`Desugar.hs`**: `desugarTopExpr (DeclareRule Nothing ...)` で `IDefineMany [autoRuleLambda, mathNormalizeRedef]` を emit
- **`EvalState.hs`**: auto-rule 名トラッキング用フィールド追加 (例: `autoRuleNames :: [String]`)
- **`Eval.hs:buildAndMergeEnvironments`**: auto-rule 名を EnvBuildResult から伝搬 (現状の `derivativeRuleNames` と同様)

`Math/CAS.hs` は **無変更**。Sweet Egison は **使わない**。control-egison の TH 制約は **無関係**。

#### Phase C (rule 昇格、ongoing)

`Math/Rewrite.hs` に **Sweet Egison でハードコード規則を追加**することで、頻出 rule を high-performance 化:

```haskell
-- Math/Rewrite.hs に新規追加
casRewriteW :: CASValue -> CASValue
casRewriteW v =
  match dfs v (PolyM CASM)   -- ← § 7.4 の PolyM/FracM/TermM があれば書きやすい
    [ [mc| ... pattern for w^3 ... -> CASInteger 1 |]
    , [mc| _ -> v |]
    ]

casRewriteSymbol = ... . casRewriteW . ...   -- パイプラインに追加
```

User の `declare rule auto term w^3 = 1` は built-in が処理した後の値 (= 既に 1)
には match しない (no-op)。よって**両層が共存しても問題ない**。

#### Phase 7.4 (任意): Sweet Egison マッチャー拡張

`Math/CAS.hs` に `PolyM`/`FracM`/`TermM` を追加。`Math/Rewrite.hs` の既存規則を
高レベルに書き直し可能になり、Phase C (昇格) の実装コストを下げる。これは
auto-rule 自動適用とは独立の改善。

#### 工数見積

| Step | 内容 | 工数 |
|---|---|---|
| Step 1-4 (上記) | mathNormalize 再生成 + 自動適用 | **3-5 日** |
| Phase 7.4 (任意) | PolyM/FracM/TermM Sweet Egison 拡張 | 3-5 日 |
| Phase C (継続) | 頻出 rule の Sweet Egison 昇格 | ongoing |

Phase 7.2 (旧版) の見積 「2週間〜1ヶ月」が **3-5 日に短縮**。Haskell 側の大規模
リファクタが不要になったため。

---

## 改訂履歴

- 2026-04-28: 初版。Phase 1.5 / 5 / 5.5 (簡易) / 6.3 / 7.4-7.6 / 8 の実装後の状態を記録
- 2026-04-29: Phase A (pattern LHS) / Phase A.5 (sub-expression rule application) を実装。§2.2 (`partialDiff` の `chainPartialDiff` 統合) 修正、`f 3 + f 4` の dispatch bug 修正、§4.2 forward reference warning 修正。`$ ^ $` を mathValue matcher に追加。§3.2 ApplyN は実装しない方針に確定。
- 2026-04-29: §2.1 Multi-param TC dispatch 完了。`Constraint` を `[Type]` に拡張、`findMatchingInstanceForTypes` を整備、EnvBuilder/Desugar/TypeClassExpand を multi-type 対応に更新。`Coerce MathValue Integer` と `Coerce MathValue (Frac Integer)` が結果型注釈で正しくディスパッチされるようになった。
