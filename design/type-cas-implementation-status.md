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
| 1.5 | `SymbolSet` の `TypeAtom` 化 | ✅ 完了 (`Apply1〜4 → ApplyN` のみ保留) | 67, 69, 80 |
| 2 | 型システムへの統合 | ✅ 完了 | 75 |
| 3 | `ScalarData → CASValue` 置換 | ✅ 完了 (既存) | - |
| 4 | プリミティブパターン | ✅ 完了 (既存) | - |
| 5 | パラメトリックマッチャー | ✅ 完了 | 60, 62, 63 |
| 5.5 | Embed/Coerce | 🟡 簡易版で動作 ([4.1](#41-cas-subtype-unification-による-embed-elaboration-の代替) 参照) | 73, 75, 77, 78, 81 |
| 6.1-6.2 | `expandAll`/`substitute` | ✅ 既存実装で動作 | 65 |
| 6.3 | `∂/∂` (Differentiable) | 🟡 部分動作 ([2.2](#22-partialdiff-を-chainpartialdiff-に委譲する統合) 参照) | 74, 76, 91, 93, 94 |
| 6.3 part 5 | `declare mathfunc` | ✅ 完了 | 90, 91 |
| 6.3 part 6 | `chainPartialDiff` ディスパッチャ | ✅ 完了 | 93, 94 |
| 6.5 | `Sd` (積分) | ❌ pre-existing バグ ([5.7](#57-sd-integration-の-lib-バグ) 参照) | - |
| 7.4 | `declare rule` 構文 | ✅ 完了 | 70, 83 |
| 7.5 | rule 適用エンジン | 🟡 literal LHS のみ ([3.1](#31-pattern-variable-を含む-rule-lhs) 参照) | 87, 89 |
| 7.5 | rule combinator (`applyRules` / `iterateRules`) | ✅ 完了 | 89 |
| 7.5 | **auto-rule 自動適用 (mathNormalize 再生成)** | ✅ 完了 | 95, 96 |
| 7.5+ | **term-level recursive + factor-containment 適用** | ✅ 完了 (built-in `casRewriteI` 等が無くても算術恒等式が動く) | 97, 98 |
| 7.6 | `simplify ... using ...` | ✅ literal LHS で完了 | 85 |
| 8 | 観察型 `typeOf` / `inspect` / `differentialClosed` | ✅ 完了 (REPL 統合は別途) | 68, 71 |
| - | multi-param TC infrastructure | 🟡 受理のみ動作 ([2.1](#21-multi-param-型クラス-dispatch) 参照) | 72 |

凡例: ✅ 設計通り完了 / 🟡 動作するが妥協あり / ❌ 未実装または既知のバグ

---

## 2. 真の技術的詰まり

実装時に「**設計通りに動かす**ためには大きな改修が必要」と判断した項目。

### 2.1 Multi-param 型クラス dispatch

**設計意図**: `class Embed a b where embed :: a -> b` や `class Coerce a b where coerce :: a -> b` のような複数型引数のクラスを定義し、結果型 `b` で適切な instance を選ぶ。

**実装した範囲**:
- `ClassInfo.classParams :: [TyVar]`、`InstanceInfo.instTypes :: [Type]` にリスト化済
- AST/パーサ/EnvBuilder は `class Embed a b where ...` / `instance Embed T1 T2 where ...` を受理 (`mini-test/72-multiparam-class.egi` 通過)

**詰まりの本質**: ディスパッチャ (`Type/Instance.hs:findMatchingInstanceForType`) が**単一の Type しか受け取らない**:

```haskell
findMatchingInstanceForType :: Type -> [InstanceInfo] -> Maybe InstanceInfo
findMatchingInstanceForType targetType instances = go instances
  where
    go (inst:rest) =
      case unifyStrict (instType inst) targetType of  -- ← instType (head のみ)
        Right _ -> Just inst
        Left _  -> go rest
```

呼び出し側 (`Type/TypeClassExpand.hs`) も同様で、第一引数の型しかディスパッチに渡さない。

**具体例**:

```egison
instance Coerce MathValue Integer where ...        -- (1)
instance Coerce MathValue (Frac Integer) where ... -- (2)

def f : Frac Integer := coerce (1 / 2)
-- 期待: 結果型 Frac Integer で (2) を選ぶ
-- 実際: 第一引数 MathValue で (1) を選ぶ → isPureInteger (1/2) = False → undefined
```

**回避策(現状)**: per-target plain function を提供する:
```egison
def coerceToInteger (x : MathValue) : Integer := ...
def coerceToFracInteger (x : MathValue) : Frac Integer := ...
```
`mini-test/81-coerce-class.egi` で動作確認。

**真の修正**: § 7.1 を参照。

### 2.2 `partialDiff` を `chainPartialDiff` に委譲する統合

**設計意図**: `Differentiable` 型クラスの `partialDiff` インスタンスが、`declare derivative` で登録された関数を自動的に拾う。

**実装した範囲**:
- `chainPartialDiff` という拡張可能ディスパッチャを desugar が生成 (`mini-test/93-chain-partial-diff.egi` 通過)
- `declare derivative <name>` ごとに `chainPartialDiff` が再定義され、`apply1 #<name>` パターンが追加される
- 直接 `chainPartialDiff (f x) x` と呼ぶと declared derivative が動く

**詰まりの本質**: `Differentiable` インスタンスを `partialDiff f x := chainPartialDiff f x` に書き換えると、runtime エラーが発生:

```
Evaluation error: Expected CASData, but found: "partialDiff"
  stack trace: Var "z" [], Var "<stdin>" []
```

「文字列 `"partialDiff"` が CASData の代わりに渡された」というメッセージ。
推測: 型クラス・メソッドの辞書ルックアップ (`__methodPartialDiff` 的な仕組み) と、user による `chainPartialDiff` の再定義の名前解決順が衝突している可能性。`TypeClassExpand.hs` の生成コードを精査する必要がある。

**回避策(現状)**: instance は `∂/∂'` に委譲し、ユーザーが declared derivative を使いたい場合は `chainPartialDiff` を明示的に呼ぶ:

```egison
declare mathfunc f
declare derivative f = \z -> 2 * z

partialDiff (f x) x      -- ∂/∂' に落ちて hardcoded 規則のみ → 正しくない結果
chainPartialDiff (f x) x -- → 2 * x ✓
```

**真の修正**: § 7.2 を参照。

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
- pattern variable LHS (`(sin $x)^2 = ...`) は引き続き Phase A (LHS Pattern 化) が必要

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

### 3.1 Pattern variable を含む rule LHS

**設計意図**:
```egison
declare rule trig_pythagorean poly (sin $x)^2 + (cos #x)^2 = 1
declare rule auto term (sqrt $y)^2 = y
```

`$x` (パターン変数バインディング) と `#x` (値参照) を LHS に書ける。

**現状**: literal LHS のみ動作。`i^2 = -1` のような **パターン変数なしの式**は CAS 正規化のおかげで `i*i` などにも match (構造的等価)。

**詰まりの本質**: 現状の `declareRuleExpr` は LHS を `expr` で parse する。expression context で `$y` は `AnonParamExpr` (`$1`/`$2`...) しか想定しておらず、`$y` のような identifier-postfix は parse できない。試すと silent な誤解釈 (`$` 演算子のような扱い) になる。

**真の修正には**: LHS の parse を `pattern` parser に切り替える必要がある。これに伴い:
- `i^2` のような literal は pattern context では `#i^#2` (value-pattern) と書く必要が出る
- 既存 mini-test 70/87/88/89 等の syntax 変更
- AST 変更: `DeclareRule (Maybe String) RuleLevel Pattern Expr` (今は `Expr Expr`)

**判断**: 影響大なので保留。design 通りの完全実装には必要。

### 3.2 `Apply1〜Apply4` → `ApplyN`

**設計意図**: `SymbolExpr` の `Apply1 a` ... `Apply4 a b c d` を `ApplyN MathFuncRef [CASValue]` に一般化。任意 arity の math function を表現可能に。

**現状**: 4 構成子のまま。

**詰まりの本質**: 純粋に作業量。広範囲のパターン (Layer 1/2)、既存テスト、各 Apply ケース、`Differentiable Factor` の dispatch、`extractSymbolExpr` 等の helper をまとめる必要。

**判断**: ユーザー合意の上で保留。`declare mathfunc` を含めて一気に実施する方が効率的。

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

### 4.2 `chainPartialDiff` の前方参照

**実装**: 各 `declare derivative <name>` の desugar が `chainPartialDiff` を再定義する。生成される body は **これまで宣言されたすべての `f`/`g`/`deriv.f`/`deriv.g` を参照する**:

```egison
def chainPartialDiff := \v dx -> match v as mathValue with
  | apply1 #f $a -> deriv.f a *' chainPartialDiff a dx
  | apply1 #g $a -> deriv.g a *' chainPartialDiff a dx
  | _ -> ∂/∂' v dx
```

最初の `declare derivative` の時点で、まだ宣言されていない後続の `g`/`deriv.g` を参照することがある。

**動作する理由**: `recursiveBindAll` (Phase 8 の binding 段階) が全 def を相互再帰的にバインドするので、参照はランタイムで解決される。

**副作用**: 型推論時には forward reference が解決されず、"Unbound variable 'g' (assuming type 'Any')" のような **警告**が出る。Cosmetic だが分かりにくい。

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

### 7.1 Multi-param TC dispatch の改修

**スコープ**:
1. `findMatchingInstanceForType :: Type -> [InstanceInfo] -> Maybe InstanceInfo` を `findMatchingInstanceForTypes :: [Type] -> [InstanceInfo] -> Maybe InstanceInfo` に変更
2. 呼び出し側 (`Type/TypeClassExpand.hs`、`Type/Infer.hs`) で **結果型を含む型情報をディスパッチに渡す**
3. これは型推論の流れに影響する。call-site で結果型が決まっていない場合 (例: `let r = coerce x in ...` で r が未注釈) は ambiguous instance としてエラーにするか、Haskell 的な **ambiguity check** を実装する

**影響範囲**: `Type/Instance.hs`、`Type/TypeClassExpand.hs`、`Type/Infer.hs`。深い改修。

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
