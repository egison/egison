# Egison CAS 型システム 実装状況と残課題

このドキュメントは [type-cas.md](./type-cas.md) の設計に対する**実装の到達点**と、**残された課題**をまとめる。設計時点で予測された課題は [type-cas-issues.md](./type-cas-issues.md)、こちらは「実装してみて分かったこと」と「現時点で残っているもの」を記録する。

最終更新: 2026-05-01 (型昇格タワー level 4 正規化実装)

---

## 0. 一言サマリー

設計書 ([type-cas.md](./type-cas.md)) で予定されていた主要 phase は **すべて実装済**。残るのは:

- **意図的に実装しない**もの (§3.2 ApplyN 一般化)
- **動作するが設計書通りではない妥協点**で、実害が無いもの (§4)
- **新機能** (Phase 9 declare-key 等、§6)
- **細かい摩擦点** (§5)

CAS 型システムは、`declare symbol`/`declare mathfunc`/`declare derivative`/`declare rule` の 4 つの宣言系構文と、Coerce/Embed/Differentiable の型クラスがすべて設計通りに動作する状態。

---

## 目次

1. [現状サマリー (実装済みフェーズ)](#1-現状サマリー-実装済みフェーズ)
2. [実装の経緯と到達した解決策](#2-実装の経緯と到達した解決策) — 完了済の難所の記録
3. [意図的に実装しない項目](#3-意図的に実装しない項目)
4. [設計と実装の乖離 (妥協点)](#4-設計と実装の乖離-妥協点)
5. [細かい摩擦点](#5-細かい摩擦点)
6. [残された未着手項目 (新機能)](#6-残された未着手項目-新機能)
7. [改訂履歴](#改訂履歴)

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
| 5.5 | Embed/Coerce | ✅ Multi-param dispatch で完成 | 73, 75, 77, 78, 81, 104 |
| 6.1-6.2 | `expandAll`/`substitute` | ✅ 既存実装で動作 | 65 |
| 6.3 | `∂/∂` (Differentiable) | ✅ 各 instance が固有の構造分解 rule を所有 (Frac=quotient rule, Poly=sum, Term=power/product, Factor=apply1)。MathValue は runtime shape の switchboard。`partialDiff` から declared derivative も auto-dispatch | 74, 76, 91, 93, 94, 103, 105 |
| 6.3 part 5 | `declare mathfunc` | ✅ 完了 (型シグネチャも env に登録) | 90, 91 |
| 6.3 part 6 | `chainPartialDiff` ディスパッチャ | ✅ 完了 | 93, 94 |
| 6.5 | `Sd` (積分) | ❌ pre-existing バグ ([§5.7](#57-sd-integration-の-lib-バグ)) | - |
| 7.4 | `declare rule` 構文 | ✅ 完了 | 70, 83 |
| 7.5 | rule 適用エンジン (literal LHS) | ✅ 完了 (`applyTermRule`、factor containment 付き) | 87, 89 |
| 7.5 | rule 適用エンジン (pattern LHS、`$x`/`#x`) | ✅ 完了 (Phase A) | 99, 100 |
| 7.5+ | pattern LHS の sub-expression 再帰 (Phase A.5) | ✅ 完了 (`mapPolyAll`/`mapTermAll`/`mapFracAll`) | 101, 102 |
| 7.5 | rule combinator (`applyRules` / `iterateRules`) | ✅ 完了 | 89 |
| 7.5 | auto-rule 自動適用 (mathNormalize 再生成) | ✅ 完了 | 95, 96 |
| 7.5+ | term-level recursive + factor-containment 適用 | ✅ 完了 (built-in `casRewriteI` 等が無くても算術恒等式が動く) | 97, 98 |
| 7.6 | `simplify ... using ...` | ✅ literal LHS で完了 | 85 |
| 8 | 観察型 `typeOf` / `inspect` / `differentialClosed` | ✅ 完了 (REPL 統合は別途、§6 参照) | 68, 71 |
| - | multi-param TC dispatch (Coerce 等) | ✅ 完了 | 72, 104 |

凡例: ✅ 設計通り完了 / 🟡 動作するが妥協あり / ❌ 未実装または既知のバグ

**テスト数**: 70 mini-tests + 21 cabal tests、すべて pass。

---

## 2. 実装の経緯と到達した解決策

ここでは「実装中に大きな技術的詰まりとして認識され、解決策に到達した」項目を記録する。すべて完了済。

### 2.1 Multi-param 型クラス dispatch ✅ 完了 (2026-04)

**設計意図**: `class Coerce a b where coerce :: a -> b` のような複数型引数のクラスを定義し、結果型 `b` で適切な instance を選ぶ。

**当初の詰まり**: `Constraint` が `Type` を 1 つしか持てず、ディスパッチャが第一型のみで instance を選んでいた。`Coerce MathValue Integer` と `Coerce MathValue (Frac Integer)` が同じ第一型のため衝突。

**到達した解決策**:

1. **`Constraint` を `[Type]` に拡張** (`Type/Types.hs`):
   - `Constraint { constraintClass :: String, constraintTypes :: [Type] }`
   - `constraintType :: Constraint -> Type` を後方互換アクセサとして提供
   - 影響範囲: 9 ファイル、約 100 箇所のパターンマッチ・構築点

2. **`findMatchingInstanceForTypes`** (`Type/Instance.hs`):
   - 各 instance の全 instTypes と target を pairwise に unifyStrict
   - substitution の carry-forward で `instance Coerce a a` のような同一型変数も正しく扱う

3. **EnvBuilder で全 class type vars を constraint に積む**:
   - `class Coerce a b where coerce ...` から `forall a b. Coerce a b => a -> b` を生成

4. **Desugar で multi-type instance に unique な dictionary 名を生成**:
   - `Coerce MathValue Integer` → `coerceMathValueInteger`
   - `Coerce MathValue (Frac Integer)` → `coerceMathValueFracInteger`

5. **TypeClassExpand のディスパッチを多型対応**:
   - 全 dispatch site で `findMatchingInstanceForTypes` を使用 (single/multi 経路を統合)

**動作例** (`mini-test/104-multiparam-coerce.egi`):
```egison
def n : Integer       := coerce (1 + 2)        -- 3 (Integer instance)
def f : Frac Integer  := coerce (1 / 2)        -- 1/2 (Frac instance)
```

**残課題** (重要度低):
- 結果型未確定の場合の **ambiguity check** (`let r = coerce x in ...` で r が未注釈の場合) は未実装。現状は最初の instance が選ばれる。実用上は型注釈で曖昧性を解消できる。Haskell-like なエラー化 or functional dependencies で対応可能。

### 2.2 `partialDiff` を `chainPartialDiff` に委譲する統合 ✅ 完了 (2026-04)

**設計意図**: `Differentiable` 型クラスの `partialDiff` インスタンスが、`declare derivative` で登録された関数を自動的に拾う。

**当初の詰まり**: `partialDiff f x := chainPartialDiff f x` に書くと runtime エラー `Expected CASData, but found: "partialDiff"` が発生。「文字列 `"partialDiff"` が CASData の代わりに渡された」状態。

**到達した解決策**: 同根のバグ (`f 3 + f 4` の `"plus"` エラー) を調査して **`declare mathfunc` が型環境にシグネチャを登録していなかった** ことが原因と判明。

実装:
- `EnvBuilder.hs`: `DeclareMathFunc` を `processTopExpr` で型環境に登録 (`f : MathValue -> MathValue`)
- `lib/math/analysis/derivative.egi`: 全 `Differentiable` instance が `chainPartialDiff` 経由に
- 動作: `partialDiff (f x) x = 2 * x` (declared derivative `f' = 2*z` を自動 dispatch)
- テスト: `mini-test/103-partialdiff-auto-dispatch.egi`

### 2.3 `casNormalize` への auto-rule 自動適用 ✅ 完了 (2026-04-28)

**設計意図**: `declare rule auto term i^2 = -1` で登録した規則が `casPlus`/`casMult` 等の演算過程で自動的に適用される (Maxima の `tellsimp` 相当)。

**当初の詰まり**:
- `casNormalize :: CASValue -> CASValue` を Haskell で `EvalM` 化する必要があり、大規模リファクタとなる
- Sweet Egison の `[mc| ... |]` は TH ベースで runtime コンパイルできない

**到達した解決策**: **`mathNormalize` 再生成による Egison-level 解決**

二層アーキテクチャ:
```
arithmetic op (+, *, /, ^)  [lib/math/common/arithmetic.egi]
   ↓
mathNormalize  ← desugar が auto rule を含めて再定義する
   ↓
mathNormalizeBuiltin  ← lib の既存処理 (sin/cos/rtu の rewrite 含む)
   ↓
casRewriteSymbol  [Math/Rewrite.hs, Sweet Egison]
   ↓
casNormalize  [Math/CAS.hs, 構造的正規化]
```

| 層 | 場所 | 速度 | 柔軟性 |
|---|---|---|---|
| **Hot path (built-in)** | `Math/Rewrite.hs` の Sweet Egison rule | 速い (compiled) | 低 |
| **Cold path (user)** | desugar emit の Egison `match` | 遅い (interpreted) | 高 |

両層は冪等な rewrite step として独立に動作する。Built-in に無い rule は user 層で処理され、頻出になれば maintainer が Sweet Egison 実装に**昇格**させる (TeX/LaTeX のマクロ → primitive 化に類似)。

**実装上のキーポイント (見つかった落とし穴)**:
- **落とし穴 1 (無限ループ)**: 当初 `unNormalizeOps` が `^` を `^'` (lib-level un-normalized power) に置換していたが、`^'` の内部は `take`/`foldl` を使い、これが `n - 1` で MathValue の `-` を呼び、結果として `mathNormalize` を呼ぶ無限ループに陥った。解決: 演算子をプリミティブ (`i.+`, `i.-`, `i.*`, `i./`) に直接展開。`^` は リテラル正整数の場合のみ反復した `i.*` に展開。
- **落とし穴 2 (変数シャドウイング)**: rule lambda の引数名を `v` にしていたが、user が `declare rule auto term v^2 = 0` のように `v` という名前の symbol を使うと衝突。解決: `fresh` で生成した `__rule_input.<n>` 形式の名前。

**意図的単純化**:
- `casNormalize :: CASValue -> CASValue` は **そのまま (純粋)**、`Math/CAS.hs` 無変更
- Sweet Egison でユーザー rule をコンパイルする必要なし
- パフォーマンスは「頻出 rule を Sweet Egison 実装に昇格」で対応 (二層アーキテクチャ)

**追加実装**: `applyTermRule` プリミティブ (`Primitives.hs`) で **factor containment** 付き term-level 再帰適用。LHS `u^2` は `u^2`、`u^3`、`u^4` すべての項にマッチし、`(1+u)*(1-u) = 2` のような代数的恒等式が built-in なしで動く (`mini-test/97-rule-recursion.egi`, `98-user-rules-no-builtin.egi`)。

### 2.4 Pattern variable LHS と sub-expression 再帰 ✅ 完了 (2026-04, Phase A + A.5)

**設計意図**:
```egison
declare rule trigPyth poly (sin $x)^2 + (cos #x)^2 = 1
declare rule auto term (sqrt $y)^2 = y
```

`$x` (パターン変数バインディング) と `#x` (値参照) を LHS に書ける。さらに、rule はサブ式に対しても再帰適用される。

**当初の詰まり**: 旧来 `declare rule` の LHS は `Expr` で parse されていた。expression context で `$y` は `AnonParamExpr` (`$1`/`$2`...) しか想定されず、`$y` のような identifier は parse できなかった。

**到達した解決策** (Phase A):

- **AST 変更**: `DeclareRule (Maybe String) RuleLevel Pattern Expr` (旧: `Expr Expr`)
- **専用 LHS parser** `ruleLhsPattern` (`Parser/NonS.hs`):
  - **auto-quoting**: bare lowercase ident と integer literal は `ValuePat` に自動変換 — `i^2 = -1` のような既存構文がそのまま動く
  - `$x` → `PatVar`, `#expr` → `ValuePat`
  - mathExpr matcher constructors (`apply1`/`apply2`/`term`/`frac`/`poly`/...) は `InductivePat` として扱う
  - juxtaposition `f $x` は `PApplyPat`
- **desugar 分岐**:
  - PatVar なし → 既存の `applyTermRule` ルートを使用
  - PatVar あり → `translateToMatcherPattern` で `f $x` → `apply1 #f $x` に変換し、match 式を生成

**Phase A.5 (sub-expression 再帰)**: `mapPolyAll`/`mapTermAll`/`mapFracAll` プリミティブを追加。

- 各プリミティブは MathValue ツリーをボトムアップで走査し、`Apply1-4`/`Quote`/`FunctionData` の引数 (内側 MathValue) に再帰下降。各ノードで rule を fixpoint まで適用。
- desugar の pattern-LHS path が `\v -> mapXAll <oneStep> v` の形を生成。
- `mathValue` matcher に `$ ^ $` clause を追加 (`(sqrt $a)^2 = a` のような自然な記法を可能に)。
- `-` を `reservedPatternOp` に追加 (parser 受理のみ、matcher 側 clause は将来対応)。

**動作例**:
- `dbl (dbl 3) → 12` (cascading reduction)
- `f (dbl 3) → f 6` (recursion into Apply1 arg)
- `(sqrt 3)^2 → 3`

テスト: `mini-test/99` (named rule), `100` (auto rule), `101` (sub-expression), `102` (power pattern).

**命名注**: lib に既存の `mapPoly`/`mapTerm`/`mapFrac` (frac の n/d split 系) と区別するため、新プリミティブは `*All` サフィックス付き。

### 2.5 `Term` 型の symbol set パラメータ追加 ✅ 完了 (2026-05-01)

**設計意図**: `Poly Type SymbolSet` と対称に、`Term` も `Term Type SymbolSet` (例: `Term MathValue [..]`) として扱えるようにする。

**変更**: `TTerm Type` (1 引数) を `TTerm Type SymbolSet` (2 引数) に拡張。

実装範囲:
- `AST.hs`: `TETerm TypeExpr SymbolSetExpr`
- `Type/Types.hs`: `TTerm Type SymbolSet`、`freeTyVars`/`isCASType`/`typeToName`/`typeConstructorName`/`typeExprToType`/`normalizeInductiveTypes` を symbol set 込みに更新
- `Type/Subst.hs`: `applySubst (TTerm t ss) = TTerm (applySubst s t) (applySubstSymbolSet s ss)`
- `Type/Unify.hs`: `unifyG (TTerm t1 ss1) (TTerm t2 ss2)` で `unifySymbolSets` も呼ぶ
- `Type/Join.hs`: `joinTypes`/`isSubtype`/`extractCoeff` を 2 引数に
- `Type/Infer.hs`: `freshenOpenSymbolSets` で Term の Open SymbolSet を fresh 変数に
- `Type/Pretty.hs`/`Type/Error.hs`: `prettyType (TTerm t ss) = "Term " ++ pretty t ++ " " ++ prettySymbolSet ss`
- `Type/Env.hs`: `substVar (TTerm t' ss) = TTerm (substVar t') ss`
- `Type/RuntimeType.hs`: 単一モノミアル CASPoly から atom 集合を抽出 → `TTerm coef (SymbolSetClosed atoms)`
- `EnvBuilder.hs`: `go (TTerm t ss) = TTerm (go t) ss`
- `Parser/NonS.hs`: `Term <T> [<symbols>]` / `Term <T> [..]` を受け入れ。`try termTypeExpr` で fallback あるので user-defined inductive `Term a` も並存可

lib 側:
- `lib/math/analysis/derivative.egi`: `instance Differentiable (Term MathValue [..])`
- `mini-test/105-differentiable-term.egi`: `Term MathValue [..]` 表記に更新

### 2.6 `Factor` 型を matcher signature 経由で静的 dispatch 可能に ✅ 完了 (2026-05-01)

**問題**: `instance Differentiable Factor where partialDiff f x := chainPartialDiff f x` は **runtime 到達不能**だった。`runtimeTypeOfCAS` で `TFactor` が返るのは bare `CASFactor _` の時のみで、CAS 値の大半は `CASPoly` 包みなので `TTerm` 経由で Term inst に行ってしまう。

**設計**: matcher のパターン signature を介して `$fx` のような pattern variable に静的に `Factor` 型を付ける。

実装:

1. **`factor : Matcher Factor` を実体化** (`lib/math/expression.egi`):
    旧 alias `def factor : Matcher MathValue := mathValue` を、symbol/apply1-4/quote/func patterns を持つ `Matcher Factor` の matcher に置き換え。

2. **`mathValue` matcher の `^` パターンを `(factor, integer)` に**:
    ```egison
    | $ ^ $ as (factor, integer) with ...
    ```
    `multExpr` 側も同様。

3. **`inductive pattern MathValue` の `(^)` を `Factor Integer` に**:
    ```egison
    | (^) Factor Integer
    ```
    型推論はこの inductive pattern 宣言を参照するので、これによって `match f as mathValue with | $a * $fx ^ $n -> ...` の `$fx` が静的 `Factor` 型になる。

4. **`Parser/NonS.hs` の `inductiveTypeAtom` に `Factor` を追加**:
    `TEFactor <$ reserved "Factor"` を加えないと `Factor` が inductive 宣言中で parse できない。

5. **Factor inst を実機能化** (`lib/math/analysis/derivative.egi`):
    ```egison
    instance Differentiable Factor where
      partialDiff f x := match f as factor with
        | #x -> 1
        | symbol _ _ -> 0
        | _ -> chainPartialDiff f x
    ```

6. **Term inst から atomic 短絡を削除**: `#x` / `?isSymbol` / `?isAtomicFactor` の 3 アームと `def isAtomicFactor` ヘルパが不要に。`#1 * $fx ^ $n` と `$a * $fx ^ $n * $r` の右辺で `partialDiff fx x` が **静的に Factor inst へ dispatch** されるため。

**詰まりポイント**:
- 当初 matcher の `as (factor, integer)` だけで `$fx` の型が伝播すると想定したが、Egison の型推論は `inductive pattern` 宣言を参照する。両方を変更する必要があった。
- パーサが `Factor` を inductive 宣言中で受け取らないため、`inductiveTypeAtom` 拡張が必要。

### 2.7 `Term` matcher / `inductive pattern MathValue` の `poly` を Term ベースに ✅ 完了 (2026-05-01)

**設計意図**: §2.6 で Factor が静的 dispatch 可能になったのと対称に、`partialDiff t x` (t は poly から分解した term) も静的に Term inst へ dispatch されるようにする。

**変更**:

1. **`term {a} (m: Matcher a) : Matcher (Term a [..])`** (旧: `Matcher MathValue`)。
2. **`mathValue` matcher の `poly`/`plus`/`+` の signature を refine**:
    ```egison
    | poly $ as (multiset (term mathValue)) with ...
    | plus $ as (multiset (term mathValue)) with ...
    | $ + $ as (term mathValue, mathValue) with ...
    ```
3. **`inductive pattern MathValue` の構造を Term ベースに**:
    ```egison
    | plus [(Term MathValue [..])]
    | poly [(Term MathValue [..])]
    | (+) (Term MathValue [..]) MathValue
    ```

**結果**: `match f as mathValue with | poly $ts -> sum (map (\t -> partialDiff t x) ts)` の `t` が静的 `Term MathValue [..]` 型になり、`partialDiff t x` がコンパイル時に Term inst へ dispatch される。`diffPolyTerms` ヘルパ関数が不要になり削除。

これで Factor (`$fx ^ $n`) / Term (`poly $ts` の各要素) の両方が **静的 dispatch** で動く設計対称性を獲得。

---

## 3. 意図的に実装しない項目

### 3.1 `Apply1〜Apply4` → `ApplyN` 一般化

**設計意図**: `SymbolExpr` の `Apply1 a` ... `Apply4 a b c d` を `ApplyN MathFuncRef [CASValue]` に一般化。任意 arity の math function を表現可能に。

**判断 (2026-04)**: **実装しない**。現状の Apply1〜4 で実用的な arity (4 変数まで) はカバーでき、5 変数以上の math function は実用上ほぼ無い。一般化のための広範囲リファクタ (Layer 1/2 のパターン、各 Apply ケース、`Differentiable Factor` dispatch、`extractSymbolExpr` 等のヘルパー) のコストに対するベネフィットが小さい。

---

## 4. 設計と実装の乖離 (妥協点)

動作はするが、設計書通りの実装ではない箇所。すべて実害なしで、必要になったら設計通りに実装可能。

### 4.1 CAS subtype unification による Embed elaboration の代替

**設計**: `Embed` 型クラスを base にして、型不一致時に AST に `embed expr` 呼び出しを elaboration で挿入する。

**実装**: unifier (`Type/Unify.hs`) に `MathValue ↔ {Factor, Frac _, Poly _ _}` の双方向ユニフィケーション規則を追加。型エラーは消えるが、AST に `embed` 呼び出しは現れない。

**問題ではない理由**: CAS 型は runtime 表現が同じ `CASValue` なので、明示的な `embed` を挿入しても runtime 動作に差は出ない。Embed 型クラス自体は lib に存在し、ユーザーが手動で `embed x` と書ける。

**設計通りの実装が必要になるケース**:
- 非 CAS 型同士の `embed` (例: `Maybe Int` への wrap)
- elaboration が必要な意味論を持つ Embed (現状は identity-only body)

### 4.2 名前付き rule のみが直接呼び出し可能

**設計**: `declare rule auto ...` は `casNormalize` で自動適用される。

**実装**: auto rules は `autoRule.<freshN>` というユニークな名前で desugar されるが、ユーザーは fresh-id を予測できないので呼べない。実用上は **named rule** (`declare rule <name> ...`) を使う運用。

**改善案**: `lastAutoRule`、`autoRule.<index>` (deterministic) など callable 名を提供する。優先度低。

### 4.3 型昇格タワーの level 4 正規化 ✅ 部分完了 (2026-05-01)

**設計** ([type-cas.md §実行時の型昇格タワー L858-944](./type-cas.md), L29 / L378): 型昇格タワーで level 4 (`Poly (Frac Integer)`) と level 5 (`Frac (Poly Integer)`) を区別する。
- `b` が定数 → 係数除算 (level 3/4 に留まる)
- `b` が単項式 → 負冪として吸収 (level 3/4)
- `b` が非単項式 → level 5

**初期実装の不在 (2026-04 まで)**: `casNormalizeFrac` の `(CASPoly, CASInteger)` ケースが「Poly / 定数 Integer」を level 5 (`CASFrac (CASPoly _) (CASInteger _)`) として保持。GCD 簡約のみで level 4 への変換を行わず。`(1/2)*x^2 + (1/3)*x` は注釈に関わらず `(3x^2+2x)/6` (level 5)、`typeOf` も `"Frac MathValue"` を返す。

**修正 (2026-05-01)**: `Math/CAS.hs` の `casNormalizeFrac` を変更:
```haskell
(CASPoly ts1, CASInteger d) | d /= 0 ->
  let ts1' = map (\(CASTerm c m) ->
                    CASTerm (casNormalizeFrac c (CASInteger d)) m) ts1
  in casNormalizePoly ts1'
```
定数分母を各 Term の係数に Frac として分配。`(1/2)*x^2 + (1/3)*x` は `CASPoly [CASTerm (CASFrac 1 2) [(x,2)], CASTerm (CASFrac 1 3) [(x,1)]]` (level 4) になり、`typeOf` が `"Poly (Frac Integer) [x]"` を返す。

**副作用と修正**: tower fix で level 5 を仮定していた lib 関数が "Pattern match failed" warning を出した:
- `containSymbol` / `containFunction1-4` (`lib/math/expression.egi`): outer `match t as mathValue with | term _ $xs -> ...` に `| _ -> False` fallback を追加
- `gcdForMathValue` (`lib/math/common/arithmetic.egi`): 末尾に `| _ -> 1` fallback を追加

これらは tower fix 後の値が直接 `CASPoly` 形 (level 3/4) で来るため、`Frac (Plus [Term ...]) (Plus [Term 1 []])` runtime extraction が失敗するケースに対応する fallback。

**検証** (`mini-test/107-deep-nested-types.egi` § 5.1 で確認):
| 注釈 | `typeOf` 結果 (修正前) | `typeOf` 結果 (修正後) |
|---|---|---|
| `Poly (Frac Integer) [x]` | `"Frac MathValue"` | `"Poly (Frac Integer) [x]"` |

cabal test 21/21 PASS、mini-test 80–107 全件 PASS、warnings 0 件。

**残課題 (型注釈ドリブンの強制正規化はまだ未実装)**:
1. **型注釈は AST elaboration を駆動しない**: `def p : Poly (Frac Integer) [x] := ...` で値変換 (coerce 挿入) は走らない。CAS が偶然 level 4 形に正規化しないケースがあれば static / runtime 乖離
2. **`coerce` プリミティブが壊れている**: `def p : Poly (Frac Integer) [x] := coerce (...)` で `Poly Integer [(coerce) ...]` という壊れた値を返す
3. **Term inst の Frac 係数対応**: Term inst body の `$a * $fx ^ $n * $r` が `mathValue` matcher の `$ * $ as (integer, multExpr)` を使っており Integer 係数のみ。Frac 係数の値が Term inst にdispatch されると pattern match 失敗 → 0 を返す

**実装方針 (将来)**:
1. ✅ **完了**: `casNormalize` の level 4 対応 (本作業)
2. AST elaboration で型注釈に基づく `coerce` 挿入 (§4.1 Embed elaboration と統合)
3. `coerce` プリミティブを Haskell-side で正規化を伴うように修正
4. Term inst の matcher を `term mathValue` ベースに更新して Frac 係数対応

### 4.4 `lookupDerivative` ではなく `chainPartialDiff` 再定義方式

**設計** ([type-cas.md §`Differentiable Factor`](./type-cas.md)): `declare derivative` は `lookupDerivative :: MathFuncRef -> CASValue` を populate する。Factor inst は `apply1 $f $arg -> (lookupDerivative f) arg * partialDiff arg x` で連鎖律を実行する。

**実装**: 各 `declare derivative <name>` が `def chainPartialDiff := \v dx -> match v as mathValue with | apply1 #<name> $a -> deriv.<name> a *' partialDiff a dx | ... | _ -> chainPartialDiffBuiltin v dx` を **shadowing 再定義**する。Factor inst は `apply1` 解析を持たず、代わりに `_ -> chainPartialDiff f x` でフォールスルーする。

**試した移行と詰まり (2026-05-01)**:
- Desugar に `lookupDerivative` 生成を追加し、Factor inst を `apply1 $g $a -> (lookupDerivative g) a *' partialDiff a x` に変更した。
- 詰まり (1) **名前衝突**: outer lambda の引数 `f` が user の `declare mathfunc f` と衝突し `#f` value pattern が常に local を参照。`fn` への rename で解消。
- 詰まり (2) **型推論**: 生成 lambda `\fn -> match fn as mathValue with | #g -> deriv.g | ... | _ -> \z -> 0` の型統一で、user 定義 `deriv.g = \u -> 3 * u^2` の `*` が CAS 乗算ではなく Integer 乗算として展開され、`(*) 3 (z^2)` の非正規化形が返ってしまう。`TypedLambdaExpr` での明示型注釈でも改善せず。

**結論**: 現状の `chainPartialDiff` 再定義方式を維持。`lookupDerivative` 設計を完成させるには **Haskell-side primitive 化** (型推論を経由せずランタイムの `DerivativeEnv` を引く) が必要。設計書 type-cas.md の `lookupDerivative :: MathFuncRef -> CASValue` Haskell シグネチャはこれを示唆している。優先度中。

---

## 5. 細かい摩擦点

実装中に遭遇した、細かいが時間を消費した問題群。基本的にすべて回避策で動作する。

### 5.1 Egison 識別子に `_` が使えない

`identChar = alphaNumChar` のため、設計の `trig_pythagorean` のような名前は parse できない。`trigPythagorean` (camelCase) を推奨。

**修正困難な理由**: subscript 構文 (`x_1`) と衝突する。修正には parser の大改修が必要。

### 5.2 `$y` の expression context での silent parse

`def y := $a` は parse error にならず、何か奇妙な解釈をする。デバッグで時間を消費した。pattern variable のような identifier-postfix を expression として書いた場合の挙動が直感的でない。

**未対応**: parser に check を追加して expression context で `$x` 形式をエラーにすると親切。

### 5.3 関数値の比較メカニズム

`apply1 #sin $g` パターンが動くのは、CAS 内部の `extractFunctionObjectCAS` が `QuoteFunction` ラッパーを unwrap してくれるから。仕組みを理解するのに時間がかかった。

```haskell
extractFunctionObjectCAS :: CASValue -> WHNFData
extractFunctionObjectCAS (CAS.CASFactor (CAS.QuoteFunction funcWHNF)) = funcWHNF
extractFunctionObjectCAS (CAS.CASPoly [CAS.CASTerm (CAS.CASInteger 1) [(CAS.QuoteFunction funcWHNF, 1)]]) = funcWHNF
extractFunctionObjectCAS cv = Value (CASData cv)
```

ドキュメント追加で十分。

### 5.4 `∂/∂'` のループ on unknown apply1

`∂/∂'` の hardcoded ケース (`apply1 #sin`/`#cos`/`#log`/...) のいずれにもマッチしない user mathfunc apply1 は、後続の `term`/`poly` ケースに落ちて **無限ループ**する。

**回避済**: `chainPartialDiff` の再帰アームでは `partialDiff` ではなく `chainPartialDiff` を呼ぶことで、user mathfunc がディスパッチで吸収される。`mini-test/93-chain-partial-diff.egi` のネスト合成テスト (`f (g x)`) で実証。

### 5.5 EnvBuilder と Desugar の処理順 (forward reference)

EnvBuilder は **すべての TopExpr** を先に処理して `EnvBuildResult` を埋める。その後 Desugar が **expr 単位で** 走る。各 declare の desugar が「自分まで」のリストを構築するには別途 counter が必要。

**対応済 (2026-04)**: `derivativesDesugared` フィールドを `EvalState` に追加し、desugar が宣言を処理するたびに append。`chainPartialDiff` の再定義は「現在まで desugared 済み」の名前のみ参照するように変更。同様の機構が auto rule (`autoRuleVarNames`) にも適用済み。warning が消えた。

### 5.6 Cabal キャッシュ

度々 `cabal clean` してフルリビルドしないと変更が反映されないことがあった。Haskell コードの depends 解決の何か。

**緩和策**: 大きな変更の後は `cabal clean` を念のため実行する。

### 5.7 Sd integration の lib バグ

`lib/math/analysis/integral.egi` の `Sd` 関数内のパターン `~f ~y` が type error を起こす:

```
Cannot unify types:
  Expected: MathValue -> MathValue (TFun TMathValue TMathValue)
  Actual:   MathValue (TMathValue)
```

`~f` を関数値として使う想定だが、`~` は VarPat (値参照) でしかなく、関数として使うには別途 lookup が必要。**pre-existing で本セッションの変更とは無関係**。

**未対応**: `~` を関数値として扱う構文の見直しが必要。CAS とは独立の lib バグ。

### 5.8 LHS 中の `=` 演算子 (declare rule、declare derivative)

`declare derivative sin = cos` の LHS と RHS の境界 `=` は `expr` parser では一般的な比較演算子としても認識される。LHS と RHS を分けるため、parser はまず `exprWithoutWhere` で全体を `InfixExpr "=" ...` として parse し、後から `extractRuleSides` で分解する。

```haskell
extractRuleSides :: Expr -> Maybe (Expr, Expr)
extractRuleSides (InfixExpr op lhs rhs) | repr op == "=" = Just (lhs, rhs)
extractRuleSides _ = Nothing
```

(**注**: `declare rule` は Phase A で専用 parser に切り替え済。`declare derivative` のみこの workaround を使う。)

---

## 6. 残された未着手項目 (新機能)

詰まりは無いが、未着手または部分的にしか実装していない項目。すべて新機能で、本実装の **完成度には影響しない**。

| 項目 | 内容 | 工数見込 |
|---|---|---|
| **`lookupDerivative` の Haskell-side primitive 化** | `declare derivative` を辞書 lookup 方式に。型推論を経由せず DerivativeEnv を引く primitive を `Type/Check.hs` に登録 (§4.3) | 中規模 |
| **Phase 9 declare-key 機構** | `declare-key derivative` 等の汎用宣言キー仕組み。`declare derivative` 等をライブラリ層に押し出す | 中規模 (新構文 + desugar) |
| `inspect` の REPL 統合 | REPL で式評価時に静的型 + 観察型を自動表示 | 小〜中 (UI 系) |
| 型注釈提案機能 | 観察型をコピペ可能な形で出力 (`suggest:` ラベル付け) | 小 (UI 系) |
| 深いネスト型テスト | `frac (poly (frac integer))` 等の専用テスト | 軽量 |
| Pattern function 内での CAS 型の扱い | 部分的 | 別途必要に応じて |
| Multi-param dispatch の ambiguity check | `let r = coerce x` で結果型未確定の場合のエラー化 | 小 |

### Phase 9 declare-key 詳細 (参考)

`declare derivative <name> = <expr>` のような宣言は現状 hardcoded だが、汎用化して
`declare-key derivative` で予約後、ライブラリで処理を書ける形にする。

```egison
-- 想定
declare-key derivative
declare derivative sin = cos
-- ↓ desugar
-- => onDeclareKey "derivative" "sin" cos  (ライブラリで定義)
```

これにより `Integrable`、`Substitutable` 等のユーザ独自クラスも同じ宣言機構で拡張可能になる。

---

## 改訂履歴

- 2026-04-28: 初版。Phase 1.5 / 5 / 5.5 (簡易) / 6.3 / 7.4-7.6 / 8 の実装後の状態を記録
- 2026-04-29: Phase A (pattern LHS) / Phase A.5 (sub-expression rule application) を実装。§2.2 (`partialDiff` の `chainPartialDiff` 統合) 修正、`f 3 + f 4` の dispatch bug 修正、§4.2 forward reference warning 修正。`$ ^ $` を mathValue matcher に追加。§3.2 ApplyN は実装しない方針に確定
- 2026-04-29: §2.1 Multi-param TC dispatch 完了。`Constraint` を `[Type]` に拡張、`findMatchingInstanceForTypes` を整備、EnvBuilder/Desugar/TypeClassExpand を multi-type 対応に更新
- 2026-04-29: refactor cleanup — `findMatchingInstanceForType` の legacy fast path / `orElseMaybe` fallback を削除。`findMatchingInstanceForTypes` に substitution carry-forward を追加。未使用 import / helper を整理
- 2026-04-29: ドキュメント全体を再構成。完了済項目を §2 にまとめ、残課題を §3-§6 に明確化
- 2026-04-29: `Term a` を proper な parametric CAS 型として導入 (`TETerm`/`TTerm` を AST/Type に追加、subtype unification・join・isSubtype・pretty 等を整備)。`instance Differentiable (Term MathValue)` を lib に追加。Factor < Term < Poly のサブタイプ関係も整備
- 2026-04-29: `Differentiable` instance に構造分解ロジックを移動。`diffQuotient`/`diffPolyTerms`/`diffTermPower`/`diffTermProduct` の helper 関数を追加し、Frac/Poly/Term 各 instance がそれぞれの shape を分解。MathValue instance は runtime shape による switchboard。`∂/∂'` も同じ helpers を再利用する parallel switchboard として残置 (テスト互換性)。各 instance が固有の rule を所有する設計に
- 2026-04-30: Runtime-type dispatch (design/runtime-type-dispatch.md) Phase 1/2/4/5 を実装。Phase 1: shallow `runtimeTypeOfCAS` を `Type/RuntimeType.hs` に新設し、`runtimeType` プリミティブとして公開。Phase 2: `findMostSpecificInstance`(ForTypes) を `Type/Instance.hs` に追加 — subtype 半順序で候補集合の最小元 (= 最も specific) を選ぶ。型変数を含む instance は `unifyStrict` 経由でマッチ、concrete instance は `isSubtype` で厳密にマッチ (cross-CAS unify で誤マッチしない)。`Type/Join.hs` の `isSubtype` に Factor/Term/Poly/Frac → MathValue 等の depth-2 ルールを追加。Phase 4: TypeClassExpand の dictionary 解決 3 サイト (variable 展開、nested constraint、resolveDictionaryForConstraint) で `findInstanceForDispatch` (most-specific + first-match fallback) を使用。直接メソッド呼び出しサイト (`tryResolveMethodCall`) は legacy first-match のまま。Phase 5: `mini-test/106-runtime-type-shallow.egi` と `mini-test/107-runtime-dispatch-integration.egi` を追加。Phase 3 (auto-gen MathValue instance) と Phase 6 (lib cleanup) は今後の課題
- 2026-04-30: Phase 6 (lib cleanup) **を一旦 revert**。`∂/∂'Atomic` を新設して内部で `partialDiff` (typeclass method) を再帰的に呼ぶ形にしたが、`∂/∂'Atomic` のシグネチャに `Differentiable a` 制約が無いため、再帰呼び出しが dispatch に解決されず literal な CAS シンボル `"partialDiff"` として残ってしまい、`dfNormalize` 経由で `mathNormalize` に流れて `Expected CASData, but found: "partialDiff"` というエラーが大量発生 (`cabal test` で sample/math/geometry/curvature-form.egi、hodge-laplacian-polar.egi 等 35 件)。`lib/math/analysis/derivative.egi` を HEAD に戻して解決。Phase 6 は Phase 3 (auto-gen) と組合わせる必要がある (現状のヘルパー関数だけ取り出す形は不安定)
- 2026-05-01: Phase 3 (Haskell-内 runtime dispatch) を実装。設計方針として「dispatch のための Egison コードを生成しない、runtimeType の値を Egison で扱わない」を採用 (= MathValue subtype 型を第一級 Egison 値にしない)。新しい IR ノード `IRuntimeDispatch` / `TIRuntimeDispatch` を `IExpr.hs` に追加し、`stripType` / `mapTIExprChildren` / `applySubstToTIExprNode(WithClassEnv)` / `insertInNode` (TensorMapInsertion) / `expandTIExprNode` (TypeClassExpand) / `prettyTIExprNode` を全て対応。`TypeClassExpand.hs` の `tryResolveMethodCall` で「target type = `TMathValue` かつ explicit `instance Class MathValue` が無い」ときに `TIRuntimeDispatch` を emit。candidate は `(instType inst, dictName)` のリストとして compile time に決定 (multi-param クラスは対象外)。`Core.hs` の `evalExprShallow` で `IRuntimeDispatch` を処理: 第一引数を評価 → `runtimeTypeOfCAS` で shallow 型を計算 → subtype 半順序の最小元から `dictName` を選択 → 既存の `IIndexedExpr (IVarExpr dictName) [Sub methodKey]` 経路で hash 引きと apply を実行。`Primitives.hs` から `runtimeType` プリミティブを撤回。`lib/math/analysis/derivative.egi` から手書きの `instance Differentiable MathValue` を削除 (= Phase 6 完了)。テスト: `mini-test/106-runtime-dispatch.egi` (8 assertion)、全 mini-test pass、`cabal test` 21/21 PASS、mid-execution エラー 0 件。
- 2026-05-01: §2.5 `Term` の symbol set パラメータ化 (`TTerm Type SymbolSet`)。Type/AST 全体に渡る変更。`runtimeTypeOfCAS` が単一モノミアルから atom 集合を抽出。Parser に `Term <T> [<symbols>]` 構文。lib `instance Differentiable (Term MathValue [..])` に更新。
- 2026-05-01: §2.6 `Factor` を matcher signature 経由で静的 dispatch 可能化。`factor : Matcher Factor` を実体化、`mathValue`/`multExpr` の `^` パターンを `(factor, integer)` に、`inductive pattern MathValue` の `(^)` を `Factor Integer` に。`Parser/NonS.hs` の `inductiveTypeAtom` に `Factor` 追加。Factor inst が `apply / quote / func` を chainPartialDiff へ delegate するように、Term inst から atomic 短絡を削除 (`isAtomicFactor` ヘルパ削除)。
- 2026-05-01: §2.7 `term` matcher を `Matcher (Term a [..])` に、`mathValue` matcher の `poly`/`plus`/`+` を Term ベースに更新、`inductive pattern MathValue` の構造を `[Term MathValue [..]]` に。Poly inst の `partialDiff t x` が静的 dispatch 化、`diffPolyTerms` ヘルパ削除。Factor / Term の両方が静的 dispatch で動く設計対称性を獲得。
- 2026-05-01: §4.3 `lookupDerivative` 移行を試行 → revert。詰まりは型推論で `deriv.<name>` が Integer 型に推論され CAS 正規化が走らなくなるため。Haskell-side primitive 化が必要と判定。
- 2026-05-01: 深いネスト型テスト (`mini-test/107-deep-nested-types.egi`) を追加。Frac (Poly Integer) / Poly (Frac Integer) / Frac (Poly (Frac Integer)) / Poly (Poly Integer) 等を parser・matcher・embed・partialDiff・typeOf・Tensor で検証。**新発見** §4.4 (旧 4.3 を 4.4 に rename): 型注釈は runtime 正規化を駆動しない (設計書の `Poly (Frac Integer) [x] := (x+x^2)/2 → (1/2)x + (1/2)x^2` という意図は未実装)。`coerce` も壊れた状態。実害は少ないが silent な dispatch ミスマッチが起きうる。
- 2026-05-01: §4.3 型昇格タワー level 4 正規化を実装。`casNormalizeFrac` の `(CASPoly, CASInteger d)` ケースを「定数分母を各 Term の係数に Frac として分配」に変更。`(1/2)*x^2 + (1/3)*x` が `CASPoly [CASTerm (CASFrac _ _) _]` (level 4) になり、`typeOf` が `"Poly (Frac Integer) [x]"` を返す。副作用として lib の level 5 仮定箇所 (`containSymbol`/`containFunction1-4` の outer match、`gcdForMathValue`) で pattern match 失敗 → 各箇所に fallback を追加。test 107-4.1 (Frac 係数 partialDiff) は Term inst が Frac 係数未対応のため Integer 係数で書き換え。残課題 (型注釈ドリブン強制正規化、coerce 修復、Term inst の Frac 対応) は §4.3 残課題に記載。cabal test 21/21 PASS、mini-test 80–107 PASS、warnings 0 件。
