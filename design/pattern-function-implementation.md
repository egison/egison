# Pattern Function Implementation

## 概要

Egison のパターン関数は `def pattern` で定義される。型検査では，各引数と結果を
capability と target の組として扱い，定義全体を一つの `DualScheme` として保存する。

```text
Dual = (capability, target)

DualScheme =
  forall capability binders, target binders.
    (argument duals) -> result dual
```

capability と target は異なる sort であり，代入・自由変数計算・一般化・インスタンス化を
混同しない。`DualScheme` がパターン関数の唯一の canonical な型情報である。

## 表層構文

```egison
def pattern twin {a} (pat1 : a) (pat2 : [a]) : [a] :=
  ($pat & ~pat1) :: #pat :: ~pat2

def pattern shuntsu {a} (pat1 : a) (pat2 : [a]) : [a] :=
  (num $s $n & ~pat1) :: num #s #(n + 1) :: num #s #(n + 2) :: ~pat2
```

- `{a}` は明示的な通常型パラメータである。
- 引数注釈と結果注釈は target 型を記述する。`Pattern a` のような wrapper は追加しない。
- capability は本体のパターン推論から得る。

## 内部表現

構文解析・脱糖・型推論後の宣言は次の形を取る。

```haskell
data TopExpr
  = PatternFunctionDecl
      String [String] [(String, TypeExpr)] TypeExpr Pattern

data ITopExpr
  = IPatternFunctionDecl
      String [TyVar] [(String, Type)] Type IPattern

data TITopExpr
  = TIPatternFunctionDecl
      String DualScheme [(String, Type)] Type TIPattern
```

型推論後の AST にも canonical `DualScheme` を残す。target projection だけを格納して
capability を失うことはない。

`Type/Types.hs` の中心となる型は次のとおりである。

```haskell
data Dual = Dual
  { dualCapability :: Capability
  , dualTarget     :: Type
  }

data DualScheme = DualScheme
  { dualCapBinders :: [CapVar]
  , dualTyBinders  :: [TyVar]
  , dualArgs       :: [Dual]
  , dualResult     :: Dual
  }
```

`dualCapBinders` と `dualTyBinders` は別々の名前空間を持つ。一回の適用では，両 binder
集合に対する一組の fresh substitution を全引数と結果へ同時に適用する。これにより，同じ
変数が表す引数間・引数結果間の相関を保つ。両 binder list は set-like であり，
instantiation は重複 binder を malformed scheme として拒否する。通常の definition-side
generalization は `Set` から binder list を構成するため，この不変量を満たす。

## 型環境

パターンコンストラクタ，パターン関数 header，検査済みパターン関数を別の環境に置く。

| 環境 | 内容 | 用途 |
|---|---|---|
| `patternEnv :: PatternTypeEnv` | 宣言済みパターンコンストラクタの `TypeScheme` | `PAT-CON` と primitive-pattern pattern の検査 |
| `patternFuncDeclEnv :: PatternTypeEnv` | body 検査前に収集した target-only header | 名前解決，前方参照，相互参照（自己参照は定義時に拒否） |
| `patternFuncEnv :: PatternFunctionEnv` | body 検査に成功した canonical `DualScheme` | 通常の named pattern-function application |
| `typeEnv :: TypeEnv` | 通常の式の型と canonical scheme の target projection | 式側の型推論 |

`PatternFunctionEnv` は次の専用環境である。

```haskell
newtype PatternFunctionEnv = PatternFunctionEnv
  { unPatternFunctionEnv :: Map String DualScheme
  }
```

`EnvBuilder` が先に収集する `PatternTypeEnv` は header-only であり，検査済みパターン関数の
型を表すものではない。header には capability 契約がないため，canonical scheme として
扱ってはならない。

別の load unit で同名のパターン関数を再定義するときは，prepass で新しい header を
登録すると同時に古い finalized `DualScheme` を無効化する。新しい body の検査に成功した
場合だけ新しい scheme を登録し直す。permissive mode で body 検査に失敗した場合も古い
scheme を復活させず，unchecked な runtime body は header-only のままにする。同じ expanded
load unit 内に同名宣言が複数ある場合は，静的環境と runtime binding の選択を曖昧にしない
ため宣言自体を拒否する。

## 定義の型検査

`inferITopExpr (IPatternFunctionDecl ...)` は次の順序で処理する。

1. 引数型と結果型の注釈を skolemize し，注釈境界を rigid に検査する。
2. 各パラメータに fresh capability を割り当て，注釈 target と組にして `Dual` を作る。
3. 本体に残る非 core パターン形式と context 条件を compatibility inventory で検査し，
   option が有効なら extension boundary を warning として報告する。
4. `inferPatfunParamDuals` にパラメータ dual を置き，本体を結果 target に対して推論する。
   本体中の `IVarPat` は対応するパラメータの capability と target を同時に参照する。
5. 本体の最終 capability と結果 target を結果 `Dual` にする。
6. 最終 substitution を引数 dual と結果 dual の双方へ適用し，注釈 skolem を戻す。
7. capability 変数と通常型変数をそれぞれ周囲の環境に対して一般化し，一つの
   `DualScheme` を作る。
8. canonical scheme を `PatternFunctionEnv` に保存する。
9. 式側で必要な通常関数型は `dualSchemeTargetScheme` で canonical scheme から射影し，
   header 環境と通常の `TypeEnv` を更新する。

target projection は常に canonical `DualScheme` から再計算し，binder と引数・結果の相関は
`DualScheme` を正本として維持する。

### パラメータ線形性

各パラメータは本体中で次の条件を満たす。

- ちょうど一回使う。
- 宣言順に使う。
- or，loop，not，forall など，実行経路によって展開回数が変わる位置では使わない。

この条件により，パターン関数適用時に引数を左から右へ一回ずつ展開できる。違反は
`PatternFunctionLinearityError` または `PatternFunctionParamUnderBranchError` とする。

## 適用の型推論

### 検査済み named application

名前解決前の `IInductiveOrPApplyPat name args` が finalized `PatternFunctionEnv` の
`name` に解決された場合は，`PAT-APP` の直接経路を使う。この named surface form と，
任意の式を関数部分に持てる明示的な `IPApplyPat` は区別する。

1. `DualScheme` の capability binders と target binders を一度だけ fresh にする。
2. 同じ二-sort substitution を全 argument dual と result dual に適用する。
3. 引数個数を検査する。
4. result target を適用位置の expected target と単一化する。
5. 引数パターンを左から右へ推論し，各 argument target と照合する。
6. 各引数の実際の capability を，対応する argument capability と capability solver で照合する。
7. 同じ最終 substitution を result capability と result target に適用し，適用全体の dual とする。

一つの paired substitution を全 argument/result dual に共有するため，capability と target の
相関を適用の最後まで維持する。この通常経路は
`--type-pm-compatibility-warnings` を有効にしても，パターン関数であることを理由に warning を出さない。

### 名前の曖昧性解消

`IInductiveOrPApplyPat name args` は次の優先順位で解決する。

1. finalized `PatternFunctionEnv` にあれば，検査済み named application とする。
2. header-only `patternFuncDeclEnv` にあれば，前方・相互参照の application とする。
3. それ以外はパターンコンストラクタとして `patternEnv` を参照する。

### compatibility fallback

canonical `DualScheme` を利用できない次の二経路だけは，Egison 拡張として target-only の
既存経路を使用する。

- body 検査がまだ完了していない header-only の前方・相互参照
- 明示的な `IPApplyPat` による expression-headed application

これらは `--type-pm-compatibility-warnings` が有効なときに warning を出す。header の
target 型は名前解決と通常型検査には使えるが，構造 capability の証拠にはならない。
明示的な `IPApplyPat` は変数ヘッドであっても通常の式環境から推論するため，同名の局所束縛を
正しく優先する。top-level pattern function と同じ綴りであることだけを理由に canonical
`DualScheme` dispatch へ切り替えない。
finalized named application や定義そのものには，未翻訳という一律 warning を出さない。
ただし，定義 body に predicate，indexed，loop などの非 core パターン形式が残る場合は，
その形式について定義時に warning を出す。これにより，finalized scheme の後続適用が
body の proof boundary を隠すことを防ぐ。

同じ定義名への直接または式・matcher 内に入れ子になった自己呼び出しは fallback に入れず，
PATFUN-DEF の nonrecursive side condition として拒否する。明示的な変数ヘッドが同名の
pattern-local let，lambda parameter，match binder などにより lexical に shadow されている場合は，
自己呼び出しとして扱わない。

## 評価

型情報を除去した後，パターン関数は `IPatternFuncExpr` として通常の定義と同じ
`recursiveBind` に参加する。

```haskell
data IExpr
  = IPatternFuncExpr [String] IPattern

data EgisonValue
  = PatternFunc Env [String] IPattern
```

`PatternFunc` は `recursiveBind` 後の環境を capture するため，通常定義，型クラス辞書，
他のパターン関数を参照できる。適用時にはパラメータ名と引数パターンを対応付け，capture
した環境で本体を評価する。`DualScheme` は静的検査用であり，この runtime 表現を変更しない。

## 実装箇所

- `Type/Types.hs`: `Dual`，`DualScheme`，自由変数計算，target projection
- `Type/Subst.hs`: capability/target の独立した代入と `applySubstDual`
- `Type/Env.hs`: `PatternFunctionEnv` とその操作
- `Type/Infer.hs`: 定義の generalization，適用の simultaneous instantiation，fallback warning
- `EvalState.hs`: header-only 環境と finalized 環境の永続化
- `IExpr.hs`: `TIPatternFunctionDecl` に canonical `DualScheme` を保持
- `Pretty.hs` / `Type/Pretty.hs`: typed declaration と dual scheme の表示
- `Eval.hs` / `Core.hs`: runtime binding と適用

## 検証観点

- 定義後の `PatternFunctionEnv` が引数・結果の全 dual を保持する。
- 多相な named application ごとに capability と target の両 binder が fresh になる。
- capability/target の重複 binder を instantiation 境界で拒否する。
- 同一 application 内では，全引数と結果が同じ instantiation を共有する。
- result target，argument target，argument capability の不一致をそれぞれ拒否する。
- finalized named application は compatibility warning を出さない。
- header-only の前方・相互参照および expression-headed application は option 有効時だけ warning を出す。
- 非 core パターン形式を含む body は，scheme を保持しつつ定義時に option-controlled warning を出す。
- 再定義の prepass は古い finalized scheme を無効化し，検査成功時だけ置き換える。
- 同じ expanded load unit の重複宣言を拒否する。
- パラメータ線形性と左から右の束縛順を維持する。

## 境界

header-only の前方・相互参照と expression-headed application は，canonical capability 契約を持たないため，
機械化された core との直接対応を主張しない。高階パターン関数を直接経路へ含めるには，
式の型にも `DualScheme` を保存して受け渡す別の設計が必要である。
