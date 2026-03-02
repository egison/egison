# ファイルマッピング: 処理フェーズと実装ファイルの対応

このドキュメントは、Egisonの各処理フェーズがどのファイルで実装されているかを示します。

## フェーズ別ファイル構成

### Phase 0: 構文解析
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `Parser/NonS.hs` | パーサー実装 | `parseTopExprs`, `parseTopExpr`, `expr` |
| `AST.hs` | ASTデータ型定義 | `TopExpr`, `Expr`, `Pattern`, `ClassDecl`, `InstanceDecl` |

### Phase 1: ファイル読み込み
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `Eval.hs` | ファイル読み込み・評価制御 | `expandLoads`, `loadEgisonFile` |

### Phase 2: 環境構築
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `EnvBuilder.hs` | 環境構築。ClassDecl/InstanceDeclからClassEnv/TypeEnvを構築 | `buildEnvironments`, `EnvBuildResult` |
| `Type/Env.hs` | 型環境・クラス環境のデータ型と操作 | `TypeEnv`, `ClassEnv`, `ClassInfo`, `InstanceInfo` |
| `EvalState.hs` | 評価状態。MonadEvalクラスで環境アクセスを抽象化 | `EvalState`, `MonadEval`, `getClassEnv`, `getTypeEnv` |

### Phase 3-4: Desugar (構文糖衣展開)
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `Desugar.hs` | TopExpr→ITopExprへの変換。クラス/インスタンス宣言の辞書生成も担当 | `desugarTopExpr`, `desugar :: Expr -> EvalM IExpr` |
| `IExpr.hs` | 内部表現のデータ型定義（IExpr系 + TIExpr系） | `IExpr`, `ITopExpr`, `TIExpr`, `TIExprNode`, `TITopExpr`, `mapTIExprChildren`, `stripType` |
| `RState.hs` | Desugar用の状態管理 | `fresh` (フレッシュ変数生成) |

**Desugar.hs の具体的な処理**:
- `ClassDeclExpr` → 辞書渡しラッパー関数を生成 (`classEqEq` 等)
- `InstanceDeclExpr` → メソッド関数 + 辞書ハッシュテーブルを生成。辞書にはスーパークラスへの参照 (`__super_ClassName`) も含む
- `DefineWithType` → 型注釈付き定義を通常の定義に変換
- 各種構文糖衣の展開（ラムダ省略、中置演算子等）

### Phase 5-6: 型推論
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `Type/Infer.hs` | IExpr型推論（統合モジュール）。制約収集、スーパークラス展開、TIExpr生成を含む | `inferIExpr`, `inferITopExpr`, `Infer`モナド, `InferState`, `expandSuperclasses`, `addConstraints` |
| `Type/Unify.hs` | 型単一化。TensorHandlingモードで3種の単一化を統合 | `unify`, `unifyStrict`, `unifyWithTopLevel`, `unifyWithConstraints`, `TensorHandling(..)` |
| `Type/Subst.hs` | 型代入 | `Subst`, `applySubst`, `composeSubst` |
| `Type/Types.hs` | 型データ型定義 | `Type(..)`, `TypeScheme(..)`, `Constraint(..)`, `ClassInfo`, `InstanceInfo` |
| `Type/Error.hs` | 型エラー・型警告の定義 | `TypeError(..)`, `TypeWarning(..)`, `formatTypeError` |
| `Type/Env.hs` | 型環境の操作 | `TypeEnv`, `ClassEnv`, `PatternTypeEnv`, `extendEnv`, `lookupEnv`, `lookupClass`, `lookupInstances` |
| `Type/Instance.hs` | インスタンス検索 | `findMatchingInstanceForType` |

**型推論の内部処理**:
- Hindley-Milner型推論 + 型クラス制約の収集
- `addConstraints` でスーパークラスを自動展開（`expandSuperclasses`）
- 推論結果から `TIExpr` (型付き内部表現) を直接生成
- `resolveConstraintsInTIExpr` で型変数の具体化後に制約を解決

### Phase 7: TypedDesugar (型駆動の変換)

（旧Phase 8。旧Phase 7 "Type Attachment" は Phase 5-6 に統合されたため欠番となり繰り上げ）

Phase 7は2つのサブフェーズに分かれる。TensorMap挿入を先に行い、その後で型クラス展開を行う（引数の型が確定した後でインスタンス選択を行うため）。

| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `Type/TypedDesugar.hs` | Phase 7のオーケストレーション | `desugarTypedTopExprT_TensorMapOnly`, `desugarTypedTopExprT_TypeClassOnly` |
| `Type/TensorMapInsertion.hs` | Phase 7a: tensorMap自動挿入 | `insertTensorMaps`, `shouldInsertTensorMap` |
| `Type/TypeClassExpand.hs` | Phase 7b: 型クラスメソッド展開（辞書パッシング） | `expandTypeClassMethodsT`, `addDictionaryParametersT`, `applyConcreteConstraintDictionaries` |
| `Type/EtaExpansion.hs` | Eta展開（dead code。同等機能がTypeClassExpand内に実装済み） | `etaExpandTITopExpr` |

**Phase 7a (TensorMapInsertion)**: スカラー型パラメータにテンソル引数が渡された場合、自動的に `tensorMap` / `tensorMap2` を挿入する。

**Phase 7b (TypeClassExpand)**: 型クラスメソッド呼び出しを辞書ベースのディスパッチに変換する。

TypeClassExpand の処理パイプライン（TIDefine の場合）:
1. `expandTypeClassMethodsT` — メソッド呼び出しを辞書アクセスに変換、制約付き変数に辞書引数を適用
2. `applyConcreteConstraintDictionaries` — 具体型の制約を持つ式に辞書を解決・適用
3. `addDictionaryParametersT` — 制約付き関数定義にラムダで辞書パラメータを追加

辞書パッシングの設計（Haskellスタイル・ネスト辞書）:
- 1制約につき1辞書パラメータ（スーパークラスは辞書内の `__super_*` 参照で辿る）
- `deExpandConstraints` で展開された制約を最小セットに戻す
- `findSuperclassPath` + `buildSuperclassChain` でメソッドアクセスチェーンを生成
- `remapDictRefsInBody` で旧辞書参照名を新チェーンアクセスにリマップ

**EtaExpansion (dead code)**:
`EtaExpansion.hs` は型クラスメソッドの eta 展開を行う独立モジュールとして設計されたが、同等の機能が `TypeClassExpand.hs` の `replaceMethodCallsInNode`（`TIVarExpr` ケース）に組み込まれている。スタンドアロンのメソッド参照（例: `foldl (+) 0 xs` の `(+)`）は TypeClassExpand 内で自動的に `\etaVar1 etaVar2 -> dictAccess etaVar1 etaVar2` に展開される。

### Phase 8-9: 束縛と評価
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `Core.hs` | 評価エンジン | `evalExprShallow`, `evalExprDeep`, `patternMatch` |
| `Match.hs` | パターンマッチング | マッチングステート管理 |
| `Data.hs` | 評価結果の型定義 | `EgisonValue`, `WHNFData`, `Env`, `EvalM` |
| `Data/Collection.hs` | コレクション操作ユーティリティ | `expandCollection`, `unconsCollection` |
| `Data/Utils.hs` | データ操作ユーティリティ | `evalRef`, `makeTuple`, `updateHash` |
| `Tensor.hs` | テンソル計算 | テンソルの生成・縮約・変換 |
| `MList.hs` | モナド的遅延リスト | `MList`, `mmap`, `mconcat` |
| `Primitives.hs` | 組み込み関数のエントリポイント | プリミティブ関数テーブル |
| `Primitives/Arith.hs` | 算術プリミティブ | `i.+`, `i.*`, `i./`, `symbolNormalize` |
| `Primitives/String.hs` | 文字列プリミティブ | 文字列操作関数 |
| `Primitives/IO.hs` | IOプリミティブ | ファイル・ネットワーク操作 |
| `Primitives/Types.hs` | 型判定プリミティブ | `isInteger`, `isFloat` 等 |
| `Primitives/Utils.hs` | プリミティブユーティリティ | `oneArg`, `twoArgs`, `scalarBinaryOp` |

## 補助モジュール

| ファイル | 役割 |
|---------|------|
| `CmdOptions.hs` | コマンドラインオプション定義 (`EgisonOpts`) |
| `Pretty.hs` | Pretty printing (IExpr, TIExpr, IPattern等) |
| `Type/Pretty.hs` | 型のPretty printing (`prettyType`, `prettyTypeScheme`) |
| `VarEntry.hs` | 環境の変数エントリ型 (`VarEntry`) |
| `Type/Check.hs` | 組み込み型環境の提供 (`builtinEnv`) |
| `Type/Tensor.hs` | テンソル型の正規化 (`normalizeTensorType`) |
| `Type/Index.hs` | テンソル添字の型定義 (`IndexKind`, `IndexSpec`) |
| `Completion.hs` | REPL用コマンドライン補完 |

### 数式出力

| ファイル | 役割 |
|---------|------|
| `MathOutput.hs` | 数式出力のエントリポイント (`prettyMath`) |
| `PrettyMath/AST.hs` | 数式用ASTの定義 (`MathExpr`, `ToMathExpr` 型クラス) |
| `PrettyMath/Latex.hs` | LaTeX出力 |
| `PrettyMath/Mathematica.hs` | Mathematica出力 |
| `PrettyMath/Maxima.hs` | Maxima出力 |
| `PrettyMath/AsciiMath.hs` | AsciiMath出力 |

### 数式処理

| ファイル | 役割 |
|---------|------|
| `Math.hs` | 数式処理のエントリポイント |
| `Math/Expr.hs` | 数式データ型 (`ScalarData`, `PolyExpr`, `TermExpr`) |
| `Math/Arith.hs` | 数式の四則演算 (`mathPlus`, `mathMult`, `mathDiv`) |
| `Math/Normalize.hs` | 数式の正規化 (`mathNormalize` のHaskell側サポート) |
| `Math/Rewrite.hs` | 数式の書き換え規則 (`rewriteSymbol`) |

## コマンドラインオプションとの対応

| オプション | フェーズ | 出力内容 | 実装箇所 |
|-----------|---------|----------|---------|
| `--dump-loads` | Phase 1 | 読み込んだファイル一覧 | `Eval.hs` |
| `--dump-env` | Phase 2 | 構築された環境（型・クラス・コンストラクタ） | `Eval.hs` |
| `--dump-desugared` | Phase 3-4 | `ITopExpr` (脱糖後) | `Eval.hs` + `Pretty.hs` |
| `--dump-typed` | Phase 5-6 | `TITopExpr` (型推論後、TypedDesugar前) | `Eval.hs` + `Pretty.hs` |
| `--dump-ti` | Phase 7a | `TITopExpr` (TensorMap挿入後) | `Eval.hs` + `Pretty.hs` |
| `--dump-tc` | Phase 7b | `TITopExpr` (TypeClass展開後) | `Eval.hs` + `Pretty.hs` |
| `--verbose` | 全て | 各段階の詳細 | `Eval.hs` |

## データフロー詳細

```
┌─────────────────────────────────────────────────────────────┐
│ Phase 0: Parser (Parser/NonS.hs)                            │
│   String → TopExpr, Expr, Pattern                           │
│   ClassDeclExpr, InstanceDeclExpr も生成                    │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 1: expandLoads (Eval.hs)                              │
│   [TopExpr] → [TopExpr]                                     │
│   Load/LoadFile を再帰展開、重複排除                         │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 2: buildEnvironments (EnvBuilder.hs)                  │
│   [TopExpr] → EnvBuildResult                                │
│   出力: TypeEnv, ClassEnv, ConstructorEnv,                  │
│         PatternConstructorEnv, PatternFuncEnv                │
│   ClassDeclExpr → ClassInfo (classSupers, classMethods)     │
│   InstanceDeclExpr → InstanceInfo (instContext, instType)   │
│   DefineWithType → TypeScheme を TypeEnv に登録             │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 3-4: Desugar (Desugar.hs)                             │
│   TopExpr → Maybe ITopExpr                                  │
│   ClassDeclExpr → ラッパー関数 + レジストリ                  │
│   InstanceDeclExpr → メソッド関数 + 辞書ハッシュ             │
│     辞書にはスーパークラス参照 (__super_*) も含む            │
│   DefineWithType / Define → IDefine                         │
│   各種構文糖衣の展開                                        │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 5-6: Type Inference (Infer.hs)                        │
│   ITopExpr → (Maybe (TITopExpr, Subst), [TypeWarning])      │
│   Hindley-Milner + 型クラス制約収集                         │
│   スーパークラス自動展開 (expandSuperclasses)               │
│   TIExpr を直接生成（各ノードに TypeScheme を付与）          │
│   ※ 旧 Phase 7 (Type Attachment) はここに統合済み           │
│   基盤: Unify.hs, Subst.hs, Types.hs, Instance.hs          │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 7a: TensorMap Insertion (TensorMapInsertion.hs)       │
│   TITopExpr → Maybe TITopExpr                               │
│   スカラー型パラメータにテンソル引数が渡された場合に         │
│   tensorMap / tensorMap2 を自動挿入                          │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 7b: TypeClass Expand (TypeClassExpand.hs)             │
│   TITopExpr → Maybe TITopExpr                               │
│   1. expandTypeClassMethodsT                                │
│      メソッド呼び出し → 辞書アクセス（チェーン経由）         │
│      制約付き変数 → 辞書引数適用                             │
│   2. applyConcreteConstraintDictionaries                    │
│      具体型の制約 → 実辞書への解決                           │
│   3. addDictionaryParametersT                               │
│      制約付き関数 → 辞書パラメータ追加                       │
│      制約は deExpandConstraints で最小化                     │
│      辞書参照は remapDictRefsInBody でリマップ               │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓ stripTypeTopExpr: TITopExpr → ITopExpr（型情報を除去）
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 8: Definition Binding (Eval.hs)                       │
│   recursiveBindAll: 全定義を相互再帰的に束縛               │
│   値定義は Env に、パターン関数は PatFuncEnv に束縛         │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 9: Evaluation (Core.hs)                               │
│   IExpr → EgisonValue                                       │
│   遅延評価（WHNF）、パターンマッチング                      │
│   辞書アクセスは IIndexedExpr → refHash で実行              │
│   補助: Primitives.hs, Match.hs, Tensor.hs                 │
└─────────────────────────────────────────────────────────────┘
```

## 設計上のポイント

### 評価時はIExpr（型情報なし）を使用

Phase 8-9の束縛・評価では、TIExprではなく型情報を抜いたIExprを使用する。
`Core.hs`のevalExprはIExprベースで実装されており、型情報は実行時には不要なため、
TypedDesugar後にIExprへ変換してから評価する。

### Phase 7 の処理順序

TypedDesugarではtensorMap挿入を型クラスメソッド展開より先に行う。
tensorMap挿入後に引数の型（スカラー vs テンソル）が確定するため、
その後のunifyStrictを使ったインスタンス選択が正しく動作する。

### 辞書パッシング（Haskellスタイル）

型クラスの辞書はハッシュテーブルで表現される。各辞書はメソッドの実装に加え、スーパークラスの辞書への参照を `__super_ClassName` キーで保持する。関数の制約パラメータは `deExpandConstraints` で最小化され、メソッドアクセスはスーパークラスチェーン経由で行われる。

### EtaExpansion（dead code）

`Type/EtaExpansion.hs` は型クラスメソッドのeta展開を行う独立モジュールとして設計されたが、同等の機能が `TypeClassExpand.hs` 内に実装されているため使われていない。将来的に削除可能。
