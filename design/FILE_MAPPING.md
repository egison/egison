# ファイルマッピング: 処理フェーズと実装ファイルの対応

このドキュメントは、Egisonの各処理フェーズがどのファイルで実装されているかを示します。

## フェーズ別ファイル構成

### Phase 0: 構文解析
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `hs-src/Language/Egison/Parser/NonS.hs` | パーサー実装 | `parseTopExprs`, `parseTopExpr`, `expr` |
| `hs-src/Language/Egison/AST.hs` | ASTデータ型定義 | `TopExpr`, `Expr`, `Pattern` |

### Phase 1: ファイル読み込み
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `hs-src/Language/Egison/Eval.hs` | ファイル読み込み・評価制御 | `expandLoads`, `loadEgisonFile` |

### Phase 2: 環境構築
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `hs-src/Language/Egison/EnvBuilder.hs` | 環境構築 | `buildEnvironments`, `EnvBuildResult` |
| `hs-src/Language/Egison/Type/Env.hs` | 型環境 | `TypeEnv`, `ClassEnv` |
| `hs-src/Language/Egison/EvalState.hs` | 評価状態 | `EvalState`, `InstanceEnv` |

### Phase 3-4: Desugar (構文糖衣展開)
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `hs-src/Language/Egison/Desugar.hs` | メインのDesugar処理 | `desugar :: Expr -> EvalM IExpr` |
| `hs-src/Language/Egison/IExpr.hs` | 内部表現データ型 | `IExpr`, `ITopExpr`, `TIExpr`, `TITopExpr` |
| `hs-src/Language/Egison/RState.hs` | Desugar用の状態管理 | `fresh` (変数生成) |

### Phase 5-6: 型推論
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `hs-src/Language/Egison/Type/Infer.hs` | IExpr型推論（統合モジュール） | `inferIExpr :: IExpr -> Infer (Type, Subst)`, `Infer`モナド |
| `hs-src/Language/Egison/Type/Unify.hs` | 単一化 | `unify` |
| `hs-src/Language/Egison/Type/Subst.hs` | 型代入 | `Subst`, `applySubst` |
| `hs-src/Language/Egison/Type/Types.hs` | 型データ型 | `Type`, `TypeScheme`, `Constraint` |
| `hs-src/Language/Egison/Type/Error.hs` | 型エラー | `TypeError`, `TypeWarning` |
| `hs-src/Language/Egison/Type/Env.hs` | 型環境 | `TypeEnv`, `extendEnv`, `lookupEnv` |

### Phase 7: Type Attachment (型情報の付与)
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `hs-src/Language/Egison/Eval.hs` | 型情報の付与 | `iTopExprToTITopExprFromScheme` |

### Phase 8: TypedDesugar (型駆動の変換)
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `hs-src/Language/Egison/Type/TypedDesugar.hs` | 型駆動変換のオーケストレーション | `desugarTypedExprT :: TIExpr -> EvalM TIExpr` |
| `hs-src/Language/Egison/Type/TensorMapInsertion.hs` | tensorMap自動挿入（第1ステップ） | `insertTensorMaps :: TIExpr -> EvalM TIExpr` |
| `hs-src/Language/Egison/Type/TypeClassExpand.hs` | 型クラスメソッド展開（第2ステップ） | `expandTypeClassMethodsT :: TIExpr -> EvalM TIExpr` |

### Phase 9-10: 評価
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `hs-src/Language/Egison/Core.hs` | 評価エンジン | `evalExpr`, `evalExprShallow`, `patternMatch` |
| `hs-src/Language/Egison/Data.hs` | 評価結果の型 | `EgisonValue` |
| `hs-src/Language/Egison/Primitives.hs` | 組み込み関数 | 算術演算、リスト操作など |

## 補助モジュール

| ファイル | 役割 |
|---------|------|
| `hs-src/Language/Egison/CmdOptions.hs` | コマンドラインオプション定義 |
| `hs-src/Language/Egison/Pretty.hs` | Pretty printing (IExpr, TIExpr, IPattern等) |
| `hs-src/Language/Egison/Type/Pretty.hs` | 型のPretty printing |
| `hs-src/Language/Egison/Match.hs` | パターンマッチング |
| `hs-src/Language/Egison/Math/*.hs` | 数式処理 |
| `hs-src/Language/Egison/Tensor/*.hs` | テンソル計算 |

## コマンドラインオプションとの対応

| オプション | フェーズ | 出力内容 | 実装箇所 |
|-----------|---------|----------|---------|
| `--dump-loads` | Phase 1 | 読み込んだファイル一覧 | `Eval.hs` |
| `--dump-env` | Phase 2 | 構築された環境 | `Eval.hs` |
| `--dump-desugared` | Phase 3-4 | `ITopExpr` (脱糖後) | `Eval.hs` + `Pretty.hs` |
| `--dump-typed` | Phase 5-6 | `TITopExpr` (型推論後) | `Eval.hs` + `Pretty.hs` |
| `--dump-ti` | Phase 8 | `TITopExpr` (TypedDesugar後) | `Eval.hs` + `Pretty.hs` |
| `--verbose` | 全て | 各段階の詳細 | `Eval.hs` |

すべてのダンプ出力はPretty Printされます（`Pretty.hs`）。

## データフロー概要

```
┌─────────────────────────────────────────────────────────────┐
│ Phase 0: Parser (Parser/NonS.hs)                            │
│   TopExpr, Expr, Pattern                                    │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 1: expandLoads (Eval.hs)                              │
│   [TopExpr]                                                 │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 2: buildEnvironments (EnvBuilder.hs)                  │
│   TypeEnv, ClassEnv, ConstructorEnv, PatternConstructorEnv, │
│   PatternFuncEnv                                            │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 3-4: Desugar (Desugar.hs)                             │
│   TopExpr → ITopExpr, Expr → IExpr                          │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 5-6: Type Inference (Infer.hs)                        │
│   IExpr → (Type, Subst)                                     │
│   Tensor Applicationの展開                                  │
│   基盤: Unify.hs, Subst.hs, Types.hs                        │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 7: Type Attachment (Eval.hs)                          │
│   IExpr + TypeScheme → TIExpr                               │
│   型スキーム（型変数・制約・型）を保持                        │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 8: TypedDesugar (TypedDesugar.hs)                     │
│   TIExpr → TIExpr                                           │
│   1. insertTensorMaps (TensorMapInsertion.hs)               │
│      - 必要に応じてtensorMap/tensorMap2を挿入               │
│   2. expandTypeClassMethods (TypeClassExpand.hs)            │
│      - 型クラスメソッド → 具体的な関数に展開                 │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓【型情報を抜く】
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 9-10: Evaluation (Core.hs)                            │
│   IExpr → EgisonValue                                       │
│   補助: Primitives.hs, Match.hs                             │
│   注: 型情報なしのIExprを評価（元のevalロジックをそのまま使用）│
└─────────────────────────────────────────────────────────────┘
```

## 設計上のポイント

### 評価時はIExpr（型情報なし）を使用

Phase 9-10の評価では、TIExprではなく型情報を抜いたIExprを使用する。
`Core.hs`のevalExprはIExprベースで実装されており、型情報は実行時には不要なため、
TypedDesugar後にIExprへ変換してから評価する。

### Phase 8 の処理順序

TypedDesugarではtensorMap挿入を型クラスメソッド展開より先に行う。
tensorMap挿入後に引数の型（スカラー vs テンソル）が確定するため、
その後のunifyStrictを使ったインスタンス選択が正しく動作する。
