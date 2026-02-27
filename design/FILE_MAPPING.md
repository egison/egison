# ファイルマッピング: 処理フェーズと実装ファイルの対応

このドキュメントは、Egisonの各処理フェーズがどのファイルで実装されているかを示します。

## 現在の実装

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
| ファイル | 役割 | 主要な型・関数 | 状態 |
|---------|------|---------------|------|
| `hs-src/Language/Egison/Eval.hs` | 型情報の付与 | `iTopExprToTITopExprFromScheme` | ✅ |

### Phase 8: TypedDesugar (型駆動の変換)
| ファイル | 役割 | 主要な型・関数 | 状態 |
|---------|------|---------------|------|
| `hs-src/Language/Egison/Type/TypedDesugar.hs` | 型駆動変換のオーケストレーション | `desugarTypedExprT :: TIExpr -> EvalM TIExpr` | ✅ 実装済み |
| `hs-src/Language/Egison/Type/TensorMapInsertion.hs` | tensorMap自動挿入（第1ステップ） | `insertTensorMaps :: TIExpr -> EvalM TIExpr` | ✅ 実装済み |
| `hs-src/Language/Egison/Type/TypeClassExpand.hs` | 型クラスメソッド展開（第2ステップ） | `expandTypeClassMethodsT :: TIExpr -> EvalM TIExpr` | ✅ 実装済み |

### Phase 9-10: 評価
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `hs-src/Language/Egison/Core.hs` | 評価エンジン | `evalExpr`, `evalExprShallow`, `patternMatch` |
| `hs-src/Language/Egison/Data.hs` | 評価結果の型 | `EgisonValue` |
| `hs-src/Language/Egison/Primitives.hs` | 組み込み関数 | 算術演算、リスト操作など |


## 重要な設計判断

### 評価時はIExpr（型情報なし）を使用

**決定**: Phase 9-10の評価では、TIExprではなく**型情報を抜いたIExpr**を使用する。

**理由**:
1. **元のevalロジックをそのまま使用可能**: `Core.hs`のevalExprはIExprベースで実装されており、変更不要
2. **実行時の最適化**: 型情報は実行時には不要なため、メモリ効率が向上
3. **TIExprの役割を明確化**: TIExprは`--dump-typed`や将来の`TypedDesugar`のためだけに必要

**実装**: `Eval.hs`のevalExpandedTopExprsTyped'内で、型推論後に`IExpr`を直接評価する。

## コマンドラインオプションとの対応

| オプション | フェーズ | 出力内容 | 実装箇所 | 状態 |
|-----------|---------|----------|---------|------|
| `--dump-loads` | Phase 1 | 読み込んだファイル一覧 | `Eval.hs` | ✅ |
| `--dump-env` | Phase 2 | 構築された環境 | `Eval.hs` | ✅ |
| `--dump-desugared` | Phase 3-4 | `ITopExpr` (脱糖後) | `Eval.hs` + `Pretty.hs` | ✅ |
| `--dump-typed` | Phase 5-6 | `TITopExpr` (型推論後) | `Eval.hs` + `Pretty.hs` | ✅ |
| `--dump-ti` | Phase 8 | `TITopExpr` (TypedDesugar後) | `Eval.hs` + `Pretty.hs` | ✅ 実装済み |
| `--verbose` | 全て | 各段階の詳細 | `Eval.hs` | ✅ |

**注**: すべてのダンプ出力はPretty Printされます（`Pretty.hs`）。

## 補助モジュール

| ファイル | 役割 |
|---------|------|
| `hs-src/Language/Egison/CmdOptions.hs` | コマンドラインオプション定義 |
| `hs-src/Language/Egison/Pretty.hs` | Pretty printing (IExpr, TIExpr, IPattern等) |
| `hs-src/Language/Egison/Type/Pretty.hs` | 型のPretty printing |
| `hs-src/Language/Egison/Match.hs` | パターンマッチング |
| `hs-src/Language/Egison/Math/*.hs` | 数式処理 |
| `hs-src/Language/Egison/Tensor/*.hs` | テンソル計算 |

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
│   TypeEnv, ClassEnv, ConstructorEnv, PatternConstructorEnv, PatternFuncEnv│
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
│ Phase 5-6: Type Inference (Infer.hs)                       │
│   IExpr → (Type, Subst)                                     │
|   Tensor Applicationの展開
│   基盤: Unify.hs, Subst.hs, Types.hs                       │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 7: Type Attachment (Eval.hs)                          │
│   IExpr + TypeScheme → TIExpr (--dump-typed用)              │
│   型スキーム（型変数・制約・型）を保持                        │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 8: TypedDesugar (TypedDesugar.hs)                    │
│   TIExpr → TIExpr                                          │
│   1. insertTensorMaps (TensorMapInsertion.hs) ✅           │
│      - 必要に応じてtensorMap/tensorMap2を挿入               │
│   2. expandTypeClassMethods (TypeClassExpand.hs) ✅        │
│      - 型クラスメソッド → 具体的な関数に展開                 │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓【型情報を抜く】
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 9-10: Evaluation (Core.hs)                            │
│   IExpr → EgisonValue                                      │
│   補助: Primitives.hs, Match.hs                             │
│   注: 型情報なしのIExprを評価（最適化 + 元のevalをそのまま使用）│
└─────────────────────────────────────────────────────────────┘
```

## Phase 8 実装完了

### 実装済みの機能

**TypedDesugar (TypedDesugar.hs)**: ✅ 完了
- 型クラスメソッド展開とtensorMap挿入のオーケストレーション

**TypeClassExpand (TypeClassExpand.hs)**: ✅ 完了
- TIExpr版の実装完了
- 型クラスメソッド → 具体的な関数への展開
- 制約を含む式の適切な処理

**TensorMapInsertion (TensorMapInsertion.hs)**: ✅ 完了
- tensorMap/tensorMap2の自動挿入
- 複数テンソル引数の検出とtensorMap2への最適化
- 型クラス展開前に実行され、引数の型を確定

### 処理順序
1. tensorMap挿入（必要に応じてtensorMap/tensorMap2を挿入）
2. 型クラスメソッド展開（型クラスメソッド → 具体的な関数）

この順序により、tensorMap挿入後に引数の型（スカラー vs テンソル）が確定し、
型クラス展開でunifyStrictを使ったインスタンス選択が正しく動作する。


   

