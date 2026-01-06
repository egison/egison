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
| `hs-src/Language/Egison/PreDesugar.hs` | 【削除予定】型推論前の部分的Desugar | `preDesugarExpr` |
| `hs-src/Language/Egison/IExpr.hs` | 内部表現データ型 | `IExpr`, `ITopExpr` |
| `hs-src/Language/Egison/RState.hs` | Desugar用の状態管理 | `fresh` (変数生成) |

### Phase 5-6: 型推論（現在の実装）
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `hs-src/Language/Egison/Type/TypeInfer.hs` | 【現在】Expr → TypedExpr | `inferTypedExpr`, `inferTypedTopExpr` |
| `hs-src/Language/Egison/Type/TypedAST.hs` | 【現在】型付きAST (Exprベース) | `TypedExpr`, `TypedTopExpr` |
| `hs-src/Language/Egison/Type/Infer.hs` | Hindley-Milner型推論の基本 | `inferExpr`, `Infer`モナド |
| `hs-src/Language/Egison/Type/Unify.hs` | 単一化 | `unify` |
| `hs-src/Language/Egison/Type/Subst.hs` | 型代入 | `Subst`, `applySubst` |
| `hs-src/Language/Egison/Type/Types.hs` | 型データ型 | `Type`, `TypeScheme`, `Constraint` |
| `hs-src/Language/Egison/Type/Error.hs` | 型エラー | `TypeError`, `TypeWarning` |

### Phase 7-8: TypedDesugar (型駆動の変換)
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `hs-src/Language/Egison/Type/TypedDesugar.hs` | 型駆動の変換 | `desugarTypedExpr` |
| `hs-src/Language/Egison/Type/TypeClassExpand.hs` | 型クラス辞書展開 | 型クラスメソッド呼び出しの解決 |

### Phase 9-10: 評価
| ファイル | 役割 | 主要な型・関数 |
|---------|------|---------------|
| `hs-src/Language/Egison/Core.hs` | 評価エンジン | `evalExpr`, `evalExprShallow`, `patternMatch` |
| `hs-src/Language/Egison/Data.hs` | 評価結果の型 | `EgisonValue` |
| `hs-src/Language/Egison/Primitives.hs` | 組み込み関数 | 算術演算、リスト操作など |

## リファクタリング後の予定

### Phase 5-6: 型推論（リファクタリング後）
| ファイル | 役割 | 主要な型・関数 | 状態 |
|---------|------|---------------|------|
| `hs-src/Language/Egison/Type/IInfer.hs` | 【統合】IExpr型推論 | `inferIExpr :: IExpr -> Infer (Type, Subst)` | ✅完成 |
| `hs-src/Language/Egison/IExpr.hs` | TIExpr定義（既存） | `TIExpr`, `TITopExpr`（型情報付きIExpr） | ✅既存 |
| `hs-src/Language/Egison/Type/TypedIAST.hs` | - | - | ❌削除予定（不要・TIExprを使う） |
| `hs-src/Language/Egison/Type/Infer.hs` | - | - | ✅削除済み（IInfer.hsに統合） |
| `hs-src/Language/Egison/Type/TypeInfer.hs` | - | - | ✅削除済み |
| `hs-src/Language/Egison/Type/TypedAST.hs` | - | - | ✅削除済み |
| `hs-src/Language/Egison/PreDesugar.hs` | - | - | ✅削除済み |

### Phase 7-8: TypedDesugar（リファクタリング後）
| ファイル | 役割 | 主要な型・関数 | 状態 |
|---------|------|---------------|------|
| `hs-src/Language/Egison/Type/TypedDesugar.hs` | IExpr+Type → TIExpr | テンソルDesugar + 型クラス辞書渡し | 修正予定 |
| `hs-src/Language/Egison/Type/TypeClassExpand.hs` | 型クラス辞書展開（本格実装） | 辞書関数への変換 | 実装予定 |

## コマンドラインオプションとの対応

| オプション | フェーズ | 出力内容 | 実装箇所 |
|-----------|---------|----------|---------|
| `--dump-loads` | Phase 1 | 読み込んだファイル一覧 | `Eval.hs` |
| `--dump-env` | Phase 2 | 構築された環境 | `Eval.hs` |
| `--dump-desugared` | Phase 3-4 | IExpr (脱糖後) | `Eval.hs` |
| `--dump-typed` | Phase 5-6 | TypedIExpr (型推論後) | `Eval.hs` |
| `--dump-ti` | Phase 7-8 | TIExpr (TypedDesugar後) | `Eval.hs` |
| `--verbose` | 全て | 各段階の詳細 | `Eval.hs` |

## 補助モジュール

| ファイル | 役割 |
|---------|------|
| `hs-src/Language/Egison/CmdOptions.hs` | コマンドラインオプション定義 |
| `hs-src/Language/Egison/Pretty.hs` | Pretty printing |
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
│ Phase 5-6: Type Inference (IInfer.hs ← 作成予定)            │
│   IExpr → TIExpr                                        │
│   基盤: Infer.hs, Unify.hs, Subst.hs                       │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 7-8: TypedDesugar (TypedDesugar.hs)                   │
│   TIExpr → IExpr                                       │
│   補助: TypeClassExpand.hs                                  │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 9-10: Evaluation (Core.hs)                            │
│   IExpr → EgisonValue                                      │
│   補助: Primitives.hs, Match.hs                             │
└─────────────────────────────────────────────────────────────┘
```

## リファクタリングのチェックリスト

### 削除するファイル
- [ ] `hs-src/Language/Egison/PreDesugar.hs`
- [ ] `hs-src/Language/Egison/Type/Infer.hs` (IInfer.hsに統合)
- [ ] `hs-src/Language/Egison/Type/TypeInfer.hs`
- [ ] `hs-src/Language/Egison/Type/TypedAST.hs`

### 作成するファイル
- [x] `hs-src/Language/Egison/Type/IInfer.hs` (Infer.hsの内容も統合) ✅完成
- [ ] ~~`hs-src/Language/Egison/Type/TypedIAST.hs`~~ ❌不要（TIExprを使用）

### 修正するファイル
- [ ] `hs-src/Language/Egison/Type/TypedDesugar.hs` (TypedIExpr対応)
- [ ] `hs-src/Language/Egison/Eval.hs` (パイプライン変更)
- [ ] `egison.cabal` (モジュールリストの更新)

### テスト
- [ ] `mini-test/`ディレクトリの既存テストが動作すること
- [ ] `lib/core/base.egi`が正しく型チェックされること
- [ ] `--dump-desugared`, `--dump-typed`, `--dump-ti`が正しく動作すること


# 修正・追記したいこと

TypedAST.hsで定義されているTypedExprは、TIExprと役割が被るためいらない。
元のIExprの形を保存しているTypedExprよりもTIExprを使うべき。
TIExprは元のIExprの形に型をつけたものなので、元のDesugar.hsがほとんどそのまま使えるはず。

現在のTypeClassExpand.hsはほとんど何もしていない。
これから実装する必要がある。

TypedDesugar.hsでは、テンソル関連のDesugarの後、型クラスのクラス辞書を渡すDesugarを行う。

---

**注**: 上記の方針が正しいことが確認され、`REFACTORING_IEXPR_V2.md`に詳細を記載。
`TypedIAST.hs`（新規作成したもの）も同様に不要であり、既存の`TIExpr`を使用すべき。
