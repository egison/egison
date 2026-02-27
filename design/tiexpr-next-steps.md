# TIExpr再帰構造への移行 - 次のステップ

## 現在の状況 (2026-01-10)

### 完了した作業
1. **データ構造定義** (IExpr.hs)
   - TIExprNode型を定義
   - TIExpr を再帰的構造に変更
   - stripType関数を新しい構造に対応
   - エクスポートリストを更新

2. **Infer.hsの主要ケース**
   - Constants, Variables, Tuples, Collections
   - Cons, Join, Hash, Vector
   - Lambda, Application, If
   - Let, LetRec
   - inferIApplicationWithContext
   - wrapWithTensorMap
   - inferIBindingsWithContext
   - inferIRecBindingsWithContext

3. **Pretty.hs**
   - TIExprのPrettyインスタンスを更新

### 残りの作業 (約75エラー)

残りのケースは主に以下のカテゴリー：

#### 1. Matcher関連 (行476-529付近)
```haskell
IMatcherExpr, IMatchExpr, IMatchAllExpr
```
これらは複雑で、パターンマッチング関連のヘルパー関数も変更が必要：
- inferMatchClauses
- inferMatchClause  
- inferIPattern
- その他パターンマッチ関連

#### 2. Inductive Data (行806付近)
```haskell
IInductiveDataExpr
```

#### 3. Tensor操作 (行987-1053付近)
```haskell
ITensorMapExpr, ITensorMap2Expr, ITensorContractExpr,
IGenerateTensorExpr, ITensorExpr, ITransposeExpr, IFlipIndicesExpr
```

#### 4. その他の式
```haskell
ISeqExpr, IWithSymbolsExpr, ICambdaExpr, IMemoizedLambdaExpr,
IDoExpr, IQuoteExpr, IQuoteSymbolExpr,
IIndexedExpr, ISubrefsExpr, ISuprefsExpr, IUserrefsExpr,
IWedgeApplyExpr, IFunctionExpr
```

## 実装戦略

### ステップ1: シンプルなケース（優先度：高）
これらは比較的単純で、同じパターンで変更可能：
```haskell
-- Sequence
ISeqExpr e1 e2 -> do
  (e1TI, s1) <- inferIExprWithContext e1 ctx
  (e2TI, s2) <- inferIExprWithContext e2 ctx
  let t2 = tiExprType e2TI
  return (mkTIExpr t2 (TISeqExpr e1TI e2TI), composeSubst s2 s1)
```

同様のパターン：
- IWithSymbolsExpr
- IMemoizedLambdaExpr
- ICambdaExpr
- IDoExpr
- IQuoteExpr, IQuoteSymbolExpr
- IWedgeApplyExpr
- IFunctionExpr

### ステップ2: InductiveData & Indexed式（優先度：中）
これらも比較的単純：
- IInductiveDataExpr
- IIndexedExpr, ISubrefsExpr, ISuprefsExpr, IUserrefsExpr

### ステップ3: Tensor操作（優先度：中）
- ITensorMapExpr, ITensorMap2Expr
- ITensorContractExpr
- IGenerateTensorExpr, ITensorExpr
- ITransposeExpr, IFlipIndicesExpr

### ステップ4: Matcher & Match式（優先度：高、複雑度：高）
最も複雑なケース：
- IMatcherExpr - パターン定義の推論
- IMatchExpr - パターンマッチング
- IMatchAllExpr - 全マッチング

必要なヘルパー関数の変更：
- inferMatchClauses - マッチ節の推論
- inferMatchClause - 単一マッチ節の推論
- inferIPattern - パターンの推論
- その他のパターンマッチングヘルパー

## 次のコンテキストウィンドウでの作業手順

1. **シンプルなケースを一括変更**
   - ISeqExpr, IWithSymbolsExpr, IMemoizedLambdaExpr等
   - パターンが似ているので、まとめて変更可能

2. **Inductive & Indexed式を変更**
   - IInductiveDataExpr
   - Index系の式

3. **Tensor操作を変更**
   - 各Tensor操作式を順番に変更

4. **Match関連を変更**（最も時間がかかる）
   - まずヘルパー関数を変更
   - 次にメインのケースを変更

5. **inferITopExprを変更**
   - Infer.hsの最後の重要な関数

6. **他のモジュールを対応**
   - Eval.hs - iTopExprToTITopExprFromScheme
   - TypeClassExpand.hs
   - TypeTensorExpand.hs

7. **コンパイル & テスト**
   - 簡単なテストから実行
   - エラーを修正

## 変更パターンのテンプレート

### 単純な式（子式1個）
```haskell
-- 変更前
IFooExpr expr -> do
  (expr', ty, s) <- inferIExprWithContext expr ctx
  return (IFooExpr expr', processTy ty, s)

-- 変更後
IFooExpr expr -> do
  (exprTI, s) <- inferIExprWithContext expr ctx
  let ty = tiExprType exprTI
  return (mkTIExpr (processTy ty) (TIFooExpr exprTI), s)
```

### 複数の子式
```haskell
-- 変更前
IBarExpr e1 e2 -> do
  (e1', t1, s1) <- inferIExprWithContext e1 ctx
  (e2', t2, s2) <- inferIExprWithContext e2 ctx
  let s12 = composeSubst s2 s1
  return (IBarExpr e1' e2', combineTypes t1 t2, s12)

-- 変更後
IBarExpr e1 e2 -> do
  (e1TI, s1) <- inferIExprWithContext e1 ctx
  (e2TI, s2) <- inferIExprWithContext e2 ctx
  let t1 = tiExprType e1TI
      t2 = tiExprType e2TI
      s12 = composeSubst s2 s1
  return (mkTIExpr (combineTypes t1 t2) (TIBarExpr e1TI e2TI), s12)
```

## 注意事項

- 各ケースで型情報を`tiExprType`で取得
- `mkTIExpr`を使ってTIExprを構築
- 制約は現時点では空リスト（Forall [] [] type）
- トップレベル定義でのみ制約を集約

## 推定作業時間

- ステップ1: 30-45分（シンプルなケース）
- ステップ2: 15-20分（Inductive & Indexed）
- ステップ3: 20-30分（Tensor操作）
- ステップ4: 60-90分（Match関連）
- ステップ5-6: 30-45分（他のモジュール）
- ステップ7: 30-60分（テスト）

合計: 約3-5時間の作業

## 参考

- design/tiexpr-migration-status.md - 現在の詳細な状況
- design/implementation.md - 全体の処理フロー
- mini-test/ - 小さなテストケース
