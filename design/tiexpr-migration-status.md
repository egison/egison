# TIExpr再帰構造への移行状況

## 目的
TIExprを再帰的な構造に変更し、各部分式が型情報を持つようにする。
これにより、TypeClassExpand時に再度型推論を実行する必要がなくなる。

## 完了した作業

### 1. データ構造の定義 (IExpr.hs)
- ✅ `TIExprNode` データ型を定義
- ✅ `TIExpr` を再帰的構造に変更: `TIExpr { tiScheme :: TypeScheme, tiExprNode :: TIExprNode }`
- ✅ 全てのIExprコンストラクタに対応するTI*コンストラクタを作成
- ✅ `stripType` 関数を新しい構造に対応
- ✅ エクスポートリストに新しい型を追加

### 2. Infer.hsの変更
- ✅ インポートリストに `TIExpr`, `TIExprNode`, `TIBindingExpr`, `TIMatchClause`, `TIPatternDef` を追加
- ✅ ヘルパー関数 `mkTIExpr :: Type -> TIExprNode -> TIExpr` を追加
- ✅ `inferIExpr` の型シグネチャを変更: `IExpr -> Infer (TIExpr, Subst)`
- ✅ `inferIExprWithContext` の型シグネチャを変更

#### 変更完了したケース:
- ✅ Constants (IConstantExpr)
- ✅ Variables (IVarExpr)
- ✅ Tuples (ITupleExpr)
- ✅ Collections (ICollectionExpr)
- ✅ Cons (IConsExpr)
- ✅ Join (IJoinExpr)
- ✅ Hash (IHashExpr)
- ✅ Vector (IVectorExpr)
- ✅ Lambda (ILambdaExpr)
- ✅ Application (IApplyExpr)
- ✅ If (IIfExpr)
- ✅ Let (ILetExpr)
- ✅ LetRec (ILetRecExpr)
- ✅ `inferIApplicationWithContext` - 関数適用の推論
- ✅ `wrapWithTensorMap` - tensorMap自動挿入
- ✅ `inferIBindingsWithContext` - Let束縛の推論
- ✅ `inferIRecBindingsWithContext` - LetRec束縛の推論

残りのエラー数: 0個（Infer.hs完了）✅

**TypeClassExpand.hsとEval.hsの対応が必要**

### 3. Pretty.hsの変更
- ✅ `TIExpr` の Pretty インスタンスを更新（stripTypeを使用）

## 未完了の作業

### Infer.hsの残りのケース
以下のケースはまだ古い型シグネチャ `(IExpr, Type, Subst)` を返している：

- ⏳ Let expressions (ILetExpr, ILetRecExpr)
- ⏳ WithSymbols (IWithSymbolsExpr)
- ⏳ Pattern matching (IMatchExpr, IMatchAllExpr, IMatcherExpr)
- ⏳ Inductive data (IInductiveDataExpr)
- ⏳ Quote expressions (IQuoteExpr, IQuoteSymbolExpr)
- ⏳ Indexed expressions (IIndexedExpr, ISubrefsExpr, ISuprefsExpr, IUserrefsExpr)
- ⏳ WedgeApply (IWedgeApplyExpr)
- ⏳ Do expressions (IDoExpr)
- ⏳ Sequence (ISeqExpr)
- ⏳ Memoized Lambda (IMemoizedLambdaExpr)
- ⏳ Cambda (ICambdaExpr)
- ⏳ Tensor operations (IGenerateTensorExpr, ITensorExpr, ITensorContractExpr, ITensorMapExpr, ITensorMap2Expr, ITransposeExpr, IFlipIndicesExpr)
- ⏳ Function reference (IFunctionExpr)

### ヘルパー関数
- ⏳ `inferIBindings` / `inferIBindingsWithContext`
- ⏳ `inferIRecBindings` / `inferIRecBindingsWithContext`
- ⏳ `inferMatchClauses`
- ⏳ `inferMatchClause`
- ⏳ `inferIPattern`
- ⏳ その他のパターンマッチ関連関数

### 他のモジュール
- ⏳ Eval.hs - `iTopExprToTITopExprFromScheme` の修正
- ⏳ TypeClassExpand.hs - TIExprNodeに対応
- ⏳ TypeTensorExpand.hs - TIExprNodeに対応
- ⏳ Desugar.hs - 必要に応じて対応

## 変更パターン

基本的な変更パターン:

```haskell
-- 変更前
IFooExpr子式 -> do
  (子式', 子式Type, s1) <- inferIExprWithContext 子式 ctx
  ...
  return (IFooExpr 子式', resultType, finalS)

-- 変更後  
IFooExpr子式 -> do
  (子式TI, s1) <- inferIExprWithContext 子式 ctx
  let 子式Type = tiExprType 子式TI
  ...
  return (mkTIExpr resultType (TIFooExpr 子式TI), finalS)
```

## 次のステップ

1. Infer.hsの残りのケースを変更（優先度順）:
   - Let/LetRec（重要度高）
   - Match/MatchAll（重要度高）
   - Tensor operations（型クラス展開で必要）
   - その他のケース

2. ヘルパー関数の変更

3. 他のモジュールの対応

4. テストして動作確認

## 注意事項

- 型推論時に制約は`InferState`に蓄積される
- 各`TIExpr`ノードは現時点では空の制約リスト `Forall [] [] type` を持つ
- トップレベル定義でのみ制約を集約する設計
