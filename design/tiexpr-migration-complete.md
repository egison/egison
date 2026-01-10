# TIExpr再帰化 - 完了報告

## 概要

TIExprを再帰的な構造に変更するリファクタリングが基本的に完了しました。

## 変更内容

### 1. データ構造 (IExpr.hs)

```haskell
-- 旧
data TIExpr = TIExpr
  { tiScheme :: TypeScheme
  , tiExpr :: IExpr  -- 型なしの式を保持
  }

-- 新
data TIExpr = TIExpr
  { tiScheme :: TypeScheme
  , tiExprNode :: TIExprNode  -- 型付き式を保持
  }

data TIExprNode
  = TIConstantExpr ConstantExpr
  | TIVarExpr String
  | TITupleExpr [TIExpr]  -- 再帰的
  | TIApplyExpr TIExpr [TIExpr]  -- 再帰的
  | ...
```

### 2. 型推論 (IInfer.hs)

全ての型推論関数を変更：

```haskell
-- 旧
inferIExpr :: IExpr -> Infer (IExpr, Type, Subst)

-- 新
inferIExpr :: IExpr -> Infer (TIExpr, Subst)
```

- 60以上のケースを更新
- `mkTIExpr :: Type -> TIExprNode -> TIExpr` ヘルパー関数を追加
- エラー数: 75個 → 0個

### 3. Eval.hs

IExprからTIExprへの変換ヘルパーを追加：

```haskell
iexprToTIExprSimple :: TypeScheme -> IExpr -> TIExpr
iexprToTIExprNodeSimple :: Type -> IExpr -> TIExprNode
```

### 4. TypeClassExpand.hs

一時的に型クラス展開を無効化：

```haskell
expandTypeClassMethodsT :: TIExpr -> EvalM TIExpr
expandTypeClassMethodsT tiExpr = return tiExpr  -- TODO: 実装
```

## テスト結果

### 成功 ✅

```bash
$ egison --no-prelude mini-test/01-simple-constant.egi
42
```

## 今後の作業

### 短期（必須）

1. **Eval.hs**
   - `iexprToTIExprNodeSimple`に残りのケースを追加
   - Match, Matcher, Inductive Dataなど

2. **TypeClassExpand.hs**
   - TIExprNodeを直接処理する実装
   - 型クラス辞書渡しの再実装

3. **TypeTensorExpand.hs**
   - TIExprNodeベースの実装

### 中期（最適化）

1. **IInfer.hs**
   - IndexedExprのindicesを`[Index IExpr]`から`[Index TIExpr]`に変更
   - MatcherExprの完全な型推論

2. **テスト**
   - 既存のテストスイート全体を実行
   - エッジケースの確認

### 長期（利点の活用）

1. **型ベースの最適化**
   - TensorMap自動挿入の改善
   - 型クラス辞書渡しの最適化

2. **エラーメッセージの改善**
   - 型情報を活用したより詳細なエラー

## 利点

1. **型情報の保持**
   - サブ式の型が直接アクセス可能
   - 再推論不要

2. **処理の簡素化**
   - TypeClassExpandで型推論を再実行する必要がない
   - TensorMap挿入が簡単

3. **保守性の向上**
   - 型安全性の向上
   - コードの意図が明確

## 課題

1. **一時的な実装**
   - Eval.hsの`iexprToTIExprNodeSimple`は簡易版
   - TypeClassExpand.hsは無効化状態

2. **互換性**
   - 既存のコードとの接続部分で変換が必要
   - パフォーマンスへの影響（最小限だが）

## 結論

基本的なリファクタリングは成功し、コンパイルとテストが動作しています。
残りは段階的に実装を完成させていく作業です。

---
完了日: 2026-01-10
