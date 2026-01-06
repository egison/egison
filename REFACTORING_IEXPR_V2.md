# IExprベース型推論リファクタリング（方針修正版）

**日付**: 2026年1月6日  
**状態**: 方針修正後の新バージョン

## 🔄 方針変更の理由

コードレビューの結果、以下の問題が判明：

### 問題点
1. **`TypedIExpr`の重複**: 新規作成した`TypedIAST.hs`の`TypedIExpr`は、既存の`TIExpr`と役割が重複
2. **既存資産の活用不足**: `IExpr.hs`に既に`TIExpr`（型情報付きIExpr）が定義済み
3. **不要な複雑化**: `TypedIExpr`という中間表現を作ったことで、パイプラインが複雑化

### 解決策
**`TypedIExpr`を削除し、既存の`TIExpr`を直接使用**

## ✅ 修正後の処理フロー

### 新しいフロー（シンプル版）

```
TopExpr (構文解析結果)
  ↓
Phase 1: expandLoads
  ↓
Phase 2: 環境構築 (buildEnvironments)
  ↓
Phase 3-4: Desugar (TopExpr → ITopExpr, Expr → IExpr)
  ↓
Phase 5-6: Type Inference (IExpr → (Type, Subst))  ← TypedIExprではなくTypeのみ
  ↓
Phase 7: IExpr + Type → TIExpr（単純な変換）
  ↓
Phase 8: TypedDesugar
  ├─ テンソル関連のDesugar (tensorMap自動挿入)
  └─ 型クラス辞書渡し (TypeClassExpand.hs)
  ↓
Phase 9-10: 評価 (TIExpr → EgisonValue)
```

### 旧フロー（不要に複雑）

```
IExpr → IInfer → TypedIExpr → TypedDesugar → TIExpr
       (Phase 5-6)  (中間表現)   (Phase 7-8)
```

### 新フロー（シンプル）

```
IExpr → IInfer → (Type, Subst)
       (Phase 5-6)
         ↓
      IExpr + Type → TIExpr (単純変換)
         ↓
      TypedDesugar → TIExpr (型駆動変換)
       (Phase 7-8)
```

## 📝 必要な修正

### 1. ✅ 完了済みで再利用可能

#### `IInfer.hs`
- **現状**: `inferIExpr :: IExpr -> Infer (TypedIExpr, Subst)`
- **修正**: 戻り値を`(Type, Subst)`に変更（TypedIExprを生成しない）
- **状態**: 基本ロジックは完成しているので、戻り値の型のみ修正

### 2. ❌ 削除すべきファイル

#### `TypedIAST.hs`
- **理由**: `TIExpr`と重複、不要
- **代替**: `IExpr.hs`の`TIExpr`を使用

### 3. 🔧 修正すべきファイル

#### `Eval.hs`
**現在の実装**:
```haskell
(result, warnings, finalState) <- liftIO $ 
  runInferWithWarningsAndState (inferITopExpr iTopExpr) initState

case result of
  Right (Just typedITopExpr, _subst) -> do
    -- typedITopExprを処理...
```

**修正後**:
```haskell
(result, warnings, finalState) <- liftIO $ 
  runInferWithWarningsAndState (inferITopExpr iTopExpr) initState

case result of
  Right (Just (iTopExpr', ty), subst) -> do
    -- iTopExpr' に ty を付与して TITopExpr 作成
    let tiTopExpr = attachTypeToITopExpr iTopExpr' ty subst
    -- TypedDesugar へ
    tiTopExpr' <- desugarTypedTopExprT tiTopExpr
```

#### `TypedDesugar.hs`
**現在の実装**: `TypedTopExpr`（Exprベース）を処理
**修正後**: `TITopExpr`（IExprベース）を処理

```haskell
-- 旧
desugarTypedTopExprT :: TypedTopExpr -> EvalM (Maybe TITopExpr)

-- 新
desugarTypedTopExprT :: TITopExpr -> EvalM (Maybe TITopExpr)
```

**処理内容**:
1. テンソル関連のDesugar
2. 型クラス辞書渡し（TypeClassExpand.hsを呼び出し）

## 📊 データ構造の比較

### TypedIExpr（不要・削除予定）
```haskell
data TypedIExpr = TypedIExpr
  { tiexprType :: Type
  , tiexprNode :: TypedINode  -- 新しいノード型を定義
  }
```

### TIExpr（既存・これを使う）
```haskell
data TIExpr = TIExpr
  { tiType :: Type
  , tiExpr :: IExpr  -- 元のIExprをそのまま保持
  }
```

**TIExprの利点**:
- 元の`IExpr`構造を保持 → `Desugar.hs`のロジックが再利用可能
- シンプルで理解しやすい
- 既存のコードと互換性が高い

## 🎯 新しい実装方針

### Phase 5-6: Type Inference (IInfer.hs)

**入力**: `IExpr`  
**出力**: `(Type, Subst)`  
**役割**: 型推論のみ。型付きASTは生成しない。

```haskell
inferIExpr :: IExpr -> Infer (Type, Subst)
inferITopExpr :: ITopExpr -> Infer (Maybe (ITopExpr, Type), Subst)
```

### Phase 7: Type Attachment（新規・簡単）

**入力**: `IExpr + Type`  
**出力**: `TIExpr`  
**役割**: 単純な変換（型を付与するだけ）

```haskell
attachTypeToIExpr :: IExpr -> Type -> TIExpr
attachTypeToIExpr iexpr ty = TIExpr ty iexpr
```

### Phase 8: TypedDesugar（既存・修正）

**入力**: `TIExpr`  
**出力**: `TIExpr`（変換後）  
**役割**: 型駆動の変換

```haskell
desugarTypedExpr :: TIExpr -> EvalM TIExpr
desugarTypedExpr (TIExpr ty iexpr) = do
  -- 1. テンソル関連のDesugar
  iexpr' <- tensorDesugar ty iexpr
  
  -- 2. 型クラス辞書渡し
  iexpr'' <- typeClassExpand ty iexpr'
  
  return $ TIExpr ty iexpr''
```

## 📋 修正タスクリスト

### 優先度1: IInfer.hsの修正
- [ ] 戻り値を`(TypedIExpr, Subst)`から`(Type, Subst)`に変更
- [ ] `TypedIAST.hs`へのimportを削除
- [ ] すべての関数シグネチャを更新

### 優先度2: TypedIAST.hsの削除
- [ ] ファイル削除
- [ ] `egison.cabal`から削除
- [ ] すべての参照を削除

### 優先度3: Eval.hsの修正
- [ ] 型推論結果の処理を修正
- [ ] `attachTypeToIExpr`ヘルパー関数を追加
- [ ] `TIExpr`を`TypedDesugar`に渡す

### 優先度4: TypedDesugar.hsの修正
- [ ] 入力型を`TITopExpr`に変更
- [ ] テンソルDesugarの実装確認
- [ ] TypeClassExpand呼び出しを追加

### 優先度5: TypeClassExpand.hsの実装
- [ ] 型クラス辞書渡しの完全実装
- [ ] インスタンス選択ロジック
- [ ] メソッド呼び出しの具体化

## ⏱️ 新しい推定作業時間

| タスク | 時間 | 状態 |
|--------|------|------|
| IInfer.hs修正 | 1-2時間 | 未着手 |
| TypedIAST.hs削除 | 30分 | 未着手 |
| Eval.hs修正 | 1-2時間 | 未着手 |
| TypedDesugar.hs修正 | 2-3時間 | 未着手 |
| TypeClassExpand.hs実装 | 4-6時間 | 未着手 |
| テスト | 1-2時間 | 未着手 |
| **合計** | **10-16時間** | - |

## 💡 このアプローチの利点

### 1. シンプルさ
- 中間表現（`TypedIExpr`）が不要
- フローが直線的: `IExpr` → `Type` → `TIExpr`

### 2. 既存コードの再利用
- `IExpr`構造を保持 → `Desugar.hs`のロジックが使える
- `TIExpr`は既存のデータ構造

### 3. 保守性
- データ構造が少ない
- 型推論と型駆動変換が明確に分離

### 4. 拡張性
- `TypedDesugar`での変換が柔軟
- 型情報を使った最適化が容易

## 📚 参考

- `IExpr.hs`: `TIExpr`の定義（189行目〜）
- `TypedDesugar.hs`: 既存の型駆動変換ロジック
- `TypeClassExpand.hs`: 型クラス辞書渡し（実装が必要）

## 🎓 学んだこと

1. **既存コードの確認の重要性**: `TIExpr`が既に存在することを見落としていた
2. **シンプルさ優先**: 中間表現を増やすと複雑化する
3. **設計レビューの価値**: コードレビューで問題を早期発見

---

**次のアクション**: `IInfer.hs`の戻り値型を`(Type, Subst)`に修正

