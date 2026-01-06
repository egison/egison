# Phase 1: 重複コード特定分析

## ファイル概要

### Infer.hs (1667行)
- **役割**: 型推論の基盤実装
- **出力**: `(Type, Subst)` - 型と代入のペア
- **主な関数**: `inferExpr`, `inferTopExpr`

### TypeInfer.hs (896行)
- **役割**: TypedAST生成
- **出力**: `(TypedExpr, Subst)` - 型付きASTと代入のペア
- **主な関数**: `inferTypedExpr`, `inferTypedTopExpr`

## 重複パターンの分類

### パターン1: 完全に`Infer.hs`を利用できる（既に実装済み）

```haskell
-- TypeInfer.hs
ConstantExpr c -> do
  (ty, s) <- Infer.inferConstant c
  return (TypedExpr ty (TConstantExpr c), s)

VarExpr name -> do
  ty <- Infer.lookupVar name
  return (TypedExpr ty (TVarExpr name), emptySubst)
```

**状態**: ✅ 既に`Infer.hs`の関数を使用している

### パターン2: ロジックが重複しているが独自実装

#### 2-1. Lambda式の推論

**Infer.hs** (252-259行):
```haskell
LambdaExpr args body -> do
  argTypes <- mapM (const $ freshVar "arg") args
  let bindings = zipWith makeBinding (extractArgNames args) argTypes
  (bodyType, s) <- withEnv (map toScheme bindings) $ inferExpr body
  let finalArgTypes = map (applySubst s) argTypes
      funType = foldr TFun bodyType finalArgTypes
  return (funType, s)
```

**TypeInfer.hs** (183-193行):
```haskell
LambdaExpr args body -> do
  paramTypes <- mapM (\_ -> freshVar "param") args
  let paramNames = map extractArgName args
      argParams = map convertArg args
      paramBindings = zipWith (\n t -> (n, Forall [] [] t)) paramNames paramTypes
  (bodyTyped, s) <- withEnv paramBindings $ inferTypedExpr body
  let finalParamTypes = map (applySubst s) paramTypes
      finalBodyType = applySubst s (texprType bodyTyped)
      funType = foldr TFun finalBodyType finalParamTypes
      bodyTyped' = applySubstToTypedExpr s bodyTyped
  return (TypedExpr funType (TLambdaExpr argParams bodyTyped'), s)
```

**重複度**: 🟡 高い（80%）
**統合可能性**: 可能 - `Infer.inferExpr`を呼び出してから、TypedASTを構築

#### 2-2. Function Application

**Infer.hs** (283-286行):
```haskell
ApplyExpr func args -> do
  (funcType, s1) <- inferExpr func
  inferApplication funcType args s1
```

**TypeInfer.hs** (218-230行):
```haskell
ApplyExpr func args -> do
  (funcTyped, s1) <- inferTypedExpr func
  argsResults <- mapM inferTypedExpr args
  let argsTyped = map fst argsResults
      argsSubst = foldr composeSubst s1 (map snd argsResults)
  resultType <- freshVar "result"
  let expectedFuncType = foldr TFun resultType (map (applySubst argsSubst . texprType) argsTyped)
  s2 <- unifyTypes (applySubst argsSubst (texprType funcTyped)) expectedFuncType
  let finalS = composeSubst s2 argsSubst
      finalResultType = applySubst finalS resultType
      funcTyped' = applySubstToTypedExpr finalS funcTyped
      argsTyped' = map (applySubstToTypedExpr finalS) argsTyped
  return (TypedExpr finalResultType (TApplyExpr funcTyped' argsTyped'), finalS)
```

**重複度**: 🟡 中程度（60%）
**統合可能性**: 部分的 - 型推論ロジックは共通、AST構築は独自

#### 2-3. Tuple式

**Infer.hs** (236-240行):
```haskell
TupleExpr es -> do
  results <- mapM inferExpr es
  let ts = map fst results
      s = foldr composeSubst emptySubst (map snd results)
  return (TTuple ts, s)
```

**TypeInfer.hs** (147-152行):
```haskell
TupleExpr es -> do
  results <- mapM inferTypedExpr es
  let typedExprs = map fst results
      types = map texprType typedExprs
      subst = foldr composeSubst emptySubst (map snd results)
  return (TypedExpr (TTuple types) (TTupleExpr typedExprs), subst)
```

**重複度**: 🟢 非常に高い（90%）
**統合可能性**: 高い - ほぼ同じロジック

### パターン3: TypeInfer.hs独自の処理

以下は`Infer.hs`にない、TypedAST固有の処理：

1. **IndexedExpr** - インデックス式のTypedAST変換
2. **HashExpr** - ハッシュ式のTypedAST変換  
3. **型情報の保持** - 各ノードに型を付与

## 統合可能な箇所のリスト

### 高優先度（重複度90%以上、統合が容易）

| 式の種類 | 行数削減見込み | 実装難易度 |
|---------|--------------|----------|
| TupleExpr | ~5行 | 簡単 |
| CollectionExpr | ~10行 | 簡単 |
| ConstantExpr | 0（既に統合済み） | - |
| VarExpr | 0（既に統合済み） | - |

### 中優先度（重複度60-80%、部分的統合）

| 式の種類 | 行数削減見込み | 実装難易度 |
|---------|--------------|----------|
| LambdaExpr | ~8行 | 中 |
| ApplyExpr | ~10行 | 中 |
| IfExpr | ~8行 | 中 |
| LetExpr | ~5行 | 中 |

### 低優先度（独自実装が必要）

| 式の種類 | 理由 |
|---------|------|
| IndexedExpr | Infer.hsにない |
| HashExpr | Infer.hsにない |
| MatchExpr | TypedAST独自の処理 |

## 提案: ヘルパー関数の導入

重複を減らすため、以下のヘルパー関数を導入：

```haskell
-- TypeInfer.hs に追加
-- | Infer.inferExprを呼び出してTypedExprを構築する汎用ヘルパー
inferAndWrap :: (TypedExprNode -> TypedExprNode) -> Expr -> Infer (TypedExpr, Subst)
inferAndWrap wrapNode expr = do
  (ty, s) <- Infer.inferExpr expr
  -- exprを再帰的にTypedExprに変換
  typedExpr <- exprToTypedExpr expr
  return (TypedExpr ty (wrapNode (texprNode typedExpr)), s)
```

## 次のステップ（Phase 2）

1. 高優先度の統合から開始
2. ヘルパー関数の実装
3. テストで動作確認
4. 段階的に中優先度へ展開

## 詳細分析: CollectionExpr の違い

### Infer.hs の実装（正確）
```haskell
CollectionExpr es -> do
  elemType <- freshVar "elem"
  s <- foldM (inferListElem elemType) emptySubst es
  return (TCollection (applySubst s elemType), s)
  where
    inferListElem eType s e = do
      (t, s') <- inferExpr e
      s'' <- unifyTypes (applySubst s eType) t
      return $ composeSubst s'' (composeSubst s' s)
```
- 全要素の型を統一
- 型不一致を検出可能

### TypeInfer.hs の実装（簡易）
```haskell
CollectionExpr exprs -> do
  results <- mapM inferTypedExpr exprs
  let typedExprs = map fst results
  elemType <- if null typedExprs
              then freshVar "elem"
              else return $ texprType (head typedExprs)
  let subst = foldr composeSubst emptySubst (map snd results)
  return (TypedExpr (TCollection elemType) (TCollectionExpr typedExprs), subst)
```
- 最初の要素の型を使用（簡略化）
- 型不一致が見逃される可能性

**問題点**: TypeInfer.hsの実装は型安全性が低い

## 統合戦略の見直し

### アプローチA: Infer.hsを呼び出してからTypedASTを構築
```haskell
-- TypeInfer.hsで
CollectionExpr exprs -> do
  -- Infer.hsで型推論（正確）
  (collType, subst) <- Infer.inferExpr (CollectionExpr exprs)
  -- 各要素をTypedExprに変換
  typedExprs <- mapM inferTypedExpr exprs
  return (TypedExpr collType (TCollectionExpr (map fst typedExprs)), subst)
```

**メリット**: 型推論ロジックを再利用、正確性維持
**デメリット**: 2回推論が走る（効率悪い）

### アプローチB: Infer.hsのロジックをそのまま使い、結果からTypedASTを構築

これは現実的ではない（TypedASTの情報が失われる）

### アプローチC（推奨）: 共通ロジックを抽出して両方で使用

```haskell
-- 新しいヘルパーモジュール: Type/InferCommon.hs
inferCollection :: (expr -> Infer (result, Type, Subst)) 
                -> [expr] 
                -> Infer (Type, Subst, [result])
inferCollection inferElem exprs = do
  elemType <- freshVar "elem"
  (results, finalSubst) <- foldM processElem ([], emptySubst) exprs
  return (TCollection (applySubst finalSubst elemType), finalSubst, reverse results)
  where
    processElem (acc, s) e = do
      (result, ty, s') <- inferElem e
      s'' <- unifyTypes (applySubst s elemType) ty
      let finalS = composeSubst s'' (composeSubst s' s)
      return (result : acc, finalS)
```

**メリット**: 
- コード重複なし
- 正確性維持
- 効率的（1回の推論）

**デメリット**: 
- 新しいモジュールが必要
- リファクタリング規模が大きい

## 推定効果

- **コード削減**: 約100-150行（TypeInfer.hsの10-15%）
- **保守性向上**: 型推論ロジックの一元化
- **バグ修正の容易さ**: 1箇所修正すれば両方に反映
- **型安全性向上**: TypeInfer.hsの簡略化された実装を正確なものに置き換え

## 推奨実装順序（Phase 2以降）

1. **簡単な統合から開始**（既に実装済みのパターンを参考）
   - ConstantExpr（完了）
   - VarExpr（完了）
   
2. **単純な式の統合**
   - TupleExpr（ほぼ同じロジック）
   
3. **複雑な式の統合**
   - CollectionExpr（正確性向上）
   - LambdaExpr
   - ApplyExpr

4. **長期的には**: 共通ロジック抽出モジュールの検討

