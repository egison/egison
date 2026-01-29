# 型推論の流れ: dot関数の詳細解説

## 対象コード

```egison
def dot := \t1 t2 -> foldl1 (+) (contract ((*) t1 t2))
```

## 最終的な型

```
{Num contractElem9} Tensor contractElem9 -> Tensor contractElem9 -> contractElem9
```

## 型推論の段階的な流れ

### Phase 1: Lambda式の解析 (IInfer.hs:969-991)

**入力**: `\t1 t2 -> foldl1 (+) (contract ((*) t1 t2))`

**処理**:
1. Lambda式のパラメータ `t1`, `t2` に対して新しい型変数を生成:
   - `t1 : t_arg1`
   - `t2 : t_arg2`

2. 環境を拡張:
   ```haskell
   schemes = [("t1", Forall [] [] t_arg1), ("t2", Forall [] [] t_arg2)]
   ```

3. Body式 `foldl1 (+) (contract ((*) t1 t2))` の型推論を開始

**現在の状態**:
- 型変数: `t_arg1`, `t_arg2`
- 制約: なし
- 環境: `{t1: t_arg1, t2: t_arg2}`

---

### Phase 2: `(*)` の適用 - 第1段階 (IInfer.hs:1901-1930)

**入力**: `(*) t1 t2`

**処理**:
1. `(*)` の型をlookup:
   - 定義: `def (*) : {Num a} a -> a -> a`
   - Instantiate: `{Num t0} t0 -> t0 -> t0`
   - 制約 `Num t0` を追加

2. `t1` の型を取得: `t_arg1`

3. `t2` の型を取得: `t_arg2`

4. 型統一 (inferIApplicationWithContext):
   ```
   関数型: {Num t0} t0 -> t0 -> t0
   引数型: [t_arg1, t_arg2]
   ```

5. パラメータ型変数を生成:
   ```
   param1 = t_param1
   param2 = t_param2
   result = t_result1
   expectedFuncType = t_param1 -> t_param2 -> t_result1
   ```

6. 関数型と期待型を統一:
   ```
   t0 -> t0 -> t0  ≈  t_param1 -> t_param2 -> t_result1
   ```
   統一結果: `[t0 = t_param1, t0 = t_param2, t0 = t_result1]`
   つまり: `t_param1 = t_param2 = t_result1 = t0`

7. 引数型とパラメータ型を統一:
   ```
   t_arg1 ≈ t0
   t_arg2 ≈ t0
   ```
   統一結果: `[t_arg1 = t0, t_arg2 = t0]`

8. TensorMap挿入判定 (Phase 8で実行):
   - `t_arg1 (= t0)` は `{Num t0}` という制約付き
   - `t0` は `Tensor a` と統一できない（Numインスタンスがないため）
   - したがってスカラー型と判定される
   - ただし、この時点ではまだ実際の値が不明なので、tensorMapは挿入されない

**現在の状態**:
- 型変数: `t0`, `t_arg1 = t0`, `t_arg2 = t0`, `t_result1 = t0`
- 制約: `[Num t0]`
- `(*) t1 t2` の型: `{Num t0} t0`

---

### Phase 3: `contract` の適用 (IInfer.hs:2020-2035)

**入力**: `contract ((*) t1 t2)`

**処理**:
1. `contract` の型をlookup:
   - 定義: `def contract : Tensor a -> [Tensor a]`
   - Instantiate: `Tensor t1 -> [Tensor t1]`

2. 引数 `(*) t1 t2` の型: `{Num t0} t0`

3. 引数型とパラメータ型を統一:
   ```
   {Num t0} t0  ≈  Tensor t1
   ```

   ここで重要なポイント:
   - `t0` は `{Num t0}` という制約がある
   - `Tensor t1` は `Num` インスタンスを持たない
   - **制約を考慮した統一 (unifyStrictWithConstraints)** が実行される
   - 結果: `t0` は `Tensor` にはなれないが、この時点で実際には `t1` と統一される必要がある

   実際の統一:
   ```
   t0 ≈ Tensor t1
   ```

   しかし `{Num t0}` という制約があるため:
   - `applySubstWithConstraintsM` が呼ばれる (IInfer.hs:527-568)
   - `adjustSubstForConstraints` が制約をチェック
   - `Num` インスタンスが `Tensor t1` に存在しないことを検出
   - 型を unwrap: `Tensor t1` → `t1`
   - 最終的な統一: `t0 = t1` (unwrap後)

4. 結果型:
   ```
   contract : Tensor t1 -> [Tensor t1]
   引数型（unwrap後）: t1
   再構築: Tensor t1  (contract の入力要件)
   結果: [Tensor t1]
   ```

**現在の状態**:
- 型変数: `t0 = t1`, `t_arg1 = t1`, `t_arg2 = t1`
- 制約: `[Num t1]`
- `contract ((*) t1 t2)` の型: `{Num t1} [Tensor t1]`

---

### Phase 4: `(+)` の型推論 (IInfer.hs:1901-1930)

**入力**: `(+)` (foldl1の引数として)

**処理**:
1. `(+)` の型をlookup:
   - 定義: `def (+) : {Num a} a -> a -> a`
   - Instantiate: `{Num t2} t2 -> t2 -> t2`
   - 制約 `Num t2` を追加

2. この時点では `(+)` は部分適用されていない
   - 型: `{Num t2} t2 -> t2 -> t2`

**現在の状態**:
- 型変数: `t0 = t1`, `t2`
- 制約: `[Num t1, Num t2]`
- `(+)` の型: `{Num t2} t2 -> t2 -> t2`

---

### Phase 5: `foldl1` の適用 (IInfer.hs:2604-2714)

**入力**: `foldl1 (+) (contract ((*) t1 t2))`

**処理**:
1. `foldl1` の型をlookup:
   - 定義: `def foldl1 : (a -> b -> a) -> [b] -> a`
   - Instantiate: `(t3 -> t4 -> t3) -> [t4] -> t3`

2. 引数の型:
   - 第1引数 `(+)`: `{Num t2} t2 -> t2 -> t2`
   - 第2引数 `contract (...)`: `{Num t1} [Tensor t1]`

3. 型統一:
   ```
   関数型: (t3 -> t4 -> t3) -> [t4] -> t3
   引数1: t2 -> t2 -> t2
   引数2: [Tensor t1]
   ```

4. パラメータ型変数の統一:
   ```
   第1パラメータ: t3 -> t4 -> t3  ≈  t2 -> t2 -> t2
   ```
   統一結果: `[t3 = t2, t4 = t2]`

5. 第2パラメータの統一:
   ```
   [t4]  ≈  [Tensor t1]
   ```
   統一結果: `t4 = Tensor t1`

6. 制約の伝播:
   - `t4 = t2` (ステップ4から)
   - `t4 = Tensor t1` (ステップ5から)
   - したがって: `t2 = Tensor t1`

7. 制約の調整:
   - `{Num t2}` という制約がある
   - `t2 = Tensor t1` となった
   - `applySubstWithConstraintsM` が実行される
   - `Num (Tensor t1)` インスタンスが存在しないことを検出
   - 型を unwrap: `Tensor t1` → `t1`
   - 最終的な統一: `t2 = t1`

8. 結果型:
   ```
   foldl1 : (t1 -> Tensor t1 -> t1) -> [Tensor t1] -> t1
   結果: t1
   ```

**現在の状態**:
- 型変数: `t0 = t1 = t2`
- 制約: `[Num t1, Num t2]` → 統一後: `[Num t1, Num t1]`
- Body式の型: `{Num t1} t1`

---

### Phase 6: Lambda式の構築 (IInfer.hs:969-991)

**処理**:
1. Lambda式のパラメータ型に substitution を適用:
   ```
   t_arg1 = t1  (統一により)
   t_arg2 = t1  (統一により)
   ```

2. Body式の型: `t1`

3. 関数型の構築:
   ```
   t1 -> t1 -> t1
   ```

   しかし、実際には引数が `Tensor t1` である可能性がある:
   - `(*)` の適用時に、引数は実際には Tensor として渡される
   - TensorMap挿入 (Phase 8) で調整される

   最終的な型:
   ```
   Tensor t1 -> Tensor t1 -> t1
   ```

4. 制約の収集:
   - `[Num t1, Num t1]` (重複あり)

**現在の状態**:
- 型: `{Num t1, Num t1} Tensor t1 -> Tensor t1 -> t1`
- 制約: `[Num t1, Num t1]`

---

### Phase 7: 型の一般化 (IInfer.hs:3054-3084)

**入力**: `{Num t1, Num t1} Tensor t1 -> Tensor t1 -> t1`

**処理**:
1. `resolveConstraintWithInstances` を各制約に適用:
   ```
   Num t1  →  Num t1 (変化なし)
   Num t1  →  Num t1 (変化なし)
   ```
   結果: `[Num t1, Num t1]` (重複あり)

2. 型変数のみの制約をフィルタリング:
   ```haskell
   isTypeVarConstraint (Constraint _ (TVar _)) = True
   ```
   両方とも型変数 `t1` なので両方残る: `[Num t1, Num t1]`

3. **重複排除 (nub)**:
   ```haskell
   generalizedConstraints = nub $ filter isTypeVarConstraint updatedConstraints
   ```
   結果: `[Num t1]`

4. 自由型変数の計算:
   ```
   envFreeVars = {} (空の環境)
   typeFreeVars = {t1}
   genVars = {t1}
   ```

5. 型変数のリネーム:
   - `t1` → `contractElem9` (fresh name generation)

6. 最終的な型スキーム:
   ```
   Forall [contractElem9] [Num contractElem9]
     (Tensor contractElem9 -> Tensor contractElem9 -> contractElem9)
   ```

**最終的な型**:
```
{Num contractElem9} Tensor contractElem9 -> Tensor contractElem9 -> contractElem9
```

---

### Phase 8: TensorMap挿入 (TensorMapInsertion.hs:221-691)

**注**: このフェーズは型推論後、評価前に実行されます

**処理**:
1. `(*)` の適用を検査:
   ```
   関数: {Num contractElem9} contractElem9 -> contractElem9 -> contractElem9
   引数: [Tensor contractElem9, Tensor contractElem9]
   ```

2. 各引数に対して `shouldInsertTensorMap` をチェック:
   ```haskell
   paramType: contractElem9 (scalar)
   argType: Tensor contractElem9 (tensor)
   ```

3. `isPotentialScalarType` の判定:
   - `contractElem9` は `{Num contractElem9}` という制約がある
   - `Tensor a` は `Num` インスタンスを持たない
   - `unifyStrictWithConstraints` が失敗
   - したがって `contractElem9` はスカラー型

4. TensorMap2の挿入判定:
   - `(*)` は binary function: `a -> b -> c`
   - 両方のパラメータがスカラー型
   - `shouldWrapWithTensorMap2` が True を返す

5. `(*)` を `tensorMap2 (*)` でラップ:
   ```
   元: (*) t1 t2
   変換後: tensorMap2 (*) t1 t2
   ```

6. 実行時の動作:
   ```egison
   t1 = [| 1, 2, 3 |]
   t2 = [| 4, 5, 6 |]

   tensorMap2 (*) t1 t2
   = [| 1*4, 2*5, 3*6 |]
   = [| 4, 10, 18 |]
   ```

---

## 重要なポイント

### 1. 制約を考慮した型統一

`applySubstWithConstraintsM` (IInfer.hs:527-568) が重要な役割を果たす:

```haskell
applySubstWithConstraintsM :: Subst -> Type -> Infer Type
applySubstWithConstraintsM s@(Subst m) t = do
  classEnv <- getClassEnv
  constraints <- gets inferConstraints
  let m' = adjustSubstForConstraints classEnv constraints m
      s' = Subst m'
  return $ applySubst s' t
```

この関数は:
1. 型変数の置換を適用する前に制約をチェック
2. 型クラスインスタンスが存在しない場合、Tensor型をunwrap
3. 例: `{Num t0} t0` が `Tensor t1` に統一される時、`Num (Tensor t1)` インスタンスが無いので `t0 = t1` に調整

### 2. 制約の重複排除

複数の演算子 (`(*)`, `(+)`) が同じ型変数に制約を追加すると重複が発生:
- `(*) : {Num a} a -> a -> a` → 制約 `Num t0`
- `(+) : {Num b} b -> b -> b` → 制約 `Num t2`
- 統一後: `t0 = t2 = t1` → `[Num t1, Num t1]`

`nub` による重複排除 (IInfer.hs:3074):
```haskell
generalizedConstraints = nub $ filter isTypeVarConstraint updatedConstraints
```

### 3. TensorMap挿入の判定

`isPotentialScalarType` (TensorMapInsertion.hs:147-154) が制約を使って判定:
```haskell
isPotentialScalarType classEnv constraints ty =
  let tensorType = TTensor (TVar freshVar)
  in case unifyStrictWithConstraints classEnv constraints ty tensorType of
       Right _ -> False  -- Tensorと統一可能 → スカラーではない
       Left _  -> True   -- Tensorと統一不可 → スカラー型
```

例:
- `{Num t0} t0` と `Tensor a` の統一を試みる
- `Num (Tensor a)` インスタンスが存在しない
- 統一失敗 → スカラー型と判定

---

## まとめ

`dot`関数の型推論は以下のステップで進行する:

1. **Lambda解析**: パラメータに型変数を割り当て
2. **`(*)` 推論**: `Num t0` 制約を追加、引数型を統一
3. **`contract` 推論**: `Tensor t1` を期待するが、制約により `t1` に unwrap
4. **`(+)` 推論**: `Num t2` 制約を追加
5. **`foldl1` 推論**: すべての型変数を統一、制約が重複
6. **Lambda構築**: 関数型を構築
7. **一般化**: 制約を重複排除し、型変数をリネーム
8. **TensorMap挿入**: 実行時に `tensorMap2` を挿入

最終結果:
```
{Num contractElem9} Tensor contractElem9 -> Tensor contractElem9 -> contractElem9
```
