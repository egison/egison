# TensorMap挿入設計

## 目標

明示的なTensor型注釈なしで、スカラー関数をテンソルに自動適用する：

```egison
def f (x: Integer) : Integer := x + 1
def t1 := [| 1, 2, 3 |]

f t1        -- => tensorMap f t1  => [| 2, 3, 4 |]

def sum {Num a} (xs: [a]) : a := foldl1 (+) xs
sum [t1, t2]  -- テンソル要素に (+) が適用される
```

## 前提条件

`tensorMap`/`tensorMap2`はスカラーに対してもそのまま関数を適用するため、テンソルかスカラーかに関わらず挿入しても結果は変わらない：

```egison
tensorMap f 5 = f 5
tensorMap2 f 3 4 = f 3 4
```

## 2つの挿入ルール

### ルール1（一般ケース）: スカラー関数にテンソルを直接渡す場合

関数適用 `f arg` において、`f` のパラメータ型がスカラーで `arg` の型がテンソルであれば `tensorMap` を挿入する。

```egison
def f (x: Integer) : Integer := x + 1

f 5         -- そのまま: f 5
f t1        -- tensorMap挿入: tensorMap (\x -> f x) t1
```

2つの引数が連続してテンソルの場合は `tensorMap2` を使う：

```egison
def add (x: Integer) (y: Integer) : Integer := x + y

add 3 4         -- そのまま: add 3 4
add t1 t2       -- tensorMap2 (\x y -> add x y) t1 t2
add t1 3        -- tensorMap (\x -> add x 3) t1
add 3 t2        -- tensorMap (\y -> add 3 y) t2
```

### ルール2（高階関数ケース）: 二項スカラー関数を引数として渡す場合

関数適用において、引数の型が「両辺ともスカラー型を受け取る二項関数」であれば、**常に** `tensorMap2` でラップして渡す。

`tensorMap2` はスカラーに対しても正しく動作するため、実際の引数がテンソルかスカラーかに関わらず常にラップする。

```egison
def sum {Num a} (xs: [a]) : a := foldl1 (+) xs
-- (+) は a -> a -> a（aはスカラー制約付き）なのでtensorMap2でラップ
```

変換後：
```egison
def sum {Num a} (xs: [a]) : a := foldl1 (tensorMap2 (+)) xs
```

## 判定ロジック

### スカラー型の判定

型が `Tensor a` とunifyできなければスカラー型とみなす：

```haskell
isPotentialScalarType :: ClassEnv -> [Constraint] -> Type -> Bool
isPotentialScalarType classEnv constraints ty =
  let freshVar = TyVar "a_scalar_check"
      tensorType = TTensor (TVar freshVar)
  in case Unify.unifyStrictWithConstraints classEnv constraints ty tensorType of
       Right _ -> False  -- Tensor a とunify可能 → スカラーでない
       Left _  -> True   -- Tensor a とunify不可 → スカラー
```

例：
- `{Num t0} t0`：`Num`インスタンスはTensorに存在しないためunify不可 → スカラー
- `Tensor t0`：`Tensor a`とunify可能 → スカラーでない
- `Integer`：具体型のためunify不可 → スカラー
- 制約なしの型変数 `a`：`Tensor b`とunify可能 → スカラーでない

### ルール1の判定: `shouldInsertTensorMap`

パラメータ型がスカラー **かつ** 引数型がテンソルの場合に挿入：

```haskell
shouldInsertTensorMap :: ClassEnv -> [Constraint] -> Type -> Type -> Bool
shouldInsertTensorMap classEnv constraints argType paramType =
  let isParamScalar = isPotentialScalarType classEnv constraints paramType
      isArgTensor = case Unify.unifyStrictWithConstraints classEnv constraints argType (TTensor (TVar freshVar)) of
                      Right _ -> True
                      Left _  -> False
  in isParamScalar && isArgTensor
```

### ルール2の判定: `shouldWrapWithTensorMap2`

二項関数の両パラメータがともにスカラー型の場合にラップ：

```haskell
shouldWrapWithTensorMap2 :: ClassEnv -> [Constraint] -> Type -> Bool
shouldWrapWithTensorMap2 classEnv constraints ty = case ty of
  TFun param1 (TFun param2 _result) ->
      isPotentialScalarType classEnv constraints param1 &&
      isPotentialScalarType classEnv constraints param2
  _ -> False
```

## 例のウォークスルー

### 例1: 直接適用（ルール1）

```egison
def f (x: Integer) : Integer := x + 1
f t1   -- t1 : Tensor Integer
```

型推論後：`f : Integer -> Integer`、`t1 : Tensor Integer`

- パラメータ型 `Integer` はスカラー
- 引数型 `Tensor Integer` はテンソル
- → `tensorMap (\x -> f x) t1`

### 例2: sum関数（ルール2）

```egison
def sum {Num a} (xs: [a]) : a := foldl1 (+) xs
```

#### Phase 5-6: 型推論後

```egison
def sum : {Num t0} [t0] -> t0 :=
  \xs ->
    foldl1 ((+) : {Num t0} t0 -> t0 -> t0) (xs : [t0])
```

- `(+)` の型は `{Num t0} t0 -> t0 -> t0`：両パラメータが `{Num t0} t0`（スカラー）
- → `tensorMap2` でラップ

#### Phase 8-1: TensorMapInsertion

```egison
def sum : {Num t0} [t0] -> t0 :=
  \xs ->
    foldl1 (tensorMap2 (+)) xs
```

#### Phase 8-2: TypeClassExpand

```egison
def sum : {Num t0} [t0] -> t0 :=
  \dict_Num xs ->
    foldl1 (tensorMap2 (dict_Num_("plus"))) xs
```

#### 呼び出し時

```egison
-- スカラーの場合: t0 = Integer
sum [1, 2, 3]   -- tensorMap2 (+) を各要素ペアに適用 → 6

-- テンソルの場合: t0 = Tensor Integer
sum [t1, t2]    -- t1 = [| 1, 2 |], t2 = [| 3, 4 |]
                -- tensorMap2 (+) t1 t2 → [| 4, 6 |]
```

## 実装上の注意点

### ラッピングは関数引数への適用時のみ

定義の右辺全体をラップしてはならない。ラップは `TIApplyExpr` の引数処理時のみ行う：

```haskell
TIApplyExpr func args -> do
  func' <- insertTensorMapsWithConstraints env cs func
  args' <- mapM (insertTensorMapsWithConstraints env cs) args
  -- 各引数に対してルール1（shouldInsertTensorMap）とルール2（shouldWrapWithTensorMap2）を適用
  let args'' = map (wrapBinaryFunctionIfNeeded env allConstraints) args'
  result <- wrapWithTensorMapIfNeeded env constraints func' funcType args'' argTypes
  ...
```

### イータ展開されたラムダの処理

TypeClassExpandの前に処理されるため、型クラスメソッドはイータ展開された形式で現れる：

```
\etaVar1 etaVar2 -> numMathExpr_("plus") etaVar1 etaVar2
```

この形式に対応するため、2引数ラムダの本体を直接 `TITensorMap2Expr` に変換する：

```haskell
wrapLambdaBodyWithTensorMap2 :: [Constraint] -> Maybe Var -> Var -> Var -> TIExpr -> TIExpr -> TIExpr
wrapLambdaBodyWithTensorMap2 constraints mVar var1 var2 body originalExpr =
  case tiExprNode body of
    TIApplyExpr func args
      | length args == 2 ->
          let newBody = TIExpr resultScheme (TITensorMap2Expr func (args!!0) (args!!1))
          in TIExpr newLambdaScheme (TILambdaExpr mVar [var1, var2] newBody)
    TITensorMap2Expr {} -> originalExpr  -- 既にラップ済み
    _ -> wrapWithTensorMap2 constraints originalExpr
```

### Wedge積への対応

`TIWedgeApplyExpr`（外積演算）に対しては、パラメータ型がスカラー関数の場合に `TITensorMap2WedgeExpr` を挿入する。テンソル引数を取る関数の場合は `TIWedgeApplyExpr` のまま保持する。

## 関連ファイル

### ソースコード

- `hs-src/Language/Egison/Type/TensorMapInsertion.hs` - TensorMap挿入の実装
- `hs-src/Language/Egison/Type/TypeClassExpand.hs` - 型クラス展開
- `hs-src/Language/Egison/Type/TypedDesugar.hs` - 処理順序の制御

### テストファイル

- `mini-test/162-sum-tensor.egi` - テンソルに対するsum関数のテスト
- `mini-test/163-sum-scalar.egi` - スカラーとテンソル両方に対するsum関数のテスト
- `mini-test/159-riemann.egi` - リーマン曲率テンソル（回帰テスト）

### ライブラリ

- `lib/math/common/arithmetic.egi` - sum関数の定義
