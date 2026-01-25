# TensorMap挿入設計

## 目標

明示的なTensor型注釈なしで、多相関数でテンソル操作をサポートする：

```egison
def sum {Num a} (xs: [a]) : a := foldl1 (+) xs

-- 使用例:
sum [1, 2, 3]        -- a = Integer, スカラー加算
sum [t1, t2]         -- a = Tensor Integer, tensorMapによる要素ごとの加算
```

## 処理フローの変更

**新しいアプローチ**: TensorMapInsertionを型クラス展開の**前**に実行する。

```
Phase 8: TypedDesugar
  ├─ 1. TensorMapInsertion.hs (tensorMap自動挿入)
  │     型クラスメソッド (+) 等にtensorMap/tensorMap2を挿入
  └─ 2. TypeClassExpand.hs (型クラスメソッド展開)
        (+) → dict_Num_("plus") 等
```

**利点**:
- 辞書アクセスパターンを扱う複雑さがない

## 設計方針

### 基本原則

仮引数の型がIntegerなどスカラー型であるのに対して、引数の型がTensor Integerなどテンソル型である場合、tensorMapやtensorMap2を挿入する。

### 挿入の判断基準

以下の条件を満たす場合、`tensorMap` が挿入される。

- 仮引数の型が `Tensor a` 型とunifyできない
  - 例：`Integer`, `Float`, `Bool` など具体的な型
  - 例：型クラス制約付きの型変数 `{Num a} a`（`Tensor` は `Num` のインスタンスでないため）

以下の場合は `tensorMap` は挿入しない：

- 仮引数の型が `Tensor a` 型とunifyできる
  - 例：型変数 `a`（任意の型を受け入れる）
  - 例：`Tensor a` 型

### is_tensor_typename が必要な場合

`is_tensor_typename` フラグが必要なのは、**仮引数の型が型コンストラクタの内側に型を持つ場合**である。
内側の型が型変数でも具体的なスカラー型でも、`is_tensor_typename`情報が必要。

**不要な場合**: 仮引数の型が型変数そのもの（`t0`）または具体的なスカラー型（`Integer`）
```egison
def inc {Num a} (x: a) : a := x + 1
-- 仮引数 x の型は a（型変数そのもの）
-- 呼び出し inc t1 で、呼び出し側が tensorMap inc t1 に変換
-- → is_tensor_a は不要

def incInt (x: Integer) : Integer := x + 1
-- 仮引数 x の型は Integer（具体的なスカラー型）
-- 呼び出し incInt t1 で、呼び出し側が tensorMap incInt t1 に変換
-- → is_tensor_Integer は不要
```

**必要な場合**: 仮引数の型が型コンストラクタの内側に型を持つ（`[t0]` や `[Integer]`）
```egison
def sum {Num a} (xs: [a]) : a := foldl1 (+) xs
-- 仮引数 xs の型は [a]（[] の内側に型変数 a）
-- 呼び出し sum [t1, t2] で、[Tensor Integer] はTensorではないのでtensorMapは挿入されない
-- しかし、内部で (+) が適用される引数は a = Tensor Integer 型
-- → is_tensor_a が必要

def sumInteger (xs: [Integer]) : Integer := foldl1 i.+ xs
-- 仮引数 xs の型は [Integer]（[] の内側に具体的な型 Integer）
-- 呼び出し sumInteger [t1, t2] で、[Tensor Integer] はTensorではないのでtensorMapは挿入されない
-- しかし、内部で i.+ が適用される引数は Tensor Integer 型
-- → is_tensor_Integer が必要
```

**複数の型コンストラクタがある場合**: それぞれに対して別々のフラグが必要
```egison
def fn (xs: [Integer]) (ys: [Integer]) : [Integer] := ...
-- xs の中身がTensorか → is_tensor_integer0
-- ys の中身がTensorか → is_tensor_integer1
-- それぞれ独立して管理する必要がある

-- 例:
fn [1, 2, 3] [t1, t2]      -- is_tensor_integer0 = False, is_tensor_integer1 = True
fn [t1, t2] [1, 2, 3]      -- is_tensor_integer0 = True, is_tensor_integer1 = False
fn [t1, t2] [t3, t4]       -- is_tensor_integer0 = True, is_tensor_integer1 = True
```

これは型クラス展開時に、型の情報を受け取り、それをもとに演算について辞書展開することに似ている。

## 詳細設計

### ステップ1: Tensor型とのunify可能性判定

```haskell
-- 型が Tensor a とunifyできるかチェック
canUnifyWithTensor :: Type -> Bool
canUnifyWithTensor (TVar _) = True           -- 型変数は任意の型を受け入れる
canUnifyWithTensor (TTensor _) = True        -- Tensor型はそのままunify可能
canUnifyWithTensor _ = False                 -- 具体的な型や制約付き型変数はunify不可

-- 型クラス制約付きの型変数の場合
-- {Num a} a は Tensor がNumのインスタンスでないためunify不可
canUnifyWithTensorConstrained :: Type -> [Constraint] -> Bool
canUnifyWithTensorConstrained (TVar v) constraints =
  not (any (constrainsAgainstTensor v) constraints)
canUnifyWithTensorConstrained t _ = canUnifyWithTensor t

-- TensorがインスタンスでないクラスによってTensorが除外されるか
constrainsAgainstTensor :: TypeVar -> Constraint -> Bool
constrainsAgainstTensor v (ClassConstraint "Num" (TVar v')) = v == v'
constrainsAgainstTensor v (ClassConstraint "Eq" (TVar v')) = v == v'
constrainsAgainstTensor _ _ = False
```

### ステップ2: 適用時のtensorMap挿入判断

関数適用 `f arg` において：
1. `f` の仮引数の型を取得
2. `arg` の型を取得
3. 仮引数の型が `Tensor a` とunifyできず、`arg`の型がTensor型なら、tensorMapを挿入

```haskell
-- 関数適用にtensorMapを挿入するか判断
shouldInsertTensorMap :: Type -> Type -> Bool
shouldInsertTensorMap paramType argType =
  not (canUnifyWithTensor paramType) && isTensorType argType

isTensorType :: Type -> Bool
isTensorType (TTensor _) = True
isTensorType _ = False
```

### ステップ3: 高階関数への対応

型クラス展開と同様に、高階関数では型情報を引数として受け取り、呼び出し時にコンパイル時展開する：

```haskell
-- 高階関数の場合、tensorMap挿入の判断を呼び出し時まで遅延させる
-- 関数が型変数を含む引数を持つ場合、適用時に型情報を渡す
insertTensorMapForHigherOrder :: TIExpr -> TIExpr
```

## 例のウォークスルー

### 単純な関数適用の場合（is_tensor不要）

```egison
def inc {Num a} (x: a) : a := x + 1

inc (t1 : Tensor Integer)
```

仮引数 `x` の型が型変数そのもの（`a`）なので、`is_tensor_a` は不要。
呼び出し側でtensorMapが挿入される。

#### Phase 5-6: 型推論後

```egison
def inc : {Num t0} t0 -> t0 :=
  \x -> (+) x 1
```

#### Phase 8-1: TensorMapInsertion

仮引数の型が型変数そのものなので、`is_tensor_t0` は追加されない：

```egison
def inc : {Num t0} t0 -> t0 :=
  \x -> (+) x 1
```

（変化なし）

#### Phase 8-2: TypeClassExpand

```egison
def inc : {Num t0} t0 -> t0 :=
  \dict_Num x -> (dict_Num_("plus")) x 1
```

#### 呼び出し時（コンパイル時展開）

```egison
-- スカラーの場合: t0 = Integer
inc 5
-- 展開後: inc numIntDict 5
-- 結果: 6

-- Tensorの場合: t0 = Tensor Integer
inc (t1 : Tensor Integer)
-- 呼び出し側でtensorMapが挿入される: tensorMap (inc numMathExprDict) t1
-- 結果: [| 2, 3 |]
```

### 具体型での高階関数の場合（is_tensor必要）

```egison
def sumInteger (xs: [Integer]) : Integer := foldl1 i.+ xs

def t1 : Tensor Integer := [| 1, 2 |]
def t2 : Tensor Integer := [| 3, 4 |]
sumInteger [t1, t2]
```

仮引数 `xs` の型が `[Integer]`（型コンストラクタの内側に具体型）なので、`is_tensor_Integer` が必要。

#### Phase 5-6: 型推論後

```egison
def sumInteger : [Integer] -> Integer :=
  \xs ->
    foldl1 (i.+ : Integer -> Integer -> Integer) (xs : [Integer])
```

この時点で：
- `(i.+)` は関数としてそのまま存在（eta展開されていない）
- `(i.+)` の型は `Integer -> Integer -> Integer`
- しかし、引数xsの中身の要素が実際にTensorかどうかは `sumInteger` の呼び出し時まで不明

#### Phase 8-1: TensorMapInsertion

型クラス展開と同様のアプローチで、tensorMap挿入の判断を呼び出し時まで遅延させる。
関数定義では `is_tensor_Integer0` を引数として受け取る形に変換：

```egison
-- is_tensor_Integer0 を引数として受け取る（型クラス辞書と同様）
def sumInteger : Bool -> [Integer] -> Integer :=
  \is_tensor_Integer0 xs ->
    foldl1
      (\e1 e2 -> (insertTensorMapIfNecessary is_tensor_Integer0 i.+) e1 e2)
      xs
```

- `is_tensor_Integer0` は型クラス辞書と同様に、`t0` がTensorかどうかの情報を持つ
- `insertTensorMapIfNecessary is_tensor_t0 (+)` は `is_tensor_t0` がTrueなら `tensorMap2 (+)`、Falseなら `(+)` に展開される


### 多相関数での高階関数の場合（is_tensor必要）

```egison
def sum {Num a} (xs: [a]) : a := foldl1 (+) xs

def t1 : Tensor Integer := [| 1, 2 |]
def t2 : Tensor Integer := [| 3, 4 |]
sum [t1, t2]
```

仮引数 `xs` の型が `[a]`（型コンストラクタの内側に型変数）なので、`is_tensor_a` が必要。

#### Phase 5-6: 型推論後

```egison
def sum : {Num t0} [t0] -> t0 :=
  \xs ->
    foldl1 ((+) : {Num t0} t0 -> t0 -> t0) (xs : [t0])
```

この時点で：
- `(+)` は型クラスメソッドとしてそのまま存在（eta展開されていない）
- `(+)` の型は `{Num t0} t0 -> t0 -> t0`
- `{Num t0} t0` は Tensorとunify不可
- しかし、引数が実際にTensorかどうかは `sum` の呼び出し時まで不明

#### Phase 8-1: TensorMapInsertion

型クラス展開と同様のアプローチで、tensorMap挿入の判断を呼び出し時まで遅延させる。
関数定義では `is_tensor_t0` を引数として受け取る形に変換：

```egison
-- is_tensor_t0 を引数として受け取る（型クラス辞書と同様）
def sum : {Num t0} Bool -> [t0] -> t0 :=
  \is_tensor_t0 xs ->
    foldl1
      (\e1 e2 -> (insertTensorMapIfNecessary is_tensor_t0 (+)) e1 e2)
      xs
```

- `is_tensor_t0` は型クラス辞書と同様に、`t0` がTensorかどうかの情報を持つ
- `insertTensorMapIfNecessary is_tensor_t0 (+)` は `is_tensor_t0` がTrueなら `tensorMap2 (+)`、Falseなら `(+)` に展開される

#### Phase 8-2: TypeClassExpand

```egison
def sum : {Num t0} [t0] -> t0 :=
  \is_tensor_t0 dict_Num xs ->
    foldl1
      (\e1 e2 -> (insertTensorMapIfNecessary is_tensor_t0 (dict_Num_("plus"))) e1 e2)
      xs
```

#### 呼び出し時（コンパイル時展開）

呼び出し時に型が確定するので、Phase 8でコンパイル時に展開される：

```egison
-- スカラーの場合: t0 = Integer
sum [1, 2, 3]
-- 展開後: sum False numIntDict [1, 2, 3]
-- insertTensorMapIfNecessary False (+) は (+) に展開される
-- 結果: 6

-- Tensorの場合: t0 = Tensor Integer
sum [t1, t2]
-- 展開後: sum True numMathExprDict [t1, t2]
-- insertTensorMapIfNecessary True (+) は tensorMap2 (+) に展開される
-- 結果: [| 4, 6 |]
```

## 実装チェックリスト

- [ ] TensorMapInsertion.hsをTypeClassExpand.hsの前に実行するように変更
- [ ] Tensor型とのunify可能性判定ロジックを実装
- [ ] `is_tensor_typename` 引数の追加ロジックを実装
- [ ] `insertTensorMapIfNecessary` の挿入ロジックを実装
- [ ] 呼び出し時の展開（True → tensorMap2、False → そのまま）を実装

## implementation.mdへの変更

```
Phase 8: TypedDesugar
  ├─ 1. TensorMapInsertion.hs (tensorMap自動挿入) ← 順序変更
  │     is_tensor_typename 引数を追加
  │     insertTensorMapIfNecessary で tensorMap 挿入を遅延
  │     呼び出し時に True/False が確定し、tensorMap2 または関数そのままに展開
  └─ 2. TypeClassExpand.hs (型クラスメソッド展開)
        (+) → dict_Num_("plus") 等
```

## 関連ファイル

- `hs-src/Language/Egison/Type/TensorMapInsertion.hs` - TensorMap挿入
- `hs-src/Language/Egison/Type/TypeClassExpand.hs` - 型クラス展開
- `hs-src/Language/Egison/Type/TypedDesugar.hs` - 処理順序の制御
- `design/implementation.md` - 処理フロー
