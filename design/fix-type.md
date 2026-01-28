# 単相的なドット積

現在の--dump-typedの結果
```
def .' : Tensor MathExpr -> Tensor MathExpr -> Tensor MathExpr := 
  (\t1 t2 -> 
    ((foldl1 : (MathExpr -> Tensor MathExpr -> MathExpr) -> [Tensor MathExpr] -> MathExpr) 
      (+' : MathExpr -> MathExpr -> MathExpr) 
      (contract 
        ((*' : MathExpr -> MathExpr -> MathExpr) 
          (t1 : MathExpr) 
          (t2 : MathExpr) 
        : MathExpr) 
      : [Tensor MathExpr]) 
    : MathExpr) 
  : MathExpr -> MathExpr -> MathExpr)
```
現在の--dump-tiの結果
```
def .' : Tensor MathExpr -> Tensor MathExpr -> Tensor MathExpr := 
  (\t1 t2 -> 
    ((foldl1 : (MathExpr -> Tensor MathExpr -> MathExpr) -> [Tensor MathExpr] -> MathExpr) 
      (\tmap2_arg1 tmap2_arg2 -> 
        (tensorMap2 
          (+' : MathExpr -> MathExpr -> MathExpr) 
          (tmap2_arg1 : Tensor MathExpr) 
          (tmap2_arg2 : Tensor MathExpr) 
        : Tensor MathExpr) 
      : Tensor MathExpr -> Tensor MathExpr -> Tensor MathExpr) 
      (contract 
        ((*' : MathExpr -> MathExpr -> MathExpr) 
          (t1 : MathExpr) 
          (t2 : MathExpr) 
        : MathExpr) 
      : [Tensor MathExpr]) 
    : MathExpr) 
  : MathExpr -> MathExpr -> MathExpr)
```

# 多相的なドット積

現在の--dump-typedの結果
```
def . : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 := 
  (\t1 t2 -> 
    ((foldl1 : (t0 -> Tensor t0 -> t0) -> [Tensor t0] -> t0) 
      (+ : {Num t0} t0 -> t0 -> t0) 
      (contract 
        ((* : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0) 
          (t1 : Tensor t0) 
          (t2 : Tensor t0) 
        : Tensor t0)
      : [Tensor t0]) 
    : t0) 
  : Tensor t0 -> Tensor t0 -> t0)

```

上記の結果は正しい。
クラス制約が必要な（辞書を渡すことが必要な）関数にのみクラス制約が付加されている。

現在の--dump-tiの結果
```
def . : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 := 
  (\t1 t2 -> 
    ((foldl1 : (t0 -> Tensor t0 -> t0) -> [Tensor t0] -> t0) 
      (\tmap2_arg1 tmap2_arg2 -> 
        (tensorMap2 
          (+ : {Num t0} t0 -> t0 -> t0) 
          (tmap2_arg1 : Tensor t0)
          (tmap2_arg2 : Tensor t0)
        : Tensor t0) 
      : Tensor t0 -> Tensor t0 -> Tensor t0) 
      (contract 
        (tensorMap2 
          (\tmapVar1 tmapVar0 -> 
            ((* : {Num t0} t0 -> t0 -> t0) 
              (tmapVar1 : t0) 
              (tmapVar0 : t0) 
            : t0) 
          : t0 -> t0 -> t0) 
          (t1 : Tensor t0) 
          (t2 : Tensor t0) 
        : Tensor t0) 
      : [Tensor t0]) 
    : t0) 
  : Tensor t0 -> Tensor t0 -> t0)
```

# TensorMap挿入と型クラス展開の仕組み

## Phase 8の処理フロー

Phase 8 (TypedDesugar) は2つのサブフェーズから構成される：

1. **Phase 8a: TensorMap挿入** (`insertTensorMaps`)
   - スカラー関数をテンソルに適用する際、要素ごとの演算に変換
   - `tensorMap` や `tensorMap2` を自動挿入

2. **Phase 8b: 型クラス展開** (`expandTypeClassMethodsT`)
   - 型クラスメソッド呼び出しを辞書ベースのディスパッチに変換
   - 辞書パラメータの追加とインスタンス選択

## TensorMap挿入の仕組み (Phase 8a)

### 目的

Egisonでは、スカラー関数をテンソルに適用すると要素ごとの演算が行われる：

```egison
-- スカラー関数
def f : Int -> Int := \x -> x + 1

-- テンソルへの適用は要素ごとに行われる
f [| 1, 2, 3 |]  -- 結果: [| 2, 3, 4 |]
```

この動作を実現するため、コンパイラは自動的に `tensorMap` を挿入する：

```egison
-- 変換前
(* : t0 -> t0 -> t0) (t1 : Tensor t0) (t2 : Tensor t0)

-- 変換後
tensorMap2
  (\x y -> (* : t0 -> t0 -> t0) (x : t0) (y : t0) : t0)
  (t1 : Tensor t0)
  (t2 : Tensor t0)
: Tensor t0
```

### 挿入判定アルゴリズム

`shouldInsertTensorMap` 関数が、引数型とパラメータ型を比較して挿入の必要性を判定：

```haskell
shouldInsertTensorMap :: ClassEnv -> [Constraint] -> Type -> Type -> Bool
```

判定条件：
1. 引数型が `Tensor a` の形
2. パラメータ型が非テンソル型（`a`, `Int`, `String` など）
3. パラメータ型が関数型でない（関数はテンソル化しない）
4. パラメータ型がテンソル化可能（型変数またはプリミティブ型）

### TensorMap2の最適化

2つの連続する引数が共にtensorMapを必要とする場合、`tensorMap2` を使用：

```egison
-- tensorMapを2回使う（非効率）
tensorMap (\x -> tensorMap (\y -> x * y) t2) t1

-- tensorMap2を使う（効率的）
tensorMap2 (\x y -> x * y) t1 t2
```

実装（`wrapWithTensorMapRecursive` 関数）：
- 最初の引数がtensorMapを必要とするか判定
- 次の引数も必要とするか判定
- 両方必要なら `tensorMap2` を生成
- 片方だけなら通常の `tensorMap` を生成

### 型の正しさの保証

TensorMap挿入で重要なポイント：

1. **ラムダパラメータの型はテンソル要素型**
   ```haskell
   -- ラムダは Tensor t0 -> Tensor t0 -> Tensor t0 を受け取る
   \tmap2_arg1 tmap2_arg2 -> ...

   -- しかし内部の変数は t0 型（要素型）
   (tmap2_arg1 : Tensor t0)  -- 外側での型
   -- tensorMap2内部では
   (\x y -> ... (x : t0) (y : t0) ...)  -- 要素型
   ```

2. **型変数の統一**
   - `currentType`（型環境の多相型）ではなく
   - `tiExprType currentFunc`（インスタンス化された型）を使用
   - これにより統一された型変数（`t0`）が保持される

3. **制約の伝搬**
   - ラムダラッパー自体は制約を持たない（単なるパススルー）
   - 部分適用の場合のみ制約を保持
   - 完全適用の結果には制約を付けない

## 型クラス展開の仕組み (Phase 8b)

### 目的

型クラスメソッド呼び出しを実行時の辞書ディスパッチに変換：

```egison
-- 変換前（型クラスメソッド）
(+ : {Num t0} t0 -> t0 -> t0) (x : t0) (y : t0)

-- 変換後（辞書ディスパッチ）
(\dict_Num ->
  (dict_Num_("plus") (x : t0) (y : t0) : t0)
: Tensor t0 -> Tensor t0 -> t0)
```

### 辞書パッシングの仕組み

型クラス制約 `{Num t0}` は実行時の辞書パラメータに変換される：

1. **関数定義時**：制約 → 辞書パラメータ
   ```egison
   -- 元の定義
   def f : {Num a} a -> a := \x -> x + 1

   -- 変換後
   def f := \dict_Num x -> dict_Num_("plus") x 1
   ```

2. **関数呼び出し時**：型に基づいて辞書を選択
   ```egison
   -- 呼び出し
   f (3 : Int)

   -- 変換後（Int用の辞書を渡す）
   f numInt 3
   ```

### Eta展開

型変数に制約がある場合、メソッド参照を明示的なラムダに展開：

```egison
-- 変換前
(+ : {Num t0} t0 -> t0 -> t0)

-- 変換後
(\etaVar1 etaVar2 ->
  (dict_Num_("plus")
    (etaVar1 : t0)
    (etaVar2 : t0)
  : t0)
: t0 -> t0 -> t0)
```

重要な型の扱い：
- **部分適用の場合**：制約を保持
  ```haskell
  -- 1引数だけ適用：まだ関数型
  (dict_Num_("plus") x : {Num t0} t0 -> t0)
  ```

- **完全適用の場合**：制約を削除
  ```haskell
  -- 2引数とも適用：値型
  (dict_Num_("plus") x y : t0)  -- 制約なし
  ```

この判定は `applyParamsToType` ヘルパー関数で行われる：
```haskell
resultType = applyParamsToType methodType (length paramExprs)
bodyScheme = case resultType of
               TFun _ _ -> methodScheme  -- 部分適用：制約保持
               _ -> Forall [] [] resultType  -- 完全適用：制約削除
```

## TensorMapと型クラス展開の相互作用

### 処理順序の重要性

TensorMap挿入は型クラス展開の**前**に行われる必要がある：

1. **TensorMap挿入後**：引数型が確定（スカラー vs テンソル）
2. **型クラス展開時**：確定した型を使ってインスタンス選択

逆順だと問題が発生：
```egison
-- 間違った順序の例
-- 型クラス展開が先だと、テンソル型を考慮せずにインスタンスを選択してしまう
```

### 具体例：多相的なドット積

元のコード：
```egison
def . : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 :=
  \t1 t2 -> foldl1 (+) (contract ((*) t1 t2))
```

Phase 8aの後（TensorMap挿入）：
```egison
def . : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 :=
  \t1 t2 ->
    foldl1
      -- (+)にtensorMap2を挿入
      (\tmap2_arg1 tmap2_arg2 ->
        tensorMap2 (+) tmap2_arg1 tmap2_arg2)
      (contract
        -- (*)にもtensorMap2を挿入
        (tensorMap2
          (\x y -> (*) x y)
          t1 t2))
```

Phase 8bの後（型クラス展開）：
```egison
def . : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 :=
  \dict_Num t1 t2 ->
    foldl1
      -- (+)の辞書ディスパッチ + tensorMap2
      (\tmap2_arg1 tmap2_arg2 ->
        tensorMap2
          (\etaVar1 etaVar2 ->
            dict_Num_("plus") etaVar1 etaVar2)
          tmap2_arg1 tmap2_arg2)
      (contract
        -- (*)の辞書ディスパッチ + tensorMap2
        (tensorMap2
          (\x y -> dict_Num_("times") x y)
          t1 t2))
```

## デバッグ用ダンプオプション

開発時は以下のオプションで各段階の出力を確認できる：

- `--dump-typed`：型推論後（Phase 6終了時）
- `--dump-ti`：TensorMap挿入後（Phase 8a終了時）
- `--dump-tc`：型クラス展開後（Phase 8完了時）

これらを比較することで、各フェーズの変換を追跡できる。

上記の結果も正しい。

--dump-tcの結果
```
[283] def . : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 := 
  (\dict_Num t1 t2 -> 
    ((foldl1 : (t0 -> Tensor t0 -> t0) -> [Tensor t0] -> t0) 
      (\tmap2_arg1 tmap2_arg2 -> 
        (tensorMap2 
          (\etaVar1 etaVar2 -> 
            (((dict_Num : Hash String _)_("plus" : String) 
              : {Num t0} t0 -> t0 -> t0) 
              (etaVar1 : t0) 
              (etaVar2 : t0) 
            : t0) 
          : {Num t0} t0 -> t0 -> t0) 
          (tmap2_arg1 : Tensor t0) 
          (tmap2_arg2 : Tensor t0) 
        : Tensor t0) 
      : Tensor t0 -> Tensor t0 -> Tensor t0) 
      (contract 
        (tensorMap2 
          (\tmapVar1 tmapVar0 -> 
            (((dict_Num : Hash String _)_("times" : String) 
              : {Num t0} t0 -> t0 -> t0) 
              (tmapVar1 : t0) 
              (tmapVar0 : t0) 
            : t0) 
          : t0 -> t0 -> t0) 
          (t1 : Tensor t0) 
          (t2 : Tensor t0) 
        : Tensor t0) 
      : [Tensor t0]) 
    : t0) 
  : Tensor t0 -> Tensor t0 -> t0)
```