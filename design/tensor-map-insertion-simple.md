# TensorMap挿入設計（簡略版）

## 目標

明示的なTensor型注釈なしで、多相関数でテンソル操作をサポートする：

```egison
def sum {Num a} (xs: [a]) : a := foldl1 (+) xs

-- 使用例:
sum [1, 2, 3]        -- a = Integer, スカラー加算
sum [t1, t2]         -- a = Tensor Integer, tensorMapによる要素ごとの加算
```

## 簡略化アプローチ

### 基本方針

型コンストラクタの内側にスカラー型（または制約付き型変数）がある場合、その型に対する演算には**常に**tensorMap/tensorMap2を挿入する。

is_tensorフラグによる条件分岐は行わず、tensorMapがスカラーに対しても正しく動作することを前提とする。

### 前提条件

tensorMap/tensorMap2がスカラーに対しても正しく動作すること：

```egison
-- Tensorの場合: 要素ごとに適用
tensorMap f [| 1, 2, 3 |] = [| f 1, f 2, f 3 |]
tensorMap2 f [| 1, 2 |] [| 3, 4 |] = [| f 1 3, f 2 4 |]

-- スカラーの場合: そのまま適用（実質的にidentity）
tensorMap f 5 = f 5
tensorMap2 f 3 4 = f 3 4
```

## 処理フロー

```
Phase 8: TypedDesugar
  ├─ 1. TensorMapInsertion.hs (tensorMap自動挿入)
  │     型コンストラクタ内のスカラー型に対する演算にtensorMap/tensorMap2を挿入
  └─ 2. TypeClassExpand.hs (型クラスメソッド展開)
        (+) → dict_Num_("plus") 等
```

## 挿入ルール

### ルール1: 仮引数の型が直接スカラー型または制約付き型変数の場合

呼び出し側でtensorMapを挿入（従来通り）：

```egison
def inc {Num a} (x: a) : a := x + 1

-- 呼び出し時
inc 5           -- そのまま: inc 5
inc t1          -- tensorMap挿入: tensorMap inc t1
```

### ルール2: 型コンストラクタ内にスカラー型がある場合

関数定義内で**常に**tensorMap/tensorMap2を挿入：

```egison
def sum {Num a} (xs: [a]) : a := foldl1 (+) xs
```

変換後：
```egison
def sum {Num a} (xs: [a]) : a := foldl1 (tensorMap2 (+)) xs
```

## 例のウォークスルー

### 例1: sum関数

```egison
def sum {Num a} (xs: [a]) : a := foldl1 (+) xs
```

#### Phase 5-6: 型推論後

```egison
def sum : {Num t0} [t0] -> t0 :=
  \xs ->
    foldl1 ((+) : {Num t0} t0 -> t0 -> t0) (xs : [t0])
```

- `xs`の型は`[t0]`（型コンストラクタ`[]`の内側に制約付き型変数`t0`）
- `(+)`の引数の型は`{Num t0} t0`でTensorとunify不可
- → tensorMap2を挿入

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
sum [1, 2, 3]
-- tensorMap2 (+) 1 2 → (+) 1 2 → 3（スカラーなのでtensorMapは実質identity）
-- tensorMap2 (+) 3 3 → (+) 3 3 → 6
-- 結果: 6

-- Tensorの場合: t0 = Tensor Integer
sum [t1, t2]  -- t1 = [| 1, 2 |], t2 = [| 3, 4 |]
-- tensorMap2 (+) t1 t2 → [| 1+3, 2+4 |] → [| 4, 6 |]
-- 結果: [| 4, 6 |]
```

### 例2: 複数パラメータ

```egison
def fn (xs: [Integer]) (ys: [Integer]) : Integer :=
  let x = head xs
      y = head ys
  in x + y
```

変換後：
```egison
def fn (xs: [Integer]) (ys: [Integer]) : Integer :=
  let x = head xs
      y = head ys
  in tensorMap2 (i.+) x y
```

呼び出し時：
```egison
fn [1, 2] [3, 4]           -- tensorMap2 (+) 1 3 → 4
fn [t1, t2] [3, 4]         -- tensorMap2 (+) t1 3 → tensorMap (\xi -> xi + 3) t1
fn [1, 2] [t3, t4]         -- tensorMap2 (+) 1 t3 → tensorMap (\yi -> 1 + yi) t3
fn [t1, t2] [t3, t4]       -- tensorMap2 (+) t1 t3 → 要素ごと加算
```

## 複雑なアプローチとの比較

### 複雑なアプローチ（tensor-map-insertion.md）

- is_tensorフラグをパラメータとして渡す
- 各変数のソースを追跡する
- 呼び出し時に条件分岐で展開

```egison
def fn : Bool -> Bool -> [Integer] -> [Integer] -> Integer :=
  \is_tensor_xs is_tensor_ys xs ys ->
    let x = head xs
        y = head ys
    in insertTensorMap2 is_tensor_xs is_tensor_ys (+) x y
```

### 簡略化アプローチ（本ドキュメント）

- is_tensorフラグ不要
- ソース追跡不要
- 常にtensorMapを挿入

```egison
def fn (xs: [Integer]) (ys: [Integer]) : Integer :=
  let x = head xs
      y = head ys
  in tensorMap2 (+) x y
```

## メリット・デメリット

### メリット

1. **実装が単純** - 型を見て機械的にtensorMapを挿入するだけ
2. **is_tensorフラグ不要** - Boolパラメータの追加・管理が不要
3. **ソース追跡不要** - 変数の出所を追跡する必要がない
4. **関数シグネチャが変わらない** - 追加パラメータがないのでAPIが自然

### デメリット

1. **わずかなオーバーヘッド** - スカラーに対してもtensorMapを通す
   - ただし、tensorMapがスカラーを即座に返すなら影響は軽微
2. **tensorMapの実装に依存** - スカラーに対するtensorMapの正しい動作が必須

## 実装チェックリスト

- [ ] tensorMap/tensorMap2のスカラー対応を確認
  - [ ] `tensorMap f scalar` が `f scalar` を返すことを確認
  - [ ] `tensorMap2 f s1 s2` が `f s1 s2` を返すことを確認
  - [ ] 混合ケース（Tensor + スカラー）の動作確認
- [ ] TensorMapInsertion.hsをTypeClassExpand.hsの前に実行するように変更
- [ ] 型コンストラクタ内のスカラー型/制約付き型変数を検出するロジック
- [ ] 該当する演算にtensorMap/tensorMap2を挿入するロジック

## 関連ファイル

- `hs-src/Language/Egison/Type/TensorMapInsertion.hs` - TensorMap挿入
- `hs-src/Language/Egison/Type/TypeClassExpand.hs` - 型クラス展開
- `hs-src/Language/Egison/Type/TypedDesugar.hs` - 処理順序の制御
- `design/tensor-map-insertion.md` - 複雑なアプローチの設計（参考）
