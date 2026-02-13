# 微分形式の添え字処理とDF インデックスのソート

## 問題の背景

### 発見された問題

`sample/math/geometry/curvature-form.egi` で外微分 `d` の実装が正しく動作していなかった：

```egi
def d (t : Tensor MathExpr) : Tensor MathExpr := !(flip ∂/∂) x t
```

この定義で計算した曲率形式 `Ω~i_j` の assertEqual が失敗：
- 期待値: `[| [| 0, (sin θ)^2 / 2 |], [| -1 / 2, 0 |] |]`
- 実際の値: `[| [| 0, (2 - 3 * (sin θ)^2) / 2 |], [| ((cos θ)^2 + 1) / (2 * (sin θ)^2), 0 |] |]`

### 原因

WedgeApply (`!`) で DF インデックスが付加されるが、その順序が不適切：
- `!(flip ∂/∂) x t` で `x` に `DF 1`、`t` に `DF 2` が付く
- `(flip ∂/∂) x_d1 t_d2` = `∂/∂ t_d2 x_d1`
- `tensorMap2` により結果のインデックス順序: `_d2_d1` (テンソル→座標)
- **外微分では座標インデックスが先であるべき**: `_d1_d2` (座標→テンソル)

## DF (Dummy Free) インデックスの構造

### 定義

```haskell
data Index a
  = Sub a
  | Sup a
  | ...
  | DF Integer Integer  -- DF id n
  deriving (Show, Eq, Ord, ...)
```

### 引数の意味

`DF id n` の2つの引数：

1. **第一引数 (id)**: 引数識別番号
   - `0` = 関数自身
   - `1` = 第1引数
   - `2` = 第2引数
   - `3` = 第3引数
   - ...

2. **第二引数 (n)**: その引数内での通し番号
   - `1` = 1番目の DF インデックス
   - `2` = 2番目の DF インデックス
   - `3` = 3番目の DF インデックス
   - ...

### appendDF の動作

```haskell
appendDF :: Integer -> WHNFData -> WHNFData
appendDF id (ITensor (Tensor s xs is)) =
  let k = fromIntegral (length s - length is)
   in ITensor (Tensor s xs (is ++ map (DF id) [1..k]))
```

テンソルの shape と現在のインデックス数の差分 `k` 個だけ DF インデックスを追加：
- `DF id 1`, `DF id 2`, ..., `DF id k`

### WedgeApply での使用例

`!(flip ∂/∂) x t` の場合 (`Core.hs` の `IWedgeApplyExpr`):

```haskell
func <- appendDF 0 <$> evalExprShallow env func     -- func に DF 0
args <- mapM (evalExprShallow env) args
let args' = map WHNF (zipWith appendDF [1..] args)  -- args に DF 1, DF 2, ...
```

結果：
- `x` (shape `[2]`) → `[DF 1 1, DF 1 2]`
- `t` (shape `[2,2]`) → `[DF 2 1, DF 2 2]`

## 解決策: DF インデックスのソート

### 実装箇所

**ファイル:** `hs-src/Language/Egison/Tensor.hs`

### 1. compareDFNumber 関数の追加

```haskell
-- | Compare DF indices by their ID and sequence numbers
-- Used for sorting DF indices before removal to ensure correct dimension order
compareDFNumber :: Index a -> Index a -> Ordering
compareDFNumber (DF id1 n1) (DF id2 n2) = compare (id1, n1) (id2, n2)
compareDFNumber _ _ = EQ
```

**ソート順序:**
- 第一引数 (id) で一次ソート: 引数の論理的順序
- 第二引数 (n) で二次ソート: 同じ引数内での通し番号順

**例:**
```
DF 1 1 < DF 1 2 < DF 1 3 < DF 2 1 < DF 2 2 < DF 2 3
```

### 2. removeDFFromTensor の修正

```haskell
removeDFFromTensor :: Tensor a -> EvalM (Tensor a)
removeDFFromTensor (Tensor s xs is) = do
  let (ds, js) = partition isDF is
  if null ds
    then return (Tensor s xs is)
    else do
      -- Sort DF indices by their ID number and sequence number before removing
      let sortedDs = sortBy compareDFNumber ds
      Tensor s ys _ <- tTranspose (js ++ sortedDs) (Tensor s xs is)
      return (Tensor s ys js)
removeDFFromTensor t = return t
```

**変更点:**
- DF インデックスを番号順にソート: `sortBy compareDFNumber ds`
- ソート後のインデックス順で `tTranspose`
- 結果の shape が正しい順序になる

### 3. removeDF の修正

`removeDF` の3つのケース（`ITensor`、`Value (TensorData ...)`、その他）すべてに同じロジックを適用。

## 効果と動作

### 修正前

```
!(flip ∂/∂) x t の計算:
1. x に DF 1 が付く: [DF 1 1, DF 1 2]
2. t に DF 2 が付く: [DF 2 1, DF 2 2]
3. tensorMap2 により結果: インデックス [DF 2 1, DF 2 2, DF 1 1, DF 1 2]
4. removeDF が順不同で削除 → shape が [t次元, t次元, x次元, x次元] の順
5. antisymmetrize が間違った次元で反対称化 → 誤った結果
```

### 修正後

```
!(flip ∂/∂) x t の計算:
1. x に DF 1 が付く: [DF 1 1, DF 1 2]
2. t に DF 2 が付く: [DF 2 1, DF 2 2]
3. tensorMap2 により結果: インデックス [DF 2 1, DF 2 2, DF 1 1, DF 1 2]
4. removeDF が番号順にソート: [DF 1 1, DF 1 2, DF 2 1, DF 2 2]
5. tTranspose で並び替え → shape が [x次元, x次元, t次元, t次元] の順 ✓
6. antisymmetrize が正しい次元で反対称化 → 正しい結果 ✓
```

### 外微分での意味

外微分 `d` が作る微分形式の構造：
- **座標インデックスが先**: `∂/∂x^i` の i
- **テンソルインデックスが後**: テンソル成分の j, k

例: `d ω~i_j` の結果は `∂ω^i_j/∂x^k` で、インデックス順序は `_k_i_j` (座標 k が最初)。

これにより、`antisymmetrize` が正しく外微分の公式を計算：
```
dω = (∂ω_b/∂x^a - ∂ω_a/∂x^b) dx^a ∧ dx^b
```

## テストケース

### 1. antisymmetrize の基本動作

**テストファイル:** `mini-test/239-antisymmetrize-test.egi`

- 反対称テンソル → そのまま保存 ✓
- 対称テンソル → ゼロになる ✓
- 一般テンソル → (A_ij - A_ji)/2 ✓

### 2. curvature-form の計算

**テストファイル:** `sample/math/geometry/curvature-form.egi`

曲率形式 `Ω~i_j = antisymmetrize(d ω~i_j + ω~i_k ∧ ω~k_j)` が正しく計算される ✓

## 一般性への影響

### 他の用途への影響

DF インデックスは WedgeApply (`!` 演算子) でのみ使用されるため：
- **影響範囲**: WedgeApply を使う微分形式の計算のみ
- **既存コード**: DF を使わない通常のテンソル計算には影響なし ✓

### sortBy のコスト

DF インデックスは通常少数（1-10個程度）なので、ソートのコストは無視できる。

## 関連ファイル

- **実装**: `hs-src/Language/Egison/Tensor.hs`
  - `compareDFNumber` (218-222行目)
  - `removeDFFromTensor` (225-235行目)
  - `removeDF` (237-256行目)

- **使用箇所**: `hs-src/Language/Egison/Core.hs`
  - `IWedgeApplyExpr` の処理 (452-466行目)
  - `appendDF` で DF インデックス付加

- **テスト**:
  - `mini-test/239-antisymmetrize-test.egi`
  - `sample/math/geometry/curvature-form.egi`

## 今後の課題

### 考慮事項

1. **3引数以上の WedgeApply**
   - 現在は2引数が主な用途
   - 3引数以上でも `DF 1, DF 2, DF 3, ...` の順でソートされるため問題なし

2. **ネストした WedgeApply**
   - `!(!(f x) y)` のようなネストでも、各レベルで DF が付く
   - 内側の DF と外側の DF の区別が必要な場合は追加の考慮が必要

3. **パフォーマンス**
   - 現在のソートは O(n log n) で n は DF インデックス数
   - 微分形式計算では通常 n < 10 なので問題なし

## まとめ

DF インデックスを **(id, n)** のタプルでソートすることで：
- 引数の論理的順序を保持
- 同じ引数内の通し番号順も保持
- 外微分などの微分形式計算で正しい次元順序を保証

この修正により、Egison の微分幾何計算が数学的に正しい結果を返すようになった。
