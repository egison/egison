# 高階引数のスカラー関数の自動変換

高階引数とは，別の関数へ引数として渡す関数である．型推論は，渡す関数の型と受け取る位置の型を照合し，成分ごとの適用が必要な引数と返り値を決める．利用者が引数型やテンソルを受け取る位置を指定する必要はない．

## 型から決まる変換

既知のスカラー型の仮引数へテンソルを渡す位置では，`tensorMap` または `tensorMap2` による関数を生成する．未確定な仮引数へテンソルを渡すだけの場合は，テンソル全体を受け取る型を選ぶ．

```egison
\k -> k [|1,2|]
-- (Tensor Integer -> a) -> a

\k -> tensorMap k [|1,2|]
-- (Integer -> a) -> Tensor a
```

高階関数へ渡す既知のスカラー関数は，周囲の定義を一般化する前に，対象となる入力と最終結果をテンソル型へ補完する．スカラー型の判定には，本体の型クラス制約と，ローカルの束縛に保持した制約も使う．

```egison
\xs -> foldl (+) 0 xs
-- [Tensor Integer] -> Tensor Integer

\xs -> foldl (\x y -> x+y) 0 xs
-- [Tensor Integer] -> Tensor Integer
```

## 未確定な受け取り口

受け取り口が周囲の単相な束縛と共有する未確定型なら，渡すスカラー関数から関数型を構成する．単相な束縛とは，すべての使用で一つの型を共有する束縛である．

```egison
\g -> g (\v -> v+10)
-- ((Tensor Integer -> Tensor Integer) -> a) -> a

(\g -> g (\v -> v+10)) (\k -> k [|1,2|])
-- [|11,12|]
```

渡す関数の仮引数そのものが未確定なら，後でスカラーになる可能性だけを理由に変換しない．多相な関数を使う際に新しく作る型変数にも，この補完を要求しない．そのため，恒等関数に渡したスカラー関数や `show map` の通常の型を保つ．関数自体を引数に取る高階関数は，このスカラー関数の補完と区別する．

既知のスカラー引数や結果の要求は固定して照合する．部分適用 `map (\x -> x+1)` は `[Tensor Integer] -> [Tensor Integer]` と推論するが，`(\f -> f 1) (\v -> v+10)` は `Integer` を報告し `11` を返す．

## 累積値への伝播

`foldl : (b -> a -> b) -> b -> [a] -> b` では，渡した関数の結果 `b` が次回の第1引数へ戻る．成分ごとの適用によって結果をテンソル型にする場合は，同じ型を共有する累積値の引数へも変換を伝える．

```egison
foldl (+) 0 [[|1,2|],[|3,4|]]
-- [|4,6|]

-- 渡された関数に対する変換
\acc x -> tensorMap2 (\a b -> a+b) acc x
```

`tensorMap2` はスカラーを階数0としても受け取るため，初回の累積値 `0` と，後続のテンソルの累積値に同じ関数を使える．結果がリストの要素になる `map` では，累積値への伝播は行わない．三つ以上の引数についても，選んだ位置に応じて `tensorMap` と `tensorMap2` を組み合わせる．

## 実装と検証

- `Type/Infer.hs` の `checkApplicationArguments` が，周囲の型変数と引数の制約を使って変換後の型を照合する．
- `Type/TensorMapInsertion.hs` の `buildCallbackLiftPlan` と `typeDirectedTensorLiftType` が，入力と結果の型を求める．
- `callbackLiftMask` が入力間の伝播を計算し，`wrapWithTypeDirectedTensorLift` が実際の関数を生成する．
- 渡された関数の式は，生成するラムダの外側で一度束縛する．入力に `IO`，`IORef`，`Port` や関数型を含む場合は，その型の成分ごとの自動変換を行わない．

推論型と挿入は `test/TensorInference.hs`，実行結果は `test/lib/core/tensor-higher-order.egi` と `test/lib/math/tensor-assembly.egi` で検査する．詳細な挿入条件は [tensor-map-insertion-simple.md](./tensor-map-insertion-simple.md) を参照する．
