# `tensorMap` 自動挿入

この文書は、型推論後の `TIExpr` に `tensorMap` または `tensorMap2` を挿入し、
スカラー関数をテンソルへ成分ごとに適用する現在の規則を定める。

`tensorMap f x` は `x` がスカラーなら通常の `f x` と同じように振る舞い、
`tensorMap2 f x y` も両引数がスカラーなら通常の `f x y` と同じように振る舞う。
この性質により、縮約の途中でスカラーからテンソルへ変わる値にも同じ wrapper を使える。

## 1. 処理順

自動挿入は型推論の後、型クラス辞書の展開より前に行う。

```text
Infer
  ↓ 型つき TIExpr
TensorMapInsertion
  ↓ tensorMap / tensorMap2 を含む TIExpr
TypeClassExpand
  ↓ 要素型の辞書アクセスを含む TIExpr
Evaluation
```

先に tensor-lift を決めることで、型クラス展開は `Tensor a` ではなく、成分へ適用される
`a` のインスタンス辞書を選べる。

## 2. 直接適用

関数適用 `f argument` について、仮引数型がスカラーとして確定し、実引数型が
`Tensor a` なら、成分ごとの適用へ変換する。

```egison
def inc (x: Integer) : Integer := x + 1
def t := [| 1, 2, 3 |]

inc t
-- tensorMap (\x -> inc x) t
```

二つの引数を同時に持ち上げられるときは `tensorMap2` を使う。三つ以上の位置が必要なら、
`tensorMap` と `tensorMap2` を入れ子にしたイータ展開を生成する。

## 3. スカラー型の判定

`isPotentialScalarType` は、型を fresh な `Tensor a` と厳密単一化できるかで判定する。
厳密単一化は `Tensor a` と `a` を同一視しない。

- `Tensor t` はテンソルである。
- `Integer` などの具体型はスカラーである。
- 制約なしの型変数 `a` は `Tensor t` にもなれるため、スカラーと断定しない。
- `{AddSemigroup a}` のような制約付き変数で `Tensor t` に対応するインスタンスがなければ、
  その位置は成分型へ適用するスカラー位置である。

`IO`、`IORef`、`Port`、関数型、およびこれらを内部に含む型は、スカラーに見えても
tensor-lift しない。成分ごとの適用へ変えると制御や資源の意味が変わるためである。

## 4. 高階関数の callback

高階関数へ関数を引数として渡す場合は、外側の関数が期待する callback 型と、渡された関数の
実際の型を比較する。

例えば、期待 callback の第2引数が `Tensor Integer` で、渡された関数の第2引数が
`Integer` なら、その位置を lift する。

```egison
map scalarFunction tensors
-- map (\x -> tensorMap scalarFunction x) tensors
```

`callbackLiftMask` は、持ち上げる callback 引数位置を固定点まで計算する。固定点とは、
一度決めた位置から新しい必要位置を導き、増えなくなるまで繰り返した結果である。

## 5. 返り値が次の引数へ戻る場合

`foldl` や `foldr` では、callback の返り値が次回の accumulator 引数へ戻る。
テンソル引数を一つ持ち上げると callback の返り値もテンソルになりうるため、同じ型の
accumulator 引数も持ち上げる。

```egison
foldl (+) 0 [[| 1, 2 |], [| 3, 4 |]]

-- 概念上の callback
\acc x -> tensorMap2 (\a b -> a + b) acc x
```

初回の `acc` はスカラー 0、次回以降はテンソルになりうるが、`tensorMap2` はどちらにも使える。
callback の返り値がリストの要素として包まれる `map` では accumulator への戻りがないため、
この伝播を起こさない。詳細は
[tensor-map-higher-order-lift.md](./tensor-map-higher-order-lift.md) を参照する。

## 6. 二引数 callback の互換経路

多相な `foldl1 (+)` のように、期待型へまだ具体的な `Tensor` が現れない場合は、型主導の種を
作れない。渡された値がちょうど二引数の lift 可能なスカラー関数で、期待型のどこかが
テンソルを取りうる場合は、互換経路として `tensorMap2` wrapper を作る。

```egison
foldl1 (+) xs
-- foldl1 (\x y -> tensorMap2 (+) x y) xs
```

期待 callback の全ての引数と返り値が、テンソルになりえない具体的な非 CAS スカラー型なら、
この wrapper は挿入しない。型主導の wrapper が作れる場合は、常にそちらを優先する。

## 7. Wedge 適用

`TIWedgeApplyExpr` の関数が二引数のスカラー関数なら、
`TITensorMap2WedgeExpr` へ変換し、微分形式の添字を補ってから成分ごとに適用する。
関数がテンソル全体を引数として受け取る場合は、通常の Wedge 適用を保つ。

既に `tensorMap` 系の内部節へ変換済みの式は再び包まない。

## 8. 実装と検証

実装は `hs-src/Language/Egison/Type/TensorMapInsertion.hs` に集約する。

- `shouldInsertTensorMap`: 直接適用の判定。
- `isPotentialScalarType`: 制約を考慮したスカラー判定。
- `callbackLiftMask`: 高階 callback の lift 位置と feedback の固定点。
- `wrapWithTypeDirectedTensorLift`: 型主導のイータ展開。
- `shouldUseTensorMap2Fallback`: 二引数互換経路の判定。
- `insertTensorMaps`: 変換の入口。

変換順は `Type/TypedDesugar.hs` が管理する。回帰は `test/lib/math/tensor.egi` と
`sample/math/geometry/` の縮約・微分形式・曲率計算で検証する。
