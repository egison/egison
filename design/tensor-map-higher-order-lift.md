# 高階 callback の tensor-lift 伝播

## 問題

高階関数にスカラー関数を渡すとき、callback の期待型だけを見ると lift が片側に寄りすぎることがある。

典型例は `foldl` である。

```egison
foldl : (b -> a -> b) -> b -> [a] -> b
```

ここで `$a = Tensor Integer$` のようにリスト要素がテンソルになると、callback の第2引数はテンソルになる。一方で callback の戻り値 `$b$` は次の反復の accumulator、つまり callback の第1引数 `$b$` に戻る。そのため、第2引数だけを lift して

```egison
\acc x -> tensorMap (\xe -> f acc xe) x
```

のようにすると、1回目の結果が `Tensor` になったあと、2回目以降の `acc` がスカラー引数位置に入ってしまう。必要なのは、戻り値 `$b$` が tensor 化し得るなら、同じ `$b$` である accumulator 引数も tensor-lift することである。

## 方針

高階関数の callback 引数について、以下の3段階で lift 位置を決める。

1. 期待 callback 型の引数に `Tensor` が現れ、渡された関数の対応引数が lift 可能なスカラー型なら、その位置を lift の種にする。
2. 種がある場合、渡された関数の戻り値も tensor 化し得るとみなす。
3. callback の戻り値が外側の高階関数の裸の戻り値、または後続引数として戻るなら、callback 戻り値と同じ期待型を持つ callback 引数にも lift を伝播する。

「裸の戻り値」とは、`[b]` や `(b, c)` の内部ではなく、外側の型としてそのまま `$b$` が現れる場合を指す。これにより、`foldl` や `foldr` は feedback として扱い、`map` や `map2` のように callback 戻り値がリスト結果の要素に入るだけの場合は accumulator 伝播を起こさない。

## 例

`foldl (+) 0 [[|1, 2|], [|3, 4|]]` では、型推論後の callback 期待型は概ね次のように見える。

```egison
Integer -> Tensor Integer -> Integer
```

第2引数が `Tensor Integer` なので、まず第2引数が lift の種になる。さらに `foldl` 全体の型では callback 戻り値 `$b$` が外側の戻り値 `$b$` として戻るので、第1引数 `$b$` にも lift を伝播する。

結果として次のような wrapper を作る。

```egison
\acc x -> tensorMap2 (\accElem xElem -> (+) accElem xElem) acc x
```

`tensorMap2` はスカラー引数に対しても通常適用として振る舞うため、初回の `acc = 0` でも、2回目以降の `acc = [|...|]` でも同じ wrapper が使える。

## 3引数以上の callback

同じ問題は、callback が3引数以上でも起こる。

```egison
foldl2 : (c -> a -> b -> c) -> c -> [a] -> [b] -> c
```

この形で `$a$` が `Tensor` になると、callback 戻り値 `$c$` が tensor 化し、その `$c$` が次の accumulator 引数に戻る。したがって、種になったテンソル引数に加えて、戻り値 `$c$` と同じ型の accumulator 引数も lift する。

実装は arity を2に限定せず、必要な lift 位置の列から `tensorMap` と `tensorMap2` を組み合わせた eta-expanded callback を生成する。

## 互換 fallback

型だけでは、汎用定義内の `foldl (*) one xs` のように、まだ具体的な `Tensor` が現れていない場合がある。この場合、上の種が作れない。

そのため、既存の互換挙動として「渡された関数がちょうど2引数の lift 可能なスカラー関数なら `tensorMap2` で包む」fallback は残す。ただし、期待 callback 型から lift 位置が分かる場合は、まず型主導の wrapper を優先する。

## 実装メモ

実装箇所は `hs-src/Language/Egison/Type/TensorMapInsertion.hs`。

- `callbackLiftMask` が lift 位置の固定点を計算する。
- `callbackResultFeedsBack` が外側の高階関数型を見て、callback 戻り値が裸で戻るかを判定する。
- `wrapWithTypeDirectedTensorLift` が lift mask に従って eta-expanded callback を作る。
- `IO`、`IORef`、`Port`、関数型などの effectful/resource-like な型は lift 対象から除外する。
