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

この変換には、型推論が期待型へテンソル情報を正しく残すことが必要である。
適用推論でデータ側のテンソル情報と持ち上げ後の戻り値型を反映する処理を、
具体例と回帰テストとともに第9節に記す。

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

## 9. 高階引数の型推論と挿入の整合

高階引数とは、別の関数へ引数として渡す関数である。
2026-08-27 の関数適用の推論順序の変更によって、データ側の Tensor が
高階引数の期待型へ反映されなくなる不具合があった。2026-09-06 に修正した。

```egison
def constant (x : Integer) : Integer := 1
map constant [[|1, 2|]]
-- Result: [[|1, 1|]]
-- Inferred type: [Tensor Integer]
```

引数の式自体は記述順に推論する。引数と期待型を照合する段階では、
テンソルを含むデータと高階引数がある場合に、データ、関数、残りのデータの順に
照合する。リストやタプルの内部の Tensor もここで扱う。
テンソルを含むデータと高階引数が揃わない適用では、記述順に照合する。

関数引数の照合では、6月に実装した型に基づく挿入の判定を共有する。
`typeDirectedTensorLiftType` は、その判定が生成する関数の型だけを返す。
例えば `Integer -> Integer` を `Tensor Integer -> Tensor Integer` に
持ち上げる場合、後者を `map` の期待型と照合する。この照合で戻り値の型変数にも
Tensor を反映するため、全体の型は `Tensor [Integer]` ではなく
`[Tensor Integer]` になる。実際の式の挿入は引き続き型推論の後に行う。

`foldl` や `scanl` のスカラー初期値は、高階引数の戻り値がテンソルになることを
反映した後に照合する。スカラーを Tensor の階数0として渡す場合には、
それを理由として適用全体を追加の Tensor で包まない。
例えば `scanl` の戻り値は `[Tensor Integer]` になる。

6月に実装した一引数・複数引数の変換器と、期待型にまだ Tensor が現れない
多相な `sum` 用の二引数の補完は維持する。補完を任意個数の引数へ一般化する
検討は、今回の推論と挿入の接続の修正とは区別する。
型スキームに新しい制約形式を追加する変更は行っていない。

通常の `cabal test` に次の回帰テストを組み込む。

- `test/TensorInference.hs`: 推論型、挿入後の型、受け手と関数引数の型の一致、
  挿入の有無、不正な要素型の拒否を確認する。
- `test/lib/core/tensor-higher-order.egi`: 定数関数を使って実行結果を確認する。
  一引数・二引数・三引数、引数順、リスト・タプル、スカラー初期値、
  部分適用したスカラー関数、let 多相、多相な sum を含む。
- 不要な持ち上げを防ぐため、普通のスカラーのリスト、多相な恒等関数、
  Tensor 全体を明示的に受け取る関数についても確認する。

推論の健全性・完全性・主要型性と挿入の一意性は、実装の回帰テストとは別に
証明する課題である。主要型性は、推論した型と既存の制約から、仕様が許す
他の型を代入で得られることを指す。型の正規化による等式の解と、使用時の
テンソル適用規則も対応させる。
