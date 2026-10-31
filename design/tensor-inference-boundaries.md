# テンソル型推論の仕様と実装の対応

## 保証する仕様

型注釈を要求せず，テンソルが関係しない式の通常の推論型を保つ．高階引数へ渡すスカラー関数の定義時補完を例外とし，未確定な引数へのテンソルは全体のまま渡す．合法な `tensorMap` の挿入で型付けできるプログラムを受理すること，推論型から選んだ挿入を再構成できることを設計の要件とする．

主要型とは，仕様が認める型を，推論した型の型変数への代入で表せる型である．型の正規化は比較に含めるが，スカラーを階数0として受け取ることと，型が等しいことは区別する．

Lean の規則と証明は [type-tensor-mech](../../type-tensor-mech/README.md)，実行による照合は [実装対応文書](../../type-tensor-mech/docs/implementation-audit.md) にまとめる．Haskell 実装との有限の比較と，独立した型規則に対する一般定理は別々の根拠である．

## 複数の高階引数の照合

同じ入力型を共有する複数の関数を渡す場合は，全引数の固定された要求を満たしてから補完する必要がある．次の例では，述語の結果が `Bool` であるため，共有する入力をスカラーにする要求がある．

```egison
def increment x := x+1
def positive x := x > 0
def mapFilter f p xs := map f (filter p xs)
def filterMap p f xs := map f (filter p xs)

\xs -> mapFilter increment positive xs
\xs -> filterMap positive increment xs
```

Haskell 実装は，前者を `Tensor Bool` と `Bool` の不一致で拒否し，後者は `[Integer] -> [Integer]` と推論する．この順序依存を解消するには，一つの適用にある固定された結果型，型変数の共有，入力と結果の対応をまとめて伝播させる必要がある．受理の条件を狭める対策にはしない．

## リスト連結の要素型

```egison
[1] ++ [[|2,3|]]
[[|2,3|]] ++ [1]
```

Haskell 実装は，前者を `[Integer]`，後者を `[Tensor Integer]` と報告する．どちらの値もテンソルを要素に含むため，共通の型は `[Tensor Integer]` とする必要がある．`IJoinExpr` の結果型は，左側の型だけでなく両側の要素型とテンソルを受け取る向きを反映することが完了条件である．

## 添字と実行時の形状

型 `Tensor a` は階数や軸長を持たない．生成，保存配列，写像の組立ては実行時に形状と添字の一致を検査する．スカラーとテンソルを混在させるリテラルについても，Egison と Lean の受理には差がある．対象と必要な接続は [実装対応文書](../../type-tensor-mech/docs/implementation-audit.md) と [残課題](../../type-tensor-mech/docs/open-problems.md) を参照する．
