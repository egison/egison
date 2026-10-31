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

## リストと分岐の共通型

```egison
[1] ++ [[|2,3|]]
[[|2,3|]] ++ [1]
```

どちらの式も `[Tensor Integer]` と推論する．スカラーは階数0として受け取り，テンソルの要素型の情報を保つ．リスト構築，先頭への追加，連結，条件分岐は，共通の結果型を求める `commonResultType` を使う．リストとタプルの内部でもテンソルの位置を保ち，未確定な要素の型は，具体的な成分から求めた共通型と照合する．`commonCollectionType` は，連結の両辺と先頭追加の末尾がリストであることも検査する．関数やその他の型構成子の内部のテンソルは，型の照合のために消去しない．

`test/TensorInference.hs` は推論型と挿入を，`test/lib/core/tensor-result-types.egi` は値と，推論型に基づく後続の写像を検査する．

スカラーを階数0として受け取る場合は，その値の型 `a` を `Tensor a` と等しくする代入を要求しない．型の等式として `a = Tensor a` が生じた場合は，正規化 `Tensor (Tensor b) = Tensor b` に従い，`a := Tensor b` という解を構成する．リストなどを介して自分自身を含む型は，循環する型として拒否する．

## 添字と実行時の形状

型 `Tensor a` は階数や軸長を持たない．生成，保存配列，写像の組立ては実行時に形状と添字の一致を検査する．スカラーとテンソルを混在させるリテラルについても，Egison と Lean の受理には差がある．対象と必要な接続は [実装対応文書](../../type-tensor-mech/docs/implementation-audit.md) と [残課題](../../type-tensor-mech/docs/open-problems.md) を参照する．
