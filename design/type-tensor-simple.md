# Egison のテンソル型と添字記法

この文書は、現在のテンソル型、添字記法、スカラー関数の自動的な tensor-lift の境界をまとめる。
テンソル計算のアルゴリズムそのものではなく、型推論と型に基づく変換を対象とする。

## 1. テンソル型

テンソルは `Tensor a` と型付けし、`a` は要素型を表す。次元数、各軸の大きさ、添字名、
共変・反変は型には含めない。

```egison
def v : Tensor Integer := [| 1, 2, 3 |]
def m : Tensor Integer := [| [| 1, 0 |], [| 0, 1 |] |]
```

入れ子の `Tensor` 型は正規化で一つにまとめる。

```text
Tensor (Tensor a)  =  Tensor a
```

実行時には、階数 0 のテンソルをスカラーそのものとして表す。例えば
`generateTensor f []` はテンソル容器ではなく `f []` を返す。大きさ 1 のベクトルは
階数 1 なので `Tensor a` のままである。

## 2. 添字記法

添字はテンソル値の各軸に付く実行時情報であり、`Tensor a` の型引数には入らない。

```egison
def g_i_j : Tensor MathValue := [| [| 1, 0 |], [| 0, r^2 |] |]_i_j
def g~i~j : Tensor MathValue := [| [| 1, 0 |], [| 0, 1 / r^2 |] |]~i~j
```

同じ名前の共変添字と反変添字は、`contract` や添字つき演算によって縮約される。縮約後が
スカラーかテンソルかは型だけからは決まらないため、次節の限定的な Tensor とスカラーの
整合規則を使う。

## 3. 三つの単一化モード

テンソルの単一化は、利用目的ごとに次の三モードを使い分ける。

### 通常の推論: `TensorConstraintAware`

型クラス制約を考慮する。制約付き変数 `a` と `Tensor t` を合わせるとき、
`Tensor t` がそのクラスのインスタンスでなければ `a = t` とし、値の側は後続の
tensor-lift に任せる。

例えば `+ : {AddSemigroup a} a -> a -> a` に `Tensor Integer` を渡す場合、
`AddSemigroup (Tensor Integer)` を要求せず、要素型 `Integer` の制約を保つ。

### 厳密な判定: `TensorStrict`

`Tensor a` と `a` を同一視しない。スカラー関数とテンソル引数を区別し、
`tensorMap` を挿入する必要があるかを判定するときに使う。

### トップレベルの期待型: `TensorTopLevel`

トップレベル定義の型注釈と推論型を合わせる境界では、`Tensor a` と `a` の外側を
取り除いて照合できる。縮約結果が実行時に階数 0 となる式へスカラー型を注釈するための規則である。
この規則は値の階数を静的に証明しないため、実際には階数 1 以上の値へスカラー注釈を付けると、
実行時の利用箇所で不整合が現れうる。

## 4. スカラー関数の tensor-lift

型推論の後、`Type/TensorMapInsertion.hs` がスカラー関数へ渡されたテンソル引数を検出し、
関数をテンソル上へ持ち上げる。tensor-lift とは、スカラー関数を各テンソル成分へ適用する
`tensorMap` または `tensorMap2` に変換することである。

```egison
def inc (x: Integer) : Integer := x + 1
inc [| 1, 2, 3 |]

-- 概念上の変換
tensorMap (\x -> inc x) [| 1, 2, 3 |]
```

二つのテンソル引数を持ち上げる場合は `tensorMap2` を使う。片方がスカラーでも
`tensorMap2` が通常適用として扱えるため、縮約や反復の途中でスカラーからテンソルへ変わる場合も
同じ変換を使える。

高階関数に渡す callback では、期待 callback 型のどの引数がテンソルかを調べてイータ展開する。
イータ展開とは、関数を `\x -> f x` の形へ明示的に包み直す変換である。`foldl` のように
callback の返り値が次回の accumulator 引数へ戻る場合は、その引数にも lift を伝播する。
詳細は [tensor-map-higher-order-lift.md](./tensor-map-higher-order-lift.md) を参照する。

期待型だけでは lift 位置が分からない、ちょうど二引数のスカラー関数には、互換経路として
`tensorMap2` で包む規則を残している。全挿入規則は
[tensor-map-insertion-simple.md](./tensor-map-insertion-simple.md) にまとめる。

`IO`、`IORef`、`Port`、関数型など、制御や資源を表す型は tensor-lift の対象にしない。

## 5. Wedge 適用

`!` を伴う Wedge 適用では、スカラー二項関数なら `tensorMap2` の Wedge 版へ変換し、
微分形式の添字を補ってから成分ごとに適用する。関数自身がテンソルを受け取る場合は、
通常の Wedge 適用を保つ。この処理も `TensorMapInsertion.hs` が担当する。

## 6. シンボル宣言

テンソル成分に使う自由な数式シンボルは、未束縛変数ではなく `declare symbol` で宣言する。

```egison
declare symbol a11, a12, a21, a22
declare symbol x, y, z : Float
```

型を省略した場合は `Integer` を使う。宣言は型環境へ登録されるため、参照時に未束縛変数の
警告を出さない。CAS のシンボル値としての構築と型環境への登録は評価の前に行う。

## 7. 処理順と実装

テンソルに関係する型処理は次の順で行う。

1. `Type/Infer.hs` が制約を考慮して型を推論し、型付き内部表現 `TIExpr` を生成する。
2. `Type/TensorMapInsertion.hs` が tensor-lift と Wedge 用の変換を挿入する。
3. `Type/TypeClassExpand.hs` が確定した要素型を使って型クラス辞書を選ぶ。
4. `Tensor.hs` と評価器が添字操作、縮約、実際の成分計算を行う。

型の正規化は `Type/Tensor.hs`、三モードの単一化は `Type/Unify.hs` にある。
処理全体の対応は [FILE_MAPPING.md](./FILE_MAPPING.md) を参照する。

主な回帰テストは `test/lib/math/tensor.egi` と、`sample/math/geometry/` のテンソル計算例である。
