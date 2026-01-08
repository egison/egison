Egisonのテンソルの添字記法に型をつけたい。

とりあえず、　Tensor a　（a 型のテンソル）とすべての形のテンソルに型をつける。

```
def (.) (t1 : Tensor a) (t2 : Tensor a) : Tensor a := contractWith (+) (t1 * t2)
```

Tensor a型は a型とunifyするとa型になる。
Tensor a型の可能性があるデータにa型であると型注釈する場合は、本当に自信がある場合のみ間違えずに行う必要がある。

```
1 : Tensor Integer -- OK: Integer型になる。Integerは0階のIntegerのテンソルと考えると問題ない。実行時エラーになる心配もない。
[| 1, 2 |] : Integer -- NG: 型検査は通るが、どこかで実行時エラーとなる可能性あり。
```

仮引数の型が  Tensor MathExpr 型（例えばMathExpr型など）とunifyできない型の場合、 Tensor MathExpr 型のデータが引数に渡されると、Egisonの論文で記述されている方法により自動で tensorMap が挿入される。
Tensor MathExpr型とunifyできる型の例には、Tensor MathExpr型、a型、Eq a型などがある。
そのため、テンソルの掛け算や、foldr関数、等価演算子などには、テンソルはテンソルとしてそのまま渡される。
対して、Num aの `+` や `-`、 `*`などの演算子については、成分ごとに処理がmapされる。

テンソルの宣言は以下のようにできる。 

```
def g_i_j : Tensor MathExpr := [| [| 1, 0 |] [|0, r^2 |] |]_i_j

def g~i~j : Tensor MathExpr := [| [| 1, 0 |] [|0, 1 / r^2 |] |]~i_j
```

以下のように、注釈された型が MathExpr 型で型推論された方が Tensor MathExpr 型だった場合、 unify されると MathExpr 型となる。

```
g_i_j . g~i~j : MathExpr -- MathExpr =:= Tensor MathExpr => MathExpr
```

この仕様はcontractされてスカラーになるかテンソルのままなのかこの型システムでは推論できないためである。
正確に型検査するには、contractの結果、スカラーになるのかテンソルになるのかユーザが注釈する必要がある。
そのために、注釈された型が MathExpr 型で型推論された方が Tensor MathExpr 型だった場合、 unify されると MathExpr 型となる。

テンソルの宣言は以下のような糖衣構文である。
添字はテンソルを使う際に初めてテンソルに渡されるという扱い。
プレースホルダ付きのテンソル型は添字を引数にとってテンソルを返す型と考えることができる。
`2#($1 + $2)` は `\x y -> x + y` の糖衣構文。

```
def g_i_j : Tensor MathExpr := [| [| 1, 0 |] [|0, r^2 |] |]_i_j

def g__ : Tensor MathExpr :=  withSymbols [i, j] transpose [i, j] [| [| 1, 0 |] [|0, r^2 |] |]_i_j
```

型パラメータ a と Tensor MathExpr 型はマッチする。
そのため、 a を引数に取る関数は Tensor MathExpr を tensorMap を挿入せずにそのまま受け取る。
そのおかげで fold 関数などもテンソルのことを意識せずに定義できる。

添字が省略されることもある。

```
def X_i : Tensor MathExpr := [| [| 1, 0 |] [|0, 1 |] |]_i

def Y_i : Tensor MathExpr := [| [| 1, 0 |] [|0, 1 |] |]_i
```

添字が省略されている場合、デフォルトでは、同じ添字が補完される。

```
X_i * Y_j : Tensor MathExpr
```

エクスクラネーションマークがついている場合、違う添字が補完される。

```
X_i !* Y_j : Tensor MathExpr
```


inverted scalar argumentは以下のように仮引数に！をつけることで表現する。

```
def ∂/∂ (f : MathExpr) (!x : MathExpr) : MathExpr :=
```
