Egisonのテンソルの添字記法に型をつけたい。

とりあえず、　Tensor a　（a 型のテンソル）とすべての形のテンソルに型をつける。

```
def (.) (t1 : Tensor a) (t2 : Tensor a) : Tensor a := contractWith (+) (t1 * t2)
```

Tensor MathExpr 型は MathExpr 型のデータにもマッチできる。
逆に MathExpr 型は Tensor MathExpr 型のデータにはマッチできない。
仮引数の型が  Tensor MathExpr 型にマッチしない型の場合、 Tensor MathExpr 型のデータが引数に渡されるとEgisonの論文で記述されている方法で自動で tensorMap が挿入される。
Tensor MathExpr 型と MathExpr 型が unify されるときは、 MathExpr　型として unify される。

例えば、 `+` や `*` などスカラーに対して定義された関数はテンソルに適用された場合、自動でテンソル積のような形でmapされる。

テンソルの宣言は以下のようにできる。 

```
def g_i_j : Tensor MathExpr := [| [| 1, 0 |] [|0, r^2 |] |]_i_j

def g~i~j : Tensor MathExpr := [| [| 1, 0 |] [|0, 1 / r^2 |] |]~i_j
```

以下のように、Tensor MathExpr 型と MathExpr 型が unify されるときは、 MathExpr　型として unify される。

```
g_i_j . g~i~j : MathExpr -- Tensor MathExpr => MathExpr
```

この仕様はcontractされてスカラーになるかテンソルのままなのかこの型システムでは判断できないためである。
ユーザがスカラーになるのか注釈する必要がある。
しかし、テンソルの積のような演算子の型はテンソルを返すように一般化したいので、ユーザがスカラーが返ってくると明示的に記述したときのみ、スカラーとして型付けするようにしたい。
そのために、Tensor MathExpr 型と MathExpr 型が unify されるときは、 MathExpr　型として unify されるようにする。

上記のテンソルの宣言は以下のような糖衣構文である。
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
