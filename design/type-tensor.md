Egisonのテンソルの添字記法に型をつけたい。

テンソルについての演算子に型をつけたい。

```
contractWith : (a -> a -> a) -> (Tensor a is) -> Tensor a (RemoveSupSubPairs is)

def (.) (t1 : Tensor a is1) (t2 : Tensor a is2) 
  : Tensor a (RemoveSupSubPairs (mergeIndices is1 is2))
  := contractWith (+) (t1 * t2)
```

`+` や `*` などスカラーに対して定義された関数はテンソルに適用された場合、自動でテンソル積のような形でmapされる。

テンソルの宣言は以下のようにできる。 

```
def g_i_j : Tensor Integer [2, 2]_i_j := [| [| 1, 0 |] [|0, r^2 |] |]_i_j

def g~i~j : Tensor Integer [2, 2]~i~j := [| [| 1, 0 |] [|0, 1 / r^2 |] |]~i_j

g_i_j . g~i~j : Integer -- Tensor Integer [] = Integer
```

上記のテンソルの宣言は以下のような糖衣構文である。
添字はテンソルを使う際に初めてテンソルに渡されるという扱い。
プレースホルダ付きのテンソル型は添字を引数にとってテンソルを返す型と考えることができる。
`2#($1 + $2)` は `\x y -> x + y` の糖衣構文。

```
def g_i_j : Tensor Integer [2, 2]_i_j := [| [| 1, 0 |] [|0, r^2 |] |]_i_j

def g__ : 2#(Tensor Integer [2, 2]_$1_$2) :=  withSymbols [i, j] transpose [i, j] [| [| 1, 0 |] [|0, r^2 |] |]_i_j
```

添字が省略されることもある。

```
def X_i : Tensor Integer [2, 2]_i := [| [| 1, 0 |] [|0, 1 |] |]_i

def Y_i : Tensor Integer [2, 2]_i := [| [| 1, 0 |] [|0, 1 |] |]_i
```

添字が省略されている場合、デフォルトでは、同じ添字が補完される。

```
X_i * Y_j : Tensor Integer [2, 2, 2]_i_j
```

エクスクラネーションマークがついている場合、違う添字が補完される。

```
X_i !* Y_j : Tensor Integer [2, 2, 2, 2]_i_j
```

0階のテンソルはスカラーであるという型規則も欲しい。

```
Tensor a [] = a
```

パラメータによって、テンソルのサイズが変わることもある。
下記のテンソル型は添字の型を引数に取らないが、それはカリー化されているからと考えることができる。

```
def zeroTensor (ns : [Integer]) : Tensor Integer ns := generateTensor (\_ -> 0) ns
```

inverted scalar argumentは以下のように仮引数に！をつけることで表現する。

```
def ∂/∂ (f : MathExpr) (!x : MathExpr) : MathExpr :=
```

## Research Qeuestions

- より良い構文はあるか？
- 自動型推論できるような型規則は設計できるか？
- できるとしたらどのような型規則になるか？