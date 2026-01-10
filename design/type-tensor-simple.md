# Egisonのテンソルの添字記法に型をつけたい。

## テンソルとスカラーの型付け

とりあえず、　Tensor a　（a 型のテンソル）とすべての形のテンソルに型をつける。
例えば、テンソル同士の掛け算の演算子である`.`には以下のように型がつく。

```
def (.) (t1 : Tensor a) (t2 : Tensor a) : Tensor a := contractWith (+) (t1 * t2)
```

`.`の返り値はスカラーである場合もある。
例えば、ベクトル同士の内積を `v1~i . v2_i` のように表現するとその評価値はスカラーになる。
しかし、型推論で`.`の返り値がスカラーかテンソルか判断することはできない。

この問題を解決するために二つの型規則を追加する。

通常、Tensor a型とa型をunifyするとTensor a型になるとする。
a型は0階のテンソルと考えることもできるので、この解釈で実行時エラーになることはない。

ただし、トップレベル定義のテンソルについてのみは、Tensor a型が a型とunifyするとa型になるようにする。
Tensor a型の可能性があるデータにa型であると型注釈する場合は、ユーザーは間違えずに型注釈する必要がある。

```
1 : Tensor Integer -- Integer型になる。Integerは0階のIntegerのテンソルと考えると問題ない。実行時エラーになる心配もない。
[| 1, 2 |] : Integer -- 型検査は通るが、どこかで実行時エラーとなる可能性あり。
```

## スカラーについてのみ定義された関数にテンソルが適用されたときの自動map処理について

仮引数の型が  Tensor MathExpr 型（例えばMathExpr型など）とunifyできない型の場合、 Tensor MathExpr 型のデータが引数に渡されると、Egisonの論文で記述されている方法により自動で tensorMap が挿入される。
Tensor MathExpr型とunifyできる型の例には、Tensor MathExpr型、a型、Eq a型などがある。
そのため、テンソルの掛け算や、foldr関数、等価演算子などには、テンソルはテンソルとしてそのまま渡される。
対して、Num aの `+` や `-`、 `*`などの演算子については、成分ごとに処理がmapされる。
そのおかげで foldr 関数などの高階関数やスカラーである数についての演算子の定義もテンソルのことを意識せずに定義できる。

```
class Num a where
  (+) (x: a) (y: a) : a

instance Num Integer where
  (+) x y := (b.+ x y)


def double {Num a} (x: a) : a := x + x

double t1
```
上記のコードの処理は以下のように行われる。
`double t1`は`tensorMap (\te1 -> double te1) t1`にIInfer.hsで変換される。
`tensorMap (\te1 -> double te1) t1`は、TypeClassExpand.hsで`tensorMap (\tmapVar1 -> double numInteger te1) t1`に変換される。

## テンソルの宣言について

テンソルの宣言は以下のようにできる。 

```
def g_i_j : Tensor MathExpr := [| [| 1, 0 |] [|0, r^2 |] |]_i_j

def g~i~j : Tensor MathExpr := [| [| 1, 0 |] [|0, 1 / r^2 |] |]~i_j
```

以下のように、トップレベルで注釈された型が MathExpr 型で型推論された方が Tensor MathExpr 型だった場合、 unify されると MathExpr 型となる。

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

# 実装の手順

関数の仮引数の型と引数の型がすでに正しく推論されていると仮定する。
下記のIApplyExprの型推論の際に型検査が失敗したら、tensorMapを挿入して型推論が通るようにプログラムを書き換える。


```
def f (x : Integer) : Integer := x

def g (x : Integer) (y : integer) : Integer := x + y

def t1 := [| 1, 2 |]

def t2 := [| 1, 2 |]

f t1 --=> tensorMap (\t1e -> f t1e) t1

g t1 t2 --=>  tensorMap (\t1e -> tensorMap (\t2e -> g t1e t2e) t2) t1
```