# Egisonのテンソルの添字記法の型付け

## 概要

Egisonでは、テンソル計算においてアインシュタインの縮約記法をサポートしています。
このドキュメントでは、テンソル型システムの設計と実装について説明します。

## テンソルとスカラーの型付け

すべてのテンソルは `Tensor a` 型（a 型のテンソル）として型付けされます。
テンソルの形状（次元）は型レベルでは区別しません。

例えば、テンソル同士の内積演算子である `.` には以下のように型がつきます：

```egison
def (.) {Num a} (t1: Tensor a) (t2: Tensor a) : Tensor a := 
  foldl1 (+) (contract (t1 * t2))
```

### 型規則

`.` の返り値はスカラーである場合もあります。
例えば、ベクトル同士の内積を `v1~i . v2_i` のように表現するとその評価値はスカラーになります。
しかし、型推論で `.` の返り値がスカラーかテンソルか判断することはできません。

この問題を解決するために、以下の型規則を採用しています：

1. **通常のunify**: `Tensor a` 型と `a` 型をunifyすると `Tensor a` 型になる
   - `a` 型は0階のテンソルと考えることができるため、この解釈で実行時エラーになることはない

2. **トップレベル定義**: トップレベル定義においては、`Tensor a` 型が `a` 型とunifyすると `a` 型になる
   - ユーザーが型注釈で `a` 型と指定した場合、その型が優先される

```egison
1 : Tensor Integer  -- Integer型になる。Integerは0階のIntegerのテンソルと考えると問題ない
[| 1, 2 |] : Integer  -- 型検査は通るが、実行時エラーになる可能性あり
```

## シンボル宣言 (declare symbol)

テンソル計算では、シンボリック変数（a11, a12 など）を使うことが多いです。
これらの変数に対する「未定義変数」警告を抑制するため、`declare symbol` 構文を用意しています。

### 構文

```egison
declare symbol <name1>, <name2>, ... : <Type>
```

型を省略した場合、デフォルトで `Integer`（= MathExpr）型になります。

### 使用例

```egison
-- シンボルを宣言
declare symbol a1, a2, b1, b2 : Integer
  
def v1 : Tensor Integer := [| a1, a2 |]
def v2 : Tensor Integer := [| b1, b2 |]

v1~i . v2_i  -- a1 * b1 + a2 * b2
v1_i . v2_j  -- [| [| a1 * b1, a1 * b2 |], [| a2 * b1, a2 * b2 |] |]_i_j

-- 行列用のシンボル
declare symbol a11, a12, a21, a22, b11, b12, b21, b22 : Integer

def m1 : Tensor Integer := [| [| a11, a12 |], [| a21, a22 |] |]
def m2 : Tensor Integer := [| [| b11, b12 |], [| b21, b22 |] |]

m1~i~j . m2_j_k  -- 行列積の結果
-- [| [| a11 * b11 + a12 * b21, a11 * b12 + a12 * b22 |], 
--    [| a21 * b11 + a22 * b21, a21 * b12 + a22 * b22 |] |]~i_k
```

### 実装

`declare symbol` の実装は以下のファイルで行われています：

1. **AST** (`Language/Egison/AST.hs`)
   ```haskell
   data TopExpr = ... | DeclareSymbol [String] (Maybe TypeExpr)
   ```

2. **IExpr** (`Language/Egison/IExpr.hs`)
   ```haskell
   data ITopExpr = ... | IDeclareSymbol [String] (Maybe Type)
   data TITopExpr = ... | TIDeclareSymbol [String] Type
   ```

3. **パーサー** (`Language/Egison/Parser/NonS.hs`)
   - `declare` を予約語として追加
   - `declareSymbolExpr` パーサーを追加

4. **型推論** (`Language/Egison/Type/IInfer.hs`)
   ```haskell
   data InferState = InferState
     { ...
     , declaredSymbols :: Map.Map String Type  -- 宣言されたシンボルと型のマッピング
     }
   ```
   - `lookupVar` で型環境に変数がない場合、`declaredSymbols` をチェック
   - 登録されている場合は警告なしでその型を返す

## スカラー関数へのテンソル適用時の自動 tensorMap 挿入

仮引数の型が `Tensor a` 型とunifyできない場合、`Tensor a` 型のデータが引数に渡されると、
自動的に `tensorMap` が挿入されます。

### 例

```egison
class Num a where
  (+) (x: a) (y: a) : a

instance Num Integer where
  (+) x y := (i.+ x y)

def double {Num a} (x: a) : a := x + x

double t1  -- テンソル t1 に対して double を適用
```

上記のコードは以下の順序で変換されます：

1. **型推論（IInfer.hs）**: 型情報を収集し、TIExpr（型付き内部表現）を生成
   ```egison
   double t1  -- double : {Num a} a -> a, t1 : Tensor Integer
   ```

2. **tensorMap 挿入（TensorMapInsertion.hs）**: 型情報を使って必要な箇所に tensorMap を挿入

3. **型クラス展開（TypeClassExpand.hs）**: 型情報に基づいて型クラスを処理

この順序が重要です。tensorMap挿入後に引数の型（スカラー vs テンソル）が確定するため、
型クラス展開でunifyStrictを使ったインスタンス選択が正しく動作します。

### 型クラス展開の詳細

型クラスの展開は一貫して辞書渡しスタイルで実装されています。

#### ケース1: 型が具体的に決まっている場合

呼び出し時に型が具体的に決まっている場合、具体的な辞書へのアクセスに変換されます：

```egison
double (1 : Integer)
-- 型推論で a = Integer と決定
-- (+) が numInteger_"plus" に展開される（辞書アクセス形式）
-- 結果: (numInteger_"plus") 1 1
```

`numInteger` は `Num Integer` インスタンスの辞書で、
`{ "plus": numIntegerPlus, "times": numIntegerTimes, ... }` のようなハッシュです。

#### ケース2: 型が多相的なまま残っている場合

関数定義自体など、型が多相的なまま残っている場合は、辞書パラメータを受け取る形に変換されます：

```egison
-- 元の定義
def double {Num a} (x: a) : a := x + x

-- 辞書パラメータを追加した形に変換
def double (dict_Num_a) (x: a) : a :=
  (dict_Num_a_"plus") x x
```

呼び出し側では、具体的な型に応じた辞書が渡されます：

```egison
double numInteger 1
-- numInteger 辞書を渡して呼び出し
```

#### tensorMap 挿入との組み合わせ

tensorMap挿入の後、TypeClassExpand.hs で型クラス展開が行われます：

```egison
double t1  -- t1 : Tensor Integer
-- 1. tensorMap が挿入される
-- 2. (+) が (numInteger_"plus") に展開される
-- 結果: tensorMap (\te1 -> (numInteger_"plus") te1 te1) t1
```

多相的な関数の場合も同様です：

```egison
double numInteger t1
-- 1. tensorMap が挿入される
-- 2. 辞書パラメータ付きの形に変換
-- 結果: tensorMap (\te1 -> (numInteger_"plus") te1 te1) t1
```

### tensorMap 挿入の条件

以下の条件を満たす場合、`tensorMap` が挿入されます：

- 仮引数の型が `Tensor a` 型とunifyできない
  - 例：`Integer`, `Float`, `Bool` など具体的な型
  - 例：型クラス制約付きの型変数 `Num a => a`（`Tensor` は `Num` のインスタンスでないため）

以下の場合は `tensorMap` は挿入されません：

- 仮引数の型が `Tensor a` 型とunifyできる
  - 例：型変数 `a`（任意の型を受け入れる）
  - 例：`Tensor a` 型
- `foldr` などの高階関数

## テンソルの宣言

テンソルは添字付きで宣言できます：

```egison
def g_i_j : Tensor MathExpr := [| [| 1, 0 |], [| 0, r^2 |] |]_i_j

def g~i~j : Tensor MathExpr := [| [| 1, 0 |], [| 0, 1 / r^2 |] |]~i~j
```

トップレベルで型注釈が `MathExpr` 型で、型推論された型が `Tensor MathExpr` 型の場合、
unifyすると `MathExpr` 型になります：

```egison
g_i_j . g~i~j : MathExpr  -- contract されてスカラーになる場合
```

この仕様は、`contract` の結果がスカラーになるかテンソルのままなのか、
型システムでは推論できないためです。

## 高階関数とテンソル

高階関数にテンソル操作を渡す場合も正しく動作します：

```egison
def foldl {a, b} (fn : b -> a -> b) (init : b) (ls : [a]) : b :=
  match ls as list something with
    | [] -> init
    | $x :: $xs ->
      let z := fn init x
       in seq z (foldl fn z xs)
  
def foldl1 {a, b} (fn : b -> a -> b) (ls : [a]) : b := foldl fn (head ls) (tail ls)

def (.) {Num a} (t1: Tensor a) (t2: Tensor a) : Tensor a := 
  foldl1 (+) (contract (t1 * t2))
```

関数が関数の引数に渡された場合、引数の関数の引数型が `Tensor` 型とunifyできない場合は、
eta展開して `tensorMap` でラップします。

## 実装ファイル

テンソル型システムに関連する主要なファイル：

| ファイル | 役割 |
|---------|------|
| `Language/Egison/Type/IInfer.hs` | 型推論、tensorMap 挿入 |
| `Language/Egison/Type/TensorMapInsertion.hs` | tensorMap 挿入のヘルパー |
| `Language/Egison/Type/TypeClassExpand.hs` | 型クラスの展開 |
| `Language/Egison/Type/Unify.hs` | 型のunification |
| `Language/Egison/AST.hs` | AST定義（DeclareSymbol含む） |
| `Language/Egison/IExpr.hs` | 内部表現（IDeclareSymbol含む） |
| `Language/Egison/Parser/NonS.hs` | パーサー（declare symbol） |
| `Language/Egison/EnvBuilder.hs` | 環境構築 |
| `Language/Egison/Desugar.hs` | デシュガー |
| `Language/Egison/Eval.hs` | 評価器 |

## テストファイル

- `mini-test/91-index-notation.egi` - テンソル添字記法とシンボル宣言のテスト
- `mini-test/91-index-notation-with-declare.egi` - declare symbol のテスト


## 高階関数の型推論の流れ その1

```
foldl1 (+) [t1, t2]
```

foldl1 : (a -> a -> a) -> [a] -> a
+ : {Num b} b -> b -> b
[t1, t2] : [Tensor Integer]

まずfoldl1の第一引数と+をunifyすると
foldl1 : {Num b} (b -> b -> b) -> [b] -> b
となる。
次に第二引数と[t1, t2]をunifyすると
unify [b] [Tensor Integer]
の結果で
foldl1 : {Num (Tensor Integer)} (Tensor Integer -> Tensor Integer -> Tensor Integer) -> [Tensor Integer] -> Tensor Integer
となる。
ただし、ここでTensor IntegerはNumのインスタンではないが、IntegerはNumのインスタンスであるので、クラス制限だけテンソルの要素を取り出し、
foldl1 : {Num Integer} (Tensor Integer -> Tensor Integer -> Tensor Integer) -> [Tensor Integer] -> Tensor Integer
と型推論するようにしたい。

## 高階関数の型推論の流れ その2

```
def (.) {Num a} (t1: Tensor a) (t2: Tensor a) : Tensor a := 
  foldl1 (+) (contract (t1 * t2))
```

t1, t2: Tensor b
とするとt1 * t2の型はTensor b
またcontract後の型は[Tensor b]となるはずである。
（現在contract後の型が[result7]でないといけないのに[result8]となっている。）

## 高階関数の型推論の流れ その3

def Ric__ : MathExpr := (withSymbols [i, j] (transpose ([(i : MathExpr), (j : MathExpr)] : [MathExpr])
 (withSymbols [m] ((sum : [MathExpr] -> MathExpr) 
 (contract ((R : Tensor MathExpr)~(m : MathExpr)_(i : MathExpr)_(m : MathExpr)_(j : MathExpr) : Tensor MathExpr) : [Tensor MathExpr])
  : MathExpr) : MathExpr) : MathExpr) : MathExpr)