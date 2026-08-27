# Egison CAS の商型

この文書は、整数の剰余環や有限体など、同じ表現に別の等価関係を入れる CAS 型の
現行仕様をまとめる。商型は [CAS 型タワー](./type-cas-tower-implementation.md) とは
独立している。

## 1. 型タワーから分ける理由

CAS 型タワーの暗黙の昇格は、値を変えずに表現だけを組み替える。商への射影はこの条件を
満たさない。例えば整数 8 を 5 を法とする剰余類へ移すと代表元は 3 になる。

また、商の等値性は型に依存する。整数としての 12 と 2 は異なるが、5 を法とする商では
等しい。このため、商型は次のように扱う。

- 基底型とは単一化しない不透明な型名を持つ。
- `declare cas-subtype` の順序と join に参加しない。
- 基底表現との移動には、生成された `projQ` と `reprQ` を明示的に使う。
- 等値性と演算には、その商型専用の型クラス instance を使う。

## 2. 宣言と展開

宣言は次の形である。

```egison
declare cas-quotient Mod7 := Integer by (\n -> modulo n 7)
```

`Eval.expandCasQuotientDecls` は、環境構築より前にこの宣言を通常の Egison 定義へ展開する。
型名 `Mod7` は内部では `TInductive Mod7 []` に対応し、同名の型とだけ単一化する。

現在生成される主な定義は次のとおりである。

```egison
def reduceMod7 := \n -> modulo n 7
def projMod7 (x : MathValue) : Mod7 := casQuotientCast (reduceMod7 x)
def reprMod7 (v : Mod7) : MathValue := casQuotientCast v
```

宣言に書いた基底型は構文と型能力の検査を受けるが、生成される境界関数の静的な型は現在
`MathValue -> Q` と `Q -> MathValue` に統一されている。このため基底型は説明情報であり、
境界をその型だけに制限する機能はまだない。

`casQuotientCast` は生成コードだけが使う表現保存の型変換である。利用者が商を横断するときは
`projQ` と `reprQ` を使う。

## 3. 自動生成する演算

処理系は次の instance を生成する。

- `Eq`
- `AddSemigroup`, `AddMonoid`, `AddGroup`
- `MulSemigroup`, `MulMonoid`
- `Ring`

加法と乗法は、代表元を `reprQ` で取り出して `MathValue` の演算を行い、結果を `projQ` で
再び簡約する。

```egison
instance AddSemigroup Mod7 where
  (+) a b := projMod7 (reprMod7 a +' reprMod7 b)
```

したがって、各演算の出口で `reduceQ` が実行され、代表元が正規化される。等値性は差を簡約して
0 になるかで判定する。

```egison
instance Eq Mod7 where
  (==) a b := reduceMod7 (reprMod7 a -' reprMod7 b) = 0
```

逆元、最大公約数、順序比較など、基底表現の演算をそのまま商へ移せない演算は自動生成しない。
必要な場合は商型上に直接定義する。

## 4. `reduce` に要求する法則

生成した演算が代表元の選び方に依存しないため、`reduce` には少なくとも次を要求する。

- 冪等性: `reduce (reduce x) = reduce x`
- 加法との整合: `reduce (x +' y) = reduce (reduce x +' reduce y)`
- 乗法との整合: `reduce (x *' y) = reduce (reduce x *' reduce y)`

宣言の展開は、固定された標本集合に対する `assertEqual` を三本生成する。これは一般の値すべてを
証明する検査ではない。標本を通過しても法則が成り立つ責任は宣言者に残る。

## 5. 利用例

```egison
declare cas-quotient Mod7 := Integer by (\n -> modulo n 7)

def a : Mod7 := projMod7 5
def b : Mod7 := projMod7 4

assertEqual "addition" (a + b) (projMod7 2)
assertEqual "multiplication" (a * b) (projMod7 6)
assertEqual "equality" (projMod7 12 == projMod7 5) True
```

基底表現と商値を一つの演算で暗黙に混ぜない。必要な方向を `reprMod7` または `projMod7` で
明示する。

## 6. 有限体と商の合成

有限体 GF(p^k) は、タワーの多項式係数に商型を追加するのではなく、`MathValue` 全体を一つの
商として簡約する。

```egison
declare symbol α
declare cas-quotient GF4 := MathValue by
  finiteFieldReduce 2 [α^2 + α + 1]

def a : GF4 := projGF4 α
def oneGF4 : GF4 := projGF4 1

assertEqual "(α+1)^2 = α"
  (reprGF4 ((a + oneGF4) * (a + oneGF4)))
  α
```

`finiteFieldReduce` は係数の法 p と生成多項式による剰余を一つの `reduce` にまとめる。この方法は
CAS タワーの値保存規則や join を変更しない。

## 7. 現在の制限

- 商型の境界関数は宣言した基底型ではなく `MathValue` を使う。
- 法則検査は有限個の標本による実行時検査であり、証明ではない。
- 商型と基底型の混在は strict 型検査では拒否される。警告を許す通常モードでは、未解決の
  記号的な式として残る場合がある。
- 商型専用の逆元や比較は利用者が定義する。

検収例は [07-modular.egi](./cas-tower-usecases/07-modular.egi)、有限体の回帰テストは
`test/lib/math/quotient-field.egi` に置く。
