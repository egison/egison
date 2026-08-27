# Egison CAS の高度な簡約

この文書は、多変数多項式 GCD、グレブナー基底、イデアル正規形、平方根と指数関数の
構造的簡約について、現在の仕様と実装境界をまとめる。完了済みの実装計画、過去の失敗例、
一時的な性能測定値は含めない。

関連する型と表現は [type-cas.md](./type-cas.md)、型タワーは
[type-cas-tower-implementation.md](./type-cas-tower-implementation.md)、商型は
[type-cas-quotient.md](./type-cas-quotient.md) を参照する。

## 1. 簡約の層

CAS の簡約は、一つの万能な正規化器ではなく、費用と数学的前提が異なる層に分ける。

| 層 | 役割 | 主な入口 |
|---|---|---|
| 構造正規化 | 積和標準形、同類項の結合、零除去、順序の正準化 | `casNormalize` |
| 有理式の約分 | 分子・分母の単項式 GCD、単変数・多変数多項式 GCD | `casNormalizeFrac` |
| 軽い自動規則 | `declare rule auto` の規則を不動点まで適用 | `mathNormalize` |
| 関数固有の簡約 | `sqrt`、`exp` などの適用時規則 | `declare apply` と Haskell の構造規則 |
| イデアル正規形 | 指定した代数関係を法として明示的に正規化 | `polyNF`, `idealNF` |
| 商型の簡約 | 各演算後に商の代表元へ射影 | `declare cas-quotient` の `reduce` |

重いイデアル正規形を全ての算術演算へ自動挿入しない。通常の演算では構造正規化と軽い規則を
使い、代数関係の下での等価判定が必要な地点だけ `idealNF` を明示する。

## 2. 多項式 GCD による有理式の約分

`Math/CAS.hs` の `casNormalizeFrac` は、まず単項式として明らかな共通因子を除き、次に
多項式 GCD を試す。

### 2.1 単変数

`univariateGcdReduce` は、分子と分母が同じ一つの原子を持ち、指数と係数が対応範囲内にあるとき、
有理数係数多項式として GCD を求める。入力が範囲外なら値を変更しない。

### 2.2 多変数

`multivariateGcdReduce` は、主変数を一つ選び、残りの原子を係数環の変数とみなす
subresultant PRS を使う。subresultant PRS は、多項式の擬除算で中間係数が過度に膨らむのを
抑える標準的な多項式剰余列である。

CAS の `SymbolExpr` は、通常のシンボル、数学関数の適用、関数シンボル、クオートされた式を
一様な原子として持てる。そのため、これらを含む多項式も追加の変数化なしで GCD の対象になる。

自由な多項式環で見つかった共通因子を除く操作は、原子が後から関係式を満たす場合にも値を保つ。
一方、関係式を使わなければ見つからない共通因子は検出しない。その場合は次節のイデアル正規形を使う。

検証は `test/lib/math/gcd.egi` と、幾何サンプルの有理式計算で行う。

## 3. グレブナー基底とイデアル正規形

グレブナー基底は、多項式の関係式の集合を、固定した単項式順序の下で一意な剰余を計算できる形へ
完備化した基底である。実装は `lib/math/algebra/groebner.egi` に置く。

### 3.1 エンジン

- `groebnerBasis`: 有理数係数の Buchberger アルゴリズム。
- `groebnerBasisField`: 係数体の演算を引数に取る版。
- `polyNF`: 多項式を基底で割った正規形。
- `polyNFField`: 指定した係数体上の正規形。
- `polyNFStatus`: 適用可否と失敗理由を含む診断用入口。

Buchberger アルゴリズムは S-多項式を繰り返し簡約し、全ての組が基底で 0 に簡約されるまで
基底を追加する。単項式順序は宣言されたシンボル順と、式に初めて現れる複合原子の順から決める。
同じ計算では同じ順序を固定し、正規形の一意性を保つ。

### 3.2 等価判定

二式 `a`, `b` がイデアル `I` の下で等しいかは、別々に正規化して比較するのではなく、
差を一度だけ正規化して判定する。

```text
idealEquals I a b  ⇔  idealNF I (a - b) = 0
```

差を一つの原子順で処理するため、辺ごとに原子順が変わることを防げる。

### 3.3 `declare ideal`

```egison
declare ideal [w^2 + w + 1]
```

`Desugar.hs` は生成元をグレブナー基底へ完備化し、各基底多項式の先頭単項式を右辺へ書き換える
term 級の自動規則を生成する。基底計算は生成された遅延定義が最初に必要になったとき一度だけ行い、
通常の算術経路では既存の軽い規則エンジンだけを使う。

グレブナー基底と正規形の計算中は自由な多項式として処理し、同じ `declare ideal` の自動規則へ
再入しない。結果を通常の CAS 値へ戻した後に、外側の通常簡約が適用される。

`'(expression)` は、`declare rule` による理論上の書き換えを抑えて生成元や規則右辺を構築する。
バッククオートは式を一つの構造的な原子にし、`'f` は関数適用をクオートするため、目的が異なる。

検証は `test/lib/math/groebner.egi`, `test/lib/math/ideal.egi`,
`test/lib/math/normalize-rules.egi` で行う。

### 3.4 三角関数の関係式

`sin x` と `cos x` は既に CAS の原子なので、専用の多項式表現へ変換する必要はない。

```egison
idealNF [((sin x)^2 + (cos x)^2 - 1)] expression
```

この方法は指定した比較地点だけで三角恒等式を使う。全ての演算へ大域的なイデアル簡約を入れないため、
通常計算の停止性と費用を保てる。

### 3.5 原子順

単項式順序の原子列は、`declare symbol` の宣言順を先に置き、その後に生成元または対象式へ現れた
複合原子を初出順で追加する。同じイデアル計算の間はこの列を固定する。

### 3.6 平方根の denesting

`lib/math/algebra/root.egi` の `sqrtDenest` は、平方根の内側に一段の平方根がある形を、
係数比較と符号条件が証明できる場合に限って外へ取り出す。

```text
sqrt(9 - 4 * sqrt(5))  ->  sqrt(5) - 2
```

一般の代数的数の根号簡約を決定する機構ではなく、深さ 2 の既知形に限定する。

### 3.7 主枝の正規化

平方根は複素平方根の主枝を使う。符号判定が負の定数であることを証明できる場合、
`sqrt(-x)` を `i * sqrt(x)` の向きへ正規化する。符号を証明できない記号式では、この変形をしない。

この規則により、同じ主枝を表す根号原子を一つの形へそろえてから、積やイデアル関係を比較できる。
分枝情報を持たない一般の記号式に対して `sqrt(x^2) = x` のような変形は行わない。

### 3.8 有限体

`finiteFieldReduce p generators` は、係数を素数 `p` で簡約し、指定した多項式関係の
グレブナー基底で正規形を求める。この reduce 関数を商型へ渡すことで有限拡大体を作る。

```egison
declare symbol alpha
declare cas-quotient GF4 := MathValue
  by finiteFieldReduce 2 [alpha^2 + alpha + 1]
```

これは `Poly (商型) atoms` を CAS タワーへ追加する方式ではない。有限体全体を一つの商型とし、
タワーの値保存埋め込みと、商の射影を分離する。検証は `test/lib/math/quotient-field.egi` で行う。

## 4. 平方根と指数関数の構造規則

`Math/Rewrite.hs` の `casRewriteSqrt` と `casRewriteExp` は、CAS の構造を直接見て、冪と積に関する
安全な規則を適用する。一般の `declare rule` として毎回 matcher を走らせず、対象となる
外側の型構成子だけで判定する。

自動規則にはトリガーとなるシンボルを記録し、対象値にそのシンボルがなければ規則適用を省く。
さらに項単位のガードで、トリガーを含まない項への matcher 呼び出しを避ける。

現在も、トリガーを含む多数の項に対する matcher の試行は線形に残る。実際の負荷が問題になった
場合は、単一因子の直接判定または先頭シンボルによる規則索引を追加する。これは性能上の課題であり、
簡約結果の仕様を変えるものではない。

## 5. 自動化する簡約と明示する簡約

次の原則を使う。

- 値の表現を一意にする安価な構造正規化は、算術演算の出口で自動適用する。
- 局所的で外側の形から安全に決まる `sqrt`、`exp` の規則は、関数適用時に自動適用する。
- 小さな term 級規則は、トリガー付きの `declare rule auto` として自動適用できる。
- 多項式全体の除算やイデアル関係を必要とする簡約は、`polyNF` または `idealNF` で明示する。
- 商の等価性と代表元は、商型の演算境界で `reduce` により自動適用する。

この分離により、重い理論を必要としない通常の CAS 計算へグレブナー基底の費用を持ち込まない。

## 6. 実装と検証

| 機能 | 実装 | 主な検証 |
|---|---|---|
| 単変数・多変数 GCD | `Math/CAS.hs` | `test/lib/math/gcd.egi` |
| グレブナー基底・正規形 | `lib/math/algebra/groebner.egi` | `test/lib/math/groebner.egi` |
| `declare ideal` | `Desugar.hs`, `EvalState.hs` | `test/lib/math/ideal.egi` |
| 規則抑制クオート | `Desugar.hs` | `test/lib/math/normalize-rules.egi` |
| sqrt/exp 構造規則 | `Math/Rewrite.hs` | `test/lib/math/algebra.egi`, `normalize-rules.egi` |
| denesting と主枝 | `lib/math/algebra/root.egi`, `lib/math/common/interval.egi` | `test/lib/math/algebra.egi` |
| 有限体 reduce | `lib/math/algebra/groebner.egi` | `test/lib/math/quotient-field.egi` |

公開例は `sample/math/algebra/groebner-basis.egi` と、代数的関係を使う
`sample/math/number/` および `sample/math/geometry/` の各プログラムである。
