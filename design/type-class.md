# Egison 型クラスシステム

この文書は、現在実装されている型クラスの仕様、辞書渡しによる実行方式、既知の制限をまとめる。
過去の移行計画や完了済みの不具合一覧は扱わない。

## 1. 表層構文

単一または複数の型パラメータを持つクラスを宣言できる。

```egison
class Eq a where
  (==) (x: a) (y: a) : Bool

class Coerce a b where
  coerce (x: a) : b
```

クラスは複数のスーパークラスを持てる。スーパークラスとは、あるクラスを使うために
同時に必要となる上位の制約である。

```egison
class Ring a extends AddGroup a, MulMonoid a
class Field a extends Ring a, MulGroup a
```

クラスやインスタンスにメソッドがなければ `where` を省略できる。

```egison
class Marker a
instance Marker Integer
```

インスタンスの文脈と関数の型注釈には、型クラス制約を書ける。

```egison
instance {Eq a} Eq (Tensor a) where
  (==) x y := tensorEq x y

def member {Eq a} (x: a) (xs: [a]) : Bool := ...
```

## 2. 型推論

型推論はメソッドの利用から `Constraint` を集める。`Constraint` はクラス名と、
宣言順に並んだ全ての型引数を持つ。

```haskell
Constraint
  { constraintClass :: String
  , constraintTypes :: [Type]
  }
```

スーパークラス制約は再帰的に展開する。例えば `Field a` があれば、`Ring a`、
`MulGroup a` と、それらのスーパークラスが利用できる。インスタンス選択は全型引数を
同時に照合するため、多引数クラスの文脈に現れる変数も位置を保って具体化される。

トップレベルの無注釈再帰は単相再帰として推論する。単相再帰とは、再帰本体の中で
自己関数を一つの型だけで使う方式である。異なる型で再帰的に使う多相再帰には明示的な
型注釈が必要となる。

## 3. 辞書渡し

辞書渡しとは、型クラスの実装をメソッド名から関数へのハッシュとして表し、制約付き関数へ
隠れた引数として渡す実行方式である。処理は次の順になる。

1. `Desugar.hs` がクラスとインスタンスから辞書とメソッド関数を生成する。
2. `Infer.hs` がメソッドの型を推論し、必要な制約を型スキームに残す。
3. `TypeClassExpand.hs` がメソッド呼び出しを辞書アクセスへ変換する。
4. 具体型が分かる呼び出しでは対応するインスタンス辞書を直接適用する。
5. 型変数が残る関数では、一つの最小制約につき一つの辞書引数を追加する。

各辞書は `__super_<Class>` という項目にスーパークラス辞書を保持する。例えば
`Field a` の辞書だけを受け取った関数でも、その項目を順に辿って `MulSemigroup a` の
`(*)` を呼べる。全スーパークラス辞書を平らに並べて渡す経路は使わない。

ローカル変数は同名のトップレベルメソッドより優先する。ラムダ、`let`、`letrec`、
`do`、match 節などの主要な束縛構文では、辞書展開がローカルな名前の範囲を追跡する。

## 4. インスタンスの選択

通常の具体型では、全ての型引数に一致する最も具体的なインスタンスを選ぶ。CAS 型については、
組み込みの部分型関係と `declare cas-subtype` で追加された関係も比較に使う。同じ値集合を
別の正規形で表す候補が並ぶ場合は、対象型の外側の型構成子と一致する候補を優先する。

静的型が `MathValue` またはそれに相当する CAS 型で、静的に辞書を決められない場合は、
[runtime-type-dispatch.md](./runtime-type-dispatch.md) の浅い実行時型ディスパッチを使う。
この実行時経路は単一パラメータクラスの第1型引数だけを対象とする。多引数クラスは静的な
インスタンス選択を使う。

## 5. 標準の代数的クラス階層

`lib/core/base.egi` は算術を一つの `Num` にまとめず、必要な演算ごとに分ける。

```text
AddSemigroup a
  └─ AddMonoid a
       └─ AddGroup a

MulSemigroup a
  └─ MulMonoid a
       └─ MulGroup a

Ring a  extends AddGroup a, MulMonoid a
Field a extends Ring a, MulGroup a
GCDDomain a extends Ring a
EuclideanDomain a extends GCDDomain a
```

これにより、関数は実際に必要な演算だけを制約として要求できる。

## 6. 現在の制限

- クラスメソッドの `:=` による既定実装は構文解析されるが、辞書生成では使われない。
  各インスタンスがメソッドを実装するか、通常のトップレベル関数として共通実装を書く。
- 多引数クラス間の `extends` は、型パラメータの対応を `ClassInfo` に保存していない。
  親子で同じ位置に同じ型引数を置く場合だけ使用する。
- 実行時型ディスパッチは単一パラメータクラスの第1引数だけを見る。複数引数による
  実行時の多重ディスパッチは行わない。
- 制約付き関数の本体が通常のラムダ形を取らない一部の内部経路と、パターン関数内の
  埋め込み式では、辞書展開のローカル名追跡が完全ではない。同名のクラスメソッドを
  その局所範囲で束縛しないことで回避できる。

## 7. 実装と検証

主要な実装箇所は次のとおりである。全処理段階との対応は
[FILE_MAPPING.md](./FILE_MAPPING.md) を参照する。

- `AST.hs`, `Parser/NonS.hs`: クラス・インスタンス構文。
- `Type/Types.hs`, `Type/Env.hs`: `ClassInfo`, `InstanceInfo`, `Constraint`。
- `EnvBuilder.hs`: クラス環境とメソッド型の登録。
- `Desugar.hs`: インスタンス辞書とスーパークラス参照の生成。
- `Type/Infer.hs`: 制約の収集とスーパークラス展開。
- `Type/Instance.hs`, `Type/Subtype.hs`: インスタンス候補の照合と具体性比較。
- `Type/TypeClassExpand.hs`: 辞書アクセスと辞書引数への変換。

標準クラス階層は `test/lib/core/base.egi`、CAS を含む利用例は `test/lib/math/`、
構文と一般的な回帰は `test/syntax.egi` で検証する。
