# パターン族の名前とターゲット型を分ける `for` 構文

状態: 実装済み（2026-10-05）。Egison インタプリタと Lean の機械化（`~/PL/type-pm-mech`）の両方が
対応する。

## 1. 構文

```egison
inductive pattern F a1 ... an for T :=
  | C1 f11 ... f1k
  | ...
```

- `F` はパターン族の名前で，`F` と同じ名前の能力コンストラクタを導入する。`a1 ... an` は族の
  型パラメータ。
- `T` はターゲット型で，宣言済みのデータ型の適用か，組み込みの基本型（`Integer`，`Float`，
  `String` など）。
- `for T` を省いた `inductive pattern T a1 ... an := ...` は，同じ名前の型 `T a1 ... an` の族を宣言する
  （従来の構文）。`for T a1 ... an` と書いても同じ意味になる。

この構文で次のことができる。

1. 組み込みの型の上の族。`inductive pattern Integer := ...` は組み込みの `Integer` とは別の
   データ型を作ってしまうが，`inductive pattern Nat for Integer := ...` は整数を分解する族を宣言する。
2. 一つの型の上の複数の族。例えば Wadler のビュー（views: 一つのデータ型を，内部表現とは別の
   コンストラクタの組で分解できるようにする仕組み）の直交座標と極座標，整数の Peano 式の分解と
   偶奇による分解。
3. 要求を変えない族の追加。`Nat for Integer` を宣言しても，`Integer` と書いたフィールドの能力は
   `Any` のままである。Peano 式に分解したいフィールドだけ `Nat` と書く。

## 2. 意味

各パターンコンストラクタ `C` のスキームは

```
Σ(C) = ∀ ā χ̄. (κ(f1) ⊣ τ(f1)), ..., (κ(fk) ⊣ τ(fk)) ⇒ (F χ1 ... χn ⊣ T)
```

である。`κ ⊣ τ` は要求対（その位置のパターンが，ターゲット型 τ の値について能力 κ を必要とすること），
`χi` はパラメータ `ai` に対応する能力変数である。フィールド `f` は型の名前と族の名前を混ぜて書き，
その要求対 `κ(f) ⊣ τ(f)` を次の規則で読む。

| フィールドの形 | 能力 κ(f) | ターゲット型 τ(f) |
|---|---|---|
| 族のパラメータ `ai` | `χi` | `ai` |
| 族の名前の適用 `G t1 ... tm`（`G` は `inductive pattern G b1 ... bm for TG` で宣言） | `G κ(t1) ... κ(tm)` | `TG` の `b1 ... bm` を `τ(t1) ... τ(tm)` で置き換えた型 |
| 同じ名前の族を持たない型の適用 `E t1 ... tm`（`Integer` など） | `Any` | `E τ(t1) ... τ(tm)` |
| タプル `(t1, ..., tk)` | `(κ(t1), ..., κ(tk))` | `(τ(t1), ..., τ(tk))` |

同じ名前の族を持つ型の名前 `D` は，族の名前として 2 行目で読む。その族のターゲット型は
`D b1 ... bm` なので，`D t1 ... tm` は `D κ(t1) ... κ(tm) ⊣ D τ(t1) ... τ(tm)` になり，従来の規則と同じ
結果を与える（例えば `[Integer]` は `[Any] ⊣ [Integer]`）。族の名前は型の引数の中にも書ける。例えば
フィールド `[Nat]` は `[Nat] ⊣ [Integer]` を表す。

能力の単一化，EvidenceOK（matcher式の能力が，各マッチャー節のプリミティブパターンパターンの根の
コンストラクタから得る能力とすべて等しく，そのような能力がなければ `Any` であるという条件），網羅性の
条件，型推論と実行時安全性の定理は，族を単位としたまま変わらない。マッチャー型は `Matcher F T` の形で，
型推論で求まるので注釈はいらない。

## 3. 宣言の条件

`for` を持つ宣言は次を満たさなければならない（違反はインタプリタが宣言を読み込むときに拒否する）。

1. 族の名前は，どのデータ型の名前とも異なる。ただし `T` がその名前の型を `a1 ... an` に適用したもの
   なら，同じ名前の型の族の宣言として受け付ける。
2. `T` はデータ型の適用か組み込みの基本型で，タプル型・関数型・マッチャー型・型変数ではない。
3. 族の型パラメータ `ai` は，すべて `T` の直接の引数である。これにより，宣言条件のうちの実行時安全性の
   ための条件（量化変数がスキームの結果の直接の引数であること）が成り立つ。
4. `T` は `for` で宣言した族の名前を含まない。

Lean の機械化では，パターンコンストラクタの結果のターゲット型は宣言済みのデータ型の適用か組み込みの
整数型 `int` である（`Foundation/Signature.lean` の `PatternTargetDeclared`）。整数型は引数を持たないので，
ターゲット型が `int` のスキームは型変数を量化しない（`SignatureRuntime.lean` の
`PatternCtorScheme.ResultDetermined`）。

## 4. 例

### 4.1 整数の Peano 式の分解

```egison
inductive pattern Nat for Integer :=
  | o
  | s Nat

def nat := matcher
  | o as () with
    | 0 -> [()]
    | _ -> []
  | s $ as nat with
    | $n -> if n > 0 then [n - 1] else []
  | #$val as () with
    | $n -> if n = val then [()] else []
  | $ as something with
    | $n -> [n]

def pred n :=
  match n as nat with
    | o -> 0
    | s $m -> m
```

- `Σ(o) = (Nat ⊣ Integer)`，`Σ(s) = (Nat ⊣ Integer) ⇒ (Nat ⊣ Integer)`。
- 型推論で `nat : Matcher Nat Integer`，`list nat : Matcher [Nat] [Integer]` が求まる。

### 4.2 一つの型の上の複数の族

```egison
inductive pattern Parity for Integer :=
  | even Integer
  | odd Integer
```

`even $k` は `2k`，`odd $k` は `2k + 1` の形の整数に照合する。`Nat` と `Parity` は同じ `Integer` の上の
別々の族で，それぞれのマッチャーは片方の族だけを実装すればよい。`parity` マッチャーの下で `s $m` を
使う式は，能力 `Nat` と `Parity` が一致しないので型エラーになる。

### 4.3 直交座標と極座標（Wadler のビュー）

Wadler の例と同じく，データ型は直交座標と極座標の二つの表現を持ち，`cart` と `pole` はどちらの表現の値も
分解する。

```egison
inductive Cpx :=
  | Cart Float Float
  | Pole Float Float

inductive pattern Cartesian for Cpx :=
  | cart Float Float

inductive pattern Polar for Cpx :=
  | pole Float Float

def cartesian := matcher
  | cart $ $ as (something, something) with
    | Cart $x $y -> [(x, y)]
    | Pole $r $t -> [(r * f.cos t, r * f.sin t)]
  | $ as something with
    | $z -> [z]

def polar := matcher
  | pole $ $ as (something, something) with
    | Cart $x $y -> [(f.sqrt (x * x + y * y), f.atan2 y x)]
    | Pole $r $t -> [(r, t)]
  | $ as something with
    | $z -> [z]
```

- 型推論で `cartesian : Matcher Cartesian Cpx`，`polar : Matcher Polar Cpx` が求まる。
- 同じ値を両方の見方で調べるときは，マッチャーのタプルを使う:
  `matchAll (z, z) as (cartesian, polar) with (cart $x _, pole $r _) -> (x, r)`。
- `cart` と `pole` を一つの族のパターンコンストラクタとして宣言し，一つのマッチャーで両方を実装することも
  できる。その形では `cart $x _ & pole $r _` のように一つのパターンの中で両方の見方を使える。

### 4.4 パラメータを持つ族

```egison
inductive pattern Bag a for [a] :=
  | empty
  | insert a (Bag a)
```

フィールド `a` は `χ ⊣ a`，フィールド `Bag a` は `Bag χ ⊣ [a]` を表す。リストの族（`[]`，`::`，`++`）とは
別の族として，多重集合として見るためのパターンコンストラクタを宣言できる。

### 4.5 型パラメータを持つ利用者定義の型の上の族

```egison
inductive Tree a := Leaf a | Node a [Tree a]
inductive pattern Tree a := leaf a | node a [Tree a]
inductive pattern Root a for Tree a := root a
```

`root $x` は葉と節のどちらでも根のラベルに照合する。`Root` は同じ名前の型の族 `Tree` とは別の族なので，
`tree` マッチャーの下の `root` パターンや `root` マッチャーの下の `node` パターンは型エラーになる。
`T` の中の族のパラメータは `T` の直接の引数でなければならないので，`Bad a for [[a]]` や
`Bad a for Tree [a]` は拒否する（§3 の条件 3）。

## 5. 実装の対応

### Egison インタプリタ

| 処理 | 場所 |
|---|---|
| 構文解析（`for` 句） | `Parser/NonS.hs` の `patternInductiveExpr`。AST は `PatternInductiveDecl name params (Maybe target) ctors` |
| 宣言の条件（§3） | `EnvBuilder.hs` の `validatePatternFamilyTarget` |
| 能力コンストラクタの登録 | `EnvBuilder.hs` の `buildCapabilityConstructorArities`（`for` の族を追加） |
| ターゲット型の記録 | `EnvBuilder.hs` の `processTopExpr` が `extendPatternFamilyTarget` で `patternFamilyTargets` に登録。ロード単位をまたぐ統合は `Eval.hs` |
| シグネチャ | フィールドと結果の型は族の名前で書く（`s : Nat -> Nat`）。`--dump-env` もこの形で表示する |
| 能力 | `Type/Infer.hs` の `capabilityTemplates`（族の名前から能力コンストラクタを作る） |
| ターゲット型 | `Type/Env.hs` の `projectPatternFamilyTargets` を，マッチャー節の `PPInductivePat` とマッチ節の `IInductivePat` の推論で適用する |

テスト:

- `test/lib/core/pattern-family-for.egi`: §4 の例（`Nat`，`Parity`，`Cartesian`／`Polar`，`Bag`，`Tree`／`Root`）と，
  `Integer` と書いたフィールドが `something` で足りること。
- `test/lib/core/type-pm-examples.egi` の `paperNat`: 論文の例。Lean の回帰と同じ演算で書いたマッチャー。
- `test/type-error/99`〜`103`: 能力の不一致，タプルのターゲット，データ型と同じ名前，ターゲットの引数でない
  パラメータ，ターゲットに現れる族。`test/Test.hs` の `patternFamilyTargetTypeErrorTests` が検査する。

### Lean の機械化

- `Foundation/Signature.lean`: `PatternTargetDeclared` と `patternTargetCheck` が整数型のターゲットを許す。
- `SignatureRuntime.lean`: `PatternCtorScheme.ResultDetermined` はターゲットの直接の引数
  （`PolyTy.patternTargetArguments?`）で量化変数を検査する。
- `Typing.lean` の `PPatTyping.someEvidence_target_shape`，`CallByNeedDispatchTyping.lean` の
  `PatternTyping.ctor_target_shape`，`CallByNeedSafety.lean` の `MatcherTupleType.patternTarget_inversion`:
  コンストラクタパターンのターゲットがデータ型か整数型なので，そのマッチャーは一つのマッチャーである。
- `IntPatternFamilyRegression.lean`: `Nat` と `Parity` を整数型の上に宣言したシグネチャが宣言条件をすべて
  満たすこと，推論・評価・動的型エラーの不在，`parity` や `something` の下の `Nat` パターンの拒否，二つの族を
  混ぜたマッチャーの拒否，タプルのターゲットと型変数を量化する整数型のスキームの拒否。
- `ParametricTreeRegression.lean`: §4.5 の `Tree a` と族 `Tree`・`Root`（ラベルは整数）。
  `test/lib/core/pattern-family-for.egi` と同じ問い合わせの推論・正確な評価結果・動的型エラーの不在と，
  族の合わないパターンの拒否。

## 6. 今後の拡張

コンストラクタごとにターゲット型を変える GADT 風の拡張（数式処理システムのパターンビューを core に入れる
ための形）は，この構文の延長として検討する。
