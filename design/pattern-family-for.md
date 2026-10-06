# パターン族の名前とターゲット型を分ける `for` 構文

状態: 実装済み（2026-10-05）。Egison インタプリタと Lean の機械化（`~/PL/type-pm-mech`）の両方が
対応する。フィールドを能力の式だけにし，`Any` を `(Any for T)` と書く構文は 2026-10-06 に実装した
（インタプリタのみ。Lean は表層構文を持たない）。

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
- フィールド `f` は能力の式で，次のどれかである。

  ```
  f ::= ai                    族の型パラメータ
      | G f ... f             宣言済みのパターン族 G の適用（リストの族は [f]）
      | (f, ..., f)           フィールドのタプル
      | (Any for t)           ターゲット型 t の上の能力 Any（t は任意の型の式）
  ```

  括弧やリストの中では `[Any for Integer]`，`(MathValue, Any for Integer)` のように括弧を省ける。
  `Integer` のように族の名前でない型をそのまま書いたフィールドはエラーになる（エラー文で
  `(Any for Integer)` と書くよう案内する）。`for` を付けられるのは `Any` だけである。ただしコアの外の
  数式処理のビュー（`MathValue` などの族）は，`(MathValue for Term MathValue [..])` のように族の能力にも
  `for` で型を添えられる（§2）。

この構文で次のことができる。

1. 組み込みの型の上の族。`inductive pattern Integer := ...` は組み込みの `Integer` とは別の
   データ型を作ってしまうが，`inductive pattern Nat for Integer := ...` は整数を分解する族を宣言する。
2. 一つの型の上の複数の族。例えば Wadler のビュー（views: 一つのデータ型を，内部表現とは別の
   コンストラクタの組で分解できるようにする仕組み）の直交座標と極座標，整数の Peano 式の分解と
   偶奇による分解。
3. 要求を変えない族の追加。`Nat for Integer` を宣言しても，`(Any for Integer)` と書いたフィールドの
   能力は `Any` のままである。Peano 式に分解したいフィールドだけ `Nat` と書く。

## 2. 意味

各パターンコンストラクタ `C` のスキームは

```
Σ(C) = ∀ ā χ̄. (κ(f1) ⊣ τ(f1)), ..., (κ(fk) ⊣ τ(fk)) ⇒ (F χ1 ... χn ⊣ T)
```

である。`κ ⊣ τ` は要求対（その位置のパターンが，ターゲット型 τ の値について能力 κ を必要とすること），
`χi` はパラメータ `ai` に対応する能力変数である。フィールド `f` には，その位置のパターンが必要とする
能力を書き，ターゲット型はその能力から補う。一つの族からはターゲット型が一つに定まるが，一つの型には
複数の族がありうる（能力と型は一対多に対応する）ので，型ではなく能力を書く。ターゲット型を定めない
能力は `Any` だけなので，`Any` はいつも `(Any for t)` と型を添えて書く。
要求対 `κ(f) ⊣ τ(f)` は次の規則で読む。

| フィールドの形 | 能力 κ(f) | ターゲット型 τ(f) |
|---|---|---|
| 族のパラメータ `ai` | `χi` | `ai` |
| 族の名前の適用 `G f1 ... fm`（`G` は `inductive pattern G b1 ... bm for TG` で宣言） | `G κ(f1) ... κ(fm)` | `TG` の `b1 ... bm` を `τ(f1) ... τ(fm)` で置き換えた型 |
| タプル `(f1, ..., fk)` | `(κ(f1), ..., κ(fk))` | `(τ(f1), ..., τ(fk))` |
| `(Any for t)` | `Any` | `t` |

同じ名前の族を持つ型の名前 `D` は，族の名前として 2 行目で読む。その族のターゲット型は
`D b1 ... bm` なので，`D f1 ... fm` は `D κ(f1) ... κ(fm) ⊣ D τ(f1) ... τ(fm)` になる。族の名前は
フィールドの引数の中にも書ける。例えばフィールド `[Nat]` は `[Nat] ⊣ [Integer]`，`[Any for Integer]` は
`[Any] ⊣ [Integer]`，`(Any for [Integer])` は `Any ⊣ [Integer]` を表す。

`Any` を型と並べて明示するので，族を持つ型の上の `Any` も書ける。例えば
`inductive pattern AssocList a b for [(a, b)] := | get a b | getAll a (Any for [b])` では，マッチャー節は
`getAll` の値のリストの位置に `something` を渡せる（フィールドを `[b]` と書くと，リストの能力 `[χb]` を
要求するので `list mb` のようなリストのマッチャーが要る）。

コアの外の例外として，数式処理のビュー（`MathValue`，`PolyExpr`，`TermExpr`，`SymbolExpr`，`IndexExpr` の
族）は，族の能力にも `for` で型を添えられる。例えば `poly [MathValue for Term MathValue [..]]` は，項の位置に
`term`・`*` などの MathValue のパターンを使えるようにしたまま，その位置のターゲット型を `Term MathValue [..]`
にする（`derivative.egi` は，この型で型クラスの実装を静的に選ぶ）。これらのビューのマッチャー定義では，
もともと宣言したフィールドの型をターゲットの根拠に使わない。

実装では，`(κ for t)` を予約語の擬似的な型名を使って `TInductive "for" [κ, t]`，`Any` を
`TInductive "Any" []` として型の式に格納する。能力は κ から（`capabilitySkeleton`），ターゲット型は t から
（`projectPatternFamilyTargets`）取る。宣言を読み込むときに，フィールドが上の形の能力の式であることを
検査する（`EnvBuilder.validatePatternConstructorFields`）。

`Any` と書いたフィールドに新しい型変数 β を対応させる案（`Any ⊣ β`）は採らない（2026-10-06 決定）。

- β を ∀ で束縛すると，β は結果に現れないので，§3 の条件 3（スキームの結果が量化変数を定めること）を
  満たさない。マッチャー節とパターンはスキームを別々に具体化し，両者をつなぐのは結果だけなので，β は
  両側で別の型に決まりうる。例えば `inductive pattern Boxed for Integer := | boxed Any`（Any に新しい型変数を対応させる仮の構文）に対して，
  マッチャー節 `boxed $ as something with | $n -> ["hello"]` は β = `String` とし，パターン `boxed $x` の
  本体 `x + 1` は β = `Integer` とするので，実行時の型エラーになる。
- β を照合ごとの抽象型（存在型）として扱えば安全だが，型システムに抽象型の導入と隠蔽を加える必要があり，
  型推論の主要型性も難しくなる。Egison のデータコンストラクタには存在型のフィールドがなく，成分の型は
  いつもターゲット型から定まるので，フィールドはいつも `(Any for Integer)`，`b`，`[b]` のようにターゲット型が
  定まる形で書ける。

型をそのまま書いたフィールド（`even Integer` を「Integer 上の Any」と読む旧構文）は廃止した（2026-10-06 決定）。
フィールドは能力であるという原則を構文で保つためで，`Any` は `(Any for Integer)` と明示する。これにより
「族の名前でなければ型として読む」という暗黙の規則がなくなり，族を持つ型の上の `Any` も書けるようになった。
フィールドの構文解析器は能力の式だけを読み（`Parser/NonS.hs` の `patternFieldArg`），関数型・マッチャー型・
数式処理の型などの型の構文は `for` の後でだけ読む。

能力の単一化，EvidenceOK（matcher式の能力が，各マッチャー節のプリミティブパターンパターンの根の
コンストラクタから得る能力とすべて等しく，そのような能力がなければ `Any` であるという条件），網羅性の
条件，型推論と実行時安全性の定理は，族を単位としたまま変わらない。マッチャー型は `Matcher F T` の形で，
型推論で求まるので注釈はいらない。

## 3. 宣言の条件

`for` を持つ宣言は次を満たさなければならない（違反はインタプリタが宣言を読み込むときに拒否する）。

1. 族の名前は，どのデータ型の名前とも異なる。ただし `T` がその名前の型を `a1 ... an` に適用したもの
   なら，同じ名前の型の族の宣言として受け付ける。
2. `T` はデータ型の適用か組み込みの基本型で，タプル型・関数型・マッチャー型・型変数ではない。
3. 族の型パラメータ `ai` は，すべて `T` の中に現れる（`[a]`，`[[a]]`，`Tree [a]`，`[Matcher Any a]` はよく，
   `Integer` に対する型パラメータはいけない）。これにより，宣言条件のうちの実行時安全性のための条件
   （スキームの結果が量化変数を定めること）が成り立つ。型の正規化は関数型・タプル型・データ型の上では
   構造的であり，マッチャー型をマッチャーのタプルに分配する正規化も，能力を固定すればターゲットについて単射である。
   能力の位置は結果の能力で定まるので，正規化した結果が等しい二つのインスタンスは，`T` に現れるすべての
   型変数で一致する。
4. `T` は `for` で宣言した族の名前を含まない。

すべてのパターン宣言のフィールドは，§1 の能力の式でなければならない（`for` の有無によらない）。
族の名前でない型（`Integer`），族の型パラメータでも族でもない名前，`Any` 以外の能力に付けた `for`
（数式処理のビューを除く）は，宣言を読み込むときに拒否する。

Lean の機械化では，パターンコンストラクタの結果のターゲット型は宣言済みのデータ型の適用か組み込みの
整数型 `int` である（`Foundation/Signature.lean` の `PatternTargetDeclared`）。条件 3 は
`SignatureRuntime.lean` の `PatternCtorScheme.ResultDetermined`（`PolyTy.determinedBounds`）で，
マッチャー型の分配の単射性は `Normalization.lean` の `Ty.normalizeMatcher_injective` である。
整数型は引数を持たないので，ターゲット型が `int` のスキームは型変数を量化しない。

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
  | even (Any for Integer)
  | odd (Any for Integer)
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
  | cart (Any for Float) (Any for Float)

inductive pattern Polar for Cpx :=
  | pole (Any for Float) (Any for Float)

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
族のパラメータは `T` の直接の引数でなくてもよい。`inductive pattern Cells a for [[a]] := cell a`，
`inductive pattern Labels a for Tree [a] := label a`，`inductive pattern Matchers a for [Matcher Any a] := nonempty`
も宣言できる。`T` に現れないパラメータは拒否する（§3 の条件 3）。

## 5. 実装の対応

### Egison インタプリタ

| 処理 | 場所 |
|---|---|
| 構文解析（`for` 句） | `Parser/NonS.hs` の `patternInductiveExpr`。AST は `PatternInductiveDecl name params (Maybe target) ctors` |
| 宣言の条件（§3） | `EnvBuilder.hs` の `validatePatternFamilyTarget` |
| 能力コンストラクタの登録 | `EnvBuilder.hs` の `buildCapabilityConstructorArities`（`for` の族を追加） |
| ターゲット型の記録 | `EnvBuilder.hs` の `processTopExpr` が `extendPatternFamilyTarget` で `patternFamilyTargets` に登録。ロード単位をまたぐ統合は `Eval.hs` |
| フィールドの構文 | `Parser/NonS.hs` の `patternFieldArg`（能力の式だけを読む）と，`EnvBuilder.validatePatternConstructorFields`（族の名前でない型や，`Any` 以外への `for` を拒否する） |
| シグネチャ | フィールドと結果の型は族の名前で書く（`s : Nat -> Nat`，`even : (Any for Integer) -> Parity`）。`--dump-env` もこの形で表示する |
| 能力 | `Type/Infer.hs` の `capabilityTemplates`（族の名前から能力コンストラクタを作る） |
| ターゲット型 | `Type/Env.hs` の `projectPatternFamilyTargets` を，マッチャー節の `PPInductivePat` とマッチ節の `IInductivePat` の推論で適用する |

テスト:

- `test/lib/core/pattern-family-for.egi`: §4 の例（`Nat`，`Parity`，`Cartesian`／`Polar`，`Bag`，`Tree`／`Root`，
  `Cells`，`Labels`，`Matchers`），`(Any for Integer)` と書いたフィールドが `something` で足りること，
  族を持つ型の上の Any（`AssocList` の `getAll a (Any for [b])`）。
- `test/lib/core/type-pm-examples.egi` の `paperNat`: 論文の例。Lean の回帰と同じ演算で書いたマッチャー。
- `test/type-error/99`〜`106`: 能力の不一致，タプルのターゲット，データ型と同じ名前，ターゲットに現れない
  パラメータ，ターゲットに現れる族，族の名前でない型のフィールド（`even Integer`），コアの族への `for`
  （`(Nat for Integer)`），Any のフィールドに置いたコンストラクタパターン。`test/Test.hs` の
  `patternFamilyTargetTypeErrorTests` が検査する。

### Lean の機械化

- `Foundation/Signature.lean`: `PatternTargetDeclared` と `patternTargetCheck` が整数型のターゲットを許す。
- `SignatureRuntime.lean`: `PatternCtorScheme.ResultDetermined` は，量化した型変数がターゲットの中に
  現れること（`PolyTy.determinedBounds`）を検査する。`Normalization.lean` の `Ty.normalizeMatcher_injective` が
  マッチャー型の分配の単射性を示し，`PolyTy.normalize_bound_eq_of_mem_determinedBounds` が，能力の位置で一致し
  正規化した結果が等しいインスタンスは，ターゲットに現れる型変数で一致することを示す。
  `PatternCtorScheme.instancesDetermined_of_resultDetermined` が結果からフィールドが定まることを導く。
- `Typing.lean` の `PPatTyping.someEvidence_target_shape`，`CallByNeedDispatchTyping.lean` の
  `PatternTyping.ctor_target_shape`，`CallByNeedSafety.lean` の `MatcherTupleType.patternTarget_inversion`:
  コンストラクタパターンのターゲットがデータ型か整数型なので，そのマッチャーは一つのマッチャーである。
- `IntPatternFamilyRegression.lean`: `Nat` と `Parity` を整数型の上に宣言したシグネチャが宣言条件をすべて
  満たすこと，推論・評価・動的型エラーの不在，`parity` や `something` の下の `Nat` パターンの拒否，二つの族を
  混ぜたマッチャーの拒否，タプルのターゲットと型変数を量化する整数型のスキームの拒否。
- `ParametricTreeRegression.lean`: §4.5 の `Tree a` と族 `Tree`・`Root`（ラベルは整数）。
  `test/lib/core/pattern-family-for.egi` と同じ問い合わせの推論・正確な評価結果・動的型エラーの不在と，
  族の合わないパターンの拒否。
- `ListPatternFamilyRegression.lean`: §4.4 の `Bag a for [a]`，`Cells a for [[a]]`，`Matchers a for [Matcher Any a]`。
  推論・正確な評価結果・動的型エラーの不在，族の合わないパターンの拒否，型変数がターゲットに現れないスキームの拒否。

## 6. 今後の拡張

コンストラクタごとにターゲット型を変える GADT 風の拡張（数式処理システムのパターンビューを core に入れる
ための形）は，この構文の延長として検討する。
