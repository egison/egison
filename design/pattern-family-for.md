# パターン族の名前とターゲット型を分ける `for` 構文（提案）

状態: 提案（未実装、2026-10-05 作成）。§4.3 で比較に使う現在の構文の例は、インタプリタの既定モードと
厳格モード（`--type-check-strict`）の両方で動作を確認した。

## 1. 現在の制限

現在のパターン宣言 `inductive pattern T a1 ... an := ...` は、データ型 `T` の上のパターン族を宣言し、
`T` と同じ名前と引数の個数を持つ能力コンストラクタを導入する。この形には次の制限がある。

1. 組み込みの型の上にパターン族を宣言できない。`inductive pattern Integer := | o | s Integer` は、組み込みの
   `Integer` とは別のデータ型 `Integer` を新しく作る。そのため `o` と `s` で整数を分解するマッチャーは、
   既定モードでも厳格モードでも型エラーになる（`Expected: Integer (TInt)`、
   `Actual: Integer (TInductive "Integer" [])`）。Lean の機械化でも、パターンコンストラクタの結果の
   ターゲット型は宣言済みのデータ型に限られる（`PatternTargetDeclared`）。
2. データ型ごとに、そのデータ型と同じ名前の族しか宣言できない。そのため、同じ型を別々の
   コンストラクタの組で分解する族を並べて持てない。例として、Wadler のビュー（views: 一つのデータ型を、
   内部表現とは別のコンストラクタの組で分解できるようにする仕組み）の直交座標と極座標や、整数の
   Peano 式の分解と偶奇による分解がある。
3. フィールドの能力はフィールドの型から決まる。パターン族を持つ型ならその能力コンストラクタ、持たない型なら
   `Any` である。そのため、ある型に族を宣言すると、その型をフィールドに持つ他のパターン宣言
   （`card Suit Integer` など）の要求まで変わる。

## 2. 構文

```egison
inductive pattern F a1 ... an for T :=
  | C1 f11 ... f1k
  | ...
```

- `F` は族の名前で、`F` と同じ名前の能力コンストラクタを導入する。`a1 ... an` は族のパラメータ（型変数）。
- `T` はターゲット型で、組み込みの基本型（`Integer`、`Float`、`String` など）か、宣言済みのデータ型の適用。
- `for T` を省略した `inductive pattern T a1 ... an := ...` は、
  `inductive pattern T a1 ... an for T a1 ... an := ...` の略記とする。この形で宣言した族を、その型の
  **既定の族**（default family）と呼ぶ（仮の用語。採用時に `EgisonTERM.md` に加える）。
  既定の族は型ごとに高々一つである。

## 3. 意味

各パターンコンストラクタ `C` のスキームは

```
Σ(C) = ∀ ā χ̄. (κ(f1) ⊣ τ(f1)), ..., (κ(fk) ⊣ τ(fk)) ⇒ (F χ1 ... χn ⊣ T)
```

である。`κ ⊣ τ` は要求対（その位置のパターンが、ターゲット型 τ の値について能力 κ を必要とすること）で、
`χi` はパラメータ `ai` に対応する能力変数である。フィールド `f` は型の名前と族の名前を混ぜて書き、
その要求対 `κ(f) ⊣ τ(f)` を次の規則で読む。

| フィールドの形 | 能力 κ(f) | ターゲット型 τ(f) |
|---|---|---|
| 族のパラメータ `ai` | `χi` | `ai` |
| 族の名前の適用 `G t1 ... tm`（`G` は `inductive pattern G b1 ... bm for TG` で宣言） | `G κ(t1) ... κ(tm)` | `TG` の `b1 ... bm` を `τ(t1) ... τ(tm)` で置き換えた型 |
| 既定の族を持たない型の適用 `E t1 ... tm`（`Integer` など） | `Any` | `E τ(t1) ... τ(tm)` |
| タプル `(t1, ..., tk)` | `(κ(t1), ..., κ(tk))` | `(τ(t1), ..., τ(tk))` |

既定の族を持つ型の名前 `D` は、族の名前として 2 行目で読む。既定の族のターゲット型は `D b1 ... bm` なので、
`D t1 ... tm` は `D κ(t1) ... κ(tm) ⊣ D τ(t1) ... τ(tm)` になる。これは現在の規則と同じ結果である
（例えば `[Integer]` は `[Any] ⊣ [Integer]`）。族の名前は型の引数の中にも書ける。例えば
フィールド `[Nat]` は `[Nat] ⊣ [Integer]` を表す。

宣言の条件:

- 族の名前は、他の族の名前とも型の名前とも重ならない（既定の族だけが型と同じ名前を持つ）。
- 族のパラメータ `ai` は、すべて `T` の直接の引数である。これで宣言条件のうちの実行時安全性のための条件
  （量化変数がスキームの結果の直接の引数であること）が成り立つ。
- `T` はタプル型・関数型・マッチャー型・型変数ではない。タプル型を除くのは、定義的等式
  `Matcher (κ1, ..., κn) (τ1, ..., τn) ≡ (Matcher κ1 τ1, ..., Matcher κn τn)` があるなかで、安全性の証明が
  コンストラクタパターンのターゲット型の形から「そのマッチャーはマッチャーのタプルではなく一つの
  マッチャーである」ことを導いているためである。

能力の単一化、EvidenceOK（matcher式の能力が、各マッチャー節のプリミティブパターンパターンの根の
コンストラクタから得る能力とすべて等しく、そのような能力がなければ `Any` であるという条件）、
網羅性の条件、型推論と実行時安全性の定理は、族を単位としたまま変わらない。マッチャー型は既存の
`Matcher F T` の形で、型推論で求まるので注釈はいらない。

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

- `Σ(o) = (Nat ⊣ Integer)`、`Σ(s) = (Nat ⊣ Integer) ⇒ (Nat ⊣ Integer)`。
- 型推論で `nat : Matcher Nat Integer`、`list nat : Matcher [Nat] [Integer]` が求まる。
- `inductive pattern Card := | card Suit Integer` の `Integer` のフィールドは `Any` のまま変わらない。
  Peano 式に分解したいフィールドだけ `card Suit Nat` と書く。

### 4.2 一つの型の上の複数の族

```egison
inductive pattern Parity for Integer :=
  | even Integer
  | odd Integer
```

`even $k` は `2k`、`odd $k` は `2k + 1` の形の整数に照合する。`Nat` と `Parity` は同じ `Integer` の上の
別々の族で、それぞれのマッチャーは片方の族だけを実装すればよい。

### 4.3 直交座標と極座標（Wadler のビュー）

Wadler の例と同じく、データ型は直交座標と極座標の二つの表現を持ち、`cart` と `pole` はどちらの表現の値も
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

- 型推論で `cartesian : Matcher Cartesian Cpx`、`polar : Matcher Polar Cpx` が求まる。
- `polar` で `cart $x $y` を照合する式は、能力が一致しないので型エラーになる。
- 同じ値を両方の見方で調べるときは、マッチャーのタプルを使う:
  `match (z, z) as (cartesian, polar) with | (cart $x _, pole $r _) -> ...`。

現在の構文との比較: 現在の構文でも、`cart` と `pole` を一つの族 `Cpx` のパターンコンストラクタとして宣言し、
一つのマッチャーで両方を実装する形は書ける（既定モードと厳格モードで実行を確認した）。この形では
`cart $x _ & pole $r _` のように、一つのパターンの中で両方の見方を使える。`for` 構文は、見方ごとに
族を分け、片方だけを実装するマッチャーを型で区別できるようにする。

### 4.4 パラメータを持つ族

```egison
inductive pattern Bag a for [a] :=
  | empty
  | insert a (Bag a)
```

フィールド `a` は `χ ⊣ a`、フィールド `Bag a` は `Bag χ ⊣ [a]` を表す。リストの既定の族 `[a]`
（`[]`、`::`、`++`）とは別の族として、多重集合として見るためのパターンコンストラクタを宣言できる。

## 5. 互換性

- 既存の `inductive pattern T := ...` は、既定の族の宣言としてそのまま動く。
- フィールドに型の名前を書いた既存の宣言の意味も変わらない（その型の既定の族、なければ `Any`）。

## 6. 完了条件

- **Lean の機械化（`~/PL/type-pm-mech`）**: スキームはすでに要求対で表され、族の名前（`PatternFamily`）と
  データ型の名前（`DataType`）も別々で、両者を結びつける宣言条件はない。したがって §4.3 のような
  データ型の上の族は、宣言条件の上では現在の Lean の core でも書ける（3 の回帰で確かめる）。
  組み込みの型の上の族（§4.1、§4.2）のために、次を行う。
  1. `PatternTargetDeclared`（`Foundation/Signature.lean`）を、組み込みの整数型 `int` も許す形に広げる。
  2. `PatternTyping.ctor_target_data`（`CallByNeedDispatchTyping.lean`）と
     `PPatTyping.someEvidence_target_data`（`Typing.lean`）の結論を「ターゲット型はデータ型か `int`」に
     広げ、それを使う箇所（`CallByNeedSafety.lean` の `MatcherTupleType.data_inversion` を使う 2 箇所と、
     マッチャー型の正規化）に `int` の場合を加える。
  3. `nat` と `Cartesian`／`Polar` の回帰（推論・評価・能力が一致しない場合の拒否）を加える。
- **Egison インタプリタ**:
  1. パーサ（`Parser/NonS.hs` の `patternInductiveExpr`）に `for` 句を加え、`PatternInductiveDecl`
     （`AST.hs`）が族の名前とターゲット型を別々に持つようにする。
  2. 現在のパターンコンストラクタの型は、フィールドの型を並べた普通の型として登録され
     （`EnvBuilder.hs` の `registerPatternConstructor`）、使う側で型から能力を作っている
     （`Type/Infer.hs` の `capabilityTemplates`、`Type/Types.hs` の `capabilitySkeleton`）。
     フィールド `Nat` の能力 `Nat` は型 `Integer` からは決まらないので、パターンコンストラクタのスキームを
     パターン関数と同じ要求対の形（`PatFuncScheme`）で登録し、§3 の表で読んだ要求対をそのまま使う。
  3. 既定の族の表（型の名前から既定の族への対応）と、§3 の宣言の条件の検査とエラーメッセージを加える。
  4. `mini-test/` に §4 の例を加え、`cabal test` で回帰がないことを確かめる。
- **論文（`~/PL/type-pm-paper`、英語版と日本語版）**: §2.2 の「各パターン族は、そのパターンが分解する
  データ型と同じ名前と引数の個数の能力コンストラクタを導入する」を、宣言が族に名前を付ける形に改め、
  §2.1 に `for` 構文と §4.1 か §4.3 の例を加える。
- **用語集（`EgisonTERM.md`）**: 「既定の族」など、採用した用語を加える。

## 7. 未決定の点

1. キーワード（`for`、`over`、`on`）。
2. 族の名前と型の名前を別の名前空間に置くか、重なりを禁止するか（§3 は禁止する案）。
3. コンストラクタごとにターゲット型を変える GADT 風の拡張（数式処理システムのパターンビューを core に
   入れるための形）を、この構文の延長で扱うか。
