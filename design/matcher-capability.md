# Matcher capability 実装契約

> 状態：2026-08-27

本文書は，Egison 本体における matcher capability 実装の要点を記す．形式規則と証明の
正本は `type-pm-mech3` であり，Haskell 実装との詳しい適合条件は
[type-pm-conformance.md](./type-pm-conformance.md)，Egison 固有拡張との境界は
[type-pm-compatibility.md](./type-pm-compatibility.md)を参照すること．

## 1. 型と変数

内部型は次の二つを区別する．

```haskell
TMatcher     Capability Type
TMatcherSlot Capability Type
```

capability と ordinary type（通常の値の型）は別の sort であり，それぞれ別の代入を
持つ．ordinary type 変数は一つの構文のまま A/R の用途フラグを持つ．A 変数は
`MatcherSlot` を含む任意の `TypeOK` 型へ置換でき，R 変数は `ResultOK` 型だけへ置換
できる．関数型では仮引数だけが `MatcherSlot` を含められる．

capability 変数には A/R，producer／consumer origin，rigidity のフラグを付けない．
明示注釈を検査するための skolem（特殊化できない定数）は別であり，通常の推論変数と
混同しない．

## 2. 単一化

capability 等式は，変数の由来に依存しない通常の最汎単一化で解く．推論状態には
capability origin ledger，protected producer 集合，過去の生成場所を保持しない．

`Matcher producer target` を `MatcherSlot consumer expectedTarget` に対して checking
するときは，次を行う．

1. `target = expectedTarget` を A/R 制約付き ordinary unification で解く．
2. `consumer` が literal `Any` なら capability 等式を加えない．
3. それ以外なら `producer = consumer` を通常の capability MGU で解く．

方向性があるのは `Matcher` から `MatcherSlot` への変換を選ぶ点だけであり，変数を
解く向きではない．したがって fresh な producer 変数も利用地点の要求で構造化できる．
一方，一般の型等式は `Matcher` と `MatcherSlot` を相互変換しない．

source tuple を一つの slot に渡す構文では，各成分を個別の slot に checking する．
成分 target の fresh 変数は A である．成分 target 自体が `MatcherSlot` になり得るため，
ここで R を使ってはならない．

型クラス instance の選択に使う `matchOneWay` は，量化された instance 側だけを具体化
する別の通常機能である．matcher-to-slot checking の capability MGU とは区別する．

## 3. matcher literal

matcher literal は clause の primitive-pattern pattern と実際の next-matcher 値から
capability evidence を作り，すべての clause で同じ target 型を共有する．target 型だけ
から capability を生成しない．

静的条件は TypePM と同じである．

- CatchAllLast：bare hole header の clause がちょうど一つあり，最後にある．
- ArmCoverage：最後の arm が変数／wildcard であるか，言及した data former の全
  constructor を general arm で網羅する．
- RootCoverage：言及した pattern former の全 pattern constructor を general clause
  で網羅する．

CatchAllLast と ArmCoverage は通常の型エラーである．RootCoverage は production
Egison の部分 matcher を維持するため，`--matcher-consistency-warnings` のときだけ警告
する．catch-all clause の arm を一つの変数 armへ限定しない．

## 4. match の検査順

`matchAll`／`matchAllDFS` の core arm は次の順で調べる．

```text
target -> pattern -> matcher checking -> body
```

`match`／`matchDFS` は target の後に matcher 式を一度 synthesize し，各 arm を次の順で
完了してから次へ進む．

```text
pattern -> matcher-slot checking -> body
```

pattern を適合性検査と arm 推論で二度調べたり，全 arm の pattern を先読みしたりしない．
すべての body と fallback は一つの fresh R 結果型を共有する．fallback は最後に arm
束縛の外で調べる．

TypePM では first-result match の `else` は必須だが，Egison では省略可能なままとする．
`--match-without-else-warnings` は省略を報告するだけで，型推論や実行結果を変えない．

## 5. 公開 signature

data constructor，pattern constructor，pattern function の scheme は閉じていなければ
ならない．すなわち，出現する変数は明示された型パラメータまたは量化された capability
変数でなければならない．宣言済みの名前付き型は，閉性検査より前に型変数表現から
具体的な型 former へ正規化する．

constructor field に現れる型／capability parameter は result から決定できなければ
ならない．field にだけ現れる変数は TypePM の `ParametersDetermined` を満たさないため
拒否する．

## 6. 警告と保証範囲

次の option は診断だけを変更する．

```text
--matcher-consistency-warnings
--match-without-else-warnings
--pattern-hole-before-primitive-value-pattern-warnings
--nested-structured-primitive-pattern-pattern-warnings
--outside-egison-core-warnings
```

pattern hole より後に primitive value pattern があるかどうかは，TypePM の
`CaptureDisciplined` と同じ深さ優先・左から右の順序で判定する．CAS，tensor，ordinary
`TAny`，legacy pattern view などは明示的な Egison 拡張であり，core 規則の失敗を契機に
選ぶ代替 solver ではない．

Haskell 実装全体と Lean 実装の対応定理は要件としない．TypePM の no-stuck 定理を
Egison 全体へそのまま主張することもない．core の対応は source-level 回帰と公開 solver
の回帰で維持する．

主な実装箇所は次のとおりである．

- `Type/Types.hs`：二添字型，A/R 用途，scheme．
- `Type/Subst.hs`：二 sort の代入．
- `Type/Unify.hs`：A/R 制約付き ordinary MGU と capability MGU．
- `Type/Infer.hs`：matcher-to-slot checking，matcher literal，match の source 順．
- `EnvBuilder.hs`：公開 signature の閉性と parameters-determined 検査．
- `CmdOptions.hs`／`Eval.hs`：診断 option．
