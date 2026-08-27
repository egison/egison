# TypePM 適合条件

この文書は，`type-pm-mech3` の形式仕様と Egison 本体の型推論器を照合した結果を，
現行実装が満たすべき条件としてまとめる．形式規則と証明の正本は
`type-pm-mech3` にあり，ここでは Haskell 実装との対応だけを定める．

対象は Paper 1 と F1 の型付け規則である．作業中の F2
（pattern function と checked MNode）および F3（loop pattern）の全体対応は，
それぞれの形式化が完了した後に別途確認する．Haskell 実装全体と Lean 実装の間の
対応定理は要件としない．

## 1. 判定基準

Egison の core 経路は，TypePM と同じ入力に対して同じ source 順で制約を生成し，
同じ A/R 制約付き単一化と matcher-to-slot checking を行う．

- TypePM が受理する core プログラムを，Egison 固有の制約で拒否しない．
- TypePM が source 順のために拒否するプログラムを，後続構文の先読みで受理しない．
- Egison 固有の型・構文・実行機能は，core の失敗時に試す代替解釈ではなく，
  入力中の明示的な拡張要素によって選ばれる規則として追加する．
- 警告オプションは診断だけを変更し，型，代入，型付き構文木，実行結果を変更しない．

## 2. 単一化と matcher-to-slot checking

capability 変数には，ordinary type の A/R フラグに相当する origin や
producer-rigidity を設けない．`Matcher` または `MatcherSlot` の内部を含む
capability 等式は通常の最汎単一化で解く．推論状態に capability origin ledger，
protected producer 集合，または構造化可能性の履歴を置かない．

`Matcher producer target` を `MatcherSlot consumer expectedTarget` に対して
checking するときは，次の二つだけを行う．

1. `target = expectedTarget` を通常の A/R 制約付き単一化で解く．
2. `consumer` が `Any` なら capability 制約を加えず，それ以外なら
   `producer = consumer` を通常の capability 単一化で解く．

これは方向付きの構文変換を利用地点で一度だけ選ぶという意味であり，
producer 側の変数を rigid にする一方向 matching ではない．一般の型等式では
`Matcher` と `MatcherSlot` を相互変換しない．

source tuple を slot に対して checking するとき，各成分の対象型には fresh な
A 変数を使う．成分対象には `MatcherSlot` が現れ得るため，R 変数を使ってはならない．

## 3. match の source 順

`matchAll`／`matchAllDFS` の core arm は，次の順で一度だけ調べる．

```text
target -> pattern -> matcher checking -> body
```

`match`／`matchDFS` は matcher 式自体を target の次に一度 synthesize し，各 arm を
次の順で完了してから次の arm へ進む．

```text
pattern -> matcher type の slot checking -> body
```

全 arm の pattern を事前走査してはならない．特に，後続 arm が得た型情報を先行 arm
の body へ逆流させない．複数 arm は Egison の拡張であるが，この逐次処理をそのまま
繰り返す．すべての body と fallback は一つの fresh な R 結果型を共有する．

## 4. first-result match の fallback

TypePM core では `else` が必須である．Egison は既存プログラムのために `else` の省略を
引き続き受理し，全 arm が失敗した場合は従来どおり `MatchFailure` とする．ただし
`--match-without-else-warnings` を指定したとき，`else` のない `match` と `matchDFS` を
TypePM core 外として警告する．

`else` がある場合は全 arm の後で，arm の束縛を含まない元の型環境で調べる．実行時も
元の値環境で評価する．

## 5. matcher literal の静的条件

次の条件は TypePM と同じ構文判定を用いる．

- catch-all は bare hole header の clause がちょうど一つあり，最後にあること．
  catch-all clause の arm を「変数 arm が一つだけ」に制限しない．
- ArmCoverage は，最後の arm が変数または wildcard であるか，その clause が言及する
  data former の宣言済み constructor を general constructor arm ですべて網羅すること．
  Bool と collection の surface 専用 pattern も，同じ網羅条件に対応させる．
- RootCoverage は，clause header が言及する pattern former の宣言済み pattern
  constructor を general constructor clause ですべて網羅すること．

ArmCoverage と constructor arity は実行時の primitive-data-pattern failure を防ぐため
通常の型エラーとする．RootCoverage は Egison の部分 matcher 拡張として受理し，
`--matcher-consistency-warnings` が指定されたときだけ TypePM の RootCoverage 違反を
警告する．オプションの有無で受理結果を変えない．

## 6. signature の条件

data constructor，pattern constructor，pattern function の公開 scheme は閉じて
いなければならない．閉じた scheme とは，出現する型変数と capability 変数がすべて
量化されている scheme をいう．宣言していない field 型変数を暗黙の共有変数として
残してはならない．

さらに，runtime dispatch が result だけから field の具体化を復元する宣言では，
field にだけ現れる量化変数を許さない．これは TypePM の
`Signature.RuntimeReady` にある parameters-determined 条件に対応する．Egison の
実行器固有の Bool／List 表現は Lean の Paper 1 評価器と異なるため，Lean 側の
constructor 名固定条件をそのまま要求しない．

## 7. 証明前提と Egison 拡張の警告

TypePM の no-stuck 定理は，型付けだけでなく `FullyApplied`，
`CaptureDisciplined`，well-formed／runtime-ready signature を仮定する．Egison の
実行器が直接扱える部分適用には `FullyApplied` を要求しない．一方，matcher header の
capture が hole より後にある場合は `CaptureDisciplined` の外なので，既存の
`--pattern-hole-before-primitive-value-pattern-warnings` で，深さ優先・左から右という
TypePM と同じ条件を警告する．既知 matcher の利用地点で未束縛値を実際に参照する場合の
ハードエラーはこの構文警告とは別に維持する．

CAS，tensor，ordinary `Any`，legacy pattern view，未形式化の surface pattern などの
拡張は `--outside-egison-core-warnings` または既存の専用 warning option で区別する．
これらの warning 経路は TypePM の core 規則を変更しない．

## 8. 回帰検査

少なくとも次を Haskell 回帰として固定する．

- polymorphic matcher producer の capability 変数を通常の等式が具体化できる．
- source tuple の成分対象が `MatcherSlot` でも checking できる．
- 後続 match arm の pattern 情報が先行 arm の body に逆流しない．
- `matchAll` は pattern の要求 capability を得てから matcher を checking する．
- `else` 省略は受理結果を変えず，専用 option のときだけ警告する．
- bare-hole catch-all の最後の wildcard armを受理する．
- user-defined algebraic data type の全 constructor arm を ArmCoverage として認める．
- RootCoverage warning は，TypePM と同じ mentioned-former 条件でのみ発生する．
- 未宣言 field 型変数と，result から決まらない field-only parameter を拒否する．

Haskell の二つの内部関数を比較するだけでは TypePM 適合性の検査にならない．上記の
source 回帰を主とし，必要に応じて Lean の executable checker と同じ入力・期待結果を
共有する．
