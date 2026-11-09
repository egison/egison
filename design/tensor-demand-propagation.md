# 固定テンソル位置へ流れる算術による仮引数の分類

利用者が型注釈を書かずに定義した関数について、仮引数をスカラー関数の引数(成分ごとに持ち上げる)とテンソル関数の引数(テンソル全体を受け取る)のどちらに分類するかを決める規則の追加である。三用語(挿入なし型推論・自動挿入型推論・統合型推論)の定義は [テンソル型と添字記法](type-tensor-simple.md)、用語は `EgisonTERM.md` §9 に従う。

## 背景

論文(type-tensor-paper §tensor-product)は、利用者定義の積

```egison
def (.) u v := contractWith (\x y -> x + y) (u * v)
```

について「`contractWith` がテンソル全体を要求するので `.` の仮引数はテンソル引数になり、注釈なしで役割が決まる」と述べ、`[|1,2|]~i . [|10,20|]_i = 50`、`A_i~j . B_j~k` の行列積などをこれに依存して説明する。しかし 2026-11-09 の照合で、Egison と Lean の両実装がこの定義を `t -> t -> Tensor t`(スカラー引数)と推論し、`dot [|1,2|]~i [|10,20|]_i` が 50 でなく `[|10,40|]` になることが分かった。

原因は規則そのものにある。`u * v : Integer` は**階数 0 の受入れ**(整数などのスカラーを、式を変えずに軸のないテンソルとして受け取る規則)で `contractWith` の `Tensor a` 位置へ挿入なしで渡せる。統合型推論は入力式全体で挿入なし型推論を優先するので、`u`・`v` はスカラー引数に決まる。`u`・`v` をテンソル引数とする読みは `*` に `tensorMap2` の挿入を要し、自動挿入型推論の側に落ちて選ばれない。使用箇所では `dot` 自体がスカラー関数として成分ごとに持ち上がり、内側の `contractWith` は階数 0 の値に作用して何もしない。Egison のライブラリ(`lib/math/algebra/tensor.egi`)は `(t1: Tensor a) (t2: Tensor a)` と型注釈を書いて内積を実現している。

## 規則

**固定テンソル位置**とは、型が `Tensor ...` に固定されている仮引数の位置である。基本操作の型(`contractWith` の第 2 引数、`tensorMap` の第 2 引数、`tensorMap2` の第 2・3 引数、`tensorShape`・`tensorIndices`・`dfOrder`・`flipIndices` の引数、`transpose` の第 2 引数、添字参照の対象)がこれにあたる。テンソル構築式 `[| ... |]` の成分の位置は含めない(成分には階数 0 のスカラーも置けるため)。Egison では `contractWith`・`tensorShape` などがライブラリ関数または環境中の関数として与えられるので、環境にある関数の型スキームで `Tensor` に固定された仮引数の位置(注釈つきのライブラリ定義と、推論済みの定義を含む)も固定テンソル位置とする。Lean の形式化では基本操作だけを対象とし、`let` で定義した関数の推論された `Tensor` 仮引数へ算術を渡す場合は対象外とする(この差は、Egison のライブラリ関数が Lean では基本操作であることによる)。

**算術連鎖**とは、二項算術演算子(`+`、`-`、`*`、`/` とそれらのプライム版、`^`)の適用を根とし、算術演算子の適用だけをたどって到達する部分式の木である。演算子の適用でない部分式を葉と呼ぶ。

**要求される仮引数**: ラムダ式 `\x1 ... xn -> body` の仮引数 `xi` は、`body` の中に、固定テンソル位置の実引数が算術連鎖であり、その葉のいずれかが `xi`(同じ束縛)である箇所があるとき、要求される仮引数である。内側の束縛で同名の変数が隠されている箇所は数えない。

**型付け**: 要求される仮引数の型を `Tensor α`(`α` は新しい型変数)で割り当てる。要求されない仮引数は従来どおり `α` である。それ以外の規則(算術の最小の持ち上げ、階数 0 の受入れ、関数引数の変換候補、候補の選択)は変えない。要求される仮引数を含む算術は、既存の持ち上げ規則によって `tensorMap`・`tensorMap2` に持ち上がる。

**根拠(論文の support)**: 固定テンソル位置の起点から、算術連鎖を逆向きにたどって要求される仮引数の型位置へ辺を張る。これにより要求される仮引数の `Tensor` は起点のある型選択になる。

規則は仮引数の分類だけを変える。リテラルや閉じた式の階数 0 の受入れ(`tensorMap f 1`、`foldl (+) 0 ts` の `0`)、関数値の階数 0 の受入れ(`ignore (\x -> x)`)、仮引数をそのまま固定テンソル位置へ渡す場合(`\t -> tensorShape t`、`contractWith combine v`、`\k -> k [|1,2|]`)は変わらない。スカラーを返す関数の適用(`tensorShape (f x)`)は算術連鎖に含めない。

## 例

| 定義 | 従来 | この規則 |
| --- | --- | --- |
| `\u v -> contractWith (\x y -> x + y) (u * v)` | `Integer -> Integer -> Tensor Integer` | `Tensor Integer -> Tensor Integer -> Tensor Integer`、生成項は `contractWith (+) (tensorMap2 (\x y -> x * y) u v)` |
| `dot [\|1,2\|]~i [\|10,20\|]_i` | `[\|10,40\|]` | `50` |
| `dot [\|1,2\|]_i [\|10,20\|]_i` | `[\|10,40\|]` | `[\|10,40\|]` |
| `dot [\|1,2\|]_i [\|10,20\|]_j` | `[\|[\|10,20\|],[\|20,40\|]\|]` | 同じ |
| `dot A_i~j B_j~k`(2×2) | 階数 3 の未縮約の積 | `[\|[\|19,22\|],[\|43,50\|]\|]` |
| `foldl dot A_i~j [B_j~k, C_k~l]` と `dot (dot A B) C` | 階数 4 の未縮約の積 | `[\|[\|41,22\|],[\|93,50\|]\|]` |
| `\x -> tensorShape (x + 1)` | `Integer -> [Integer]`(使用時に成分ごと) | `Tensor Integer -> [Integer]`、`(\x -> tensorShape (x + 1)) [\|1,2\|]` は `[2]` |
| `\x y -> contractWith (+) (x * y + 1)` | `Integer -> Integer -> Tensor Integer` | `Tensor Integer -> Tensor Integer -> Tensor Integer` |
| `\k -> k [\|1,2\|]`、`\t -> tensorShape t`、付録 B.3 の `f4`(`combine x x` は関数適用) | — | 変わらない |
| `\xs -> foldl (+) 0 xs`、`\xs -> map (\x -> x+1) xs`、表 2 の 14 例 | — | 変わらない |

2026-11-09 の影響調査(type-tensor-paper `tmp/review-2026-11-09/impact/`)では、Lean 側の 376 例(論文の全例・表 2・`tests/`・`audits/`・数値例)で変わるのは論文の `.` の 10 例だけ、Egison 側の 193 ファイルと `test/TensorInference.hs` の 183 項目には変わる定義はなかった(ライブラリの `(.)` などは注釈つき)。

要求される仮引数が同じ定義の別の箇所でスカラーとして必要になる場合(たとえば整数のリストの要素として消費される場合)は型エラーになる。調査した範囲にそのような定義はない。必要になれば、従来のスカラーの読みを定義の候補(族の一員)として残す拡張で対応する。

## 実装

### Egison

`Language.Egison.Type.TensorDemand`(新規)の `demandedParameters :: TypeEnv -> [String] -> IExpr -> [Bool]` が、ラムダ本体から要求される仮引数を求める。固定テンソル位置は、`ITensorContractExpr`・`ITensorMapExpr`・`ITensorMap2Expr`・`ITensorMap2WedgeExpr`・添字参照(`IIndexedExpr`・`ISubrefsExpr`・`ISuprefsExpr`・`IUserrefsExpr`)・`ITransposeExpr`・`IFlipIndicesExpr` の対象位置と、環境(`lookupEnv`)にある変数を関数位置とする適用で型スキームの仮引数が `TTensor` の位置である。内側のラムダ・let・パターン・`withSymbols` で束縛し直された名前は除く。

`Type/Infer.hs` のラムダの推論(`ILambdaExpr`、注釈のない場合)で、要求される仮引数に `TTensor` を付けた新しい型変数を割り当てる。注釈つきの仮引数(`inferLambdaWithArgumentTypes` に期待型が渡る場合)は変えない。挿入(`TensorMapInsertion`)は変更せず、テンソル型の仮引数に対する算術は既存の判定で `tensorMap2` になる。

検証: `test/TensorInference.hs` に `.` の定義の型、内積・行列積・`foldl` の連鎖の値、`tensorShape (x + 1)`、変わらない例(`\k -> k [|1,2|]`、`foldl (+) 0`)を加える。`cabal test`、`sample/math` の実行、`type-tensor-mech/tests/local_inference.py --egison-dir ../egison` で Lean との一致を確認する。

### Lean(type-tensor-mech)

ラムダの規則(`SourceGeneration.Generates.lambda`、`SourceDirectGeneration.Generates.lambda` など)に、`BinderStep` の直後の決定的な段階 `DemandStep body` を加える。本体から要求される仮引数を判定する構文的な関数が真のとき、割り当てたばかりの型行と文脈の型 `α` を `Tensor α` に置き換える(新しい型変数は増やさない)。固定テンソル位置は基本操作のテンプレート(`primitiveTemplate`)の `Tensor` 位置と構成子宣言のフィールドとする。要求の根拠は `SourceSupportTraversal.walk` に、固定位置の起点から算術連鎖の葉の仮引数への辺として加える。列挙(`Enumeration`)にも同じ段階を加え、`source_sound`・`source_complete`・改名・整形式性・挿入・選択・評価の各定理のラムダの場合を更新する。通常の HM 保存は、テンソル構文のない式では要求が生じないことの補題で保つ。

検証: `lake build --wfail`、`make test`、影響調査の検出器(`impact/Detect.lean`)と 376 例で変更前後の差分が `.` の例だけであること、論文側 `checks/check_table2.py --require-all`。

### 論文(type-tensor-paper)

G-Lam の規則に仮引数の分類の前提を加え、固定テンソル位置・算術連鎖・要求される仮引数を本文で定義する。support のグラフに要求の辺を加える。§tensor-product の記述は現行のまま成り立つようになる。付録 B.3 の `f4` の記述など、この規則と無関係に実装と食い違う箇所は別途直す。英語版・日本語版の両方を更新する。
