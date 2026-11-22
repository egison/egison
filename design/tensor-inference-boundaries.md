# テンソルの設計仕様と実装・検証の対応

状態：`↑` による持ち上げの許可の設計を確認する段階である．規則と期待結果の正本は [設計仕様](tensor-types.md)，作業項目は [実装・証明の課題](../../type-tensor-mech/docs/design.md) とする．Egison と Lean の実装や既存テストの成功を，この仕様全体の実装・証明済みの根拠にしない．

## 1. 実装へ接続する要件

| 対象 | 仕様の要件 |
| --- | --- |
| 定義の推論 | 本体を一度だけ型付けし，型・許可・共有制約・挿入位置を保存する |
| 許可の判定 | 適用する関数の入力位置の `↑` を保持し，関数自身の許可で適用と受け渡しの変換を決める |
| 条件付き型 | データ要素や関数の入出力と，本体内の各適用で条件を共有できる．`pred1 : Integer^p -> Bool` では算術と等値比較が同じ条件を使う．`id` と `zip` では通常の型変数にテンソル型を代入できる |
| 関数の受け渡し | `↑` のある入力位置の部分集合を選び，入力・結果・共有制約を照合して関数全体を一度だけ包む |
| 関数の共有 | 受け取った関数は一回の呼出しの中で一つの型を持ち，各適用・転送・再帰で同じ型・値を使う．渡す関数の変換は渡す時点で一度だけ行う |
| 普通の型変数 | 明示的なテンソル型にも具体化し，変換不要なら挿入しない |
| 部分適用 | 残る型・変換制約を保持し，引数の括り方で判断を変えない |
| 型注釈 | 固定の入力型と型変数の共有を保ち，正しい縮約結果の注釈を信頼する |
| 実行 | 関数式と各部分適用の評価順，形状と添字の検査を保つ |

Egison の変更先は [FILE_MAPPING.md](FILE_MAPPING.md)，現行の処理の流れは [implementation.md](implementation.md) にある．`Type/Unify.hs` の型クラス付き照合，厳密な型の照合，トップレベル注釈の照合を，仕様の型の等式・包含・変換と接続する必要がある．

## 2. 論文と Lean

[論文](../../type-tensor-paper/main.tex) は，仕様の例と六群の証明目標を述べる設計稿である．[Lean](../../type-tensor-mech/README.md) の公開入口は `Conditional.Symbolic.Source.infer` である．ソース，数値 JSON，Python の回帰は同じ推論・辞書生成・評価を使う．整数入力の条件付き署名も公開表示へ接続している．全ソースの許可推論と主要型性，一般署名，旧証明の置換は [残課題](../../type-tensor-mech/docs/open-problems.md)，証明済みの定理は [証明の対応](../../type-tensor-mech/docs/details.md) に記す．

型保存，完全性，主要型性，通常の型構造と計算の保存，共有，添字・形状の対応について，論文の主張を満たすまで実装と証明を接続する．Egison のライブラリの `contract`・`contractWith` と Lean の基本操作は，還元関数の入力と結果，空でない列の畳込み，型付けと評価を対応させる．

## 3. 検査対象と検証状況

設計仕様の全署名と例を期待結果とする．次のファイルは検査の所在であり，`↑` の仕様との照合は未完了である．検査では値だけでなく，推論型と挿入箇所も確認する．

| 検査対象 | 既存の検査 |
| --- | --- |
| 型・許可・挿入，受け取り側との型の整合 | `test/TensorInference.hs` |
| 高階関数，部分適用，別名，捕捉，再帰 | `test/lib/core/tensor-higher-order.egi`，`tensor-function-families.egi` |
| リストと分岐の共通型，階数 0 の包含 | `test/lib/core/tensor-result-types.egi`，`tensor-result-admission.egi` |
| 積，テンソル型の位置からの入力の決定 | `test/lib/core/tensor-demand.egi`，`indexed-definition-constraints.egi` |
| 形状と添字，生成関数が返すテンソルの組立て | `test/lib/math/tensor.egi`，`tensor-assembly.egi` |
| 名前のない軸と添字の有効範囲，微分形式 | `test/lib/math/symbol-scopes.egi`，`mini-test/9921-appended-index-prefix.egi` |
| Lean の規則の検査 | `type-tensor-mech/tests/algebraic.py`，`tensor_demand.py`，`specification_alignment.py` |
| テンソル公開操作の正常結果とエラー | `type-tensor-mech/tests/egison-tensor-operations.json` |

特に次を確認する．

- 仕様に挙げた入力全体の許可と条件付き型を定義から推論する．`filter` の一つの述語は固定の `Bool` を返し，述語のテンソルには外側の写像を使う．
- `map tensorRank` の結果を `[Integer]`，`map tensorShape` の結果を `[[Integer]]` とし，挿入しない．`consume` のように結果だけがテンソルの関数もそのまま渡す．
- `foldl` の累積値とリスト要素は独立に具体化できる．加算の持ち上げが必要な場合には，その結果と次の累積値を一致させる．
- 受け取った関数を一回の呼出しの中で一つの型で使う．`gp k xs := (map k xs, k 0)` は `(Integer^p -> b^q)↑ -> [Integer^p]↑ -> ([b^q], b^q)` で，`gp inc [[|1,2|]]` は渡す時点で `inc` を一度だけ包み `([[|2,3|]], 1) : ([Tensor Integer], Tensor Integer)` を返す．`gp inc [1,2]` は `([2,3], 1) : ([Integer], Integer)` を返す．`twin f := (f 0, f [|1|])` は `(Tensor Integer -> b^q)↑ -> (b^q, b^q)` で，`twin inc` は `(1, [|2|]) : (Tensor Integer, Tensor Integer)`，`twin tensorShape` は `([], [1])` を返す．`let f = inc in (f 0, f [|1|])` と `twinTwo f g := (f 0, g [|1|])` の `twinTwo inc inc` は `(Integer, Tensor Integer)` を返す．`foldl` の再帰では同じ関数をそのまま引き継ぐ．
- `pred1` は定義から `Integer^p -> Bool` を推論し，直接・名前付き・保存済みのどの形でもテンソルのリストの `filter` で受理する．整数とテンソルのいずれも比較は一回である．
- 再帰的な `mapInc` の本体で要素がテンソルなら `inc x` に挿入する．直接の仮引数を受ける未注釈の `ignore x := let v = x+1 in True` は `Integer↑ -> Bool` を推論し，`ignore [|2,3|]` は `[|True,True|]` を返すことを検査する．関数のテンソルに対する `map [|inc,inc|] [1,2]` は，各関数にリスト全体を渡して `[|[2,3],[2,3]|]` を返す．
- 直接の仮引数の型を，その値が渡る先の入力の型で決める．`^p` の付いた入力にも `Tensor` の入力にも渡らない仮引数には，本体が条件分岐，`id` などの多相関数の呼出し，局所関数，無関係な束縛を含んでいても `↑` を推論する．`keepNonZero x := if isZero x then 100 else x` は `Integer↑ -> Integer` で `keepNonZero [|0,5|]` は `[|100,5|]`，`pairId x := (x+1, id x)` は `pairId [|1,2|]` が `[|(2,1),(3,2)|]` となる．受け取った関数に渡る仮引数と，要素が `^p` の関数に渡る仮引数は `^p` とする．`applyBoth k x := (x+1, k x)` は `applyBoth tensorShape [|1,2|]` が `([|2,3|], [2])`，`single x := mapInc [x]` は `single [|1,2|]` が `[[|2,3|]]` となる．`(==)` は `Eq` クラスのメソッドであり，`Eq` がテンソルのインスタンスを持つので，その入力は `a^p` になる．テンソルのインスタンスを持たないクラスのメソッドの入力は `↑` を持ち，テンソルには成分ごとに適用する．組み込みの `(=)` は `a^p -> a^p -> Bool` と宣言する（現在の `Type/Check.hs` は `a -> a -> Bool` と宣言している）．`^p` の付いたこれらの入力を，`id` のような型変数だけの入力と区別する．
- 関数やリストの直接の仮引数の `↑` を，仮引数の順序や再帰の形によらず定義から推論する．利用者が定義した `discard k x := let unused = k x in True` に `[|inc,inc|]` を渡すと `[|True,True|]` を返す．引数の順を入れ替えた `mapFlip xs f` は `[a^p]↑ -> (a^p -> b^q)↑ -> [b^q]` で，`mapFlip [1,2] [|inc,inc|]` は `[|[2,3],[2,3]|]`，`mapFlip [|[1,2],[3,4]|] inc` は `[|[2,3],[4,5]|]` となる．利用者が再帰で定義した `zip` は `[a]↑ -> [b]↑ -> [(a, b)]` となる．`observe k x := (tensorShape k, k x)` のように `Tensor` の入力に渡る関数の仮引数は `Tensor (a -> b)` とし，`observe [|inc,inc|] 2` は `([2], [|3,3|])` を返す．
- `tensorShape (x+1)` はスカラーとテンソルの入力に同じ本体を使う．テンソル構築の形状エラーを持ち上げによって回避しない．
- `sum2`，`twiceSum`，`mysum`，独立した複数の定義使用，入れ子の `map` は，本体を再推論せず，保存した制約から型と挿入を決める．
- 関数の入力の反変な包含，リスト・分岐の共通型，正しいトップレベル注釈を保つ．

共通型は `commonResultType`・`commonCollectionType`，組立ては `Core.normalizeTensorWHNF`，追加添字は `refTensorWithOverride`，内部の空の配列への参照は `emptyReference` が検査対象となる．実装後に回帰検査，数式処理の警告・エラー・実行時間，Lean の全検査と公理監査を行う．
