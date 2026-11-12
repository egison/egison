# テンソルの設計仕様と実装・検証の対応

状態：`↑` による持ち上げの許可の設計を確認する段階である．規則と期待結果の正本は [設計仕様](type-tensor-simple.md)，作業項目は [実装・証明の課題](tensor-lifting-tasks.md) とする．Egison と Lean の実装や既存テストの成功を，この仕様全体の実装・証明済みの根拠にしない．

## 1. 実装へ接続する要件

| 対象 | 仕様の要件 |
| --- | --- |
| 定義の推論 | 本体を一度だけ型付けし，型・許可・共有制約・挿入位置を保存する |
| 許可の判定 | 非自明な変換を受け取れる具体化がある仮引数に限り `↑` を付ける |
| 関数の受け渡し | `↑` の位置で，入力と結果を照合して関数全体を一度だけ包む |
| 関数の共有 | 各適用・転送・再帰で同じ型と値を使う |
| 普通の型変数 | 明示的なテンソル型にも具体化し，変換不要なら挿入しない |
| 部分適用 | 残る型・変換制約を保持し，引数の括り方で判断を変えない |
| 型注釈 | 固定の入力型と型変数の共有を保ち，正しい縮約結果の注釈を信頼する |
| 実行 | 関数式と各部分適用の評価順，形状と添字の検査を保つ |

Egison の変更先は [FILE_MAPPING.md](FILE_MAPPING.md)，現行の処理の流れは [implementation.md](implementation.md) にある．`buildFunctionFamily`・`instantiateFunctionRecipe` などの処理が存在することは，`↑` の判定を実装した根拠ではない．`Type/Unify.hs` の型クラス付き照合，厳密な型の照合，トップレベル注釈の照合を，仕様の型の等式・包含・変換と接続する必要がある．

## 2. 論文と Lean

[論文](../../type-tensor-paper/main.tex) は，仕様の例と六群の証明目標を述べる設計稿である．[Lean](../../type-tensor-mech/README.md) の現行入口は `TensorFlags.inferAndInsert` である．この識別子は実装の所在を示すもので，`↑` の推論が完成したことを示さない．証明の接続は [残課題](../../type-tensor-mech/docs/open-problems.md)，既存の変換の定理は [変換と生成](../../type-tensor-mech/docs/candidate-generation.md) に記す．

型保存，完全性，主要型性，通常の型構造と計算の保存，共有，添字・形状の対応について，論文の主張を満たすまで実装と証明を接続する．Egison のライブラリの `contract`・`contractWith` と Lean の基本操作は，還元関数の入力と結果，空でない列の畳込み，型付けと評価を対応させる．

## 3. 既存の検査と新設計への接続

設計仕様 §12 の例は新しい期待結果である．次のファイルは現行の検査の所在であり，まだ新設計に更新していない．検査では値だけでなく，推論型と挿入箇所も確認する．

| 検査対象 | 既存の検査 |
| --- | --- |
| 型・許可・挿入，受け取り側との型の整合 | `test/TensorInference.hs` |
| 高階関数，部分適用，別名，捕捉，再帰 | `test/lib/core/tensor-higher-order.egi`，`tensor-function-families.egi` |
| リストと分岐の共通型，階数 0 の包含 | `test/lib/core/tensor-result-types.egi`，`tensor-result-admission.egi` |
| 積，テンソル型の位置からの入力の決定 | `test/lib/core/tensor-demand.egi`，`indexed-definition-constraints.egi` |
| 形状と添字，生成関数が返すテンソルの組立て | `test/lib/math/tensor.egi`，`tensor-assembly.egi` |
| 名前のない軸と添字の有効範囲，微分形式 | `test/lib/math/symbol-scopes.egi`，`mini-test/9921-appended-index-prefix.egi` |
| Lean の規則と Egison との比較 | `type-tensor-mech/tests/algebraic.py`，`local_inference.py`，`tensor_demand.py` |
| テンソル公開操作の正常結果とエラー | `type-tensor-mech/tests/egison-tensor-operations.json` |

特に次を確認する．

- `map`・`apply`・`foldl`・`discard` の許可と，`filter` の固定した `Bool` の要求を定義から推論する．
- `map tensorRank` の結果を `[Integer]`，`map tensorShape` の結果を `[[Integer]]` とし，挿入しない．`consume` のように結果だけがテンソルの関数もそのまま渡す．
- `foldl` の累積値とリスト要素は独立に具体化できる．加算の持ち上げが必要な場合には，その結果と次の累積値を一致させる．
- `gp` の二つの使用と `foldl` の再帰で，同じ変換済みの関数を使う．`gp` の第二成分は，値が1でも `Tensor Integer` になる例を保つ．
- スカラー述語は，直接・名前付き・保存済みのどの形でもテンソルのリストの `filter` で拒否する．明示的な成分ごとの適用を持つ述語や多相的な定数述語は受理する．
- `tensorShape (x+1)` はスカラーとテンソルの入力に同じ本体を使う．テンソル構築の形状エラーを持ち上げによって回避しない．
- `sum2`，`twiceSum`，`mysum`，独立した複数の定義使用，入れ子の `map` は，本体を再推論せず，保存した制約から型と挿入を決める．
- 関数の入力の反変な包含，リスト・分岐の共通型，正しいトップレベル注釈を保つ．

共通型は `commonResultType`・`commonCollectionType`，組立ては `Core.normalizeTensorWHNF`，追加添字は `refTensorWithOverride`，内部の空の配列への参照は `emptyReference` が検査対象となる．実装後に回帰検査，数式処理の警告・エラー・実行時間，Lean の全検査と公理監査を行う．
