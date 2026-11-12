# Egison 設計文書

`design/` には，言語の設計仕様，実装との対応，検収用プログラム，採否を検討中の提案，決定した設計への移行作業を置く．仕様が未実装の決定を含む場合は，その状態と確認済みの範囲を明記する．
解決済みの不具合報告、廃案、作業途中の調査記録は残さず、必要なら Git の履歴を参照する。

文書の役割は次のように分ける．

- **仕様**: 言語機能と制約を定義する．未実装の決定を含む文書では，冒頭にその旨を記し，実装状況への参照を置く．
- **実装マッピング**: 仕様がどの処理段階・ファイルで実装されるかを示す。
- **検収資料**: 実装済み機能を実行可能な例で確認する。
- **提案**: まだ実装していない言語機能の案。現在の制限、案の仕様、完了条件を記す。実装したら仕様に移し、
  廃案になったら削除する。
- **移行作業**: 決定した仕様へ実装・論文・証明を合わせる作業を記す．仕様そのものは重複して定義せず，完了したら削除する．

## 全体構成

- [implementation.md](./implementation.md): パースから評価までの処理の流れ。
- [FILE_MAPPING.md](./FILE_MAPPING.md): 処理段階と Haskell モジュールの詳細な対応表。
- [interpreter-performance.md](./interpreter-performance.md): 性能の計測方法と対象となる処理。

## パターンマッチと TypePM

- [pattern.md](./pattern.md): パターン宣言、matcher、match 式の型付け。
- [pattern-function-implementation.md](./pattern-function-implementation.md): パターン関数の `PatFuncScheme` と実装契約。
- [matcher-capability.md](./matcher-capability.md): Egison 本体における二-sort matcher capability の実装範囲。
形式仕様と証明の正本は `type-pm-mech` に置く。Egison 側の文書は、実装との対応と
Egison core 外の拡張だけを記述する。
- [pattern-family-for.md](./pattern-family-for.md): パターン族の名前とターゲット型を分ける
  `inductive pattern F for T` 構文。組み込みの型の上の族と、一つの型の上の複数の族。

## 型クラスとテンソル

- [type-class.md](./type-class.md): 型クラス、辞書渡し、スーパークラス、現在の制限。
- [runtime-type-dispatch.md](./runtime-type-dispatch.md): CAS 値に対する浅い実行時型ディスパッチ。
- [type-tensor-simple.md](./type-tensor-simple.md): **テンソルの設計仕様の正本**．型とテンソル印，型推論と挿入，高階関数，型注釈，形状，添字，期待結果を統合する．テンソル印を使う新しい型推論は未実装．
- [tensor-inference-boundaries.md](./tensor-inference-boundaries.md): 現行実装との差，Egison・論文・Lean の対応，検証する性質と既存の検査．
- [tensor-flag-types.md](./tensor-flag-types.md): **移行作業**．設計確認後の試作，実装，論文，Lean の更新手順と完了条件．

テンソルの設計を確認するときは `type-tensor-simple.md` を読む．§4 はスカラー関数にテンソルを渡す場合，§5 は高階関数にスカラー関数を渡す場合の挿入を説明する．§2 の型の一覧，§5.1 の `filter` の通常の型と述語の受理・拒否，§7 の型注釈，§8 の挿入なし型付け，§12 の期待結果も合わせて確認できる．実装状況と未完了の作業は，続く二文書に分けている．

## CAS

- [type-cas.md](./type-cas.md): CAS 型システムの総合仕様。
- [type-cas-tower-implementation.md](./type-cas-tower-implementation.md): CAS タワーの仕様と実装箇所の対応。
- [type-cas-quotient.md](./type-cas-quotient.md): 型タワーと独立した CAS 商型。
- [cas-simplification.md](./cas-simplification.md): グレブナー基底を含む CAS 簡約。
- [matrix.md](./matrix.md): 逆行列と，空の部分行列を含む分解・結合。
- [function-symbol.md](./function-symbol.md): 関数シンボル、微分索引、代入。
- [cas-tower-usecases/](./cas-tower-usecases/): CAS タワーと商型の実行可能な検収例。

## 文書を更新するときの基準

1. 現行実装を説明する記述の誤りは修正する．決定済みの未実装仕様は，実装済みと読める書き方にせず，実装との差を対応文書に明記する．
2. 完了した計画表や一時的なテスト結果は残さない。
3. 採否未定の提案と，決定した仕様への移行作業を区別する．規則と例は仕様の正本にまとめ，移行作業には未完了の作業と完了条件を記す．
4. 実装ファイルの一覧を重複させず、詳細な一覧は [FILE_MAPPING.md](./FILE_MAPPING.md) に集約する。
5. 文書から参照するテストは、現在追跡されている `test/` または検収用プログラムに限る。
