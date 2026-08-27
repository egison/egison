# Egison 設計文書

`design/` には、現行実装を説明する仕様、実装マッピング、検収用プログラムだけを置く。
解決済みの不具合報告、廃案、作業途中の調査記録は残さず、必要なら Git の履歴を参照する。

文書の役割は次の三つに分ける。

- **仕様**: 現在の言語機能と制約を定義する。
- **実装マッピング**: 仕様がどの処理段階・ファイルで実装されるかを示す。
- **検収資料**: 実装済み機能を実行可能な例で確認する。

## 全体構成

- [implementation.md](./implementation.md): パースから評価までの処理の流れ。
- [FILE_MAPPING.md](./FILE_MAPPING.md): 処理段階と Haskell モジュールの詳細な対応表。

## パターンマッチと TypePM

- [pattern.md](./pattern.md): パターン宣言、matcher、match 式の型付け。
- [pattern-function-implementation.md](./pattern-function-implementation.md): パターン関数の `DualScheme` と実装契約。
- [matcher-capability.md](./matcher-capability.md): Egison 本体における二-sort matcher capability の実装範囲。
- [type-pm-compatibility.md](./type-pm-compatibility.md): `type-pm-mech3` の core と Egison 固有拡張の境界。

形式仕様と証明の正本は `type-pm-mech3` に置く。Egison 側の文書は、実装との対応と
Egison core 外の拡張だけを記述する。

## 型クラスとテンソル

- [type-class.md](./type-class.md): 型クラス、辞書渡し、スーパークラス、現在の制限。
- [runtime-type-dispatch.md](./runtime-type-dispatch.md): CAS 値に対する浅い実行時型ディスパッチ。
- [type-tensor-simple.md](./type-tensor-simple.md): テンソル型と添字記法の基本仕様。
- [tensor-map-insertion-simple.md](./tensor-map-insertion-simple.md): スカラー関数をテンソルへ持ち上げる変換。
- [tensor-map-higher-order-lift.md](./tensor-map-higher-order-lift.md): 高階関数の callback に対する tensor-lift の伝播。

## CAS

- [type-cas.md](./type-cas.md): CAS 型システムの総合仕様。
- [type-cas-tower-implementation.md](./type-cas-tower-implementation.md): CAS タワーの仕様と実装箇所の対応。
- [type-cas-quotient.md](./type-cas-quotient.md): 型タワーと独立した CAS 商型。
- [cas-simplification.md](./cas-simplification.md): グレブナー基底を含む CAS 簡約。
- [function-symbol.md](./function-symbol.md): 関数シンボル、微分索引、代入。
- [cas-tower-usecases/](./cas-tower-usecases/): CAS タワーと商型の実行可能な検収例。

## 文書を更新するときの基準

1. 現行仕様と実装が一致しない記述は、履歴注記を足すのではなく修正または削除する。
2. 完了した計画表や一時的なテスト結果は残さない。
3. 未実装事項を残す場合は、現在の制限と完了条件だけを記す。
4. 実装ファイルの一覧を重複させず、詳細な一覧は [FILE_MAPPING.md](./FILE_MAPPING.md) に集約する。
5. 文書から参照するテストは、現在追跡されている `test/` または検収用プログラムに限る。
