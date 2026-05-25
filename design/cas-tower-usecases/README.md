# 拡張可能 CAS 型タワーのユースケース集

このディレクトリは、[type-cas-tower.md](../type-cas-tower.md) の設計を詰めるための
**aspirational sample code** を集める。各ファイルは現在の Egison では動かない
(構文・semantics が未実装) が、ユーザがこう書きたい/こう振る舞ってほしいという
意図を示す具体例として機能する。

## ファイル一覧

各 use case は以下の観点を含む:
- **Declare**: ユーザがどう型を宣言したいか
- **Use**: どう演算したいか (型推論・型注釈の使用例)
- **Expected**: 期待する型と正規形
- **Required machinery**: この case を成立させるために必要な実装

| 番号 | ファイル | 主な機能 | Phase |
|---|---|---|---|
| 01 | `01-type-alias.egi` | 単純な型エイリアス | α |
| 02 | `02-gaussian-integers.egi` | 代数拡大 `Z[i]` + UFD typeclass による素因数分解 | α + 既存 declare rule + 新 `UFD` class |
| 03 | `03-subtype-promotion.egi` | サブタイプによる自動昇格 (`Integer → GaussianInt`) | β |
| 04 | `04-gaussian-poly.egi` | 中間型 `Poly GaussianInt [x]` | α + β |
| 05 | `05-quadratic-extension.egi` | 二次代数拡大 `Z[√2]` | α |
| 06 | `06-combined-extensions.egi` | 複数代数拡大 `Z[i, √2]` の交わり | α + β |
| 07 | `07-modular.egi` | 剰余体 `Z/nZ` (新 CASValue 構造) | ε |
| 08 | `08-chebyshev.egi` | 多項式の異なる正規化基底 | γ |
| 09 | `09-quaternion.egi` | 四元数 (非可換、新 CASValue 構造) | ε |

## 読み方

各ファイルの先頭に **意図 (intent)** ブロックがあり、その下に
desugar 後の挙動・必要な runtime ロジック・open question 等のコメント
ブロックが続く。コードの下半分は通常の sample のように `assertEqual`
を含むが、現状は構文エラー or 未実装機能で動かない。

## 設計レビューでの使い方

1. ユースケースを順に読む
2. 「これを実現するには Phase X までが必要」を確認
3. Phase ごとにどの構文・データ型・関数追加が要るか抽出
4. type-cas-tower.md にフィードバック
