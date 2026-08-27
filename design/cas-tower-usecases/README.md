# 拡張可能 CAS 型タワーの検収例

このディレクトリには、[type-cas-tower-implementation.md](../type-cas-tower-implementation.md) と
[type-cas-quotient.md](../type-cas-quotient.md) の現行機能を検証する、実行可能な Egison
プログラムだけを置く。穴を含む設計スケッチや、対象外の将来案は置かない。

## 実行

各ファイルは `assertEqual` を含み、単独で実行できる。

```sh
cabal run egison -- -t design/cas-tower-usecases/01-type-alias.egi
```

Haskell 系のビルド・テストは、ワークスペース規則どおり一度に一つの `cabal` コマンドだけを実行する。

## ファイル一覧

| ファイル | 検証する機能 |
|---|---|
| `01-type-alias.egi` | 透明な `declare cas-type` と多段別名 |
| `02-gaussian-integers.egi` | Z[i] の基本演算、共役、ノルム |
| `03-subtype-promotion.egi` | `declare cas-subtype` による昇格 |
| `04-gaussian-poly.egi` | 入れ子と平らな多項式正規形の選択 |
| `05-quadratic-extension.egi` | Z[√2] とシンボルが関係式を担う商 |
| `06-combined-extensions.egi` | 原子集合の join と複数拡大の合成 |
| `07-modular.egi` | `declare cas-quotient`, `proj`, `repr`, 演算ごとの reduce |
| `08-join-completion.egi` | join の半束性検査、完備化候補、細分化 warning |

暗黙 join は関数適用位置だけで使い、一般の単一化へは入れない。06 は明示注釈なしの
`def c := a + b` でこの経路を検証する。実装との対応は
[type-cas-tower-implementation.md §3, §6](../type-cas-tower-implementation.md) を参照する。

## 対象外の例

- Chebyshev 基底のように同じ表現の読み方を変える型は、値を保って正規形だけを変える
  CAS 型タワーの対象外である。
- Quaternion の非可換乗算は、可換な単項式を前提とする現在の `CASValue` の対象外である。
- Z[i] の素因数分解のように長いアルゴリズム本体が主題となる例は、型機構の検収には含めない。

これらの制限は設計文書に記し、実行不能な `.egi` スケッチは管理しない。
