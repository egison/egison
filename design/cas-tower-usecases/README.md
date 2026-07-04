# 拡張可能 CAS 型タワーのユースケース集

このディレクトリは、[type-cas-tower.md](../type-cas-tower.md) と [type-cas-quotient.md](../type-cas-quotient.md) の実装 (Phase α–δ / 商機構 q1–q4) の**検収基準**。

> **2026-07-04 更新**: 実装完了に伴い、**本体 01–08 の全ファイルがそのまま実行可能**になった (旧 API `coerce` の除去・`_` 入り識別子の camelCase 化・γ′ の flat-exit に合わせた比較の意味論的等価化を実施):
>
> ```sh
> for f in design/cas-tower-usecases/0*.egi; do gtimeout 30 cabal run egison -- -t "$f"; done
> ```
>
> 唯一の既知の制限は 06 に明記: 閉じた原子集合が異なる operand の暗黙 join は推論に未配線 (明示注釈で共通型へ上げる。implementation.md §7)。

**収録基準 (2026-07-04)**: **本機構で簡潔に書けるものだけ**を置く。確定済みの設計判断 D1–D5 ([type-cas-tower.md §8](../type-cas-tower.md)) の構文・意味論に整合し、`...` の穴や長いアルゴリズム本体を含まないこと。基準を満たさないものは [deferred/](./deferred/) に隔離する。

## ファイル一覧

| 番号 | ファイル | 主な機能 | 必要フェーズ |
|---|---|---|---|
| 01 | `01-type-alias.egi` | 透明型エイリアス (D3) | α |
| 02 | `02-gaussian-integers.egi` | Z[i]: 基本演算・共役 (substitute 1 行)・ノルム | α + 既存規則 |
| 03 | `03-subtype-promotion.egi` | subtype 自動昇格・骨格 join | β |
| 04 | `04-gaussian-poly.egi` | 中間型: nested vs flat の正規形選択 (**中核ケーススタディ**) | α+β+γ |
| 05 | `05-quadratic-extension.egi` | Z[√2]: シンボル担持商・Pell 方程式 | α + 既存規則 |
| 06 | `06-combined-extensions.egi` | Z[i, √2]: 原子集合 join・Galois ノルム (substitute) | α+β |
| 07 | `07-modular.egi` | Z/7Z: 商機構 (proj/repr・per-op reduce・型 dispatch Eq) | 商 q1–q4 |
| 08 | `08-join-completion.egi` | D1: join 半束性検査・完備化・細化単調性 | β |

**[deferred/](./deferred/)**: 収録基準を満たさないもの — 機構スコープ外 (Chebyshev = 再解釈型、Quaternion = ε) と、機構的には書けるが簡潔にならないもの (Z[i] の divMod/UFD 素因数分解)。分類基準は [deferred/README.md](./deferred/README.md)。

## 読み方

各ファイルの先頭に **Intent / Required machinery / Resolved (確定済み設計判断との対応) / Open questions (残存)** を記載する。コード部分は `assertEqual` を含み、**すべて実行して検証できる** (上記コマンド)。

## 検収基準としての使い方

1. **Phase α**: 01・02・05 が動くこと
2. **Phase β**: 03・06・08 が動くこと (08 は半束性検査のエラーメッセージ・完備化提案まで含む)
3. **Phase γ–δ**: 04 が動くこと (注釈による nested ↔ flat の正規形選択)
4. **商機構 q1–q4**: 07 が動くこと
