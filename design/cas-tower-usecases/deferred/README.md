# deferred: 簡潔に書けない / スコープ外のユースケース

親ディレクトリ ([cas-tower-usecases/](../)) は「**本機構 (タワー α–δ + 商機構 q1–q4) で簡潔に書けるもの**」だけを収録する方針 (2026-07-04)。この基準を満たさないものをここに隔離する。

## 1. 機構スコープ外 (設計判断により対象外)

- [`chebyshev.egi`](./chebyshev.egi) — 基底変更は「再解釈型」で、タワーの基盤前提 (表現の読み ⟦·⟧ の型非依存性) を壊す。将来「全単射再解釈型」機構を設計するなら復活候補 ([type-cas-tower.md §6.2](../../type-cas-tower.md))
- [`quaternion.egi`](./quaternion.egi) — 非可換乗算は可換性前提の `CASTerm`/Monomial 表現で表せず、Phase ε (新 `CASValue` constructor) が必要。実装・論文スコープ外 ([type-cas-tower.md §8 既定 3](../../type-cas-tower.md))

## 2. 機構的には書けるが、サンプルが簡潔にならない

- [`gaussian-factorization.egi`](./gaussian-factorization.egi) — Z[i] の EuclideanDomain (divMod の丸め) と UFD 素因数分解。パターン2 instance として設計スコープ内だが、アルゴリズム本体が長く `...` の穴を含む。02 (基本演算・共役・ノルム — こちらは簡潔) から分離

## 3. ファイルにせず親側にポインタだけ残したもの

- GF(7)[x] / GF(p^k) 合成 (旧 07 末尾のスケッチ) → [type-cas-quotient.md §7–8](../../type-cas-quotient.md) (q5、optional)
- Galois ノルム (旧 06 末尾の stub) → **deferred 入りを取消**: 共役 = シンボルの符号反転代入なので `substitute` で簡潔に書けることが判明し、実装形で 06 に残置
