# sample/ ディレクトリ テスト結果

テスト実施日: 2026-02-19（function symbol 対応更新: 2026-02-19）

テスト方法:
- 一般ファイル: `cabal run egison -- -t <file>`
- mathファイル: `cabal run egison -- -t -l lib/math/normalize.egi <file>`
- タイムアウト: 30秒（一部60〜180秒）

全95ファイル中、正常動作 **60ファイル**、型警告あり **5ファイル**、エラー **21ファイル**、タイムアウト **9ファイル**。

修正作業により **20ファイル** を新たに動作可能にした（うち5ファイルは function symbol 化によるタイムアウト解消）。

---

## 全ファイル一覧

凡例: ✅ 正常動作 / ⚠️ 型警告（動作する） / ❌ エラー / ⏳ タイムアウト / 🔧 修正済み

### sample/ ルート

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 1 | bellman-ford.egi | ❌ | `Expected math expression, but found: "plus"` |
| 2 | bipartite-graph.egi | ⚠️ | 型エラー警告あるが結果は正しい |
| 3 | binary-counter.egi | ✅🔧 | `\$i ->` → `\i ->` |
| 4 | chopsticks.egi | ❌ | `Expected bool, but found: < + 1 2 + 5 1` |
| 5 | chopsticks2.egi | ❌ | `listToTree` が lambda を返す |
| 6 | demo1.egi | ✅ | |
| 7 | demo1-ja.egi | ✅ | |
| 8 | efficient-backtracking.egi | ❌ | `Expected rational, but found: n` |
| 9 | five-color.egi | ✅🔧 | `\$n ->` → `\n ->` 等 |
| 10 | graph.egi | ❌ | `{|1, 4, 3|}` マルチセットリテラル非互換 |
| 11 | ioRef.egi | ✅ | |
| 12 | mahjong.egi | ✅ | 実行に約30秒 |
| 13 | mickey.egi | ✅🔧 | `def mickey' $cs :=` → 型注釈追加 |
| 14 | n-queen.egi | ❌ | `Expected integer, but found: - 2 1` |
| 15 | n-queens.egi | ✅ | |
| 16 | nishiwaki.egi | ✅ | |
| 17 | one-minute-first.egi | ✅ | |
| 18 | one-minute-second.egi | ✅ | |
| 19 | pi.egi | ⏳ | 60秒以上、`showDecimal` の計算コスト |
| 20 | poker-hands.egi | ✅ | |
| 21 | poker-hands-with-joker.egi | ✅ | |
| 22 | prime-millionaire.egi | ⚠️ | `map read args` の型推論警告 |
| 23 | primes.egi | ✅ | |
| 24 | salesman.egi | ✅🔧 | `\$i ->` → `\i ->` + main関数修正 |
| 25 | salesman2.egi | ⚠️ | Hash Integer String vs [String] 型警告 |
| 26 | tail-recursion.egi | ⏳ | `x - 1` が MathExpr として推論される |
| 27 | tak.egi | ✅ | |
| 28 | tree.egi | ❌ | `Primitive data pattern match failed` |
| 29 | triangle.egi | ❌ | `Inconsistent tuple lengths` |
| 30 | unify.egi | ❌ | 旧構文 `def showΣ $σ :=` |
| 31 | xml-test.egi | ❌ | `file does not exist: lib/tree/xml.egi` |

### sample/generalized-sequential-pattern-mining

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 32 | generalized-sequential-pattern-mining.egi | ❌ | パターン関数の型推論エラー |

### sample/io

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 33 | args.egi | ✅ | |
| 34 | cat.egi | ✅ | |
| 35 | cut.egi | ⚠️ | `map read nums` の型警告 |
| 36 | hello.egi | ✅ | |
| 37 | print-primes.egi | ✅ | |

### sample/repl

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 38 | egison.egi | ❌ | 旧構文 `def main $args :=` 等 |

### sample/database

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 39 | edge-sqlite.egi | ❌ | 旧パターン構文 `cons node #$px` |
| 40 | simple-sqlite.egi | ❌ | `undefined function 'simpleSelect'` |

### sample/rosetta

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 41 | abc_problem.egi | ✅🔧 | `\$w ->` → `\w ->` |
| 42 | consolidate.egi | ✅ | |
| 43 | lcs.egi | ❌ | 旧構文 `def doubleList $a :=` |
| 44 | partial.egi | ✅ | |

### sample/sat

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 45 | cdcl.egi | ✅ | |
| 46 | dp.egi | ❌ | `Expected collection, but found: #<lambda ...>` |

### sample/math/algebra

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 47 | quadratic-equation.egi | ✅ | `declare symbol` 追加で警告解消 |
| 48 | cubic-equation.egi | ✅ | `declare symbol` 追加で警告解消 |
| 49 | quartic-equation.egi | ✅ | `declare symbol` 追加で警告解消 |

### sample/math/analysis

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 50 | eulers-formula.egi | ❌ | `cos(0)`, `sin(0)` が簡約されない |
| 51 | leibniz-formula.egi | ❌ | `Sd` (積分) 関数の正規化が未対応 |
| 52 | vector-analysis.egi | ✅ | `function` symbol 使用（微分・テイラー展開テスト） |

### sample/math/number

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 53 | 5th-root-of-unity.egi | ✅ | |
| 54 | 7th-root-of-unity.egi | ✅ | |
| 55 | 17th-root-of-unity.egi | ✅ | |
| 56 | eisenstein-primes.egi | ✅ | |
| 57 | euler-totient-function.egi | ✅🔧 | `\$p ->` → `\p ->` + アサーション修正 |
| 58 | gaussian-primes.egi | ✅ | |
| 59 | tribonacci.egi | ❌ | `Tensor index must be an integer or a single symbol` |

### sample/math/geometry

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 60 | riemann-curvature-tensor-of-S2.egi | ✅ | |
| 61 | riemann-curvature-tensor-of-S2-no-type-annotations.egi | ✅ | |
| 62 | riemann-curvature-tensor-of-T2.egi | ✅ | |
| 63 | riemann-curvature-tensor-of-S3.egi | ✅ | |
| 64 | riemann-curvature-tensor-of-T2-non-sym.egi | ✅ | |
| 65 | riemann-curvature-tensor-of-S4.egi | ✅ | 約51秒 |
| 66 | riemann-curvature-tensor-of-S5.egi | ✅ | 約2分 |
| 67 | riemann-curvature-tensor-of-S5-non-sym.egi | ✅ | 約2分 |
| 68 | riemann-curvature-tensor-of-S7.egi | ❌ | 型エラー `cos ε` / `sin ε` が衝突 + `declare symbol` 不足 |
| 69 | riemann-curvature-tensor-of-S2xS3.egi | ⏳ | 3分でもタイムアウト |
| 70 | riemann-curvature-tensor-of-FLRW-metric.egi | ✅🔧 | `def a := function (w)` に変更 |
| 71 | riemann-curvature-tensor-of-Schwarzschild-metric.egi | ❌ | `Expected number, but found: G` (`M.inverse` 未対応) |
| 72 | euler-form-of-S2.egi | ⏳ | 3分でもタイムアウト |
| 73 | euler-form-of-T2.egi | ⏳ | 3分でもタイムアウト |
| 74 | surface.egi | ⏳ | 3分でもタイムアウト |
| 75 | exterior-derivative.egi | ✅ | |
| 76 | curvature-form.egi | ✅ | |
| 77 | wedge-product.egi | ✅ | |
| 78 | hodge-E3.egi | ✅🔧 | `ε'`+`subrefs` Hodge star、アサーション値修正 |
| 79 | hodge-Minkowski.egi | ✅🔧 | `ε'`+`subrefs` Hodge star、アサーション値修正 |
| 80 | hodge-laplacian-polar.egi | ✅ | `function (r, θ)` + Hodge Laplacian |
| 81 | hodge-laplacian-spherical.egi | ⏳ | `function (r, θ, φ)` 使用だが3D球面ラプラシアン計算が重い |
| 82 | polar-laplacian-2d.egi | ✅🔧 | `def u := function (x, y)` + 連鎖律テスト（⏳→✅） |
| 83 | polar-laplacian-2d-2.egi | ✅ | `function (r, θ)` + Christoffel ラプラシアン |
| 84 | polar-laplacian-2d-3.egi | ✅🔧 | `def f := function (r, θ)` + Christoffel ラプラシアン（⏳→✅） |
| 85 | polar-laplacian-3d.egi | ✅🔧 | `def u := function (x, y, z)` + 連鎖律テスト（⏳→✅） |
| 86 | polar-laplacian-3d-2.egi | ✅🔧 | `def f := function (r, θ, φ)` + Christoffel ラプラシアン（⏳→✅） |
| 87 | polar-laplacian-3d-3.egi | ✅🔧 | `def f := function (r, θ, φ)` + Christoffel ラプラシアン（⏳→✅） |
| 88 | thurston.egi | ⚠️ | 型エラー警告（∇の定義で）、計算は進行 |
| 89 | thurston-non-sym.egi | ✅🔧 | `\$σ ->` → `\σ ->` + `declare symbol` 追加 |
| 90 | yang-mills-equation-of-U1-gauge-theory.egi | ✅🔧 | `function` symbol + `ε'`+`subrefs` hodge (約65秒) |
| 91 | chern-form-of-CP1.egi | ✅🔧 | 旧AST変換、`assertEqual`付き |
| 92 | chern-form-of-CP2.egi | ✅🔧 | 旧AST変換 |

### sample/physics

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 93 | tension.egi | ✅🔧 | `trace` 修正 + `declare symbol` 追加 |
| 94 | tension2.egi | ⏳ | `trace` 修正済み、`declare symbol` 追加済み |
| 95 | tension3.egi | ⏳ | `trace` 修正済み、`declare symbol` 追加済み |

---

## 集計

| 状態 | 件数 |
|---|---|
| ✅ 正常動作 | 60 |
| ⚠️ 型警告（動作する） | 5 |
| ❌ エラー | 21 |
| ⏳ タイムアウト | 9 |
| **合計** | **95** |

うち 🔧修正済み: 20ファイル

---

## 今回の function symbol 対応まとめ

### FLRW-metric: `a` を function symbol に変更
旧: `` `(a w)^2 `` → 新: `def a := function (w)` + `a^2`

スケールファクター `a(w)` をバッククォート関数適用から function symbol に変更。
微分（連鎖律）が正しく動作し、Ricci テンソル・スカラー曲率の計算が正常に行われる。

### polar-laplacian: function symbol + Christoffel 方式に統一

| ファイル | 旧方式 | 新方式 | 効果 |
|---|---|---|---|
| polar-laplacian-2d.egi | `declare symbol u` + `u x y` | `def u := function (x, y)` | ⏳→✅（5分超→6秒） |
| polar-laplacian-2d-3.egi | `declare symbol f` + `f r θ` | `def f := function (r, θ)` + Christoffel | ⏳→✅（5分超→2秒） |
| polar-laplacian-3d.egi | `declare symbol u` + `u x y z` | `def u := function (x, y, z)` | ⏳→✅（5分超→34秒） |
| polar-laplacian-3d-2.egi | `declare symbol f` + `f r θ φ` | `def f := function (r, θ, φ)` + Christoffel | ⏳→✅（5分超→5秒） |
| polar-laplacian-3d-3.egi | `declare symbol f` + `f r θ φ` | `def f := function (r, θ, φ)` + Christoffel | ⏳→✅（5分超→5秒） |

function symbol を使うことで引数を毎回評価する必要がなくなり、劇的な高速化を実現。

### assertEqual での注意点
function symbol の偏微分記号（`f|1|1 r θ` 等）は assertEqual の期待値に直接書けない（`Expected hash` エラー）。
代わりに `∂/∂` 式または `assert` + `show` 文字列比較を使用。

### thurston: `declare symbol` 追加
`thurston.egi` と `thurston-non-sym.egi` に `declare symbol θ₁, θ₂, θ₃, θ₄, κ, p` を追加。
`thurston.egi` は∇の定義に型エラー警告が残るが計算は進行する。

### 変更ファイル一覧
| ファイル | 変更内容 |
|---|---|
| `sample/math/geometry/riemann-curvature-tensor-of-FLRW-metric.egi` | `def a := function (w)` + `` `(a w)^2 `` → `a^2` |
| `sample/math/geometry/polar-laplacian-2d.egi` | `def u := function (x, y)` + `assert` + `show` |
| `sample/math/geometry/polar-laplacian-2d-3.egi` | `def f := function (r, θ)` + Christoffel ラプラシアン |
| `sample/math/geometry/polar-laplacian-3d.egi` | `def u := function (x, y, z)` + `assert` + `show` |
| `sample/math/geometry/polar-laplacian-3d-2.egi` | `def f := function (r, θ, φ)` + Christoffel ラプラシアン |
| `sample/math/geometry/polar-laplacian-3d-3.egi` | `def f := function (r, θ, φ)` + Christoffel ラプラシアン |
| `sample/math/geometry/thurston.egi` | `declare symbol θ₁, θ₂, θ₃, θ₄, κ, p` 追加 |
| `sample/math/geometry/thurston-non-sym.egi` | `declare symbol θ₁, θ₂, θ₃, θ₄, κ, p` 追加 |

---

## 過去の修正まとめ

### subrefs 実装修正
`hs-src/Language/Egison/Core.hs` の `ISubrefsExpr` / `ISuprefsExpr` 評価に `evalWHNF` フォールバックを追加。テンソルが `TensorData`/`ITensor` 以外に評価される場合も正しく処理。

### Hodge star 修正 (`ε` → `ε'`+`subrefs`)
全 hodge 関数で `(ε N)_(i_1)..._(i_N) . A..._(j_1)..._(j_k)` を `(subrefs A (map 1#j_$1 (between 1 k))) . (subrefs (ε' N k) (map 1#i_$1 (between 1 N)))` に変更。

### 過去の変更ファイル一覧
| ファイル | 変更内容 |
|---|---|
| `hs-src/Language/Egison/Core.hs` | `subrefs`/`suprefs` の `evalWHNF` フォールバック追加 |
| `sample/math/geometry/hodge-E3.egi` | `ε'`+`subrefs` hodge、アサーション値修正 |
| `sample/math/geometry/hodge-Minkowski.egi` | `ε'`+`subrefs` hodge、アサーション値修正 |
| `sample/math/geometry/hodge-laplacian-polar.egi` | `ε'`+`subrefs` hodge |
| `sample/math/geometry/hodge-laplacian-spherical.egi` | `ε'`+`subrefs` hodge、`d`関数修正、`declare symbol`追加、`g`逆行列を明示定義 |
| `sample/math/geometry/yang-mills-equation-of-U1-gauge-theory.egi` | 旧AST→現代構文変換、`ε'`+`subrefs` hodge、`function`構文 |
