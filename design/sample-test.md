# sample/ ディレクトリ テスト結果

テスト実施日: 2026-02-19

テスト方法:
- 一般ファイル: `cabal run egison -- -t <file>`
- mathファイル: `cabal run egison -- -t -l lib/math/normalize.egi <file>`
- タイムアウト: 30秒（一部60秒）

全95ファイル中、正常動作 **50ファイル**、エラー **45ファイル**。

修正作業により **15ファイル** を新たに動作可能にした。

---

## 全ファイル一覧

凡例: ✅ 正常動作 / ❌ エラー / ⏳ タイムアウト / 🔧 修正済み

### sample/ ルート

| # | ファイル | 状態 | 備考 |
|---|---|---|---|
| 1 | bellman-ford.egi | ❌ | `Expected math expression, but found: "plus"` |
| 2 | bipartite-graph.egi | ✅ | |
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
| 22 | prime-millionaire.egi | ❌ | `map read args` の型推論エラー |
| 23 | primes.egi | ✅ | |
| 24 | salesman.egi | ✅🔧 | `\$i ->` → `\i ->` + main関数修正 |
| 25 | salesman2.egi | ❌ | Hash Integer String vs [String] 型エラー |
| 26 | tail-recursion.egi | ❌ | `x - 1` が MathExpr として推論される |
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
| 33 | args.egi | ❌ | 型エラー (部分的に動作) |
| 34 | cat.egi | ❌ | `[]` と `[String]` の型不一致 |
| 35 | cut.egi | ❌ | `map read nums` の型不一致 |
| 36 | hello.egi | ✅ | |
| 37 | print-primes.egi | ⏳ | 無限出力（正常動作だがタイムアウトに見える） |

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
| 52 | vector-analysis.egi | ❌ | 旧構文 `def trace %t :=` |

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
| 65 | riemann-curvature-tensor-of-S4.egi | ⏳ | 約51秒で完了（5分テスト） |
| 66 | riemann-curvature-tensor-of-S5.egi | ⏳ | 約2分で完了（5分テスト） |
| 67 | riemann-curvature-tensor-of-S5-non-sym.egi | ⏳ | 約2分で完了（5分テスト） |
| 68 | riemann-curvature-tensor-of-S7.egi | ❌ | 型エラー `cos ε` / `sin ε` が衝突 + `declare symbol` 不足 |
| 69 | riemann-curvature-tensor-of-S2xS3.egi | ⏳ | 5分でもタイムアウト |
| 70 | riemann-curvature-tensor-of-FLRW-metric.egi | ✅🔧 | 型注釈追加 + `declare symbol` 追加 |
| 71 | riemann-curvature-tensor-of-Schwarzschild-metric.egi | ❌ | `Expected number, but found: G` (`M.inverse` 未対応) |
| 72 | euler-form-of-S2.egi | ⏳ | 5分でもタイムアウト |
| 73 | euler-form-of-T2.egi | ⏳ | 5分でもタイムアウト |
| 74 | surface.egi | ⏳ | 5分でもタイムアウト |
| 75 | exterior-derivative.egi | ✅ | |
| 76 | curvature-form.egi | ✅ | |
| 77 | wedge-product.egi | ✅ | |
| 78 | hodge-E3.egi | ✅🔧 | `ε'`+`subrefs` Hodge star、アサーション値修正 |
| 79 | hodge-Minkowski.egi | ✅🔧 | `ε'`+`subrefs` Hodge star、アサーション値修正 |
| 80 | hodge-laplacian-polar.egi | ✅ | `ε'`+`subrefs` に変更 |
| 81 | hodge-laplacian-spherical.egi | ⏳ | `subrefs`修正済みだが3D球面ラプラシアン計算が重い |
| 82 | polar-laplacian-2d.egi | ⏳ | 5分でもタイムアウト |
| 83 | polar-laplacian-2d-2.egi | ✅ | |
| 84 | polar-laplacian-2d-3.egi | ⏳ | 5分でもタイムアウト |
| 85 | polar-laplacian-3d.egi | ⏳ | 5分でもタイムアウト |
| 86 | polar-laplacian-3d-2.egi | ⏳ | 5分でもタイムアウト |
| 87 | polar-laplacian-3d-3.egi | ⏳ | 5分でもタイムアウト |
| 88 | thurston.egi | ❌ | 型エラー `R'{_i_j}_k~l` の命名不一致 |
| 89 | thurston-non-sym.egi | ✅🔧 | `\$σ ->` → `\σ ->` |
| 90 | yang-mills-equation-of-U1-gauge-theory.egi | ✅🔧 | 旧AST変換 + `ε'`+`subrefs` hodge (約65秒) |
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
| ✅ 正常動作 | 50 |
| ❌ エラー | 28 |
| ⏳ タイムアウト | 17 |
| **合計** | **95** |

うち 🔧修正済み: 15ファイル

---

## 今回の修正まとめ

### subrefs 実装修正
`hs-src/Language/Egison/Core.hs` の `ISubrefsExpr` / `ISuprefsExpr` 評価に `evalWHNF` フォールバックを追加。テンソルが `TensorData`/`ITensor` 以外に評価される場合も正しく処理。

### Hodge star 修正 (`ε` → `ε'`+`subrefs`)
全 hodge 関数で `(ε N)_(i_1)..._(i_N) . A..._(j_1)..._(j_k)` を `(subrefs A (map 1#j_$1 (between 1 k))) . (subrefs (ε' N k) (map 1#i_$1 (between 1 N)))` に変更。

`ε'` は出力インデックスをソート済みに制約し（上三角形式）、`subrefs` で正しくインデックス名を割り当てる。これにより：
- Hodge star の出力が元のS式コードと一致
- Yang-Mills の `δ F` 因子が元のコードと一致（-2因子。`ε` 版では -4 になっていた）

### 変更ファイル一覧
| ファイル | 変更内容 |
|---|---|
| `hs-src/Language/Egison/Core.hs` | `subrefs`/`suprefs` の `evalWHNF` フォールバック追加 |
| `sample/math/geometry/hodge-E3.egi` | `ε'`+`subrefs` hodge、アサーション値修正 |
| `sample/math/geometry/hodge-Minkowski.egi` | `ε'`+`subrefs` hodge、アサーション値修正 |
| `sample/math/geometry/hodge-laplacian-polar.egi` | `ε'`+`subrefs` hodge |
| `sample/math/geometry/hodge-laplacian-spherical.egi` | `ε'`+`subrefs` hodge、`d`関数修正、`declare symbol`追加、`g`逆行列を明示定義 |
| `sample/math/geometry/yang-mills-equation-of-U1-gauge-theory.egi` | 旧AST→現代構文変換、`ε'`+`subrefs` hodge、`function`構文 |
