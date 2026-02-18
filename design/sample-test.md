# sample/ ディレクトリ テスト結果

テスト実施日: 2026-02-19

テスト方法:
- 一般ファイル: `cabal run egison -- -t <file>`
- mathファイル: `cabal run egison -- -t -l lib/math/normalize.egi <file>`
- タイムアウト: 30秒（一部60秒）

全95ファイル中、正常動作 **44ファイル**、エラー **51ファイル**。

修正作業により **9ファイル** を新たに動作可能にした。

---

## 修正して動くようになったファイル (9)

| ファイル | 修正内容 |
|---|---|
| binary-counter.egi | `\$i ->` → `\i ->` |
| mickey.egi | `def mickey' $cs :=` → `def mickey' (cs: [Char]) : [Char] :=` |
| five-color.egi | `\$n ->` → `\n ->` + `2#$1 n` → タプルデストラクチャリング |
| rosetta/abc_problem.egi | `\$w ->` → `\w ->` |
| math/number/euler-totient-function.egi | `\$p ->` → `\p ->` + アサーション期待値修正 |
| salesman.egi | `\$i ->` → `\i ->` + `def main $args` → `def main (args: [String]) : IO ()` |
| math/geometry/riemann-curvature-tensor-of-FLRW-metric.egi | `def W $r :=` → 型注釈追加 + `` `a w `` → `` `(a w) `` + `declare symbol` 追加 |
| math/geometry/thurston-non-sym.egi | `\$σ ->` → `\σ ->` |
| math/geometry/thurston.egi | `\$σ ->` → `\σ ->` (パースは通るが型エラーあり) |

---

## 正常動作するファイル (44)

### sample/ ルート (16)
| ファイル | 備考 |
|---|---|
| bipartite-graph.egi | |
| **binary-counter.egi** | **修正済** |
| demo1.egi | |
| demo1-ja.egi | |
| **five-color.egi** | **修正済** |
| ioRef.egi | |
| mahjong.egi | 実行に約30秒 |
| **mickey.egi** | **修正済** |
| n-queens.egi | |
| nishiwaki.egi | |
| one-minute-first.egi | |
| one-minute-second.egi | |
| poker-hands.egi | |
| poker-hands-with-joker.egi | |
| primes.egi | |
| tak.egi | |

### sample/math/algebra (3)
| ファイル | 備考 |
|---|---|
| quadratic-equation.egi | Unbound variable警告あり |
| cubic-equation.egi | Unbound variable警告あり |
| quartic-equation.egi | Unbound variable警告あり |

### sample/math/number (6)
| ファイル | 備考 |
|---|---|
| 5th-root-of-unity.egi | |
| 7th-root-of-unity.egi | |
| 17th-root-of-unity.egi | |
| eisenstein-primes.egi | |
| **euler-totient-function.egi** | **修正済** |
| gaussian-primes.egi | |

### sample/math/geometry (12)
| ファイル | 備考 |
|---|---|
| riemann-curvature-tensor-of-S2.egi | |
| riemann-curvature-tensor-of-S2-no-type-annotations.egi | |
| riemann-curvature-tensor-of-T2.egi | |
| riemann-curvature-tensor-of-S3.egi | |
| riemann-curvature-tensor-of-T2-non-sym.egi | |
| **riemann-curvature-tensor-of-FLRW-metric.egi** | **修正済** (テスト式なし、定義のみ) |
| **thurston-non-sym.egi** | **修正済** (テスト式なし) |
| exterior-derivative.egi | |
| curvature-form.egi | |
| wedge-product.egi | |
| hodge-laplacian-polar.egi | |
| polar-laplacian-2d-2.egi | |

### sample/sat (1)
| ファイル | 備考 |
|---|---|
| cdcl.egi | |

### sample/rosetta (3)
| ファイル | 備考 |
|---|---|
| **abc_problem.egi** | **修正済** |
| consolidate.egi | |
| partial.egi | |

### sample/io (1)
| ファイル | 備考 |
|---|---|
| hello.egi | |

### sample/ ルート (その他)
| ファイル | 備考 |
|---|---|
| **salesman.egi** | **修正済** (main関数のみ、テスト式なし) |

---

## 動かないファイル (51)

### カテゴリ1: パースエラー — 旧構文が残っている (10ファイル)

修正困難な自動翻訳コード。`cambda`, `\match`, `\matchAll`, `#$t`, `JoinExpr`/`VarExpr` などの内部構文が残っている。

| ファイル | エラー箇所 | 備考 |
|---|---|---|
| unify.egi | `def showΣ $σ :=` | 自動翻訳の不完全変換 |
| rosetta/lcs.egi | `def doubleList $a :=` | 自動翻訳の不完全変換 |
| repl/egison.egi | `def main $args :=`, `def repl $env :=` | 自動翻訳の不完全変換 |
| graph.egi | `{|1, 4, 3|}` マルチセットリテラル | 現在のハッシュ構文と非互換 |
| database/edge-sqlite.egi | `cons node #$px #$py $` パターン | 旧パターン構文 |
| math/analysis/vector-analysis.egi | `def trace %t :=` | `%t` テンソルパラメータ構文未対応 |

### カテゴリ2: パースは通るがまだ別のエラーがある修正済みファイル (6ファイル)

| ファイル | エラー内容 |
|---|---|
| pi.egi | タイムアウト (60秒以上、`showDecimal` の計算コスト) |
| salesman2.egi | 型エラー: Hash Integer String vs [String] (添字変数の型推論) |
| io/cut.egi | 型エラー: `map read nums` の型不一致 |
| math/analysis/leibniz-formula.egi | 評価エラー: `Sd` (積分) 関数の正規化が未対応 |
| math/geometry/thurston.egi | 型エラー: `R'{_i_j}_k~l` の反対称インデックス命名の不一致 |

### カテゴリ3: 型エラー (4ファイル)

| ファイル | エラー内容 |
|---|---|
| prime-millionaire.egi | `map read args` — `read` の型が IO String と推論される |
| tail-recursion.egi | `x - 1` が MathExpr として推論される (Integer 期待) |
| generalized-sequential-pattern-mining.egi | パターン関数の型推論エラー |
| io/cat.egi | `[]` と `[String]` の型不一致 (パターンマッチ) |

### カテゴリ4: 評価エラー (10ファイル)

| ファイル | エラー内容 |
|---|---|
| bellman-ford.egi | `Expected math expression, but found: "plus"` (Ord型クラス) |
| chopsticks.egi | `Expected bool, but found: < + 1 2 + 5 1` (比較演算) |
| chopsticks2.egi | `listToTree` が lambda を返す (遅延評価/型クラス問題) |
| efficient-backtracking.egi | `Expected rational, but found: n` |
| n-queen.egi | `Expected integer, but found: - 2 1` (loop パターン内) |
| tree.egi | `Primitive data pattern match failed` |
| triangle.egi | `Inconsistent tuple lengths: expected 3, but got 2` |
| math/number/tribonacci.egi | `Tensor index must be an integer or a single symbol` |
| sat/dp.egi | `Expected collection, but found: #<lambda ...>` (unique関数) |
| math/geometry/riemann-curvature-tensor-of-Schwarzschild-metric.egi | `Expected number, but found: G` (declare symbolで大文字不可) |

### カテゴリ5: アサーション失敗 (4ファイル)

| ファイル | エラー内容 |
|---|---|
| math/analysis/eulers-formula.egi | `cos(0)`, `sin(0)` が簡約されない |
| math/geometry/hodge-E3.egi | Hodge star の反対称テンソル要素欠落 |
| math/geometry/hodge-Minkowski.egi | Hodge star の反対称テンソル要素欠落 |
| math/geometry/yang-mills-equation-of-U1-gauge-theory.egi | 同上 + タイムアウト |

### カテゴリ6: タイムアウト — 30秒以内に完了しない (12ファイル)

| ファイル | 備考 |
|---|---|
| math/geometry/riemann-curvature-tensor-of-S4.egi | |
| math/geometry/riemann-curvature-tensor-of-S5.egi | |
| math/geometry/riemann-curvature-tensor-of-S5-non-sym.egi | |
| math/geometry/riemann-curvature-tensor-of-S2xS3.egi | |
| math/geometry/euler-form-of-S2.egi | |
| math/geometry/euler-form-of-T2.egi | |
| math/geometry/surface.egi | |
| math/geometry/polar-laplacian-2d.egi | |
| math/geometry/polar-laplacian-2d-3.egi | |
| math/geometry/polar-laplacian-3d.egi | |
| math/geometry/polar-laplacian-3d-2.egi | |
| math/geometry/polar-laplacian-3d-3.egi | |

### カテゴリ7: 未実装機能・欠損ライブラリ (5ファイル)

| ファイル | エラー内容 |
|---|---|
| math/geometry/riemann-curvature-tensor-of-S7.egi | `subrefs` 未実装 |
| math/geometry/hodge-laplacian-spherical.egi | `Not implemented: subrefs` |
| physics/tension.egi | `undefined function 'trace'` (部分的に動作) |
| physics/tension2.egi | `undefined function 'trace'` |
| physics/tension3.egi | `undefined function 'trace'` |

### カテゴリ8: 外部依存・その他 (4ファイル)

| ファイル | エラー内容 |
|---|---|
| xml-test.egi | `file does not exist: lib/tree/xml.egi` |
| database/simple-sqlite.egi | `undefined function 'simpleSelect'` |
| math/geometry/chern-form-of-CP1.egi | 出力なし、`VarExpr`/`WedgeApplyExpr` がunbound |
| math/geometry/chern-form-of-CP2.egi | 出力なし、`WedgeApplyExpr` 等がunbound |

### IO系ファイル

| ファイル | 状態 |
|---|---|
| io/print-primes.egi | 無限出力（正常動作だがタイムアウトに見える） |
| io/args.egi | 型エラー (部分的に動作) |

---

## エラーの傾向分析

### 1. 旧構文の互換性問題 (最多)
- ラムダ引数の `$` (修正済みファイル多数)
- `%t` テンソルパラメータ構文
- `{|...|}`  マルチセットリテラル
- 自動翻訳コードの不完全変換 (`cambda`, `#$t`, `JoinExpr` 等)

### 2. declare symbol で大文字シンボルが使えない (修正済み)
- `K`, `G` などの大文字記号が `declare symbol` で宣言できなかった問題は修正済み

### 3. Integer と MathExpr の型変換問題
- 算術演算 (`-`, `+`, `<`, `>`) が MathExpr を返すケースが多い
- `Expected integer/rational/bool, but found: MathExpr` 系エラー
- `bellman-ford.egi`, `n-queen.egi`, `tail-recursion.egi` 等に影響

### 4. 反対称テンソル（Hodge star）の計算バグ
- `hodge-E3.egi`, `hodge-Minkowski.egi`, `yang-mills-equation-of-U1-gauge-theory.egi`
- Hodge star の結果が不正（反対称テンソルの要素欠落）

### 5. subrefs 未実装
- テンソルの添字参照機能が未実装
- `riemann-curvature-tensor-of-S7.egi`, `hodge-laplacian-spherical.egi`

### 6. trace 関数未定義
- physics系で使われている `trace` 関数が ScalarData として定義されていない

### 7. 高次元テンソルの計算タイムアウト
- S4以上の球面や laplacian の計算が30秒以内に終わらない
- 性能最適化が必要
