# サンプルの動作状況

2026-11-09 に，成功記録のあるサンプルと標準テストに含まれるサンプル，計71ファイルを
再検証し，**71ファイルすべての成功**を確認した．型エラー・実行時エラー・警告はない．
`sample/` 全体には99ファイルある．残り28ファイルは今回の検証対象に含めていない．

検証対象は，従来の本ファイルに成功実績が記録されていた65ファイル（長時間の実行で
成功していたS5の2例とYang–Millsの例を含む），その後
[`sample-timings.txt`](../scripts/sample-timings.txt) に加わった4ファイル，
`test/Test.hs` の標準テストに加わった2ファイルである．
[`working-samples.txt`](../scripts/working-samples.txt) に全対象と参照時間を保持する．
一度成功したファイルを，失敗するようになったという理由でこの一覧から除かない．

## 再検証の方法

リポジトリ直下で実行する．

```sh
gtimeout 900 scripts/run-sample-suite.sh --working
```

各ファイルを `cabal run -v0 egison -- --no-banner --type-check-strict -t <file>` で直列実行する．
`--type-check-strict` は型エラーを検出した時点で失敗にし，型検査なしの評価へ進まない．
`-t` はトップレベルの式と検査を実行し，`main` は呼び出さない．
終了コード0に加え，出力に警告・型エラー・構文エラー・実行時エラー・検査失敗がないことを
成功条件とする．各ファイルの出力はスクリプトが表示するログディレクトリに保存する．

制限時間は参照時間の1.5倍を切り上げ，最低30秒とする．時間切れの後に処理系の
プロセスが残っていれば，次のファイルの実行を止める．
特定のファイルだけを再実行する場合は，`--working` の後にそのパスを渡す．

## 実測結果

以下の全ファイルが成功した．時間はApple Silicon上のGHC 9.10.1環境で，
Cabalからの起動を含めて秒単位で計測した経過時間である．
参照時間は過去の記録または新規対象の制限時間設定に用いた値であり，
同じ機材・負荷で計測した性能比較ではない．

### `sample/`（16ファイル）

| ファイル | 今回の実測 | 参照時間 |
| --- | ---: | ---: |
| [binary-counter.egi](binary-counter.egi) | 2秒 | 1秒 |
| [bipartite-graph.egi](bipartite-graph.egi) | 2秒 | 2秒 |
| [demo1-ja.egi](demo1-ja.egi) | 2秒 | 1秒 |
| [demo1.egi](demo1.egi) | 3秒 | 1秒 |
| [five-color.egi](five-color.egi) | 2秒 | 1秒 |
| [mahjong.egi](mahjong.egi) | 5秒 | 43秒 |
| [mickey.egi](mickey.egi) | 2秒 | 1秒 |
| [n-queens.egi](n-queens.egi) | 2秒 | 2秒 |
| [one-minute-first.egi](one-minute-first.egi) | 2秒 | 1秒 |
| [one-minute-second.egi](one-minute-second.egi) | 3秒 | 7秒 |
| [poker-hands-with-joker.egi](poker-hands-with-joker.egi) | 3秒 | 13秒 |
| [poker-hands.egi](poker-hands.egi) | 2秒 | 3秒 |
| [primes.egi](primes.egi) | 2秒 | 1秒 |
| [salesman.egi](salesman.egi) | 2秒 | 1秒 |
| [tak.egi](tak.egi) | 5秒 | 1秒 |
| [tree.egi](tree.egi) | 2秒 | 30秒 |

### `sample/io/`（4ファイル）

| ファイル | 今回の実測 | 参照時間 |
| --- | ---: | ---: |
| [args.egi](io/args.egi) | 2秒 | 1秒 |
| [cat.egi](io/cat.egi) | 2秒 | 1秒 |
| [hello.egi](io/hello.egi) | 2秒 | 1秒 |
| [print-primes.egi](io/print-primes.egi) | 2秒 | 1秒 |

### `sample/math/algebra/`（5ファイル）

| ファイル | 今回の実測 | 参照時間 |
| --- | ---: | ---: |
| [canonical-form-absorption.egi](math/algebra/canonical-form-absorption.egi) | 2秒 | 1秒 |
| [cubic-equation.egi](math/algebra/cubic-equation.egi) | 3秒 | 5秒 |
| [groebner-basis.egi](math/algebra/groebner-basis.egi) | 2秒 | 1秒 |
| [quadratic-equation.egi](math/algebra/quadratic-equation.egi) | 2秒 | 2秒 |
| [quartic-equation.egi](math/algebra/quartic-equation.egi) | 3秒 | 2秒 |

### `sample/math/analysis/`（2ファイル）

| ファイル | 今回の実測 | 参照時間 |
| --- | ---: | ---: |
| [eulers-formula.egi](math/analysis/eulers-formula.egi) | 2秒 | 8秒 |
| [vector-analysis.egi](math/analysis/vector-analysis.egi) | 2秒 | 5秒 |

### `sample/math/geometry/`（30ファイル）

| ファイル | 今回の実測 | 参照時間 |
| --- | ---: | ---: |
| [chern-form-of-CP1.egi](math/geometry/chern-form-of-CP1.egi) | 2秒 | 2秒 |
| [chern-form-of-CP2.egi](math/geometry/chern-form-of-CP2.egi) | 3秒 | 1秒 |
| [curvature-form.egi](math/geometry/curvature-form.egi) | 2秒 | 5秒 |
| [euler-form-of-S2.egi](math/geometry/euler-form-of-S2.egi) | 2秒 | 4秒 |
| [euler-form-of-T2.egi](math/geometry/euler-form-of-T2.egi) | 2秒 | 5秒 |
| [exterior-derivative.egi](math/geometry/exterior-derivative.egi) | 2秒 | 1秒 |
| [hodge-E3.egi](math/geometry/hodge-E3.egi) | 2秒 | 3秒 |
| [hodge-Minkowski.egi](math/geometry/hodge-Minkowski.egi) | 3秒 | 18秒 |
| [hodge-laplacian-polar.egi](math/geometry/hodge-laplacian-polar.egi) | 2秒 | 2秒 |
| [hodge-laplacian-spherical.egi](math/geometry/hodge-laplacian-spherical.egi) | 3秒 | 13秒 |
| [kahler-geometry-of-CP1.egi](math/geometry/kahler-geometry-of-CP1.egi) | 2秒 | 1秒 |
| [polar-laplacian-2d-2.egi](math/geometry/polar-laplacian-2d-2.egi) | 2秒 | 2秒 |
| [polar-laplacian-2d-3.egi](math/geometry/polar-laplacian-2d-3.egi) | 2秒 | 1秒 |
| [polar-laplacian-2d.egi](math/geometry/polar-laplacian-2d.egi) | 3秒 | 7秒 |
| [polar-laplacian-3d-2.egi](math/geometry/polar-laplacian-3d-2.egi) | 2秒 | 6秒 |
| [polar-laplacian-3d-3.egi](math/geometry/polar-laplacian-3d-3.egi) | 2秒 | 5秒 |
| [polar-laplacian-3d.egi](math/geometry/polar-laplacian-3d.egi) | 4秒 | 48秒 |
| [riemann-curvature-tensor-of-FLRW-metric.egi](math/geometry/riemann-curvature-tensor-of-FLRW-metric.egi) | 2秒 | 1秒 |
| [riemann-curvature-tensor-of-S2-no-type-annotations.egi](math/geometry/riemann-curvature-tensor-of-S2-no-type-annotations.egi) | 2秒 | 5秒 |
| [riemann-curvature-tensor-of-S2.egi](math/geometry/riemann-curvature-tensor-of-S2.egi) | 2秒 | 4秒 |
| [riemann-curvature-tensor-of-S3.egi](math/geometry/riemann-curvature-tensor-of-S3.egi) | 3秒 | 18秒 |
| [riemann-curvature-tensor-of-S4.egi](math/geometry/riemann-curvature-tensor-of-S4.egi) | 4秒 | 43秒 |
| [riemann-curvature-tensor-of-S5-non-sym.egi](math/geometry/riemann-curvature-tensor-of-S5-non-sym.egi) | 7秒 | 104秒 |
| [riemann-curvature-tensor-of-S5.egi](math/geometry/riemann-curvature-tensor-of-S5.egi) | 7秒 | 104秒 |
| [riemann-curvature-tensor-of-Schwarzschild-metric.egi](math/geometry/riemann-curvature-tensor-of-Schwarzschild-metric.egi) | 3秒 | 12秒 |
| [riemann-curvature-tensor-of-T2-non-sym.egi](math/geometry/riemann-curvature-tensor-of-T2-non-sym.egi) | 2秒 | 10秒 |
| [riemann-curvature-tensor-of-T2.egi](math/geometry/riemann-curvature-tensor-of-T2.egi) | 3秒 | 10秒 |
| [surface.egi](math/geometry/surface.egi) | 2秒 | 4秒 |
| [wedge-product.egi](math/geometry/wedge-product.egi) | 2秒 | 1秒 |
| [yang-mills-equation-of-U1-gauge-theory.egi](math/geometry/yang-mills-equation-of-U1-gauge-theory.egi) | 11秒 | 224秒 |

### `sample/math/number/`（8ファイル）

| ファイル | 今回の実測 | 参照時間 |
| --- | ---: | ---: |
| [17th-root-of-unity.egi](math/number/17th-root-of-unity.egi) | 2秒 | 3秒 |
| [5th-root-of-unity.egi](math/number/5th-root-of-unity.egi) | 3秒 | 1秒 |
| [7th-root-of-unity.egi](math/number/7th-root-of-unity.egi) | 2秒 | 1秒 |
| [eisenstein-primes.egi](math/number/eisenstein-primes.egi) | 2秒 | 2秒 |
| [elliptic-curve-over-F7.egi](math/number/elliptic-curve-over-F7.egi) | 3秒 | 1秒 |
| [euler-totient-function.egi](math/number/euler-totient-function.egi) | 2秒 | 1秒 |
| [gaussian-primes.egi](math/number/gaussian-primes.egi) | 2秒 | 3秒 |
| [tribonacci.egi](math/number/tribonacci.egi) | 2秒 | 2秒 |

### `sample/physics/`（1ファイル）

| ファイル | 今回の実測 | 参照時間 |
| --- | ---: | ---: |
| [tension.egi](physics/tension.egi) | 2秒 | 4秒 |

### `sample/rosetta/`（3ファイル）

| ファイル | 今回の実測 | 参照時間 |
| --- | ---: | ---: |
| [abc_problem.egi](rosetta/abc_problem.egi) | 2秒 | 3秒 |
| [consolidate.egi](rosetta/consolidate.egi) | 3秒 | 1秒 |
| [partial.egi](rosetta/partial.egi) | 2秒 | 1秒 |

### `sample/sat/`（2ファイル）

| ファイル | 今回の実測 | 参照時間 |
| --- | ---: | ---: |
| [cdcl.egi](sat/cdcl.egi) | 2秒 | 8秒 |
| [dp.egi](sat/dp.egi) | 3秒 | 30秒 |

## 今回未検証のファイル

次の28ファイルについて，今回の実行結果は記録していない．
この区分は現在の失敗を意味せず，成功記録に基づく今回の復旧範囲との区別である．

- [bellman-ford.egi](bellman-ford.egi)
- [chopsticks.egi](chopsticks.egi)
- [chopsticks2.egi](chopsticks2.egi)
- [database/edge-sqlite.egi](database/edge-sqlite.egi)
- [database/simple-sqlite.egi](database/simple-sqlite.egi)
- [efficient-backtracking.egi](efficient-backtracking.egi)
- [generalized-sequential-pattern-mining.egi](generalized-sequential-pattern-mining.egi)
- [graph.egi](graph.egi)
- [io/cut.egi](io/cut.egi)
- [ioRef.egi](ioRef.egi)
- [math/analysis/leibniz-formula.egi](math/analysis/leibniz-formula.egi)
- [math/geometry/riemann-curvature-tensor-of-S2xS3.egi](math/geometry/riemann-curvature-tensor-of-S2xS3.egi)
- [math/geometry/riemann-curvature-tensor-of-S7.egi](math/geometry/riemann-curvature-tensor-of-S7.egi)
- [math/geometry/thurston-non-sym.egi](math/geometry/thurston-non-sym.egi)
- [math/geometry/thurston.egi](math/geometry/thurston.egi)
- [n-queen.egi](n-queen.egi)
- [nishiwaki.egi](nishiwaki.egi)
- [physics/tension2.egi](physics/tension2.egi)
- [physics/tension3.egi](physics/tension3.egi)
- [pi.egi](pi.egi)
- [prime-millionaire.egi](prime-millionaire.egi)
- [repl/egison.egi](repl/egison.egi)
- [rosetta/lcs.egi](rosetta/lcs.egi)
- [salesman2.egi](salesman2.egi)
- [tail-recursion.egi](tail-recursion.egi)
- [triangle.egi](triangle.egi)
- [unify.egi](unify.egi)
- [xml-test.egi](xml-test.egi)

## 更新方法

- サンプルが成功したら `working-samples.txt` に追加し，この表を実測結果で更新する．
- 処理系・ライブラリの変更後は全対象を再実行し，診断と所要時間を確認する．
- 計算結果を確かめる既存の `assertEqual` などの検査を削除して成功扱いにしない．
- 数学サンプル全体の一覧は `sample-timings.txt`，復旧後の継続検査の対象は
  `working-samples.txt` を使う．`scripts/run-sample-suite.sh` は引数なしなら前者を使う．

設計は[テンソル型と添字記法](../design/type-tensor-simple.md)，
[行列の逆行列と部分行列](../design/matrix.md)，
[数式処理の型システム](../design/type-cas.md)を参照する．
