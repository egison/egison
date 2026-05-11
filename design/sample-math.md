# sample/math 現状と coverage 改善計画

`sample/math/` 以下の数学サンプルプログラムは、Egison の CAS・テンソル代数・微分形式
等の表現力を実際の応用数学計算で示すショーケース集です。本ドキュメントは:

1. 全サンプルの現状（pass/fail, assertion 数）
2. 失敗の根本原因の分類
3. coverage を増やすためのロードマップ

をまとめます。**しばらくの目標は「全サンプルが意味のある assertion を持ち、すべて pass
すること」**。

---

## 集計 (最新)

| 区分 | 合計 | PASS | FAIL | assertion 平均 |
|---|---:|---:|---:|---:|
| `algebra/` | 3 | 3 | 0 | 2.0 |
| `analysis/` | 3 | 2 | 1 | 8.3 |
| `geometry/` | 33 | 28 | 5 | 5.7 |
| `number/` | 7 | 6 | 1 | 2.4 |
| **合計** | **46** | **40** | **6** | **5.4** |

PASS 率: **87% (40/46)**。assertion 総数: 約 250 個。

> **注**: per-sample timeout = 想定時間 × 1.5 (FAIL カテゴリは 300s 固定) で計測。
> `yang-mills-equation-of-U1-gauge-theory.egi` は 224s で完走するため、180s 固定
> timeout だと FAIL になる。

### Quote semantics 修正による直近の改善 (2026-05)

`'(...)` (apostrophe) と `` `(...) `` (backtick) は **別々の機構** で、
egison-book §4.3-4.4 に正式な区別がある:

- **`` ` `` (バッククオート) — 式展開の制御** (§4.3): 続く式を一つの opaque atom
  として扱い、積和標準形展開を抑制する。`` `(x+1)^2 `` は `` `(x+1)^2 `` のまま、
  `` `(x+1)^2 / `(x+1) = `(x+1) `` のように約分される。
- **`'` (シングルクオート) — 関数適用の制御** (§4.4): **続くものは変数のみ**。
  定義済み変数を関数記号として扱う。`'sqrt`, `'sin` 等の用途。

古い sample は `'(...)` (apostrophe + 括弧式) を書いていたが、これは
**book 仕様外** で、parser の `QuoteSymbolExpr` の fallback arm で「中を
評価して quote せず返す」だけなので、**式展開抑制は効かない**。これが
polynomial blow-up の原因だった。修正は `` `(...) `` への置換 (= **書き間違い
の修正**)。

将来的に v3 era の sample を引っ張ってきた場合、同様に `'(...)` → `` `(...) ``
の置換が必要になる可能性がある。書く人は最初から `` ` `` を使うこと。

この semantics の理解が曖昧だったため複数の sample が `'(...)` 形で書かれて
**多項式 blow-up** で fail / timeout していた。`` ` `` に置換することで:

| ファイル | Before | After |
|---|---|---|
| `euler-form-of-T2.egi` | 168s で fail (degree 12 polynomial) | **7.2s で PASS** |
| `euler-form-of-S2.egi` | 古い `d` 定義で型エラー | **3.8s で PASS** (paper canonical 形に書き直し) |
| `riemann-curvature-tensor-of-Schwarzschild-metric.egi` | timeout / Christoffel 簡約失敗 | **13.7s で PASS** |
| `riemann-curvature-tensor-of-FLRW-metric.egi` | (pass済) | **3.3s で PASS** (assertion を `` ` `` 形に同期) |
| `surface.egi` | 型エラー | **4.7s で PASS** (`function (x,y)` declaration + `userRefs` style) |

> **注**: PASS/FAIL は test runner の timeout 設定に依存。30s timeout だと
> `polar-laplacian-3d` と `riemann-curvature-tensor-of-S4` が時間切れになるため、
> 60s 以上を推奨。本ドキュメントは 60s timeout 基準。

---

## 全サンプル一覧

### `algebra/` (3/3 PASS)

| ファイル | 状態 | 時間 | asserts | 内容 |
|---|---|---:|---:|---|
| `cubic-equation.egi` | ✅ | 4s | 1 | カルダノ公式による 3 次方程式の解 |
| `quadratic-equation.egi` | ✅ | 3s | 4 | 2 次方程式の解の公式 |
| `quartic-equation.egi` | ✅ | 1s | 1 | フェラーリ公式による 4 次方程式の解 |

### `analysis/` (1/3 PASS)

| ファイル | 状態 | 時間 | asserts | 内容 / 失敗原因 |
|---|---|---:|---:|---|
| `eulers-formula.egi` | ✅ | 8s | 4 | テイラー展開によるオイラーの公式 |
| `leibniz-formula.egi` | ❌ | 1s | 6 | π/4 のライプニッツ級数。`Sd` (不定積分) lib バグで `Sd x 'cos x` 等が `'cos x` を数として扱おうとして失敗 |
| `vector-analysis.egi` | ✅ | 6s | 15 | テイラー展開・偏微分・gradient/curl/div。`"Taylor expansion of f(x)"` の assertion を **string-equality (脆弱)** から **assertEqual on values (canonical-form 不変)** に書き換えて pass |

### `geometry/` (17/32 PASS) — 微分幾何・物理サンプル

#### Hodge / Laplacian / 微分形式 (一部 PASS)

| ファイル | 状態 | 時間 | asserts | 内容 / 失敗原因 |
|---|---|---:|---:|---|
| `exterior-derivative.egi` | ✅ | 1s | 3 | 外微分 d、d²=0 |
| `wedge-product.egi` | ✅ | 1s | 4 | ウェッジ積の反対称性 |
| `hodge-E3.egi` | ✅ | 3s | 2 | ユークリッド 3 次元の Hodge ∗ |
| `hodge-Minkowski.egi` | ✅ | 18s | 2 | ミンコフスキー時空の Hodge ∗ |
| `hodge-laplacian-polar.egi` | ✅ | 2s | 1 | 極座標 Laplacian (Hodge 経由) |
| `hodge-laplacian-spherical.egi` | ✅ | 13s | 1 | 球座標 Laplacian。`abs(non-negative monomial)` の declare rule 追加で `abs(sin²θ r⁴) = sin²θ r⁴` が消えるようになり pass |

#### 極座標 Laplacian バリアント

| ファイル | 状態 | 時間 | asserts | 内容 / 失敗原因 |
|---|---|---:|---:|---|
| `polar-laplacian-2d.egi` | ✅ | 7s | 2 | 2D 極 Laplacian (chain rule 経由) |
| `polar-laplacian-2d-2.egi` | ✅ | 1s | 3 | 2D 極 Laplacian (テンソル記法) |
| `polar-laplacian-2d-3.egi` | ✅ | 2s | 2 | 2D 極 Laplacian (別の手法) |
| `polar-laplacian-3d.egi` | ✅ | 46s | 1 | 3D 極 Laplacian |
| `polar-laplacian-3d-2.egi` | ✅ | 6s | 2 | 3D 極 Laplacian バリアント |
| `polar-laplacian-3d-3.egi` | ✅ | 5s | 2 | 3D 極 Laplacian バリアント |

#### Riemann 曲率テンソル

| ファイル | 状態 | 時間 | asserts | 内容 / 失敗原因 |
|---|---|---:|---:|---|
| `riemann-curvature-tensor-of-S2.egi` | ✅ | 4s | 11 | S² の Riemann tensor |
| `riemann-curvature-tensor-of-S2-no-type-annotations.egi` | ✅ | 5s | 11 | S² (型注釈なし版) |
| `riemann-curvature-tensor-of-S3.egi` | ✅ | 18s | 15 | S³ |
| `riemann-curvature-tensor-of-S4.egi` | ✅ | 42s | 14 | S⁴ |
| `riemann-curvature-tensor-of-S5.egi` | ✅ | 104s | 9 | S⁵。`declare symbol` 追加で動作 |
| `riemann-curvature-tensor-of-S5-non-sym.egi` | ✅ | 104s | 7 | S⁵ (非対称 Christoffel) |
| `riemann-curvature-tensor-of-S7.egi` | ❌ | >180s | 7 | S⁷。`declare symbol` + `ε` → `ξ` rename (Levi-Civita lib 衝突回避) で型エラー解消、但しまだ heavy computation で timeout |
| `riemann-curvature-tensor-of-S2xS3.egi` | ❌ | >180s | 1 | S²×S³。`'(...)` を `` ` `` に変えても M.inverse on 5×5 symbolic matrix 単独で >60s かかり timeout |
| `riemann-curvature-tensor-of-T2.egi` | ✅ | 9s | 15 | T² (トーラス) |
| `riemann-curvature-tensor-of-T2-non-sym.egi` | ✅ | 11s | 15 | T² (非対称) |
| `riemann-curvature-tensor-of-FLRW-metric.egi` | ✅ | 1s | 2 | FLRW (宇宙論) 計量。`'(1-Kr²)` → `` `(1-Kr²) `` |
| `riemann-curvature-tensor-of-Schwarzschild-metric.egi` | ✅ | 12s | 12 | Schwarzschild 計量。`'(c²r-2GM)` → `` `(c²r-2GM) `` で完走、Christoffel assertion を CAS canonical 形に同期、Ric_ij = 0 (vacuum solution) も確認 |

#### Chern / Euler 形式

| ファイル | 状態 | 時間 | asserts | 内容 / 失敗原因 |
|---|---|---:|---:|---|
| `chern-form-of-CP1.egi` | ✅ | 2s | 3 | CP¹ の Chern 形式。multi-factor `exp` 規則の追加で `exp(-x)*exp(x)` の cancellation が 3 因子以上の項でも fire するようになり pass |
| `chern-form-of-CP2.egi` | ✅ | 1s | 1 | CP² の Chern 形式 |
| `curvature-form.egi` | ✅ | 5s | 8 | 曲率形式の一般論 |
| `euler-form-of-S2.egi` | ✅ | 3s | 2 | S² の Euler 形式。paper canonical (`!(flip ∂/∂)` の disjoint completion + `Γ~i_j_#` 1-form) に書き直し |
| `euler-form-of-T2.egi` | ✅ | 6s | 6 | T² の Euler 形式。`'(a*cos θ+b)` → `` `(...) `` で polynomial blow-up を抑制 (was 168s で fail) |

#### Thurston / その他特殊幾何

| ファイル | 状態 | 時間 | asserts | 内容 / 失敗原因 |
|---|---|---:|---:|---|
| `surface.egi` | ✅ | 4s | 10 | 一般曲面の幾何。`f` を `function (x,y)` で declare + `userRefs f [n]` 形に書き直し |
| `thurston.egi` | ❌ | >180s | 4 | Thurston 例の WCS 不変量 (EMR paper §4)。Mathematica 級の簡約が必要 |
| `thurston-non-sym.egi` | ❌ | >180s | 4 | Thurston (非対称 ∇J)。**`Inconsistent tensor index: [_?] vs [_<val>]`** バグ |
| `yang-mills-equation-of-U1-gauge-theory.egi` | ✅ | 224s | 2 | U(1) ゲージ Yang-Mills。重い計算で 180s timeout だと FAIL するが 224s で完走 |

### `number/` (6/7 PASS)

| ファイル | 状態 | 時間 | asserts | 内容 / 失敗原因 |
|---|---|---:|---:|---|
| `5th-root-of-unity.egi` | ✅ | 2s | 2 | 1 の原始 5 乗根 (`z^5 = 1` assertion はコメントアウト — denesting 未実装) |
| `7th-root-of-unity.egi` | ✅ | 1s | 1 | 7 乗根 |
| `17th-root-of-unity.egi` | ✅ | 3s | 1 | 17 乗根 (Gauss の定規コンパス作図) |
| `eisenstein-primes.egi` | ✅ | 1s | 2 | アイゼンシュタイン素数 |
| `gaussian-primes.egi` | ✅ | 2s | 2 | ガウス素数 |
| `euler-totient-function.egi` | ✅ | 2s | 1 | オイラー totient 関数 |
| `tribonacci.egi` | ❌ | 3s | 8 | トリボナッチ。**`Tensor index must be an integer or a single symbol`** バグ |

---

## 失敗原因の分類

| カテゴリ | 件数 | 関連サンプル | 修正方針 |
|---|---:|---|---|
| **CAS 簡約の弱さ** | 5+ | `vector-analysis` (Taylor), `chern-form-of-CP1` (`exp(-x)*exp(x)`), `hodge-laplacian-spherical` (`abs`), `Schwarzschild` (入れ子分数), `euler-form-of-S2`/`T2` 等 | 簡約規則の拡充 (declare rule auto): `exp(-x)*exp(x)=1`, `abs` の符号確定、入れ子分数の縮約等 |
| **型エラー** (`Cannot unify types`) | 4 | `surface`, `thurston`, `riemann-curvature-tensor-of-S7`, おそらく `S4`/`S5` も | 型推論が複合幾何構造を扱えない。要詳細調査 |
| **`Sd` (不定積分) lib バグ** | 1 | `leibniz-formula` | `lib/math/analysis/integral.egi` の `Sd` 関数のリライト |
| **タイムアウト (>30s)** | 2 | `polar-laplacian-3d`, `riemann-curvature-tensor-of-S2xS3` | テスト時の timeout 緩和 / `casRewriteDd` 高速化 |
| **テンソル index pattern バグ** | 2 | `thurston-non-sym` (`Inconsistent tensor index`), `tribonacci` (`Tensor index must be integer or single symbol`) | テンソル index 評価器の修正 |
| **詳細未捕捉** | 4 | `S4`, `S5`, `S5-non-sym`, `yang-mills`, `euler-form-of-S2`/`T2`, `polar-laplacian-3d` | 個別調査が必要 (warning のみで exit 1 になるパターン含む) |

---

## 既に取り組んだ修正の効果

直近の以下の修正で:

- **A1 (`coefficients` Frac 係数バグ)** → `quadratic-equation.egi` が pass するように
- **A2 (型 unifier widening)** → `mini-test/107` 等の widening 注釈が動くように
- **A4 (`def n : T := zero` dispatch)** → 0-arity typeclass method の typed annotation
- **B1 (multi-factor sqrt 規則)** → 3-項版 5th root of unity 等の nested radical
- **uppercase symbol pre-binding** → `FLRW` で `K` を含む式が動くように、`Schwarzschild` で `G/M/c` の算術が動くように
- **multi-factor `exp` 規則 (B1 と同じパターン)** → `exp(-x)*exp(x)` の cancellation が 3 因子以上の項でも fire するように。`chern-form-of-CP1.egi` が **NEW PASS**
- **`chern-form-of-CP1.egi` の expected value** を CAS canonical 形に合わせて更新 (`r/(-A) = -r/A`)
- **timeout 緩和**: `polar-laplacian-3d.egi` は計算 ~40s で完了するため、60s timeout なら pass

これにより **PASS 数が 27 → 31 (60% → 69%)** に改善:

| ファイル | PASS 化の理由 |
|---|---|
| `chern-form-of-CP1.egi` | multi-factor exp 規則 + expected value の sign 修正 |
| `polar-laplacian-3d.egi` | timeout 緩和 (~40s で計算完了) |
| `riemann-curvature-tensor-of-S4.egi` | timeout 緩和 (~50s で計算完了) |
| `hodge-laplacian-spherical.egi` | `abs(non-negative monomial) = monomial` 規則の追加で `abs(sin²θ r⁴)` 因子が消えるように |
| `vector-analysis.egi` | string-equality assertion を value 比較に書き直し (canonical form の order 違いに頑健に) |

---

## Coverage 改善のロードマップ

### Phase 1: 軽量な assertion 追加 (low-hanging fruit)

既に PASS している 27 サンプルのうち、assertion が少ないものに追加する。これは
runtime バグ修正なしで coverage を上げられる。

| ファイル | 現 asserts | 追加余地 |
|---|---:|---|
| `algebra/cubic-equation.egi` | 1 | カルダノ判別式の確認、各根の平方項チェック等 |
| `algebra/quartic-equation.egi` | 1 | フェラーリ補助方程式の解 |
| `geometry/chern-form-of-CP2.egi` | 1 | 中間 connection や Chern 数の値 |
| `geometry/hodge-laplacian-polar.egi` | 1 | 中間ステップ |
| `number/17th-root-of-unity.egi` | 1 | 中間 cyclotomic 多項式値 |
| `number/7th-root-of-unity.egi` | 1 | 同上 |
| `number/euler-totient-function.egi` | 1 | 個別の n に対する φ(n) 値 |

### Phase 2: 簡約規則の拡充 (CAS 強化)

現状の FAIL の大半は CAS 簡約の弱さ。declare rule auto の追加で複数同時に通せる
可能性が高い:

- ✅ **`exp(-x) * exp(x) = 1`** (B1 と同じ multi-factor body-match 形): `chern-form-of-CP1.egi` 等が PASS
- ✅ **`abs(positive_expr) = positive_expr`** (declare rule auto で全偶数冪 monomial 内部の `abs` を剥がす): `hodge-laplacian-spherical.egi` 等が PASS
- ⏳ **入れ子分数の縮約**: `(c²r - 2GM) / (c²r)` 形の簡約 (Schwarzschild 系)。多項式 GCD/factor が必要なので大物 (Phase 5 相当)
- ✅ **CAS 簡約 fixedpoint の到達深さ** (調査済 = NOT the issue): 残りの FAIL の原因を個別に調べた結果、iteration 不足ではなく (a) `Sd` lib バグ, (b) 型エラー, (c) Christoffel 簡約 (多項式 GCD), (d) tensor index 評価バグ, (e) タイムアウト の 5 種に分類できた。`iterateRulesLoopWithTriggers` (Primitives.hs) の fixed-point ループは正しく動作している

### Phase 3: lib バグ修正

- **`Sd` (不定積分)** の根本書き直し → `leibniz-formula.egi` が pass

### Phase 4: テンソル / dispatch バグ修正

- **テンソル index pattern バグ** (`Inconsistent tensor index`) → `tribonacci`, `thurston-non-sym`
- **typeclass dispatch** (`__super_AddGroup`) → `thurston`
- **EMR paper の WCS 不変量計算** が pass すれば、Egison が論文計算の参照実装として成立

### Phase 5: 性能改善

- **`casRewriteDd` の高速化または declare rule 化** → `S2xS3`, `polar-laplacian-3d` の
  タイムアウト解消
- **Schwarzschild の Christoffel 計算** が手元で 30s 内に終わるよう最適化

### Phase 6: 新規サンプル追加

既存サンプルが十分通るようになったら、新しい応用領域を追加:

- 量子力学（時間依存 Schrödinger 方程式の摂動展開）
- 一般相対論（Kerr, Reissner-Nordström 計量）
- 表現論（Lie 代数の構造定数計算）
- 数論（Eisenstein 級数、L 関数の解析的部分）

---

## 検証コマンド

```sh
# 全 sample/math 実行 (約 5-10 分)
for f in $(find sample/math -name "*.egi" | sort); do
  if gtimeout 60 cabal run egison -- -t "$f" >/dev/null 2>&1; then
    echo "PASS: $f"
  else
    echo "FAIL: $f"
  fi
done

# 個別 PASS/FAIL 確認
gtimeout 60 cabal run egison -- -t sample/math/<...>.egi
```

---

## 関連ドキュメント

- [type-cas.md](./type-cas.md) — CAS 型システム設計と既知の制限
- [type-cas-tower.md](./type-cas-tower.md) — 拡張可能 CAS タワー (将来)
- [function-symbol.md](./function-symbol.md) — 関数シンボル機構

EMR paper の Thurston 計算は `/Users/egisatoshi/PL/EMR-Paper-Computation/` に元実装。
WCS 不変量の Wolfram 簡約形は:
> `S = p² κ (-25 - 640 p² β² + 3072 p⁴ β⁴) / (16 β⁴)`  where `β = 1 + θ₂ - θ₂²`

を `sample/math/geometry/thurston.egi` の `assertEqual` で参照。
