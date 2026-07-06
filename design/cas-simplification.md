# CAS 高度簡約の設計と実装記録 — グレブナー基底ほか

このドキュメントは、Egison CAS の数式簡約をグレブナー基底などの理論に基づく
本格的な簡約へ拡張する設計と、その実装記録をまとめる。
**フェーズ G0–G6 は 2026-07-06 に全て実装完了**した。
§0 が現状の全体像、§1–§4 が設計 (歴史的な判断根拠を含む)、§5 がフェーズ表と
各フェーズの実施結果、§7 が決定事項 (DG1–DG5) である。

最終更新: 2026-07-06 (G0–G6 完了・API 磨き・G7 構想を反映)

**関連文書**: [type-cas.md](./type-cas.md) (簡約パイプライン・規則機構・タワー)、
[type-cas-tower.md](./type-cas-tower.md) (決定 D1–D5)、
[type-cas-quotient.md](./type-cas-quotient.md) (商機構)、
[type-cas-tower-implementation.md](./type-cas-tower-implementation.md) (実装記録の様式)

---

## 0. 現状の全体像 (2026-07-06 時点)

### 0.1 何ができるようになったか

中核 2 本+周辺一式が実装済み:

- **(A) 多変数多項式 GCD による約分 (G1)** — 正規化パイプライン
  (`casNormalizeFrac`) の第 2 段として自動で発火。
  `(c²r−2GM)X / ((c²r−2GM)Y) → X/Y` のようなパラメータ・適用原子込みの約分が通る。
- **(B) グレブナー基底によるイデアル正規形 (G2/G3)** —
  値レベル (`groebnerBasis` / `polyNF` / `idealNF` / `idealEquals`) と
  宣言レベル (`declare ideal` = 宣言時に一度の完備化 → term 級自動規則の機械生成)。
  手書きだった w の規則は `declare ideal [w^2 + w + 1]` 1 本に置換済みで、
  規則集合の停止性・合流性は定理になった。
- **規則抑制クオート `'( )`** — declare rule 書き換えを発火させずに式を構築する。
  イデアル生成元の構築 footgun の恒久解 (クオート 3 兄弟: `` ` `` = 構造 /
  `'f` = 適用 / `'( )` = 理論の凍結)。
- **sqrt の総合強化** — 負冪の縮約 (|k|≥2、G4 追補)・
  冪/ペア融合規則の Haskell 化 (`casRewriteSqrt`、G6)・
  深さ 2 denesting (`√(9−4√5) → √5−2`、G5)。
- **thurston.egi の完全解決** — 従来「120s 予算で未完・最終 assert は一度も
  不成立」だったものが、**13.5s で完走・WCS 不変量 S の検証込みで全 assert 成立**
  (sqrt 負冪修正 5.2× → G6 で累積 46×; S の閉形式一致は quote 関係式の GB を
  比較点で明示適用して判定)。

性能の代表値 (2026-07-06、~8 倍速マシン):
op-cost プローブ (sqrt 原子 60 演算) 0.47s (G4 時点 12s)・thurston 13.5s・
代表サンプル全て基準以下 (riemann S2 0.58s / Schwarzschild 1.2s / cubic 0.42s)。

### 0.2 実装マップ

| 部品 | 場所 | 検収 |
|---|---|---|
| 多変数 GCD (subresultant PRS) | `hs-src/.../Math/CAS.hs` `multivariateGcdReduce` | test/lib/math/gcd.egi |
| GB エンジン (Buchberger + NF、自由理論・grevlex) | `lib/math/algebra/groebner.egi` | test/lib/math/groebner.egi |
| 診断/糖衣 (`polyNFStatus`/`idealNF`/`idealEquals`) | 同上 | 同上 |
| `declare ideal` (desugar → idealRules.N + autoRule.N) | `hs-src/.../Desugar.hs`、π は `EvalState.declaredSymbolOrder` | test/lib/math/ideal.egi |
| `'( )` クオート | `Desugar.hs` (QuoteSymbolExpr 非変数形 → unNormalizeOps) | test/lib/math/normalize-rules.egi |
| sqrt 冪/ペア融合 | `hs-src/.../Math/Rewrite.hs` `casRewriteSqrt` | 同上 + mini-test 117 |
| denesting | `lib/math/algebra/root.egi` `sqrtDenest` | 同上 |
| trigIdeal ヘルパ | `lib/math/algebra/groebner.egi` | sample groebner-basis.egi |
| 公開ショーケース | `sample/math/algebra/groebner-basis.egi`・`sample/math/geometry/thurston.egi` | サンプル自身の assert |

書籍 (egison-book) にも反映済み: casdetail「イデアルの宣言」節・mexpr「規則抑制
クオート」節・cas 章の GB 実装済み化+denesting 例 (ja/en、全例実測)。

### 0.3 残っているもの

- **G7 (未着手、§3.7 に設計)**: 根号原子の分枝追跡 — 4 項 root-of-unity の
  完全解決 (現状 10 項 → 6 項の部分達成で停止)。
- **G0–G8 全フェーズ+q5 合成まで実装済み** (2026-07-07 完了)。
  `Poly Q [α]` は「タワー係数スロット拡張」ではなく**商の合成**で解決
  (type-cas-quotient.md q5 の結論)。残る据え置きは
  **規則エンジンのさらなる単価削減 (項単位トリガー等) のみ**。
- 規則エンジンのさらなる単価削減 (項単位トリガー等) — ユーザ定義の重い規則の一般問題。
- ※ 2026-07-07 消化済み: exp 構造規則の移植 (G6 追補、係数バグ修正込み)・
  トリガーの頭名絞り込み・thurston の Unbound 警告 (β 前方参照+裸の縮約添字 b、
  警告 10 → 0)。※ 非問題と判明: `sqrt (x⁻⁸)` は既に x⁻⁴ に簡約される
  (以前の観測は 'sqrt クオート形による誤認)・sample 内の手書き規則は存在しない。

### 0.4 主要な設計判断と教訓 (詳細は各節)

- **発火点原則の実証**: quote 関係イデアルの常時適用は破綻する (715s 異常終了)。
  重い関係式は `polyNF`/`idealNF` で比較点に一度だけ (§5 G4 追補)。
- **GB/NF は自由理論で計算し境界で規則世界に戻す** (G2→G3 で最終化)。
  これが declare ideal の自己再入を構成的に不可能にする。
- **自動規則はロードバッチ単位で効く** (再帰束縛)。テストハーネスの
  別バッチバグもこれに起因だった (type-cas.md「重要な性質」参照)。
- **`i.quotient` (0 方向) と `i.modulo` (floor) は負数で不整合ペア** —
  組で使うと値を壊す (sqrt 負冪修正時の教訓)。
- 等価判定は**差 1 回のゼロ判定**で (別々 NF は既定 π が辺ごとに変わる)。

---

## 1. 設計時点の簡約機構と限界 (G0 以前のスナップショット)

> **注**: 本節はプロジェクト開始時 (2026-07-06 朝、G0 以前) の記録であり、
> 以下に挙げる限界の大半は G1–G6 で解消済み。現状は §0 を参照。
> 設計判断の根拠として原文のまま残す。

### 1.1 いまあるもの

| 層 | 機構 | 内容 |
|---|---|---|
| 構造正規化 | `casNormalize` (Math/CAS.hs) | 積和標準形・項の結合・零除去・正準順ソート・モノミアル GCD (`casTermsGcd`) |
| 約分 | `univariateGcdReduce` (同上) | **単変数のみ**の Euclid 互除法 (ℚ 上、次数上限 200)。`(x²−1)/(x−1) → x+1` |
| 書き換え規則 | `declare rule auto` (lib/math/normalize.egi) | 算術演算ごとに `iterateRulesCAS` が不動点まで適用。トリガーフィルタで無関係な計算はスキップ |
| 関数適用時簡約 | `declare apply` | `sqrt 8 → 2√2` などアルゴリズム的簡約 (適用時に一度) |
| 明示簡約 | `simplify e using name` | 名前付き規則の一回適用 |
| 商 | `declare cas-quotient` | per-op reduce (ℤ/nℤ、多項式底の打ち切り等) |

### 1.2 限界の実例

いずれも「多項式・有理式の代数的な簡約が浅い」ことに帰着する:

1. **多変数の約分ができない** —
   `(c²r − 2GM)·X / ((c²r − 2GM)·Y) → X/Y` (Schwarzschild の Christoffel)、
   トーラス T2 の `(b + a cos θ)^k` の次数縮小。
   単変数 GCD のスコープ外で、現在は未約分の `CASFrac` のまま膨らむ。
2. **`thurston.egi` (>180s FAIL、"Mathematica 級簡約必要")** —
   計量が `κ / sqrt β`、`β = \`(1 + θ₂ − θ₂²)` (クオート多項式) を含み、
   曲率計算の中間式が「`sqrt β`・クオート・パラメータ κ を原子とする有理式」として
   約分されずに爆発する。必要なのは (a) 原子を変数とみなす多変数約分と
   (b) `(sqrt β)² = β` の相互作用の下での簡約。
3. **代数拡大の規則が手書き** —
   `i^2 = -1` は 1 本で済むが、1 の原始 n 乗根は
   `w^3 = 1` と `w^2 = -1 - w` のように**複数の規則を人間が正しい向きに**書いており、
   合流性 (どの順で適用しても同じ正規形) は目視保証にすぎない。
   拡大が 2 つ以上絡む場合 (例: `√2` と `√3` から `√6` が湧く系) は書き切れない。
4. **入れ子根号 (denesting)** —
   `5th-root-of-unity.egi` の 4 項版
   `((−1 + √5 + √(−5−2√5) + √(−5+2√5))/4)^5 = 1` が未解決
   (3 項版は multi-factor sqrt 規則で解決済み)。
5. GF(p^k) (係数商 q5) のような**多項式イデアルで定義される代数構造**を
   正規形付きで扱う一般的な方法がない。

スコープ外の隣接課題: `riemann-curvature-tensor-of-S2xS3` の `M.inverse` (5×5 symbolic) は
**線形代数の問題** (LU 分解等) であり本書の対象外。ただし逆行列計算後の式膨張には
(A) の約分が効くので、間接的には貢献する。

---

## 2. スコープ

**目標**:

- (A) 多変数多項式 GCD と、それによる `casNormalizeFrac` の約分の一般化
- (B) グレブナー基底 (Buchberger) とイデアル正規形 (多変数除算) の実装
- (B') GB による**規則集合の自動生成** — シンボル担持商 (`declare rule auto` で表す代数拡大) の完備化
- 三角関数簡約 (`sin θ / cos θ` を**原子のまま** `(sin θ)² + (cos θ)² − 1` のイデアルで扱う — 専用の変数化は不要、§3.4)
- 深さ 2 の平方根 denesting (`√(a + b√c)`)

**非目標** (別線または将来):

- 多項式の完全因数分解 (Zassenhaus / LLL)。GCD と GB は因数分解なしで実装できる
- Risch アルゴリズム (記号積分)。`Sd` の拡張は別書
- 記号行列の高速逆行列 (S2xS3 問題)
- 数値近似・区間演算

---

## 3. 理論の棚卸しと、それぞれが解く問題

### 3.1 多変数多項式 GCD — subresultant PRS

**理論**: 多変数 GCD は主変数を 1 つ選び、残りを係数 (ℤ[他の原子]) とみなした
単変数多項式として Euclid 互除法を回す再帰で計算できる。
素朴な擬除算は係数が指数的に膨らむため、**subresultant PRS**
(primitive part / content の分離と、subresultant による中間係数の抑制) を使うのが定石。
より高速な modular / sparse 法 (Brown, Zippel) は将来の最適化として保留。

**Egison 特有の好条件**: ランタイムの `Monomial = [(SymbolExpr, Integer)]` は
**任意の原子** (シンボル・`'cos θ` のようなシンボリック適用・クオート多項式・関数シンボル)
を一様に変数として扱う flat 表現である。したがって多変数 GCD を実装すれば、
`cos θ` やクオートを含む thurston / T2 型の約分が**追加の仕掛けなしで**対象になる。

**健全性の注意**: 原子が関係式 (`i² = −1` 等) を満たす場合、自由多項式環での共通因子は
商環でも共通因子なので**約分は常に値保存** (健全)。ただし関係式の下でしか成り立たない
約分は見逃す (不完全)。規則適用 (`mathNormalize`) が先に走るパイプライン順序なので、
実用上は「規則で正規形に落ちた後の自由環 GCD」で十分に効く。

**解く問題**: §1.2-1 (Schwarzschild / T2)、§1.2-2 の (a)。

### 3.2 グレブナー基底 — Buchberger と正規形

**理論**: 単項式順序 (lex / grlex / grevlex) を固定すると、イデアル I の GB G に対する
**多変数除算の剰余 (正規形 NF_G) が一意**になる。
Buchberger のアルゴリズムは S-多項式の簡約で G を完備化する。
NF_G は「I を法とする標準代表元」を与える — これが簡約の数学的な意味での正規形である。

- 応用 1: **イデアル所属判定** — `NF_G(f) = 0` ⟺ f ∈ I。ゼロ認識 (「この式は 0 か」) の決定手続き
- 応用 2: **剰余環の演算** — ℚ[x̄]/I での計算は「演算 + NF_G」
- 応用 3: **消去** (lex 順序) — 変数消去・代数的従属関係の発見 (将来: `solve` の基盤)

**ローラン多項式**: 負冪は `r⁻¹ ↦ s, 関係式 rs − 1` で標準環に落として計算し、逆変換する
([type-cas.md §グレブナー基底との互換性](./type-cas.md) に設計済み)。

**解く問題**: §1.2-3・-5、および 3.3・3.4 の基盤。

### 3.3 GB × `declare rule` — 規則集合のオフライン完備化 (本設計の要)

シンボル担持商 (`declare symbol w` + 規則) は数学的には
**ℚ[w]/(最小多項式) のイデアル剰余**である。現在の手書き規則は、実は
「そのイデアルの GB を降冪の書き換え規則として読んだもの」に一致している:

```
イデアル (w² + w + 1) の GB (lex, 1 変数なので自明) = {w² + w + 1}
  → 書き換え規則: w² → −w − 1        (現在の手書き declare rule と同じ内容)
```

そこで次の統合を提案する:

- **宣言時に一度だけ** 生成元から GB を計算し、GB の各元 g を
  「先頭単項式 LT(g) → −(残りの項)」の **term 級 `declare rule auto` に落として登録**する
- オンライン (毎演算) で走るのは従来どおり軽い規則適用 + トリガーフィルタのみ。
  **Buchberger は演算経路に入れない** (発火点は増やさない — tower D2 の議論と同じ原則)

得られる保証:

- **停止性**: GB 由来の書き換えは単項式順序を厳密に下げるので必ず停止する
- **合流性**: GB の定義そのものにより正規形が一意 —
  手書き規則の「向き」や「網羅性」を人間が考える必要がなくなる
- 複数拡大の合成 (√2 と √3 → √6、GF(4) の α² = α + 1 と mod 2 など) も
  生成元を並べるだけで完備化される

インターフェース (DG2 決定 2026-07-06: `declare ideal` を G3 の成果物に**格上げ**する。
理由は 2 つ — (i) 規則の登録はトップレベル宣言行為であり、評価中の関数から
`declare rule` を発行する手段はない (ii) 書き順 π の捕捉は構文が見える場所でしか
できない (§3.5)。実装は `declare cas-quotient` と同じ「素朴なマクロ展開」パターン):

```egison
-- ライブラリ関数 (G2): 第一級の値として GB を計算・利用する
groebnerBasis     : [MathValue] -> [MathValue]
groebnerBasisWith : [MathValue] -> [MathValue] -> [MathValue]  -- 第1引数 = 変数優先順位
polyNF            : [MathValue] -> MathValue -> MathValue      -- 基底 -> 式 -> 正規形

-- 宣言糖衣 (G3): GB を計算し、各元を declare rule auto term に展開する
declare ideal [w^2 + w + 1]
```

**入出力例** (√2・√3 の系; ライブラリ既知の `sqrt` は既存規則が先に効くので、
新しい代数量はユーザーがシンボルで導入するのが実用形):

```egison
declare symbol s2, s3, s6   -- √2, √3, √6 のつもり

groebnerBasis [s2^2 - 2, s3^2 - 3, s6 - s2 * s3]
-- [s2^2 - 2,      s3^2 - 3,        s6^2 - 6,
--  s2 * s3 - s6,  s2 * s6 - 2 * s3,  s3 * s6 - 3 * s2]
```

ユーザーが書いたのは自明な 3 本だが、Buchberger が完備化して
**{1, √2, √3, √6} の乗法表 6 本** (s2·s6 → 2·s3 = 「√2·√6 = 2√3」のような
人間が書き忘れる規則を含む) を返す — §1.2-3 の「書き切れない」問題の解。
変数の優先順位は `declare symbol s2, s3, s6` の宣言順がそのまま π になる (§3.5)。
各元がそのまま term 級規則になる。`polyNF` は正規形とゼロ認識を与える:

```egison
def gb := groebnerBasis [s2^2 - 2, s3^2 - 3, s6 - s2 * s3]

polyNF gb ((s2 + s3)^2)   -- 2 * s6 + 5   ((√2+√3)² = 5 + 2√6)
polyNF gb (s2 * s3 - s6)  -- 0            (イデアル所属判定)
```

なお「積を √6 にまとめる向き」(`s2*s3 → s6`) は grevlex が総次数の低い形を
正規形に選ぶことから変数順位によらず出る (頑健)。

### 3.4 三角関数簡約 — sin θ / cos θ を原子のまま扱う (trigNF は不要)

**2026-07-06 決定 (ユーザ指摘)**: 専用関数 trigNF は設けない。
flat な `Monomial` は `sin θ` / `cos θ` のような適用を**そのまま原子として扱う**
(§3.1 の好条件) ので、教科書式の「s, c への変数化 + 逆写像」は化粧にすぎず不要。
イデアルを `(sin θ)² + (cos θ)² − 1` のまま与えれば GB/NF が直接動く:

```egison
declare symbol θ

-- 常時適用したい場合 (プログラム単位のオプトイン):
declare ideal [(sin θ)^2 + (cos θ)^2 - 1]   -- sin を先に書いた → sin を残す (§3.5)

-- 以下すべて 2026-07-06 実測 (G3 実装後の実挙動):
(sin θ)^4 - (cos θ)^4    -- 2 * 'sin θ^2 - 1   (自動)
(sin θ)^2 + (cos θ)^2    -- 1
(sin θ)^2 + 2 * (cos θ)^2  -- - 'sin θ^2 + 2

-- 一回だけ適用したい場合:
polyNF (trigIdeal θ) ((sin θ)^2 + 2 * (cos θ)^2)   -- 2 - (sin θ)^2
```

- `trigIdeal θ` は角 θ のピタゴラス生成元リストを返すだけの 1 行ヘルパ。
  **生成元を素の式で書くと既存の自動ピタゴラス規則に食われて 0 になる**
  (footgun、2026-07-06 実測: `(sin θ)^2 + (cos θ)^2 - 1` は `0` に評価される)
  ため、**規則抑制クオート `'( )`** (G3 で導入、下記 DG5) の中で組む:

  ```egison
  def trigIdeal (θ: MathValue) : [MathValue] :=
    ['((sin θ)^2 + (cos θ)^2 - 1)]

  trigIdeal θ   -- ['sin θ^2 + 'cos θ^2 - 1]   (実測; 0 に潰れない)
  ```

  (`'( )` 導入前の等価な書き方は非正規化コンストラクタの手書き
  `('sin θ)^'2 +' ('cos θ)^'2 -' 1`。)
  複数の角は `trigIdeal θ ++ trigIdeal φ` と連結すればよい。
  `declare ideal` は構文フォームなので生成元に同じ扱いが暗黙に適用され、この問題と無縁
- sin と cos のどちらを残すかの選択も §3.5 の順序規則に統一される
  (declare ideal の生成元内の出現順)。専用の構文もフラグも増えない
- 多倍角・加法定理は具体原子の関係式として生成元に足せば入る。
  **全角度に効く普遍規則はパターン規則の領分のまま** (GB 生成規則は具体原子に対する
  ground 規則。既存の自動ピタゴラス規則 2 本は保守的な既定として残す)
- 自動適用の是非について: 旧設計が「向きの曖昧さ」を理由に自動化を退けた論点は、
  順序を固定した GB では合流性として解消される。ただし正規形の好みは残るので、
  自動にするかは `declare ideal` を書くかどうかでユーザが選ぶ

**解く問題**: §1.2-2 の一部、thurston 型の三角有理式の膨張。

### 3.5 順序は宣言順から (declaration order) — 各種「選択」の統一原理

GB/NF には「どの変数を優先するか」「sin と cos のどちらを残すか」という
**数学的にはどちらでも正しいが形が変わる選択**が随所にある。
これをオプションフラグではなくユーザーの入力から自然に読む。

**決定 (2026-07-06 ユーザ提案)**: 基準となる順序は **`declare symbol` の宣言順に決め打ち**する。

> π = シンボルが `declare symbol` で宣言された順。**先に宣言されたものほど正規形に残る**
> (実装上は順位の下位に置く — 下記実装注)。
> `declare symbol` に現れない複合原子 (`sin θ` のような適用・クオート) は、
> `declare ideal` の**生成元内の出現順**で π の末尾に拡張する
> (declare は構文フォームなので AST が見える)。
> 値レベル関数 (`groebnerBasis` / `polyNF`) では明示引数 `groebnerBasisWith [atoms] gens`
> で上書きでき、省略時の複合原子は原子名の辞書順で埋める (決定的な fallback)。

当初案は「式ごとの出現順」だったが、**書き順は正規化で消える** (2026-07-06 実測:
`(cos θ)^2 + (sin θ)^2 + sin θ` と `sin θ + (cos θ)^2 + (sin θ)^2` はどちらも
同じ値 `'sin θ + 1` になり、値から書き順は復元できない) ため、
式レベルの捕捉には構文フォームの増設が要った。宣言順への単純化 (ユーザ判断) により:

- π は**大域状態** (`EvalState` に宣言順リストを 1 本足すだけ) となり、
  構文フォームでも値レベル関数でも同じ π が引ける — 二層問題が解消
- ユーザの制御手段は「宣言の順番」という既にある行為そのもの
  (§3.3 の例なら `declare symbol s2, s3, s6` の並びがそのまま π)
- 式ごとに選好を変える粒度は落ちるが、その用途は明示引数版で足りる

**実装注 — 「残す」= 順序の下位**: グレブナー簡約は順序で最大の単項式を消す方向に
書き換えるため、「先に宣言されたものを残す」は「π の先頭を変数順位の**下位**に置く」と
実装する (π = 残したい順のリスト)。

**成立する性質**:

- **健全性は π に依存しない**: どの全順序 π から作った単項式順序でも GB/NF は正しい。
  変わるのは正規形の**かたち**と性能だけ。GCD に至っては結果が (単元倍を除き)
  順序不変なので、この機構は GB/NF の形の選択のみに関わる
- **決定的**: π はプログラムの宣言列の関数なので、同じプログラムは常に同じ結果
- **注意 (仕様として明記)**: 宣言順の違うプログラムは違う正規形を得うる。
  これは欠陥ではなく本機構の目的そのもの (宣言順 = 選好チャンネル)

### 3.6 平方根の denesting (深さ 2)

`√(a + b√c)` は `a² − b²c` が平方数 d² のとき
`√((a+d)/2) ± √((a−d)/2)` に外せる (古典公式; Borodin–Fagin–Hopcroft–Tompa)。
一般の denesting (Landau) はガロア群の計算が要るのでスコープ外とし、
この**深さ 2 の判定 + 変形**のみを `declare apply sqrt` の分岐
(整数の完全平方因子抽出と同じ場所) に追加する。

**G0 (i) の実測 (2026-07-06)**: §1.2-4 の 4 項版について、
積の融合そのものは既に機能する
(`sqrt (-5-2*sqrt 5) * sqrt (-5+2*sqrt 5) → 'sqrt 5` — 既存 sqrt ペア規則の記号引数版)。
しかし 5 乗全体は `'sqrt (-4*'sqrt 5 + 9)` や `'sqrt (2*'sqrt 5 - 5)` のような
**ℚ(√5) 係数の入れ子根号**が 10 項ほど残って崩れない。
このうち `√(9−4√5)` 型は a=9, b=−4, c=5 で a²−b²c = 1 = 1² となり
**本節の深さ 2 denesting がそのまま適用できる** (→ √5 − 2)。
残りの `√(±(5−2√5))` 型は a²−b²c = 5 (非平方) で denesting 不可 —
完全に 1 へ畳むには根号原子どうしの関係式 (t_k² = 中身) を添加した
イデアル計算 (3.2/3.3) と符号 (分枝) の追跡が要る。
G5 (denesting) 単独でどこまで縮むかを G5 の検収で測る。

---

### 3.7 根号原子の関係式イデアルと分枝追跡 (G7 構想 — 2026-07-06 設計検討、未着手)

**問題**: G5 後も 4 項 root-of-unity `z₄⁵` は 6 項の残差で止まる (§3.6)。
残差の根号原子 (`√(±2√5−5)` など) は denesting 不可で、互いの**積の関係**
(たとえば `√(2√5−5)·√(−2√5−5) = ±i·√5`) を使わないと畳めない。
この関係式は t² = 中身 のイデアルからは出ない: イデアルは各 tₖ ↦ −tₖ の
符号反転で不変なので、**分枝 (どちらの平方根か) の情報を原理的に持たない**。
正規形は「符号の 2 群の軌道の代表」までで、主枝の値まで潰すには分枝の追跡が要る。

**設計案 (推奨: 数値分枝証明書つきの関係式添加)**:

1. 式に現れる根号原子 t₁…tₙ を列挙し、ペア積 tᵢtⱼ を既存の融合機構で
   1 つの根号 (または有理数×根号) に畳んだ形と比較する。
2. 両辺の**主枝の数値** (複素区間演算) を評価し、符号 (±1、虚数なら ±i) を決定。
   区間が符号を分離しない場合は精度を上げて再評価。
3. 決定した符号つき関係式 `tᵢtⱼ − σᵢⱼ·(畳んだ形)` を生成元に加え、
   `idealNF` で正規形を取る。z₄⁵ はこの添加で 1 に畳まれる見込み
   (残差 6 項の構造は係数×{1, t, √5t, …} の格子で、ペア関係で全て消える形)。
4. 成果物イメージ: `radicalRelations (e: MathValue) : [MathValue]`
   (符号決定済みのペア関係式を返す) + それを使う明示適用
   (`idealNF (radicalRelations e) e` / assert 用の糖衣)。
   常時適用にはしない (発火点原則)。

**健全性の設計**: 数値による符号決定は「主枝でその関係式が成り立つ」ことの
証明書であり、書き換えの健全性はこの証明書に依存する。第 1 段階は
「設定可能な精度の区間演算 + 分離しなければ精度倍増」の半決定手続きとして
実装し、ドキュメントに明記する。厳密な分離下界 (終結式・高さ経由) の計算は
のちの精密化として据え置く。虚数分枝 (radicand < 0) は既存の i を使って
実部・虚部に分ければ実区間演算に落ちる。

**リスク**: (i) 精度不足による符号誤り → 区間演算の分離を必須にし、
点評価では決して決めない。(ii) 原子数 n に対しペアは O(n²) —
実用対象 (n ≤ 4 程度) では問題にならない。
(iii) 関係式の添加順で GB が肥大する可能性 → 対象を「式に現れた原子」に限定。

**解く問題**: §1.2-4 の完全解決 (`z₄⁵ = 1`)、根号入り式の強いゼロ認識一般。

**G7 実施結果 (2026-07-07 実装完了 — 案 A、設計より簡潔な形で成立)**:

- **意味論の決定 (案 A)**: 実装前の実測で、既存エンジンは「整数 radicand は
  適用時に i を抽出して主枝正しい / 多項式 radicand は形式的融合
  (両負で符号が主枝と逆)」という**非対称な混在意味論**だったことが判明。
  Wolfram の比較 (既定は融合しない・Sqrt[負定数] は I 抽出・Root は
  最小多項式+数値で択一した根番号) を踏まえ、**定数 radicand 全体へ
  i 抽出を拡張して主枝に統一**する形で案 A を実装した。
- **数値符号証明書 = 区間演算ライブラリ** (`lib/math/common/interval.egi` 新設):
  有理数端点の区間 (`iSqrtFloor`・`sqrtInterval`・`constInterval`)、
  精度エスカレーション (k = 16/64/256) つきの `signOfConst`
  ("pos"/"neg"/"unknown"、有理数比較は分子経由 — 実行時 Ord が分数を
  持たないため)。シンボル・quote・i を含む値は即 "unknown" (fail-open)。
- **配線**: `root.egi` の `rt''` n=2 分岐 — 定数 radicand が "neg" 証明されたら
  `i *' sqrt(−x)`。以後**残存する sqrt 原子の radicand は全て正**になり、
  素朴融合 √a·√b = √(ab) が主枝で健全になる (両負ペアの特別扱いが消滅)。
- **z₄⁵ = 1 成立**: 実部は算術だけで 1 に畳まれ、純虚の残差 4 項は
  ペア積関係式のイデアルで消える — **原子が全部正なので関係式に符号選択が
  不要**になり (正の根の積は正)、当初設計の「関係式の符号を証明書で決める」
  段が丸ごと不要になった。証明書の仕事は原子構築時の 1 回だけ:

  ```egison
  idealNF [t₁²−(5+2√5), t₂²−(5−2√5), t₁t₂−√5, (√5)²−5, i²+1] (z₄⁵ − 1)  -- 0
  ```

- **挙動変更**: mini-test 117 の 3 因子積は形式値 +5 → **主枝値 −5** に更新
  (旧値は主枝で誤り)。3 項の z⁵ = z'⁵ = 1 は主枝でも成立し不変。
  roots-of-unity・方程式系・thurston (13.0s)・全サンプル無傷。
- **検収**: mini-test 141 (証明書・i 正規化・z₄⁵) + committed スイート
  (normalize-rules に主枝ケース、groebner に z₄⁵)。### 3.8 GF(p^k) — GB エンジンの係数体パラメータ化 (G8 構想 — 2026-07-07 設計精密化、未着手)

**目標**: GF(p^k) = 係数商 (`declare cas-quotient` の mod p) × シンボル担持商
(α の最小多項式 = `declare ideal`) の合成。GF(4) 等のデモが検収。

**着手時の発見 — reduce だけでは足りない**: 当初案「係数演算を商の reduce で
パラメータ化」は不完全。エンジンは係数の**除算**を 2 箇所で使う:
(i) `makeMonic` の先頭係数割り、(ii) `polyNFStep`/`sPolynomial` の単項式除算
(`u /' lt` の係数比)。𝔽_p では有理数除算は無意味で、**モジュラ逆元**
(Fermat: c^(p−2) mod p、または拡張 Euclid) が要る。

**精密化した設計**: 差し替え点は係数**体演算のレコード** 1 つ:

```egison
-- (正規化 reduce, 除算 divide) のペア。既定 = (id, /')
def groebnerBasisField (fieldOps: (MathValue -> MathValue, MathValue -> MathValue -> MathValue))
                       (atoms: [MathValue]) (gens: [MathValue]) : [MathValue]
```

- エンジン内部 (`polyNFStep`・`sPolynomial`・`makeMonic`・係数正規化) を
  `*Field` 変種に一般化し、既存 API は `(id, /')` の部分適用として再定義
  (挙動不変のリファクタリング)。
- 各演算ステップの出力に reduce を係数ごとに適用 (`mapTerms` 相当の係数写像)。
  除算はレコードの divide を使う。
- cas-quotient 側から `(reduceQ, divideQ)` を取り出して渡す。divideQ は
  商宣言から自動導出できる (p 素数なら Fermat 逆元 — 既存の elliptic-curve
  sample に inv7 の前例)。
- **ガード**: 零因子 (p 非素数の Z/nZ) では体でないため Buchberger の
  除算が破綻し得る — 素数 p (体) に限定し、それ以外は fail-open。
- 検収: GF(4) = quotient Z2 × ideal [α²+α+1] で α の乗法表・
  (α+1)² = α (実測すべき期待値は実装時に確定)、既存 GB テストが
  既定演算で不変であること。

**工数**: 中 (エンジンの体演算一般化リファクタ+商側の演算取り出し+デモ)。

**G8 実施結果 (2026-07-07 実装完了)**: 既定エンジンには触れず、
`Field` 変種一式 (`groebnerBasisField red fdiv atoms gens` /
`polyNFField red fdiv atoms gs f` + 内部の StepF/LoopF/sPolynomialF/
makeMonicF/reduceBasisF) を groebner.egi に追加。係数の除算は
`monoQuotF` (係数は fdiv、指数部は体非依存)、各ステップの出力に
`reduceCoeffs red` を適用。順序・可除性・minimize/sort は既存ヘルパを共有。

- **𝔽₅**: `groebnerBasisField red5 fdiv5 [] [2x²−2] = [x²+4]`
  (monic 化が 2 の逆元 3 を経由、正準代表 mod 5)。x⁴ の NF = 1。
- **GF(4)** (𝔽₂[α]/(α²+α+1)): 基底 [α²+α+1]、
  **(α+1)² = α**・α·(α+1) = 1 (逆元対)・α³ = 1 — 体の構造が全部出る。
- 体演算はユーザーが商宣言に渡すのと同じ関数を渡せばよく
  (red = 商の reduce、fdiv = Fermat 逆元)、`declare cas-quotient` との
  合成は関数の受け渡しで成立する。**係数位置の商を型として持つ
  `Poly Q [α]` 形の統合 (真の q5) はタワー/商機構側の課題として残る**。
- 検収: mini-test 140 + committed スイート (groebner.egi) に GF(4) 追補。
  既定エンジン不変 (`groebnerBasis [2x²−2] = [x²−1]` を明示 assert)。
- **q5 合成の成立 (2026-07-07 追補)**: `finiteFieldReduce p gens` ヘルパを追加し、
  `declare cas-quotient GF4 := MathValue by finiteFieldReduce 2 [α²+α+1]` で
  **GF(p^k) が第一級の型**になった (per-op reduce・Eq dispatch・宣言時合同律
  チェック込み)。base = MathValue なので **x の多項式も型の中** — 係数規律が
  項の内側まで効き、標数 2 の (αx+y)² = α²x²+y² が型付きで成立。
  タワー係数スロット拡張は不要と判明 (詳細 = type-cas-quotient.md q5 の結論)。
  検収 = test/lib/math/quotient-field.egi + mini-test 142。

## 4. アーキテクチャへの写像

### 4.1 置き場所の原則

| 部品 | 置き場所 | 根拠 |
|---|---|---|
| 多変数 GCD (PRS) | **Haskell** (`Math/CAS.hs` の `univariateGcdReduce` の隣、または `Math/Groebner.hs`) | 正規化パイプラインの内側 = ホットパス。単変数版の前例に従う |
| Buchberger + NF | まず **Egison ライブラリ** (`lib/math/algebra/groebner.egi`) でスパイク → 遅ければ **Haskell** (`Math/Groebner.hs`) | 宣言時に一度なら Egison 実装でも許容の可能性。poly/term マッチャーによる Buchberger は本・論文の題材としても価値がある。`casRewriteDd` の前例どおり、計測してから降ろす |
| 規則生成 (3.3) | desugar / EnvBuilder (案 2 の場合)。案 1 ではユーザコード | 既存の `declare rule` 機構をそのまま出口に使う |
| trigIdeal / denest | Egison ライブラリ (`declare apply` と関数) | 明示適用のみでホットパスに乗らない |

### 4.2 内部表現 (Haskell 側)

GB/GCD モジュールは `CASPoly` を直接使わず、**固定した原子リスト上の指数ベクトル**
(`data GPoly = GPoly { atoms :: [SymbolExpr], terms :: [(Coeff, [Int])] }` 程度) に
変換して計算し、結果を `CASPoly` に戻す:

- 単項式順序をモジュール内で自由に選べる (既定 **grevlex**。変数優先順位は
  書き順 π — §3.5 — から取り、消去タスクのみ lex(π)。
  `casNormalize` の正準順とは独立 — 変換境界で吸収)
- 係数は `Frac Integer` (= ℚ)。**パラメータ (κ, G, M, ...) も原子として変数側に含める**
  (DG3)。これで ℚ(params) 係数の実装を先送りできる
- ローラン入力は 3.2 の `rs − 1` 変換で受ける
- 次数・項数・係数桁のガードを付け、超過時は**未簡約のまま返す** (fail-open —
  単変数 GCD の次数上限 200 と同じ流儀)

### 4.3 発火点 — 何を自動にし、何を明示にするか

| 機能 | 発火点 | 理由 |
|---|---|---|
| 多変数 GCD 約分 | **自動**: `casNormalizeFrac` の Poly/Poly 分岐 (単変数版の直後に第 2 段として) | 約分は常に値保存・向きが一意。ガード付きなら安全 |
| GB 由来の書き換え規則 | **自動**: 生成された `declare rule auto` (従来機構) | オンラインは軽い規則適用のみ。停止・合流は GB が保証 |
| GB 計算そのもの | **宣言時 or 明示呼び出しのみ** | 二重指数の最悪計算量を演算経路に入れない |
| `polyNF f gs` / `groebnerBasis gs` | **明示** (ライブラリ関数) | 応用 (ゼロ認識・消去) はユーザ操作 |
| 三角イデアル / denest | **明示** (`declare ideal`・`polyNF (trigIdeal θ)` / `declare apply sqrt` 内の判定) | 方向の選択がユーザ依存 / 適用時 1 回で足りる |

### 4.4 既存機構との接続

- **`casNormalizeFrac`**: 単変数 GCD の「第 2 段 (多変数化)」として設計済みの位置に入れる
  ([type-cas.md §GCD 簡約](./type-cas.md))
- **`declare rule`**: 3.3 の出口。規則のトリガーフィルタは LHS から自動抽出されるので
  生成規則にもそのまま効く
- **`declare cas-quotient`**: 底が多項式環の商の `reduce` に `polyNF` を使えば、
  座標環 ℚ[x̄]/I が per-op reduce で正しく回る (現在ユーザが手書きしている
  打ち切り関数の一般化)。q5 (商係数の Poly) が実装されれば GF(p^k) まで届く
- **観察型・タワー**: 影響なし (簡約は値レベル。型機構は D5 のまま)

---

## 5. 実装フェーズ

| Phase | 内容 | 工数 | 依存 |
|---|---|---|---|
| **G0** | スパイク: (i) ~~4 項 root-of-unity が既存 sqrt ペア規則で落ちるか実測~~ **済 (2026-07-06、§3.6)**: 積融合は機能、ℚ(√5) 係数の入れ子根号が残る — 一部は G5 で denest 可、完全解決は G2+G5 (ii) thurston の膨張点の特定 (どの中間式が何原子の有理式か) (iii) T2/Schwarzschild で「多変数 GCD があれば約分できる」ことを手計算で確認しピン留め mini-test 化 | 半日 | なし |
| **G1** | **多変数 GCD (subresultant PRS)** を Haskell で実装、`casNormalizeFrac` 第 2 段に配線。ガード (次数・項数) 付き — **実施済 (下記)** | 中 | G0 |
| **G2** | **Buchberger + NF** を Egison ライブラリでスパイク (`groebnerBasis` / `polyNF`)。教科書例 (cyclic-3 等) で検収し、性能を計測。遅ければ Haskell (`Math/Groebner.hs`) へ移植し、lib 版は仕様の実行可能ドキュメントとして残す — **実施済 (下記; Egison 実装のままで十分高速、Haskell 移植は不要)** | 中〜大 | G1 (PRS の部品を再利用) |
| **G3** | **規則生成 = `declare ideal`** (DG2 決定): GB → term 級 `declare rule auto` へのマクロ展開 (cas-quotient と同型)。書き順 π は desugar が AST から抽出。w / 1 の n 乗根 / √2×√3 / GF(4) を機械生成規則で置き換えて既存 sample が回ることを確認 — **実施済 (下記; w 規則の置換+三角イデアル自動適用まで確認、規則抑制クオート `'( )` も同時導入)** | 小〜中 | G2 |
| **G4** | **三角イデアル**: `trigIdeal` ヘルパ (非正規化コンストラクタで生成元を組む) + `declare ideal` / `polyNF` の直接適用 (変数化・逆写像は不要 — §3.4)。T2 / thurston での効果測定 — **実施済 (下記; T2 は共存中立、thurston は前提誤認が判明し原因を特定 → G6 へ)** | 小〜中 | G2, G3 |
| **G5** | **denesting (深さ 2)** を `declare apply sqrt` に追加 — **実施済 (下記)** | 小 | なし (G0 の結果次第で不要になる可能性あり) |
| **G6** | **sqrt/exp/rt/rtu/abs term 規則ファミリーの高速化** (G4 分析参照): Haskell 移植 (Rewrite.hs へ戻す; normalize.egi 内 FunctionData 前例と同じ判断) または規則エンジンの per-term no-op 単価削減。thurston は sqrt 負冪修正+明示 polyNF で**成立済 (79.7s)** のため、検収 = sqrt 維持計算の演算単価 (op-cost プローブ 12s → 1s 未満) と thurston のさらなる短縮 — **実施済 (下記; sqrt 2 規則の移植で 0.48s / thurston 13.2s)** | 中 | G4 (分析) |
| **G7** | **根号原子の分枝追跡** (§3.7): ペア積関係式の数値分枝証明書つき添加 + `idealNF` 明示適用。検収 = 4 項 root-of-unity `z₄⁵ = 1` — **実施済 (§3.7 実施結果; 定数 radicand の i 正規化に単純化して成立)** | 中 | G2, G5 |
| **G8** | **GF(p^k)** (§3.8): GB エンジンの係数体演算 (reduce, divide) パラメータ化 + cas-quotient との合成。検収 = GF(4) デモ — **実施済 (§3.8 実施結果; (α+1)²=α 等成立、既定エンジン不変)** | 中 | G2, 商 q1–q4 |

**検収** (フェーズごとに egison/CLAUDE.md の標準検査 + 以下):

- G1: `(c²r−2GM)X/((c²r−2GM)Y) → X/Y` の mini-test、T2 の `(b + a cos θ)^k` 次数縮小、
  Schwarzschild sample の出力簡素化 (実行時間も比較)
- G2: GB 教科書例 + `NF` 一意性 (同じ f を項順を変えて与えても NF が一致) の property 的 mini-test
- G3: `lib/math/normalize.egi` の w 規則を生成版に差し替えても
  `5th/7th/17th-root-of-unity` が全部通る
- G4: thurston が S まで到達する (最終目標; 部分達成でも中間式サイズの縮小を記録)
- G5: 5th-root 4 項版の assertion 追加

### G1 実施結果 (2026-07-06 実装完了)

- **実装**: `Math/CAS.hs` の `multivariateGcdReduce` (`univariateGcdReduce` の直後、
  `casNormalizeFrac` Poly/Poly 分岐の第 2 段として配線)。内部表現は固定原子リスト上の
  **指数ベクトル** (`[(Integer, [Integer])]`、降順 grevlex 維持・同類項マージ)。
  原子順は `show` の辞書順 (宣言順 π への切替は G2 の順序基盤と同時に)。
  content/primitive part 再帰 + **Knuth の除算不要擬剰余** + **subresultant PRS**
  (β = g·h^δ の厳密除算、h 更新は δ>1 で g^δ/h^(δ−1) の厳密除算)。
- **値保存**: 両辺を共通の分母払い係数でスケール → 同一の gcd で厳密除算 →
  共通の整数 content と符号 (分母先頭正) で再スケール。分子と分母は常に同じ倍率。
- **fail-open ガード**: 原子 2〜8 個・項数 ≤200・総次数 ≤60・PRS 中間項数 ≤2000・
  ローラン/非有理係数は即 Nothing。加えて**決定的コプライム事前フィルタ**:
  固定素数点 (2 組) での整数評価の gcd が 1 なら PRS をスキップ
  (共通因子 g は g(pt) | gcd(p(pt), q(pt)) を満たすため。ホットパス保護)。
- **検収**: mini-test **133** — 多変数約分 (`(xy+y²)/(x+y) → y`、差の平方)、
  **パラメータ込み共通因子** (`(c²r−2GM)X/((c²r−2GM)Y) → X/Y`)、
  **適用原子** (`(b+a·cos θ)²/(b+a·cos θ) → b+a·cos θ`)、有理係数、coprime 不変、値保存。
- **回帰**: mini-test 95+1/96 PASS・cabal test PASS・重量サンプル劣化なし
  (Schwarzschild 1.14s / T2 0.97s / hodge-spherical 1.20s / hodge-Minkowski 1.69s /
  polar-laplacian-3d 4s — いずれも従来比同等以下)。
- **thurston**: 予測どおり G1 単独では未解決 (120s 超)。G4 (三角イデアル) 以降の対象。
- **書籍への波及**: `(x²−y²)/(x−y)` が約分されるようになったため、
  egison-book の mexpr 章バッククオート節の動機を「多変数は約分されない」から
  「展開させたくない・因数分解形を保つ」に書き換え、cas 章 §9.3 の
  「グレブナー基底は未実装」の段落を「多項式 GCD は実装済 / イデアル正規形は未実装」に更新 (ja/en)。

### G2 実施結果 (2026-07-06 実装完了 — Egison 実装のまま採用)

- **実装**: `lib/math/algebra/groebner.egi` (約 250 行、既定ロードに追加 — `Egison.hs`)。
  **Egison 自身で** poly/term matcher の上に実装: 原子列挙は
  `poly (term _ (($x, _) :: _) :: _)` の非決定パターン 1 本、S-ペア列挙は join-cons
  (`_ ++ $f :: _ ++ $g :: _`)、簡約 1 ステップは「基底元 g と f の項 u で LT(g) | u
  なるものを非線形パターンで探す」1 つの `matchAll`。書籍・論文の題材品質を意図した構成。
- **API** (§3.3 のとおり + keep-接頭辞semantics):
  `groebnerBasis gens` / `polyNF gs f` は原子名の辞書順を既定 π とし、
  `groebnerBasisWith atoms gens` / `polyNFWith atoms gs f` の**明示リストは
  「残したい原子の接頭辞」** — 残りの原子は名前順で末尾 (=上位=消される側) に自動延長
  (`extendAtoms`)。`polyNFWith [('sin θ)] …` が「sin を残す」と読める。
- **アルゴリズム**: grevlex (優先順ベクトルで先頭差分比較)、素朴 Buchberger
  (criteria なし)、reduced GB (minimize + 相互簡約 + monic + LT 昇順)。
  fail-open ガード: 非多項式 (真の分数)・ローラン (負冪) は入力をそのまま返す。
  NF ループに fuel 10000 (自動規則干渉の安全網)。
- **発見 1 — 自動規則との干渉は「構築時」だけでなく「演算中」にも起こる** (§3.4 の
  footgun の深い版): 簡約ステップを正規化演算 `f - q * g` で組むと、`q * g` の積の中に
  生成元の形 (sin²+cos²−1) がそのまま現れ、既存ピタゴラス自動規則が積を 0 に潰して
  ステップ全体が no-op になる (ライブロック)。G2 当初の対策は「非正規化結合+
  mathNormalize 1 回+進捗フィルタ」だったが、**G3 で最終形に更新: エンジン全体を
  自由理論で走らせる** (全演算を `-'`/`*'`/`/'` に統一、mathNormalize は polyNF の
  出口で 1 回だけ)。規則が一切発火しないので除算は厳密になり、進捗フィルタも不要。
  さらに `declare ideal` の自己再入 (規則リストの遅延計算中に当の規則が発火して
  リストを再帰的に強制する循環) も構成的に不可能になる。
  「GB/NF は自由理論で計算し、境界で規則世界に戻す」が最終的な意味論。
- **発見 2 — 三角の既定 π は組み込み規則と逆向き**: 名前順 fallback は 'cos < 'sin で
  cos 残しだが、組み込みピタゴラス規則は sin 残し。既定 `polyNF (trigIdeal θ)` は
  自動規則の正規形で止まり (正直な不動点)、`polyNFWith [('sin θ)] …` で設計 §3.4 の
  期待どおり動く。G3 の `declare ideal` は生成元内出現順 (sin を先に書く) が π になるので
  自然に整合する — G4 で thurston に当てるときは **sin 残しの向きで**。
- **検収** (mini-test **134** + 公開 sample **`sample/math/algebra/groebner-basis.egi`**):
  乗法表 6 本が §3.3 の設計出力と字面一致 (`s2 s6 - 2 * s3` を含む)、
  `polyNF gb ((s2+s3)²) = 2·s6+5`、所属判定 0、**NF のイデアル平行移動不変**
  (`polyNF gb (f + (s2²−2)(s6+7)) = polyNF gb f`)・冪等、教科書例 **cyclic-3** が
  `[z+y+x, y²+x²+xy, x³−1]` (被約 GB)、keep-接頭辞 (`[y]`→`−y³`)、三角
  (`sin⁴−cos⁴ → 2sin²−1`、`cos⁶ → (1−sin²)³`、所属 0)、ガード
  (空/[5]→[1]/重複/ローラン/分数/単変数 x⁶ mod (x²−2) = 8)。
- **性能**: 乗法表+cyclic-3+三角ぜんぶ込みでファイル全体 1.1〜1.3s (起動込み;
  計算自体は <1s) — **Haskell 移植は不要と判断**。回帰: 代表 7 sample 劣化なし
  (riemann 0.59s)・mini-test 97/97・cabal test PASS (warning ゼロ)。
- **G3 への引き継ぎ**: `declare ideal` マクロは本 lib の `groebnerBasis` を宣言時に呼び、
  各元を term 級 `declare rule auto` に落とす。π は AST の出現順 → `groebnerBasisWith`
  の明示引数に渡す。規則の停止・合流は GB 由来で定理 (§3.3)。

### G3 実施結果 (2026-07-06 実装完了 — `declare ideal` + 規則抑制クオート `'( )`)

- **規則抑制クオート `'( )` (DG5)**: `'(式)` は式を**自由理論の構造演算**
  (`i.+`/`i.*` 系 = declare rule RHS と同じ扱い) で構築する — `declare rule` の
  書き換えは中で発火しない。実装は desugar 1 ケース (`QuoteSymbolExpr` の
  非変数形を `unNormalizeOps` に写すだけ; `'f`・`'(^)` の関数クオートは
  desugar 後 `IVarExpr` かで弁別し無傷)。実測:
  `'((sin θ)^2 + (cos θ)^2 - 1)` は 3 項のまま (素の式は 0)、`'(w^2+w+1)` も保持、
  `'(2*x+x) = 3x`・`'((x+1)^2) = x²+2x+1` (構造正規化は生きる)。
  従来の `'(非変数)` は評価素通しの no-op だったため後方互換の実害なし
  (surface/chern-form/euler-form の既存使用は全て挙動不変を実測)。
  クオート 3 兄弟の物語: `` ` `` = 構造を凍結 (原子化)、`'f` = 適用を凍結、
  `'( )` = 理論 (規則) を凍結。
- **`declare ideal [g₁, …, gₖ]` の実装**: desugar が
  `def idealRules.N := idealTermRules [π] [g₁', …]` (gᵢ' = 生成元の unNormalizeOps 済) +
  `def autoRule.N := \v -> applyIdealRules idealRules.N v` +
  `mathNormalize` 再定義 (declare rule auto と同一機構・トリガー含む) を発行。
  **GB 計算は初回発火時に 1 回だけ** (遅延 define; Buchberger は演算経路に入らない)。
  規則リスト計算は自由理論エンジンなので、**当の autoRule が既に登録済みでも
  自己再入しない** (G2 結果の更新参照)。各 GB 元 g は `(LT(g), LT(g) -' g)` の
  term 級書き換えペアになり、`applyTermRule` (手書き term 規則と同一の
  monomial-containment マッチング・冪分割つき) で適用される。
- **π (DG1 の配管)**: EvalState に `declaredSymbolOrder` を新設し
  `declare symbol` の desugar が CAS 型 (Integer/MathValue) のシンボルを宣言順で記録。
  π = 宣言順シンボル列 ++ **生成元 AST 内の出現順の複合原子** (適用・クオート;
  desugar が IExpr を左→右走査、構造演算子は除外)。値レベル `polyNFWith` 等の
  明示引数と同じ「先頭 = 残す」semantics に接続。
- **検収**: mini-test **135** ('( ) の意味論・後方互換) / **136** (declare ideal) 全 pass。
  ラジカル: 宣言後 `s2*s6 → 2*s3`・`(s2+s3)² → 2s6+5` が**自動**発火。
  **三角: §3.4 の設計例が字面どおり自動で成立** —
  `(sinθ)⁴−(cosθ)⁴ → 2sin²−1`、`(sinθ)²+2(cosθ)² → 2−sin²`、他の角 φ は不変
  (ground 規則)。出現順 π (sin が先 → sin 残し) は組み込みピタゴラス規則と同方向で
  共存も確認。
- **w 規則の置換 (受入基準)**: normalize.egi の手書き 2 本
  (`w^3 = 1` / `w^2 = -1-w`) を `declare ideal [w^2 + w + 1]` 1 本に置換。
  生成される規則 w² → −1−w が w³ を 2 ステップで包摂し、
  5th/7th/17th-root-of-unity・cubic・quartic 全サンプルが同速で通過。
  規則集合の完全性が「慣習」から「定理」になった (§3.3 の狙い)。
- **分数の分母**: 生成規則は分母に届かないが、**手書き term 規則も同じ**
  (実測: `declare rule auto term t^2 = 2` でも `1/(t+1)+1/(t−1)` の分母 t²−1 は
  未簡約; 組み込み i/w 規則も `1/(i+1)` を触らない)。既存システムと完全に一貫。
- **回帰**: 代表 7 sample 劣化なし (riemann 0.58s)・mini-test 全件・cabal test
  (結果は下の標準検査ログ参照)。
- **残り (G3 スコープ外)**: 5th-root 等のサンプル内手書き規則の ideal 化は任意の
  後続作業。GF(4) は係数商 (cas-quotient) との合成が要るため商機構側の課題。

### G4 実施結果 (2026-07-06 測定完了 — T2 は中立共存、thurston は原因特定 → G6)

- **前提の修正 (重要)**: thurston.egi には**三角関数が存在しない** (θ₁–θ₄ は座標
  シンボル、原子は `'sqrt β` と quote 多項式 `` `(1+θ₂−θ₂²) `` 等)。設計時の
  「thurston = 三角有理式の膨張」は誤認だった。G4 の対象を
  **T2 (真の三角原子) の効果測定** と **thurston の膨張点分析 (= 未実施だった G0(ii))**
  に再構成した。
- **T2 実測**: `declare ideal` を θ・φ の 2 本追加しても**全 assert 通過・
  1.04s → 1.06s (中立)**。T2 の結果形は cos の 1 次と quote 原子だけで、既存の
  組み込みピタゴラス poly 規則で十分 — イデアル規則 (lone cos²⁺ の書き換え) の
  出番がない。結論: 三角イデアルは T2 に**無害に共存**でき、lone 偶数冪が現れる
  計算で効く (書籍・论文の位置づけ: opt-in の正規形選択)。
- **thurston 分析 (G0(ii) 完了)**:
  - sqrt 冪は既に完全簡約 (実測: `(√β)^16 → β^8`、`(√β)² − β → 0`、
    `(√β)^-8 → β^-4`) — 「未簡約 sqrt 冪」は原因ではない。
  - 段階計測: Γ 全 64 成分 1.3s (最大 384 字)・R は**ゼロに潰れる成分でも
    1 成分 ~0.34s**・S は 240 置換 × 3 重縮約で組合せ倍加。
    **フル実行は 623.8s (10.4 分) で S まで完走** (rc=124@120s の従来観測は
    単に予算不足だった)。最終 assert は不一致 (下記)。
  - **決定的 A/B (原因の特定)**: 同一 60 演算の fold で
    **sqrt 原子入り 7.02s vs 素シンボル 0.35s (計算部 >100×)**。結果サイズは同規模
    (4067 vs 2302 字) → 壁は**式サイズ爆発ではなく、sqrt 原子を含む値の
    正規化単価**。
  - 帰属の深掘り: sqrt ペア融合規則の無効化で 7.0 → 4.2s (40%)。ところが
    **LHS を 2-cons に絞っても 7.08s と不変** (semantics 保存は sanity 8 assert で確認
    後、利得ゼロのため revert) — コストは本体の再構築ではなく
    **LHS マッチ試行そのもの** (mapTermAll + matcher 機構が sqrt 系 term 規則
    ~4 本 × 全項 × fixpoint 反復 × 全演算で走る)。パターンの書き方では削れない。
  - **結論**: thurston はイデアルでは解けない (必要な関係式は既に発火している)。
    真の対策 = sqrt/exp/rt/rtu/abs term 規則ファミリーの **Haskell 移植**
    (normalize.egi 自身の FunctionData 注記「Egison 級 poly 規則は >120s vs
    Haskell 数秒」と同じ判断) か、規則エンジンの per-term no-op 単価削減
    → **フェーズ G6 に切り出し**。
  - 完走時の S は期待の閉形式 `p²κ(−25−640p²β²+3072p⁴β⁴)/(16β⁴)`
    (正規形では `192p⁶κ − 40β̂⁻²p⁴κ − (25/16)β̂⁻⁴p²κ` の 3 項) に一致せず。
    実測 S には **`('sqrt β̂)` の負の偶数冪 (⁻²/⁻⁴/⁻⁶/⁻⁸) が未簡約で大量に残る**。
    単体では `(√β)⁻⁸ → β⁻⁴` と潰れる (冪演算経路) のに S で残る理由 =
    **lib の sqrt 冪簡約規則の条件が k ≥ 2 (正冪のみ)** で、分数正規化が
    単項分母を Laurent 冪に折り込む経路で生まれた負冪が守備範囲外に落ちるため。
    → **最有力の小修正: 規則本体を |k| ≥ 2 に拡張** (i.quotient/i.modulo は
    負冪でも値保存: (√a)⁻⁸ → a⁻⁴、(√a)⁻³ → a⁻¹·(√a)⁻¹)。
  - S − 期待値 の意味論的ゼロ判定 (434.9s): **False — ただし不決定**。
    判定自体が同じ正規化ギャップの影響下にあり、S 側の sqrt 負偶数冪項は
    期待値側に相殺相手がないため、数学的にゼロでも字面ゼロに到達できない。
    `'sqrt (β^-8)` (負冪 Laurent を引数にした sqrt 適用) が簡約されない
    同族ギャップも確認 — declare apply sqrt の負冪radicand対応は小課題として記録。

### G4 追補: sqrt 負冪規則の修正 (2026-07-06 実装、効果 5.2×)

- **修正**: normalize.egi の sqrt 冪簡約規則の発火条件を k ≥ 2 → **|k| ≥ 2** に拡張。
  余りは `n − 2·(i.quotient n 2)` で直接計算する (∈ {−1,0,1}、冪演算経路の
  既存正規形 `(√a)⁻³ = (√a)⁻¹·a⁻¹` と一致)。
  ⚠ **実装注意: `i.quotient` (0 方向切り捨て) と `i.modulo` (floor) は負数で
  整合しないペア** — `2·quotient(−3,2) + modulo(−3,2) = −1 ≠ −3`。
  負冪で quotient と modulo を組で使ってはならない (値破壊)。
- **効果 (thurston)**: **623.8s → 119.2s (5.2 倍)**。負冪が早期に潰れて
  sqrt 因子が消えるため、以降の演算ではトリガーフィルタが sqrt 規則ごと
  スキップする副次効果 (G4 本文の「per-op 単価」問題の大部分がこの 1 点だった)。
  **S から `'sqrt` が完全に消滅** — 残差は β̂ (quote) と θ₂ の Laurent 混在項のみ。
- **assert は依然不成立**だが残差の形から次の機構が特定できた:
  期待の 3 項形 (θ₂ を含まない) への到達には **quote 関係式
  `β̂ = 1 + θ₂ − θ₂²` の適用**が必要 (GB 規則 θ₂² → 1+θ₂−β̂ で θ₂ 高次冪が
  {1, θ₂} × ℚ(β̂) 基底に畳まれ、係数相殺が合流的に起こる)。
- **常時適用 (`declare ideal` を計算全体に効かせる) は破綻 — 明示適用の原則の実証**:
  thurston 冒頭で `declare ideal [1 + θ₂ - θ₂^2 - β]` を宣言した変種は
  **715s で異常終了** (user 229s vs real 715s = GC/ページングのスラッシング —
  メモリ爆発)。ミニプローブでも同一 60 演算が 11.9s → **>300s (25 倍超)**。
  原因: 生成規則のトリガー (θ₂) がほぼ全ての値にマッチし、全項で規則マッチが
  走る上に、θ₂² → 3 項の書き換えが中間式を系統的に膨張させる。
  §4 の発火点原則 (「重い簡約は演算経路に入れない」) が quote 関係イデアルにも
  そのまま当てはまる — **正しい形は値レベルの明示適用**。
- **最終結果 (2026-07-06): thurston 完全成立**。最終 assert を
  「分母を払った差の GB 正規形ゼロ判定」に書き換え:

  ```egison
  def quoteGb := groebnerBasis ['(1 + θ₂ - θ₂^2 - β), '(1 + θ₂ - `(1 + θ₂))]
  assertEqual "WCS invariant S" (polyNF quoteGb (16 * β^8 * (S - sExpected))) 0
  ```

  完備化 GB は `[θ₂ − q̂ + 1, q̂² − 3q̂ + β̂ + 1]` (q̂ = `(θ₂+1))。
  クロスチェックとして `expandAll S = expandAll sExpected` (quote 展開 →
  G1 の多項式 GCD で約分) も True。**sample 全体が 79.7s・rc=0・失敗ゼロ**
  (従来: 120s 予算では未完、933s 予算でも assert 不成立)。
  timings は 200 → 80 に更新。
- **ハーネスの教訓 2 件** (デバッグで判明、要 doc 周知):
  (i) **`polyNF` は与えられた生成元を完備化しない** (設計どおり: gs が GB のとき
  一意正規形)。生の生成元では非合流 — `polyNF (groebnerBasis gens) f` が正しい
  イディオム (q̂³ が簡約されない反例で確認)。
  (ii) **両辺を別々に polyNF すると既定 π が辺ごとに異なり得る**
  (原子集合が違うため) — 等価判定は**差のゼロ判定** 1 呼び出しで行う。
- **回帰**: mini-test **100/100** (新設 137 = 負冪ケース)・cabal test PASS・
  sqrt 重サンプル (riemann/quadratic/cubic/5th-root/eulers) 劣化なし。
- **trigIdeal を lib へ** (`lib/math/algebra/groebner.egi`、`'( )` 版)。
  sample/groebner-basis.egi は lib 参照に変更。ローカル再定義 (shadowing) との
  共存も確認。
- **回帰**: mini-test 99/99・cabal test PASS・代表 7 sample 劣化なし (riemann 0.60s)。

### G6 実施結果 (2026-07-06 実装完了 — sqrt 2 規則の Haskell 移植)

- **移植対象の絞り込み**: トリガーフィルタにより sqrt-only 値で実際に走るのは
  sqrt 冪規則と sqrt ペア融合規則の **2 本だけ** (rt/rtu/exp/abs は trigger 不一致で
  既にスキップ) — ファミリー全部ではなくこの 2 本を `Math/Rewrite.hs` の
  `casRewriteSqrt` に移植し、lib の該当 declare rule 2 本を削除。
- **実装**: `casRewriteSymbol = casRewriteDd . casRewriteSqrt`。
  **項レベル fast-path** が本体 — 「|冪|≥2 の sqrt 因子」か「冪 1 の sqrt 因子 2 個以上」を
  持つ項だけ書き換え、**単独 (√a)^±1 の項 (曲率計算の大半) は再構築すら
  しない** (lib 規則では不可能だった省略)。冪縮約は q=quot n 2・r=n−2q (負冪込み)。
  ペア融合は「全 radicand を casMult で融合 → 単項なら平方部を抽出
  (整数係数は試し割り・上限 10⁶ で fail-open、指数は div/mod 2)」の単一経路 —
  旧 lib の整数 gcd 経路と同値なことは契約テストで確認
  (`√2·√8→4`、`√6·√10→2√15`、`√(2x)·√(2y)→2√(xy)`、`√x·√(xy²)→xy`、
  多項式積は `√(x+1)·√(x−1)→√(x²−1)` のまま)。
  負の radicand 冪が多項式 radicand で真の分数を作るため、結合は
  casPlus/casDivide 経由 (生の CASPoly 再構築ではなく)。
- **契約**: 移植前に 14 ケースの show 出力をピン留めし、
  移植後 (Haskell 単独) で **diff ゼロ**を確認。
- **入れ子 radicand の後追い対応 (mini-test 117・test/lib/math/algebra.egi が検出)**:
  旧 lib 規則は mapTermAll 経由で **Apply 引数・quote の中まで再帰**し、
  さらに applyRuleFix の**不動点反復**と「lib sqrt 呼び出しによる content 再分離」の
  往復で入れ子根号 (5 乗根の radical 形など) を畳んでいた。移植版も
  (i) 因子引数・quote 内容・level-4 係数への再帰、
  (ii) 融合 radicand への再帰適用、
  (iii) **多項 radicand の整数 content 分離** (平方部は外へ、残りは独立の √int 原子 —
  `√5·√(−10√5−50) = 5·√(−2√5−10)` の正準化)、
  (iv) **等価判定つき不動点駆動** (fuel 100; content 分離ペアは同形再生成 → 等価で停止)
  を実装して一致。`z⁵ = 1` (z = 5 乗根の radical 形)・3 因子積など全ケース回復、
  op-cost は 0.47s のまま。
- **効果**: op-cost プローブ **12.01s → 0.48s (25 倍、検収 <1s クリア)** —
  素シンボル比 1.5 倍まで縮小 (G4 時点は >100 倍)。
  **thurston 79.7s → 13.2s** (sqrt 負冪修正からの累積では **623.8s → 13.2s = 47 倍**)、
  全 assert 通過。サンプル横断でも劣化なし
  (cubic 0.59→0.43s と改善、Schwarzschild 1.15s、hodge-Minkowski 1.69s、
  polar-3d 3.69s、groebner-basis 1.20s)。
- **規則数**: 組み込み declare rule 18 → **16** (mini-test 83/84/88/92/96 の期待値更新)。
### G6 追補: exp 構造規則の移植とトリガー精度 (2026-07-06/07)

- **exp 移植 (casRewriteExp)**: `(exp a)^n → exp(n·a)` と多因子積の融合
  (全 exp 因子を 1 原子に合流、総和 0 は 1 に) を Haskell へ。
  値規則 (exp 0 / exp 1 / exp(n·i·π)) は lib に残す。
  契約 11 ケースで検収 — うち 1 件で**旧ペア規則の係数消失バグを発見**:
  `2·exp x·y·exp y` が `y·e^{x+y}` になっていた (数値検証で誤り確定、
  正しくは `2y·e^{x+y}`)。移植版は係数を保存 (= バグ修正、契約を更新)。
- **効果の正直な評価**: op-cost (exp 版) は 20.7s → 14.6s (**~30%**)。
  sqrt の 25 倍が出ない理由も特定 — **exp の代数は融合のたびに新しい原子を
  生む** (exp(2x), exp(3x), …)。fold 中に相異なる原子が増殖し、多項式演算
  そのものが重くなる (規則を全部切った床が 13.1s)。sqrt は逆に原子が
  radicand の量に潰れて消えるため規則コストが支配的だった。
  つまり exp の残りコストは規則層ではなく値の代数に由来する。
- **トリガー精度の改善 (規則エンジン一般)**: 適用パターンのトリガーを
  **頭の関数名のみ**に絞った (`extractTriggerSymbols` / `exprNames` の
  引数不降下)。`log (exp $n)` のトリガーが {log, exp} → {log} になり、
  exp 値で log 規則が試行されなくなる。健全性: パターンにマッチする項は
  必ず頭の因子を含む (トリガー集合は小さいほどスキップが増える)。
  これは残課題「規則エンジン一般の per-term 単価」への部分的な回答でもある。
- 組み込み規則 16 → **14**。回帰: 13 sample 全て基準以下 (eulers 0.82→0.70s)・
  thurston 13.3s・mini-test 102/102・cabal PASS。committed スイートに exp
  ケース追補 (係数バグの回帰テスト込み)。

- **残り (このフェーズのスコープ外)**: 規則エンジン自体のさらなる単価削減
  (項単位トリガー等) はユーザ定義規則の一般問題として別課題。

### G5 実施結果 (2026-07-06 実装完了 — 深さ 2 denesting)

- **実装**: `root.egi` の `rt''` の n=2 分岐を `sqrtDenest` に差し替え。
  適用経路は content 分離済みの原始 poly radicand を受けるので、
  「整数 a>0, b≠0, c>0 の `a + b√c`」形を poly/term パターンで判定し、
  `a² − b²c = d²` (完全平方; `pF` で判定、r ≤ 10¹² の fail-open ガード) のとき
  `√((a+d)/2) + sign(b)·√((a−d)/2)` に開く (古典 BFHT 条件)。
  半分が有理数になる場合も再帰的な sqrt 適用が処理 (`√(2+√3) = (√6+√2)/2`)。
- **検収** (mini-test **138** + 委員会スイート normalize-rules.egi に追補):
  `√(9−4√5) = √5−2`・`√(7+4√3) = 2+√3`・`√(3+2√2) = 1+√2`・`√(11−6√2) = 3−√2`・
  有理半・**非対象形は不変** (`√(5−2√5)`、`√(2+√2)`)・平方の復元 (値保存)。
- **4 項 root-of-unity (§1.2-4) の再測定**: `z₄⁵` は G0(i) 時点の ~10 項から
  **6 項に縮小 (部分達成)**。残差の `√(±2√5−5)` は a²−b²c = 5 (非平方) で
  denesting 対象外 — §3.6 の予測どおり、完全に 1 へ畳むには根号原子の関係式を
  添加したイデアル計算+分枝 (符号) 追跡が必要 (将来課題として据え置き)。
- **注意 (設計メモ)**: denesting は**適用時のみ** (`declare apply sqrt` 経路)。
  Haskell 側 casRewriteSqrt の融合が直接生成する原子には適用されない
  (明示適用の原則どおりホットパスに乗せない)。
- **回帰**: 10 sample 基準どおり (riemann 0.62s / Schwarzschild 1.19s / polar-3d 3.80s)・
  thurston 13.9s 成立・mini-test **101/101**・cabal test PASS
  (denesting の追補を含む committed スイートで)。

### 事後の API 磨き (2026-07-06 — thurston デバッグの教訓の還元)

- **polyNFStatus / polyNFStatusWith**: `("ok" | "fail-open" | "fuel", 正規形)` を返す
  診断つき variant。polyNF が入力をそのまま返す 2 つのサイレントケース
  (多項式片の外 = fail-open / fuel 枯渇) を真の非ゼロ正規形と区別できる。
  polyNF(With) は同 variant の snd に再定義 (挙動不変)。
- **idealNF / idealNFWith**: `polyNF (groebnerBasis gens) f` の一発形。
  「polyNF は完備化しない」footgun の安全な入口。
- **idealEquals**: 差 1 回のゼロ判定によるイデアル法等価。別々に NF すると
  既定 π が辺ごとに変わる問題を封じる。入力は多項式限定 (分数・ローランは
  fail-open → False) と明記。
- 検収: mini-test 139 + committed スイート (groebner.egi) 追補。

**これで G0–G6 全フェーズ完了** (2026-07-06)。据え置きの将来課題:
根号原子の関係式イデアル+分枝追跡 (4 項 root-of-unity の完全解決 — §3.7 に設計検討)、
exp ペア規則の Haskell 移植 (必要になったら)、GF(p^k) (商機構 q5 との合成)。

---

## 6. リスク

| 項目 | 内容 | 対処 |
|---|---|---|
| 係数膨張 | PRS/Buchberger の中間係数が桁爆発 | subresultant + content 分離、整数化 (分母払い)、係数桁ガードで fail-open |
| 次数・項数爆発 | GB は最悪二重指数 | 実用対象 (曲率計算の分母、拡大の最小多項式) は小さい。ガード + 明示適用の原則で演算経路を守る |
| 順序依存の正規形 | GB/NF は単項式順序で形が変わる | 既定 grevlex に固定し観察可能な仕様として文書化。消去は lex を明示指定 |
| 規則生成と手書き規則の共存 | 同じ拡大に両方あると冗長適用 | 生成規則で手書きを**置き換える** (G3 の検収)。トリガーフィルタで実害は小さい |
| 自由環 GCD の不完全性 | 関係式下でのみ成り立つ約分は見逃す | 仕様として明記 (健全性優先)。必要なら「規則正規形後に GCD」の順序で実用十分 |
| thurston が G1+G4 でも落ちない | sqrt β の平方 (β) との相互作用が GCD/GB の外にある | `(sqrt β)² → β` は既存規則が処理済み。残る場合は β をクオートでなく生多項式で持つ運用も検討 (G0 で特定) |

---

## 7. 決定事項 DG1–DG4 (未決ゼロ)

2026-07-06 の議論で 4 点すべて決定 (DG4 は論点ごと解消)。実装に着手可能。

| # | 論点 | 決定 |
|---|---|---|
| DG1 | 単項式順序の既定と指定方法 | **決定 (2026-07-06)**: 順序型は grevlex 固定 (消去のみ lex)。**変数優先順位は `declare symbol` の宣言順に決め打ち** (§3.5; 先に宣言=残る=順位下位)。複合原子は declare ideal の生成元内出現順で拡張、値レベルは明示引数 or 原子名辞書順 fallback |
| DG2 | `declare ideal` 構文の要否 | **決定 (2026-07-06 ユーザ承認)**: `declare ideal` を G3 の成果物に格上げ。規則登録はトップレベル宣言でしかできず、書き順 π の捕捉にも構文が必要なため (§3.3)。エンジン (G2) はライブラリ関数でスパイクする点は従来どおり |
| DG3 | パラメータ係数の扱い | **決定 (2026-07-06 ユーザ承認)**: 原子=変数として一様に扱う (実装ゼロ追加; GCD 主導の約分はこれで足りる)。ℚ(params) 係数 (再帰 GCD、先頭係数の可逆化) は取り逃し例が出てから — その際は**生成点意味論** (分母非零の側条件は追跡しない) を仕様に明記する |
| DG5 | 生成元保護の構文 | **決定 (2026-07-06 ユーザ提案・承認)**: 規則抑制クオート `'( )` を導入 — `'(式)` の中では `declare rule` 書き換えが発火しない (構造正規化はする)。`.` 等の新記号案は縮約 `(.)`・名前空間・float と衝突するため `'` の拡張を採用 (従来の `'(非変数)` は no-op で後方互換)。trigIdeal・`polyNF` の生成元・`declare rule` RHS の可読形として機能 |
| DG4 | trigNF の正規形の向き | **解消 (2026-07-06 ユーザ指摘)**: trigNF 自体を廃止 — sin θ / cos θ は原子のまま `declare ideal` / `polyNF` で直接扱う (§3.4)。向きは §3.5 の順序規則 (生成元内の出現順) に統一され、専用構文の要否という論点ごと消えた |

---

## 8. 関連文書・参考

- 実装様式: [type-cas-tower-implementation.md](./type-cas-tower-implementation.md)
  (フェーズ + 実施結果の対で記録する)
- 既存の簡約アーキテクチャ: [type-cas.md §実装状況 Phase 7](./type-cas.md)
- 理論: Cox–Little–O'Shea *Ideals, Varieties, and Algorithms* (Buchberger / 順序 / 消去)、
  Knuth TAOCP vol.2 & Geddes–Czapor–Labahn *Algorithms for Computer Algebra* (subresultant PRS)、
  Borodin–Fagin–Hopcroft–Tompa (sqrt denesting)
