# Egison CAS 商型機構 (構想)

このドキュメントは、係数領域の商 (ℤ/nℤ、打ち切り羃級数 ℤ[x]/(xⁿ) など) を CAS 型システムで扱うための、**型昇格タワーから独立した機構**の構想をまとめる。2026-07-04、[type-cas-tower.md](./type-cas-tower.md) §8 D4 の判断 (商型はタワーの枠組みとは違う枠組みで実現すべき) を受けて分離。実装は未着手。

最終更新: 2026-07-04 (初版)

---

## 1. なぜタワーから分離するか

タワー ([type-cas-tower.md](./type-cas-tower.md)) の意味論は「**単一の大域理論を共有する部分環の、単射埋め込みによる束**」であり、全機構がこの前提に依存する:

| タワーの概念 | 前提 | 商で破れる点 |
|---|---|---|
| `embed` / `⊂` | 値保存 ⟦embed v⟧ = ⟦v⟧ (mono) | 射影は epi: `8 ↦ 3` で値そのものが変わる |
| `Eq MathValue` 一本 | 等価性は ambient 領域から一様に継承 | 商の等価性は**型依存**: x ~ y ⟺ reduce(x−y) = 0。未簡約の中間値 12 と 2 は mod 5 で等しいが `x − y = 0` 判定では不等。`#x` 非線形パターンの意味論的照合も誤る |
| `join` / 保守性 | 上界 = 共通の包含先 | ℤ と ℤ/5ℤ に共通の包含先はない (混在は型エラーにすべき) |
| 観察型 | 表現から型を読める | `CASInteger 3` が ℤ か ℤ/5ℤ かは表現から決まらない |

原則: **タワー型に「追加の等式」を課すことは、生成イデアルによる商にほかならない**。理論が変わる場所は商であり、タワー (理論を共有する部分環の束) ではなくこの機構で扱う。

## 2. すでに言語にある商: シンボル担持商

ℤ[i] = ℤ[X]/(X²+1)、ℤ[ε]/(ε⁵) (双対数/ジェット)、1 の冪根 ℤ[ζ]/(Φₙ(ζ)) は現行の

```egison
declare symbol i
declare rule auto term i^2 = -1
```

で健全に扱えている。これらは**商をシンボルが担う**形であり、シンボルの大域一意性がスコープの役割を果たすため、大域規則でも他の型に漏れない (trigger filter が原子で局所化する)。

本機構が対象にするのは、**シンボルが商を担えないケース = 係数領域の商**のみ: 表現が既存型 (`CASInteger` / `CASPoly`) とそのまま重なり、関係式を担う専用原子が存在しないもの。

## 3. 宣言形 (仮)

```egison
declare cas-quotient Zmod5 := Integer by (\n -> modulo n 5)
declare cas-quotient Series5 := Poly Integer [x] by (truncateAbove 5)
```

構成要素:

- **base 型** `T` — タワー内の任意の型
- **reduce : T → T** — 冪等な代表元選択関数 (射影)。宣言と同時に登録
- 宣言された型は**新しい nominal 型**として導入される (base の透明エイリアスではない)。表現が base と同一なので透明では unsound — 例: `Zmod5` が `Integer` の透明エイリアスだと `x ^ (a : Zmod5)` のように mod 値を `Integer` 文脈 (冪指数) へ流用できてしまう (3 ≡ 8 (mod 5) だが x³ ≠ x⁸)。nominal 性は本機構に内在し、タワー側 cas-type (透明エイリアスのみ、tower.md §8 D3) とは独立

## 4. 順序との関係: 参加しない

- `⊂` 辺は張らない。`MathValue` との間にも暗黙の関係を作らない (表現上は `CASValue` を共有するが、**意味領域が異なる**)
- `join` に参加しない → `(3 : Zmod5) + (4 : Integer)` は**型エラー** (タワー版旧設計の「`MathValue` に退化」より強い防壁)
- 横断は明示関数のみ:
  - `proj : T → Q` — reduce して包む (`coerce` の商版)
  - `repr : Q → T` — 代表元の取り出し (表現は保つが商の意味は失う)

## 5. 演算と等価性

**準同型演算は自動導出**: reduce が環準同型の核による射影である限り、`+` / `*` / `neg` / `^` は商の普遍性により base へ委譲できる:

```egison
instance Ring Zmod5 where
  (+) a b := proj (repr a + repr b)   -- base の演算 + 出口で reduce
  (*) a b := proj (repr a * repr b)
  ...
```

**演算ごとに reduce が走る**ため代表元は常に正規に保たれ、タワー内で扱った場合に問題だった表示乖離 (`inspect (a*b)` が未簡約の 12 を返す) や中間肥大 (`(x+1)^100` の係数が C(100,50) ≈ 10²⁹ に膨れる、Series5 の全次数展開) は構造的に生じない。tower.md 旧 D2 の「発火点」ジレンマは商側ではこれで解消される。

**準同型でない演算は商上に直接実装** (type-cas.md のパターン2): `inv` (mod 5 の `inv 2 = 3` は ℤ に存在しない)、`gcd`、大小比較、整除など。base への委譲を機械的に適用してはならない。

**等価性は型 dispatch**:

```egison
instance Eq Zmod5 where
  (==) a b := reduce (repr a - repr b) == 0
```

`Eq MathValue` への sibling fallback を**禁止**する (型依存の等価性)。matcher の `#x` 照合や rule エンジンが商値に触れる経路では、この instance に dispatch できることが実装上の要件。

## 6. 法則 (合同性)

lifted 演算が同値類上 well-defined であるための条件。宣言時 PBT (tower.md §8 D5 と同じ機構) で検査可能:

- 冪等: `reduce (reduce x) = reduce x`
- 合同 (∘ = `+`, `*`): `reduce (x ∘ y) = reduce (reduce x ∘ reduce y)`
- 符号: `reduce (- x) = reduce (- (reduce x))`

## 7. 合成: GF(p^k)

係数商とシンボル担持商の合成で有限体が表現できる:

```egison
declare cas-quotient Zmod2 := Integer by (\n -> modulo n 2)
declare symbol α
declare rule auto term α^2 = α + 1    -- GF(4) の既約多項式 (シンボル担持)
-- Poly Zmod2 [α] = GF(4)
```

(係数型スロット `Poly a atoms` の `a` に cas-quotient 型を許す拡張が必要 — 下表 q5)

## 8. 実装フェーズ (タワー α–δ とは独立)

| 要素 | 内容 | 依存 |
|---|---|---|
| q1 | nominal 型導入機構 (本機構専用 — タワー側 cas-type は透明エイリアスのみのため共用しない) | なし (tower α と独立) |
| q2 | `declare cas-quotient` 構文 + `DeclareEnv` `"quotient"` 登録 | q1 |
| q3 | 準同型 instance (Ring 等) の自動導出 + `Eq` の型 dispatch | q2 |
| q4 | PBT による合同律検査 | q2 |
| q5 | `Poly (商型) atoms` の係数拡張 (GF(p^k) 用、optional) | q3 |

## 9. 論文上の位置づけ

これで CAS 型システムは **3 つの直交機構**に整理される:

| 機構 | 対象 | メタ定理 |
|---|---|---|
| タワー (単射埋め込み + canonical form 選択) | 部分環の束、表現の選択 | coherence・保守性 |
| シンボル担持商 (`declare symbol` + 大域規則) | 原子が関係式を担う代数拡大 | 合流性・停止性の基準 |
| 係数商 (`declare cas-quotient` + reduce) | 表現を共有する商 | 合同律 ⟹ well-definedness (普遍性) |

## 11. 実装ノート (2026-07-04、q1–q4 実装完了)

実装 (mini-test/129 が検収) は本構想を**マクロ展開**として実現した — 「理論をできるだけ単純に」を実装にも適用した形:

- **展開**: `declare cas-quotient Q := T by f` は評価パイプライン冒頭 (`Eval.expandCasQuotientDecls`、環境構築より前) で通常の TopExpr 列に展開される: `def reduceQ := f` (ユーザ AST を直接接ぎ木)、`projQ`/`reprQ` (型シグネチャ付き def)、準同型 instance 群 (`Eq`/`AddSemigroup`/…/`Ring`、本体は `projQ (reprQ a ∘' reprQ b)`)、q4 の合同律 assertion 3 本 (冪等・`+'` 合同・`*'` 合同、整数サンプルバッテリ)。生成コードはテンプレート文字列を既存パーサで再パースするだけの**素朴なマクロ**
- **q1 (nominal) の実現**: 新しい Type コンストラクタは足さず、Q を `Q → TInductive Q []` として **cas-type alias 環境に登録**する。全注釈 seam が alias 展開で名前を不透明型に写すため、同名同士しか単一化せず、`declare cas-subtype` は「CAS 型ではない」として拒否 (D4 の順序不参加が自動で成立)。重複宣言・alias/inductive との衝突も alias 環境の検査に相乗り
- **横断は明示** (§4 の open question を確定): `projQ : MathValue → Q` / `reprQ : Q → MathValue`。注釈による暗黙 proj は導入せず将来の糖衣候補。static な橋渡しは専用の unsafe cast primitive `casQuotientCast : ∀ a b. a → b` (生成コード専用、ユーザ非公開) 1 個
- **混在演算**: instance 不一致で dispatch 不成立。gradual typing では Warning + 未解決シンボリック値として**可視的に**失敗し、strict モードでは型エラー。§4 の「型エラーにする」は strict モードで満たされる
- q5 (`Poly Q atoms` の係数拡張) は未実装のまま (optional)

## 10. 改訂履歴

- 2026-07-04: 初版。tower.md §8 D4 の分離判断を受けて切り出し。
- 2026-07-04 (2): q1 の nominal をタワー D3 から独立化 (タワー側 cas-type は透明エイリアスのみの方針を受けて)。§3 に型混同を静的に弾く例 (`x ^ (a : Zmod5)`) を tower.md 旧 D3 から移設。
- 2026-07-04 (3): **q1–q4 実装完了** — §11 実装ノートを追加 (マクロ展開方式、nominal は alias 環境登録、横断は明示 projQ/reprQ で確定、casQuotientCast primitive)。
