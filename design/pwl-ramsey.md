# Ramsey R(3,3) = 6 の証明：パターンマッチ指向スタイルの比較

## 定理

K₆（6頂点の完全グラフ）の辺を赤・青の2色で塗ると、必ず単色三角形が存在する。

---

## 基本定義

### Lean 4 での定義

```lean
inductive Color | red | blue

def monochromatic (edge : Sym2 (Fin 6) → Color) (x y z : Fin 6) : Prop :=
  ∃ c, edge ⟦(x,y)⟧ = c ∧ edge ⟦(y,z)⟧ = c ∧ edge ⟦(x,z)⟧ = c

theorem ramsey_3_3_6 (edge : Sym2 (Fin 6) → Color) :
    ∃ (x y z : Fin 6), monochromatic edge x y z
```

### パターンマッチ指向での定義

```egison
inductive Color | red | blue

theorem ramsey_3_3_6 (edge : Sym2 (Fin 6) → Color)
    matches ($x, $y) -> $c :: (#y, $z) -> #c :: (#z, #x) -> #c :: _
    as multiset (Sym2 (Fin 6) -> Color)
```

`matches` は「このパターンが任意の `edge` に対して必ずマッチする」という主張。
定理の証明は、このパターンが網羅的であることを示すことに対応する。

この記法により以下が吸収される：
- **`monochromatic` の定義**: パターン自体が「単色三角形」を表現
- **`∃ (x y z : Fin 6)`**: パターン変数 `$x, $y, $z` に吸収
- **`∃ c`**: パターン変数 `$c` と非線形パターン `#c` に吸収

`as multiset (Sym2 (Fin 6) -> Color)` はマッチャーの指定であり、
関数 `edge` を入出力ペアの multiset として扱うことを示す。
`Sym2 (Fin 6)` は順序なしペアなので、`($x, $y)` は順序を問わずマッチする。

---

## A. Lean 4 / Mathlib スタイル

```lean
def same_color_neighbors (edge : Sym2 (Fin 6) → Color) (v : Fin 6) (c : Color) :
    Finset (Fin 6) :=
  Finset.univ.filter (fun w => w ≠ v ∧ edge ⟦(v, w)⟧ = c)

theorem ramsey_3_3_6 (edge : Sym2 (Fin 6) → Color) :
    ∃ (x y z : Fin 6), monochromatic edge x y z := by
  let v : Fin 6 := 0
  -- 鳩の巣原理（色を存在量化で受ける）
  have h_pigeonhole : ∃ c, (same_color_neighbors edge v c).card ≥ 3 := by
    by_contra h
    push_neg at h
    have hr := h .red
    have hb := h .blue
    have h_total : (same_color_neighbors edge v .red).card
                 + (same_color_neighbors edge v .blue).card = 5 := by
      ...
    omega
  obtain ⟨c, hc⟩ := h_pigeonhole
  -- S から 3 頂点を取り出す
  let S := same_color_neighbors edge v c
  obtain ⟨x, hx, y, hy, z, hz, hxy, hxz, hyz⟩ :=
    Finset.exists_three_le_card S hc
  -- v-x, v-y, v-z は全て色 c
  have edge_vx : edge ⟦(v, x)⟧ = c := by
    exact (Finset.mem_filter.mp hx).2.2
  have edge_vy : edge ⟦(v, y)⟧ = c := by
    exact (Finset.mem_filter.mp hy).2.2
  have edge_vz : edge ⟦(v, z)⟧ = c := by
    exact (Finset.mem_filter.mp hz).2.2
  -- x-y の色で場合分け
  by_cases hxy_color : edge ⟦(x, y)⟧ = c
  · exact ⟨v, x, y, ⟨c, edge_vx, hxy_color, edge_vy⟩⟩
  · -- y-z の色で場合分け
    by_cases hyz_color : edge ⟦(y, z)⟧ = c
    · exact ⟨v, y, z, ⟨c, edge_vy, hyz_color, edge_vz⟩⟩
    · -- x-z の色で場合分け
      by_cases hxz_color : edge ⟦(x, z)⟧ = c
      · exact ⟨v, x, z, ⟨c, edge_vx, hxz_color, edge_vz⟩⟩
      · -- 全て c でない → 反対色を導出
        have hxy_c' : edge ⟦(x, y)⟧ = opposite c := by
          cases edge ⟦(x, y)⟧ <;> cases c <;> simp_all
        have hyz_c' : edge ⟦(y, z)⟧ = opposite c := by
          cases edge ⟦(y, z)⟧ <;> cases c <;> simp_all
        have hxz_c' : edge ⟦(x, z)⟧ = opposite c := by
          cases edge ⟦(x, z)⟧ <;> cases c <;> simp_all
        exact ⟨x, y, z, ⟨opposite c, hxy_c', hyz_c', hxz_c'⟩⟩
```

### 特徴

- 約40行
- `obtain` によるデータの分解: 2箇所
- `have` による辺の色の証明: 3箇所（`Finset.mem_filter` の分解）
- `by_cases` の3段入れ子
- 最深ケースで反対色を導出する `have` が3箇所

---

## B. パターンマッチ指向スタイル

### 補助補題

```egison
-- 鳩の巣原理: v からの 5 辺を 2 色で塗ると、同色 3 辺以上が存在する
-- 主定理の外側マッチのワイルドカード節の背理法に使用
lemma pigeonhole_edges (edge : Sym2 (Fin 6) → Color) (v : Fin 6)
    matches (#v, $x) -> $c :: (#v, $y) -> #c :: (#v, $z) -> #c :: _
    as multiset (Sym2 (Fin 6) -> Color) := by
  -- v の次数は 5（K₆ で自己ループなし）
  -- 5 辺を 2 色に分けるので、鳩の巣原理から ⌈5/2⌉ = 3
  ...

-- 2色の3値の網羅性:
-- 3つの Color 値に対し、以下の4パターンのいずれかが必ずマッチする
-- 主定理の内側マッチのワイルドカード節の背理法に使用
lemma two_color_exhaustive (c : Color) ((x, y, z) : (Color, Color, Color))
    matches (#c, _, _) | (_, #c, _) | (_, _, #c) | ($c', #c', #c') := by
  cases x <;> cases y <;> cases z <;> cases c <;> simp_all
  -- 2⁴ = 16 通りの全数検査で自動証明
```

`pigeonhole_edges` は主定理と同じ `matches ... as multiset` 形式。
`∃ c` やカーディナリティ `≥ 3` がパターン変数と `::` に吸収される。

`two_color_exhaustive` は `|`（パターンの選択肢）を使った `matches`。
ターゲット `(a, b, d)` が4つのパターンのいずれかに必ずマッチすることを主張する。
主定理や `pigeonhole_edges` ではターゲットが引数 `edge` なので暗黙だが、
ここではターゲットが構成された式 `(a, b, d)` なので明示が必要。
Color は代数的データ型なので `as` は不要。

### 証明

```egison
theorem ramsey_3_3_6 (edge : Sym2 (Fin 6) → Color)
    matches ($x, $y) -> $c :: (#y, $z) -> #c :: (#z, #x) -> #c :: _
    as multiset (Sym2 (Fin 6) -> Color) := by

  -- ★ 外側のパターンマッチ: edge から同色 3 辺を取り出す
  match edge as multiset (Sym2 (Fin 6) -> Color)
    with
  | ($v, $x) -> $c ⇒ edge_vx
    :: (#v, $y) -> #c ⇒ edge_vy
    :: (#v, $z) -> #c ⇒ edge_vz
    :: _ =>

    -- ★ 内側のパターンマッチ: 残り 3 辺の色で場合分け
    --
    -- 各ケースでは、パターンマッチで束縛された値を返すだけでよい。
    -- 返された束縛が定理の matches パターンを満たすことは、
    -- 外側・内側のマッチの ⇒ で得られた証明項から自動的に補完される。
    -- 例: 最初のケースでは edge_vx, edge_vy, hxy_c から
    --   ($x, $y) -> $c :: (#y, $z) -> #c :: (#z, #x) -> #c :: _
    --   の各制約（同色性）が充足されることが導かれる。
    match edge as multiset (Sym2 (Fin 6) -> Color)
    | (#x, #y) -> #c ⇒ hxy_c =>
        exact ⟨v, x, c, y⟩

    | (#y, #z) -> #c ⇒ hyz_c =>
        exact ⟨v, y, c, z⟩

    | (#x, #z) -> #c ⇒ hxz_c =>
        exact ⟨v, x, c, z⟩

    | (#x, #y) -> $c' :: (#y, #z) -> #c' :: (#x, #z) -> #c' :: _ =>
        exact ⟨x, y, c', z⟩

    | _ => by
        -- 網羅性: 上の 4 ケースにマッチしないと仮定して背理法。
        -- two_color_exhaustive により 4 ケースは全可能性を尽くすので矛盾。
        exfalso
        have h := two_color_exhaustive c (edge ⟦(x,y)⟧) (edge ⟦(y,z)⟧) (edge ⟦(x,z)⟧)
        simp_all

  | _ => by
      -- 網羅性: 同色 3 辺が取り出せないと仮定して背理法。
      -- 鳩の巣原理（色を存在量化で受ける。ここは Lean 4 版と同じ）
      have h_pigeonhole := pigeonhole_edges edge v
      obtain ⟨c, hc⟩ := h_pigeonhole
      -- hc より同色 3 辺が存在するので矛盾。
      exfalso
      exact absurd hc (by omega)
```

### 特徴

- 約30行（証明本体）+ 補助補題2つ
- `matches ... as multiset` により定理文自体がパターン（`monochromatic` 定義・`∃` 量化が不要）
- `obtain` / `have`: 0箇所（全て multiset マッチと `⇒` に吸収）
- `by_cases`: なし（フラット4ケースのマッチ）
- 反対色の導出が不要（最後のケースの multiset マッチが直接捕捉）
- 各ケースではパターンマッチの束縛を返すだけでよく、
  定理の `matches` パターンを満たすことは `⇒` の証明項から自動補完される
- `with` と網羅性の役割が分離：
  - `with`: マッチャーの使用が正当であることの証明（multiset マッチャーのみ）
  - 網羅性: 最後のワイルドカード節にマッチしないことを背理法で証明

---

## C. 比較

### 量的比較

| | Lean 4 | パターンマッチ指向 |
|---|---|---|
| 定理の主張 | `∃ (x y z), monochromatic edge x y z` | `matches ($x, $y) -> $c :: ...` |
| 補助定義 | `monochromatic` が必要 | 不要（パターンが定義） |
| 存在量化 | 明示的に `∃` | パターン変数に吸収 |
| 行数（証明本体） | 約40行 | 約30行 |
| 補助補題 | なし | 2つ（鳩の巣原理、2色網羅性） |
| `obtain`（3頂点の取り出し） | 1箇所 | 0箇所（multiset `::` に吸収） |
| `have`（辺の色の証明） | 3箇所 | 0箇所（`⇒` に吸収） |
| 辺の色の場合分け | `by_cases` 3段入れ子 | フラット4ケース |
| 反対色の導出 | `have` 3箇所 + `cases ... <;> simp_all` | multiset マッチで自動 |
| マッチャー正当性の証明 | 不要（標準パターンマッチのみ） | 1箇所（`with` で multiset マッチャー） |
| 網羅性の証明 | 不要（`by_cases` は構造的に網羅的） | 2箇所（ワイルドカード節で背理法） |

### 補助補題を含めた総量

Lean 4 版は `by_cases` が構造的に網羅的なので補助補題が不要だが、
証明本体が長い（約40行）。さらに `monochromatic` の定義が別途必要。

パターンマッチ指向版は `monochromatic` の定義が不要で証明本体も短い（約30行）が、
網羅性の背理法証明のために補助補題 `two_color_exhaustive` が必要。
ただしこれは `decide` で閉じる1行の補題である。
`pigeonhole_edges` は Lean 4 版でも `h_pigeonhole` として
実質同じ内容を証明している（形式が異なるだけ）。

### 差を生む4つの要因

パターンマッチに起因する差は以下の4点に集約される。

#### 1. `matches` による定理文のパターン化

Lean 4 では定理の主張に補助定義 `monochromatic` と存在量化 `∃` が必要：

```lean
theorem ramsey_3_3_6 (edge : Sym2 (Fin 6) → Color) :
    ∃ (x y z : Fin 6), monochromatic edge x y z
```

パターンマッチ指向版では、定理の主張がパターンそのもの：

```egison
theorem ramsey_3_3_6 (edge : Sym2 (Fin 6) → Color)
    matches ($x, $y) -> $c :: (#y, $z) -> #c :: (#z, #x) -> #c :: _
    as multiset (Sym2 (Fin 6) -> Color)
```

`matches` は「任意の `edge` に対してこのパターンが必ずマッチする」という主張であり、
定理の証明はこのパターンの網羅性を示すことに対応する。
`as multiset (Sym2 (Fin 6) -> Color)` により、関数 `edge` を
入出力ペアの multiset として扱う。

これにより `monochromatic` の定義が不要になり、
「単色三角形が存在する」という主張がパターンとして直接読める。
パターン変数 `$x, $y, $z, $c` が存在量化を、
非線形パターン `#c` が「同色」という条件を吸収している。

#### 2. multiset マッチ + `⇒` による取り出しと証明の同時取得

Lean 4 では、同色3頂点の取り出しと辺の色の証明が分離している：

```lean
-- ステップ1: 3頂点を取り出す
obtain ⟨x, hx, y, hy, z, hz, ...⟩ := Finset.exists_three_le_card S hc
-- ステップ2: 各辺の色を証明する（Finset.mem_filter を分解）
have edge_vx : edge ⟦(v, x)⟧ = c := (Finset.mem_filter.mp hx).2.2
have edge_vy : edge ⟦(v, y)⟧ = c := (Finset.mem_filter.mp hy).2.2
have edge_vz : edge ⟦(v, z)⟧ = c := (Finset.mem_filter.mp hz).2.2
```

パターンマッチ指向版では、これが1つのパターンに統合される：

```egison
| (⟦(#v, $x)⟧, $c) ⇒ edge_vx
  :: (⟦(#v, $y)⟧, #c) ⇒ edge_vy
  :: (⟦(#v, $z)⟧, #c) ⇒ edge_vz
  :: _ =>
```

グラフそのものを直接パターンマッチすることで、
`(⟦(#v, $x)⟧, $c)` にマッチした時点で `edge ⟦(v,x)⟧ = c` が
マッチャーの意味論から直接得られる。

Lean 4 では `obtain` で取り出した `hx : x ∈ S` から
`edge ⟦(v,x)⟧ = c` を導くために `Finset.mem_filter` の分解が必要であり、
これは `same_color_neighbors` の定義に依存する推論である。
パターンマッチ指向版では、グラフを直接マッチすることで
この中間データ構造への依存を回避している。

これは2020年論文の「パターン後の項束縛」の仕組みの適用であり、
同論文の `!(#a :: _) ⇒ ha : ¬a ∈ l₂` と同じ原理で動く。

#### 3. 非線形パターンによるフラットな場合分け

Lean 4 では、3辺の色それぞれについて順番に `by_cases` する：

```lean
by_cases hxy_color : edge ⟦(x, y)⟧ = c
· ...
· by_cases hyz_color : edge ⟦(y, z)⟧ = c
  · ...
  · by_cases hxz_color : edge ⟦(x, z)⟧ = c
    · ...
    · have hxy_c' : edge ⟦(x, y)⟧ = opposite c := by ...
      have hyz_c' : edge ⟦(y, z)⟧ = opposite c := by ...
      have hxz_c' : edge ⟦(x, z)⟧ = opposite c := by ...
      ...
```

3段入れ子になり、最深ケースでは「c でない → 反対色」の導出に
`have` 3箇所 + `cases ... <;> simp_all` が必要。

パターンマッチ指向版では4ケースがフラットに並ぶ：

```egison
| (#c, _, _) ⇒ hxy_c =>         ...
| (_, #c, _) ⇒ hyz_c =>         ...
| (_, _, #c) ⇒ hxz_c =>         ...
| ($c', #c', #c') ⇒ hxy_c', hyz_c', hxz_c' =>  ...
| _ => by exfalso; ...
```

最後のケース `($c', #c', #c')` は「3辺が全て同色だが c ではない」を
1つのパターンで表現し、同時にその色 `c'` と3つの等式の証明を束縛する。
`opposite c` の導出も `cases ... <;> simp_all` も不要。

#### 4. `with` とワイルドカードによる証明責務の分離

2020年論文の設計では、パターンマッチに伴う証明責務が2つに分離される：

**マッチャーの正当性（`with`）**: そのマッチャーを使ってパターンマッチして
良いことの証明。外側のマッチでは、`graph` を multiset として分解・再構成
しても元の命題が保存されることを `with multiset_of_edges graph` で証明する。
2020年論文の `with perm.trans h₂ h₁.symm : l₁ ~ l₂` と同じ役割である。
内側のマッチでは Color は代数的データ型なので `with` は不要。

**網羅性（ワイルドカード + 背理法）**: 最後のマッチ節をワイルドカードにし、
その節にマッチしないことを背理法で証明する。
外側のマッチでは `hc`（鳩の巣原理）から矛盾を導き、
内側のマッチでは `two_color_exhaustive`（2色3値の全数検査）から矛盾を導く。

Lean 4 の `by_cases` は「命題 P か ¬P か」という構造的に網羅的な分岐なので
両方の証明責務が不要であり、この点はトレードオフである。
一方、パターンマッチ指向版では `with` と網羅性が明示的に分離されることで、
「なぜこのマッチャーが使えるのか」と「なぜこのケース分けで十分なのか」が
証明中に独立した根拠として記録される。

---

## D. 2020年論文からの発展

2020年論文では素因数分解の一意性の証明において：
- `⇒ ha : ¬a ∈ l₂`（not パターンによる項束縛）が1箇所使われた
- multiset マッチでリストの要素を順序不問に取り出した
- `with perm.trans h₂ h₁.symm : l₁ ~ l₂` でマッチャーの正当性を証明した
- 網羅性は自動化により省略された（条件を満たす/満たさないの2分岐のみ）

本証明では以下の発展が見られる：

### 1. `matches` による定理文のパターン化

2020年論文では定理の主張は従来通り命題として記述し、
証明中にパターンマッチを使うという構成だった。
本証明では `matches ... as multiset (Sym2 (Fin 6) -> Color)` により、
定理の主張そのものをパターンとして表現する。

これにより補助定義（`monochromatic`）と存在量化（`∃`）がパターンに吸収され、
「何を証明するのか」がパターンとして直接読める定理文になる。
定理の証明は「このパターンが任意の入力に対してマッチする」ことを示す
網羅性の証明に対応する。

### 2. `with` とワイルドカード網羅性の分離

2020年論文では `with` にマッチャーの正当性証明を書くという設計が示されたが、
網羅性の証明は自動化により省略されていた。
本証明では網羅性の自動化が困難なケースが現れるため、
ワイルドカード節による背理法という明示的な手法を採用する。

これにより `with` と網羅性の役割が完全に分離される：
- `with`: マッチャーの使用が正当であることの証明
- ワイルドカード節: 全ケースが尽くされていることの背理法による証明

### 3. `⇒` による項束縛の大規模な活用

2020年論文では `⇒` が1箇所（`ha : ¬a ∈ l₂`）だったが、
本証明では6箇所（外側3つの `edge_vx`, `edge_vy`, `edge_vz` +
内側3つの `hxy_c` 等）で使用され、`have` を完全に代替している。

### 4. 非線形パターンによる暗黙的等式の活用

非線形パターンによる等式が5箇所（`#v` × 3 + `#c` × 2）で使用される。
特に内側の `($c', #c', #c')` は、変数束縛と等式制約を同時に表現し、
Lean 4 では `have` 3箇所 + `cases ... <;> simp_all` を要する
反対色の導出を1パターンで置き換えている。

### 5. グラフの直接パターンマッチ

「グラフそのものを直接パターンマッチする」というアプローチは、
中間データ構造（`same_color_neighbors` 等の `Finset`）を経由すると
`⇒` で得られる証明項に定義の展開が必要になるという問題を回避する。
これは2020年論文では論じられていなかった設計上の知見である。

### 6. 応用による網羅性証明の多様性

網羅性の証明が応用によって異なる種類になる：
- 外側: 鳩の巣原理（組合せ論的な議論）
- 内側: 有限型の全数検査（`decide` で閉じる）
- 2020年論文: 置換の推移性のみ（1種類）

このことは、網羅性証明を完全に自動化する設計よりも、
ユーザーが明示的に証明を与える設計の方が汎用性が高いことを示唆している。