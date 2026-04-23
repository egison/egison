# `type-cas.md` 更新指示書

## 改訂の主旨

現行設計の **3スロット `Poly a [cs] [ss] [fs]`** を廃止し、**1スロット `Poly a [atoms]`** に簡素化する。代わりに、評価後の `CASValue` から最も具体的な型を逆算して報告する **観察型 (observed type) 機構** を追加する。

この改訂により、以下を達成する:

- 静的型システムの大幅な簡素化(CF/S/AF 3分類、disjoint 制約、スロット独立の包含/join/embed がすべて消える)
- 型注釈の構文的軽量化(混在ケースで生 `Poly` を書く頻度が減る)
- 誤注釈による silent な数学的誤りの foot-gun の除去
- インタラクティブな数式計算 workflow の一級サポート

**維持する方針**: `MathValue` 第一級、flat ランタイム Monomial、embed/coerce/join の基本構造、型クラス階層、`declare` 系宣言、簡約規則の3層構造(term/poly/frac)、実行時型昇格タワー。

## 章ごとの更新指示

### `## 概要` および `### 基本原則`(L3-L24)

- 「Poly a [cs] [ss] [fs]」を「Poly a [atoms]」に差し替え
- 基本原則の例示コードを1スロット記法に更新: `Poly Integer [i]`, `Poly (Frac Integer) [x]` 等
- 「`Poly` の3つの原子集合スロット」の記述を「`Poly` の1つの原子集合スロット」に変更
- **新しい段落を追加**: 観察型の導入について。「静的型は最小限に保ち、評価後に得られる `CASValue` から最も具体的な型を逆算して報告する。これをこの設計書では観察型 (observed type) と呼ぶ」と明示的に定義。Success typing (Lindahl & Sagonas, 2006) との関係に1文触れる

### `### 型システムの基盤`(L26-L30)

- 「`Poly` の3つの原子集合スロット」への言及を削除
- 「微分演算子 `∂/∂` が型保存になる十分条件を型レベルで表現する」の段落を削除。代わりに「差分閉性は実行後に観察型として報告される(型レベルでは静的保証しない)」に書き換え

### `### 設計のレイヤー` テーブル(L32-L38)

- 「`Differentiable` 型クラス」の行を更新。静的型保存の主張を削除し、単純に「微分演算の提供」に
- **新しい行を追加**: 「観察型機構 | `typeOf :: CASValue → Type` による評価後の具体型報告」

### `### 宣言環境`(L40-L82)

変更不要。そのまま残す。

### `## 型の構成要素` / `### 組み込み型`テーブル(L88-L104)

- `ConstantFactor`, `AppliedFactor` 行を削除
- `Factor` 行を「原子的な数式要素全体(`Symbol` と `Apply1`/`Quote` ベースの合成原子の和)。`MathValue` の subtype」に単純化
- `Term a [cs] [ss] [fs]` を `Term a [atoms]` に
- `Poly a [cs] [ss] [fs]` を `Poly a [atoms]` に。説明文から「3スロット」と「微分で閉じている性質」を削除
- `Poly a [..] [..] [..]` を `Poly a [..]` に

**Symbol 型の扱いについての注記を追加**: `Symbol` は型として残す(`∂/∂` の第2引数の型制約として機能するため)。ただし `ConstantFactor` / `AppliedFactor` は型としては持たない。理由: 誤注釈の foot-gun を避けるため、かつ `Differentiable MathValue` が always-AF 規則 + Apply1/Symbol のランタイム構成子で振り分けるので型レベル分類は冗長。

### `### 型エイリアス`(L106-L113)

- 1スロット記法に更新:
  ```egison
  type GaussianInt := Poly Integer [i]
  type Zsqrt2      := Poly Integer [sqrt 2]
  type RatFunc     := Frac (Poly Integer [x])
  ```

### `### 糖衣構文`(L115-L146)

**このセクションは全削除**。理由: 1スロット化により `ConstantPoly` / `SymbolPoly` / `AppliedPoly` の糖衣が不要になる。`Poly a [atoms]` 自体が十分軽量で、糖衣を入れる動機がなくなる。

`OpenPoly a := Poly a [..]` のみ、短縮形として残す価値を検討(任意)。

### `### Poly はローラン多項式環`(L148-L168)

- コード例を1スロット記法に更新: `Poly Integer [r]` 等
- 内容は変更なし

### `### グレブナー基底との互換性`(L170-L188)

変更不要。

### `### 閉じた Poly と開いた Poly`(L190-L238)

- 「3スロットそれぞれを独立に閉じる/開くことができる」の記述を削除
- 1スロットでの閉じた/開いたの説明に集約
- **disjoint 制約のサブセクション(L223-L237)を全削除**。1スロット化により disjoint 制約自体が不要
- 例を1スロット記法に更新

### `#### 開いた Poly のランタイム表現と型具体化`(L239-L286)

- 3スロットへの言及を削除し、1スロットでの `[..]` の扱いに書き換え
- `polyAdd :: Poly a [..] -> Poly a [..] -> Poly a [..]` のように更新
- 名前付き型変数の例も1スロット化: `polyAdd :: {atoms} Poly a [atoms] -> Poly a [atoms] -> Poly a [atoms]`
- join の合流例から「cs₁ ∪ cs₂」等を消し、`atoms₁ ∪ atoms₂` のみに

### `#### 型構文の実装`(L288-L308)

- `TPoly Type SymbolSet SymbolSet SymbolSet` を `TPoly Type SymbolSet` に戻す
- 文法を1スロット版に更新:
  ```
  <poly-type> ::= 'Poly' <type-atom> <slot-set>
  ```
- 「3 スロット専用構文」「コレクション型 `[T]` との曖昧性」の議論は1スロット化しても残る(`Poly T [x]` vs `[T]`)ので、この曖昧性回避の議論は維持

### `### 型の構成例`(L310-L328)

- 全例を1スロット記法に更新:
  ```
  Poly Integer [x]                         -- Z[x, x⁻¹]
  Poly Integer [x, y]                      -- Z[x, y, x⁻¹, y⁻¹]
  Poly Integer [i]                         -- Z[i]
  Poly Integer [sqrt 2]                    -- Z[√2]
  Poly Integer [sqrt 2, x]                 -- Z[√2, x, x⁻¹]
  Poly Integer [sin x]                     -- Z[sin x]
  ...
  ```

### `### 入れ子の Poly と多変数の Poly`(L330-L338)

- コード例を1スロット記法に更新
- 「構成的な内部表現 > 表現の対応」への参照リンクは維持

### `## 構成的な内部表現` / `### 表現の対応`(L341-L385)

- 「`Monomial` は型レベルでのみ 3 スロット ... に分かれ」の段落を削除
- 「ランタイムは flat」という記述は維持(1スロットでもそのまま flat)
- 分類判定の境界操作リストから、CF/S/AF 分類への言及を削除。`coerce` は「原子が `atoms` に含まれるか」だけ検証
- `term m` マッチャーの説明を更新: 「1 スロット(flat)で分解」は元から1スロットだったので、ほぼそのまま。「3 スロットへの振り分けはマッチャー側では行わない」の文を削除(もはや3スロットが存在しないため)
- 例の型注釈を1スロット記法に更新

### `### Haskell での表現`(L387-L425)

- `Monomial = [(SymbolExpr, Integer)]` はそのまま維持
- コメントから「型レベルの 3 スロット」への言及を削除
- 「flat に保持することで」の利点リストから、スロット関係の記述を削除。代わりに「1スロット化により、型とランタイムの構造が自然に一致する」を追加

### `### 境界での名前集合照合`(L427-L483)

- タイトルは維持
- 「CF / S / AF 分類」への言及を「`atoms` の照合」に書き換え
- `coerceToClosed` のシグネチャを `[SymbolExpr] -> CASValue -> Either CoerceError CASValue` に変更
- `let union = cs ++ ss ++ fs` を削除
- `Differentiable MathValue` のディスパッチセクションは、always-AF 規則の説明として維持(むしろ1スロット化で必然性が増す)

### `### EgisonValue との統合` / `### SymbolExpr の内部参照` / `### 演算の構成性`(L485-L534)

変更不要(スロット構造に依存しない)。

### `### 正規化規則`(L536-L581)

- 零の正規表現テーブルから3スロット言及を削除
- 全体的に1スロット記法で統一

### `## シンボルと不定元`(L585-L594)

- 「`ss` スロットに入る」を「`atoms` スロットに入る」に修正
- 「合成原子... `declare mathfunc`」の扱いは維持

### `## Factor 型と数学関数` / `### Factor の3分類`(L598-L620)

**このセクション全体を大幅縮小**。以下の方針で書き直す:

- 「3分類」というフレーミングを削除
- `Factor` は `Symbol` と `Apply1`/`Quote` ベースの合成原子の和、という定義だけ残す
- ランタイム構成子(`Symbol _ _ _` vs `Apply1 _ _` vs `Quote _`)で区別し、型としては `Symbol` のみ持つ、と明示
- `AppliedFactor` / `ConstantFactor` という型名は型システムから削除する旨を記す

### `### 3分類の動機` / `### 静的な分類`(L622-L670)

- 両サブセクションを削除
- 代わりに短い節「Factor の扱い」を新設し、「分類はランタイムの `SymbolExpr` コンストラクタで行い、型レベルには現れない。`∂/∂` は `Differentiable MathValue` の always-AF 規則で動作する」と記述

### `### 内部表現`(L669-L687)

- 「3分類は型レベルでのみ区別」を「分類はランタイムの SymbolExpr コンストラクタで識別」に書き換え
- 利点リストを以下に簡素化:
  - `casNormalize` の内部ループで分類判定を行わない
  - `Poly` への embed は Monomial に `(sym, 1)` を追加するだけ
  - ランタイム分類関数を持たない
  - `coerce` は原子が `atoms` に含まれるかだけを検証
  - `Differentiable MathValue` は always-AF 規則で動作

### `### Factor の各分類から Poly への埋め込み`(L689-L700)

**セクション全体を削除**し、短い段落に置き換え:

> Factor 値が `Poly` に埋め込まれるとき、Monomial の末尾に `(sym, 1)` を追加する。型レベルでは `atoms` スロットに原子が加わったことを表明する。

### `### 数学関数の宣言` / `### 数学関数の適用規則`(L702-L790)

変更不要(スロット構造に依存しない)。例中の型注釈は1スロット記法に更新。

### `### Factor の自動昇格`(L792-L806)

- 例の型注釈を1スロット記法に更新(`Poly Integer [sqrt 2]` 等)
- 「ConstantFactor は cs スロットに入る」を「`atoms` スロットに入る」に修正

### `### CASFactor コンストラクタ` / `### sqrt の定義全体像`(L808-L854)

- 型注釈を1スロット記法に更新
- 内容は変更なし

### `## 型の包含関係`(L857-L924)

**このセクション全体を大幅に書き直す**:

- `ConstantFactor ⊂ Factor`, `Symbol ⊂ Factor`, `AppliedFactor ⊂ Factor` の3行 → `Symbol ⊂ Factor` の1行のみに
- 「各分類は対応するスロットに入る形で Poly に埋め込まれる」の4行を削除
- `Factor ⊂ Poly Integer [..]` は維持
- `Term a [atoms] ⊂ Poly a [atoms]`
- **原子集合の包含** サブセクション(L896-L916): 3スロットの独立包含を削除。`Poly a [atoms₁] ⊂ Poly a [atoms₂] when atoms₁ ⊆ atoms₂` と `Poly a [atoms] ⊂ Poly a [..]` のみに簡素化
- 包含の伝播規則はそのまま維持(スロットに依存しない)

### `## 実行時の型昇格タワー`(L928-L1014)

- タワー表(L934-L941)の level 3-5 を1スロット記法に更新: `Poly Integer [..]`, `Poly (Frac Integer) [..]`, `Frac (Poly Integer [..])`
- 「スロット情報との合成」サブセクション(L974-L986)を大幅縮小。「スロットは atoms の和集合で計算」のみに
- 全コード例を1スロット記法に更新

### `## 自動変換の3つの仕組み` / `### 1. embed`(L1016-L1094)

- `Embed` インスタンスから CF/S/AF の3つを削除、`Embed Factor (Poly Integer [..])` のみに
- 「原子集合の拡大」インスタンスを1スロットに簡素化:
  ```egison
  instance {atoms₁ ⊆ atoms₂} Embed (Poly a [atoms₁]) (Poly a [atoms₂])
  ```
- 実装方式の説明はそのまま維持

### `#### 式レベル型注釈` / `##### 注釈粒度の選び方`(L1096-L1131)

- 注釈粒度テーブルを簡素化: `ConstantFactor` / `AppliedFactor` の行を削除。`MathValue` / `Poly Integer [..]` / `Poly Integer [sqrt 2]` / `Term Integer [sqrt 2]` の4段階に
- **新しい段落を追加**: REPL や inspect コマンドでの観察型の表示について。「注釈を省いた場合、REPL は評価後に観察型を表示する。ユーザーはこれをコピーして `def` の注釈にできる」

### `#### coerce`(L1133-L1175)

- ランタイム検証テーブルから CF/S/AF 3行を削除
- `Poly a [..] → Poly a [atoms]` の検証内容に簡素化: 「全項のモノミアルの各原子が、`atoms` の名前集合に含まれるか」
- `Symbol` への coerce は維持(ランタイムコンストラクタで判定可能)
- embed との対称性の段落は維持

### `### 2. join` / `#### join の計算規則`(L1177-L1263)

- 全 join 規則を1スロットに書き直し:
  ```
  join(Poly a [atoms₁], Poly b [atoms₂]) = Poly (join(a, b)) [atoms₁ ∪ atoms₂]
  join(Poly a [atoms], b) = Poly (join(a, b)) [atoms]  -- b が a に embed 可能
  join(Poly a [..], Poly b [..]) = Poly (join(a, b)) [..]
  join(Poly a [atoms], Poly b [..]) = Poly (join(a, b)) [..]
  -- Frac 系は変更なし
  ```
- フォールバックの例、具体例、型推論との統合も1スロット記法に更新

### `### 3. tensorMap`(L1303-L1326)

- コード例を1スロット記法に更新
- 内容は変更なし

### `## 代数的型クラス階層`(L1330-L1406)

- 設計原則セクションで「3スロット」「[cs]`[ss]`[fs]`」への言及をすべて `atoms` に置き換え
- 「閉性は演算ごと・型ごとに独立」サブセクションの例を1スロット記法に更新

### `### 等価性: Eq MathValue と subtype からの fallback`(L1425-L1468)

変更不要。

### `### インスタンスの2つのパターン`(L1470-L1534)

- Ring / GCDDomain のインスタンス例を1スロット記法に更新
- `instance Ring (Poly a [..])`, `instance EuclideanDomain (Poly a [..])` 等

### `#### インスタンス解決メカニズム`(L1536-L1606)

- 単一化の例を1スロットに更新:
  ```
  instance Ring (Poly a [α]) where ...
  ```
- 部分特化の例も1スロット化
- 「具体性の判定」の「原子集合の具体性」はそのまま維持

### `#### 型推論の挙動`(L1608-L1633)

- 全例を1スロット記法に更新
- `Poly Integer [i, x]` のような和集合結果も1スロット化

### `### 微分演算子 ∂/∂`(L1635-L1809)

**このセクションは重要な書き直しが必要**:

- `Differentiable` インスタンスから `ConstantFactor` と `AppliedFactor` を削除
- `Differentiable Symbol` は維持(単純ケース)
- `Differentiable MathValue` を中心に据え、always-AF 規則を第一義的なディスパッチとして記述
- 「微分で閉じる条件」サブセクション(L1755-L1772): 静的型保存の主張を削除し、**観察型で報告される性質**として書き換え:

  > `Poly c [atoms]` の式を `∂/∂` で微分した結果、観察型が入力と同じ `atoms` を持つ場合、その演算は微分で閉じていると言える。観察型機構がこれを自動的に検出・報告する。型レベルでの静的保証は行わない。

- `coerce` で narrow できる例は維持するが、「成功が保証される場合」の説明を「観察型と宣言型が一致すれば coerce が成功する」に書き換え
- 利用例の型注釈をすべて1スロット化

### `### 積分演算子 Sd` / `### substitute と expandAll` / `### type class が不要な演算子`(L1811-L1866)

変更なし。

### `## 簡約規則の宣言`(L1869-L2005)

- 全例の型注釈を1スロット記法に更新
- 「規則適用の型情報フィルタ」(L1966-L1998): `atoms(T) = cs ∪ ss ∪ fs` を `atoms(T) = atoms` に修正
- `[..]` のチェックは「いずれかのスロットに」を「`atoms` が」に単純化
- その他の内容は変更なし

### `## CASValue のパターンマッチ`(L2008-L2094)

- マッチャー定義から `constantFactor`, `appliedFactor` を削除
- `factor`, `symbol` マッチャーのみ残す(`factor` は `symbol`/`apply1`/`quote` の和)
- `poly {a} (m : Matcher a) : Matcher (Poly a ..)` に1スロット化
- `term {a} (m : Matcher a) : Matcher (Term a ..)` も同様
- `:+` パターンの意味論説明を1スロット記法に更新
- 利用例から「symbol/constantFactor/appliedFactor マッチャーでの分類」を削除し、「必要なら `factor` マッチャーの内部で `symbol`/`apply1`/`quote` でパターンマッチ」に
- 「term m で得た flat モノミアルの各要素を分類」は「必要に応じて各要素を `factor` マッチャーで分解」に

### `## Axiom/FriCAS との比較`(L2097-L2118)

- 比較表の「シンボル集合の制御」を「1スロット (`[atoms]`) の閉じた/開いた集合を選択可能。観察型機構が評価後に具体的な原子集合を報告」に書き換え
- 「微分の型保存」を「観察型で事後報告(静的保証なし)」に書き換え
- 「新しい数の導入」から「Factor 3分類」を削除し、「`declare mathfunc`」のみに
- **新しい行を追加**: 「観察型 | なし | 評価後に最具体型を自動報告(Success typing 系列)」

## 新設するセクション

### `## 観察型 (Observed Types)`

新しいトップレベルセクションとして、以下の順序で追加する。場所は `## Axiom/FriCAS との比較` の直前が自然。

#### `### 動機`

インタラクティブな数式計算では、ユーザーは計算を始める前に型を決められないことが多い。静的型注釈を最小にし、評価後の `CASValue` から観察される最具体型を報告することで、型付き CAS のもう一つの運用モデルを提供する。

#### `### 観察型の定義`

観察型 (observed type) とは、評価後の `CASValue` から、以下の手続きで逆算される最も具体的な型である:

- `CASInteger` → `Integer`
- `CASFrac num den` → `Frac (join of typeOf num, typeOf den)`
- `CASPoly terms` → `Poly (join of term coefficients) [collected atoms]`
- `CASFactor (Symbol ...)` → `Symbol`
- `CASFactor (Apply1 ...)` → `Factor`(または `Poly Integer [that apply1]` として昇格)

`typeOf :: CASValue → Type` として実装する。

#### `### 静的型と観察型の関係`

静的型推論で決まる型(典型的には `MathValue` や `Poly a [..]`)と、観察型は多くの場合異なる。前者は可能性の上界、後者は実現した最具体の型。両者のギャップが `coerce` が成功するための情報である。

`τ_static >= τ_observed` が常に成り立ち、`coerce : τ_static → τ_observed` は成功する(定義上)。

#### `### 表示フォーマット`

REPL および `inspect` プリミティブでは、静的型と観察型を両方表示する:

```
> (x + sqrt 2)^2
x^2 + 2*sqrt(2)*x + 2
  static:   MathValue
  observed: Poly Integer [x, sqrt 2]
```

観察型が静的型と一致する場合、観察型のみ表示する:

```
> def p : Poly Integer [x] := x + 1
p : Poly Integer [x]
```

#### `### 型注釈の提案`

REPL は観察型を、ユーザーがコピーして `def` の注釈に使える形式で提示できる。例:

```
> def p := expandAll ((x + sqrt 2)^2)
p = x^2 + 2*sqrt(2)*x + 2
  observed: Poly Integer [x, sqrt 2]
  suggest:  def p : Poly Integer [x, sqrt 2] := expandAll ((x + sqrt 2)^2)
```

これにより、REPL で探索的に書いたコードを、型付きプログラムに昇格させる workflow が自然になる。

#### `### 差分閉性の観察型による報告`

3スロット版で型レベルで保証していた差分閉性は、観察型機構で事後報告される:

```
> def f : Poly Integer [x, y] := x^2 * y + 3 * x * y^2
> ∂/∂ f x
2*x*y + 3*y^2
  observed: Poly Integer [x, y]
  note:     differential-closed (atoms preserved)
```

微分前後で観察型の `atoms` が等しいとき、差分閉性ラベルを付与する。型レベルでの静的保証は行わないが、実用上の確認には十分。

#### `### 既存研究との関係`

観察型の発想は success typing (Lindahl & Sagonas, 2006; Erlang/Dialyzer) と近い。成功した実行から得られる型情報を事後的に報告する点で共通する。CAS の文脈への適用は本設計の貢献。soft typing (Cartwright & Fagan, 1991) や gradual typing (Siek & Taha, 2006) とも関連がある。

#### `### 実装の位置づけ`

観察型の計算は `Math/CAS.hs` の `typeOf :: CASValue → Type` として実装する。実装は再帰的で、embed/coerce 機構の逆方向に対応する。REPL 出力の拡張は Phase 7 の後半または Phase 8 として追加する。

## 実装 TODO セクションの更新

### Phase 1.5

以下に書き換え:

- [ ] `SymbolSet` を1スロット版 `SymbolSetClosed [SymbolExpr] | SymbolSetOpen | SymbolSetVar TyVar` に
- [ ] パーサーで `Poly Integer [sqrt 2, sin x]` のような1スロット注釈をパース
- [ ] `SymbolExpr` の構造的 `Eq` を確認
- [ ] ~~disjoint 制約の検証~~ → **不要**(1スロット化により不要)
- [ ] 名前集合メンバーシップ関数(変更なし)
- [ ] `casPlus`/`casMult`/`casNormalize` は変更不要

### Phase 2

- `TPoly Type SymbolSet SymbolSet SymbolSet` を `TPoly Type SymbolSet` に戻す
- 3スロット化に関する「TODO」ブロックを削除
- `TFactor` は単一型として維持、`TConstantFactor` / `TAppliedFactor` は導入しない

### Phase 5 および 5.5

- `constantFactor`, `appliedFactor` マッチャーを削除。`symbol`, `factor` のみに
- `Embed` インスタンスから `ConstantFactor` / `AppliedFactor` の行を削除
- 3スロット化に関する TODO を削除し、1スロット版として記述

### Phase 6 以降

- 例と TODO の型注釈を1スロット記法に更新

### 新 Phase の追加

Phase 7 の後に以下を追加:

#### `### Phase 8: 観察型機構`

- [ ] `typeOf :: CASValue → Type` を `Math/CAS.hs` に実装
- [ ] 入れ子 `CASValue`(多段の `CASPoly`、`CASFrac` の内部など)の再帰的 typeOf
- [ ] REPL での静的型 + 観察型の表示
- [ ] `inspect` プリミティブの追加
- [ ] 型注釈提案機能(観察型をコピペ可能な注釈として出力)
- [ ] 差分閉性ラベルの付与(`∂/∂` 前後の atoms 比較)
- [ ] mini-test: REPL 出力、`inspect` 呼び出し、各型での typeOf

## 全体に渡る機械的な書き換え

以下は grep/sed で機械的に処理できる変更:

- `Poly a [cs] [ss] [fs]` → `Poly a [atoms]`
- `Poly a [..] [..] [..]` → `Poly a [..]`
- `Poly a [cs_] [ss_] [fs_]`(変数付き)→ `Poly a [atoms_]`
- `[cs] [ss] [fs]` 単独 → `[atoms]`
- `cs ∪ ss ∪ fs` → `atoms`
- `cs ∩ ss ∩ fs` と disjoint 言及 → 削除または注記
- `ConstantFactor`(型として)→ 削除または `Factor` に
- `AppliedFactor`(型として)→ 削除または `Factor` に
- `ConstantPoly`, `SymbolPoly`, `AppliedPoly` → 1スロット記法の直接表現に

ただし、**コードではない説明文脈での言及**(例: 「以前は3スロットだったが1スロットに変更した」のような歴史的記述)は、設計書の readability のために残しても良い。

## 書き換え時の注意点

1. **`Symbol` 型は残す**: 型システムから `Symbol` を消さないこと。`∂/∂` の第2引数の型制約として機能している
2. **`Factor` 型は残す**: `Factor` 型自体は維持する。ただし真部分型としての CF/AF は消える
3. **`Differentiable Symbol` インスタンスは残す**: 単純な `if x == s then 1 else 0` の振る舞いを持つので有用
4. **ランタイム表現は変更なし**: `CASValue` の定義、`Monomial` の定義、`casPlus`/`casMult`/`casNormalize` のロジック、いずれも変更不要
5. **matcher の `factor` 内部**: `symbol`/`apply1`/`quote` のパターンは維持。分類マッチャーのみ削除

## 段階的移行の順序

1枚の巨大コミットではなく、以下の順序で段階的に書き直すのが安全:

1. **観察型セクションを先に新設**: 新しい設計思想の軸を立てる
2. **1スロット化の主要セクション**: 概要、型の構成要素、型の構成例
3. **Factor 関連の縮小**: Factor の3分類セクションの削除
4. **自動変換の3つの仕組み**: embed/coerce/join の1スロット化
5. **型クラス階層**: インスタンス定義の1スロット化
6. **微分演算子**: 観察型による差分閉性報告への書き換え
7. **簡約規則とパターンマッチ**: 1スロット化
8. **Axiom との比較表**: 観察型の追加
9. **実装 TODO**: Phase の整理と Phase 8 の追加
10. **最終チェック**: 残っている 3スロット言及の掃討

各段階で設計書全体の一貫性を確認すること。特に、新設の観察型セクションと既存の embed/coerce/join の関係、および観察型と差分閉性の関係については、複数箇所での記述が整合していることを入念にチェックする。