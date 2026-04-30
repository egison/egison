# Runtime-Type Dispatch (型と shape 統合の type-class 拡張)

## 0. 目的

CAS の型クラスにおいて、**static / runtime のいずれの状況でも O(1) で dispatch** し、かつユーザーは specific 型に対する instance だけを書けばよい仕組みを設計する。

### スコープ: 最大ネスト深さ 2 に限定

任意ネスト深さの型 (例: `Frac (Poly Integer [x, y])`) はサポート**しない**。許す型は:

- 単純型: `Integer`, `Factor`, `MathValue`
- 1 レベル parametric 型: `Term a`, `Frac a`, `Poly a [..]`、ただし `a` は単純型または型変数

つまり instance 宣言の最大ネスト深さは 2 (`Frac Integer`、`Poly (Frac Integer) [..]` 等)。

理由:
- **実用ケースの 90%+ は depth ≤ 2 で十分** (商の微分、多項式の微分、整数 vs 有理係数の特殊化など)
- depth を制限すると `runtimeType` が **O(1)** になる (値全体を走査せずに済む)
- specificity ordering の比較も定数時間で可能
- 設計と実装が大幅に簡素化

### 複雑度

- **runtimeType の計算**: **O(1)** — 値の outer constructor + 1 レベル下のみ参照
- **instance lookup**: **O(N²)** (N = 候補 instance 数、partial order の最大元探索)。N は通常 5〜10 の小定数
- **method 呼出**: **O(1)** (dictionary 経由)

総合: 実質定数時間。現状の `match`-base O(N_patterns) 線形探索を置き換える。

### 将来拡張

3 レベル以上のネスト型に対応が必要になったら、以下の選択肢:
- (a) `runtimeType` を再帰版に拡張 (= 元設計、O(値サイズ))
- (b) instance 側で内側を `match` で分解
- (c) 値構築時の型タグキャッシュで O(1) を維持

詳細は §7 を参照。

---

## 1. 現状と問題点

### 1.1 現状の dispatch 構造

```
                 partialDiff (TC method)
                       │
        ┌──────────────┼──────────────┐
        ▼              ▼              ▼
   Frac inst      Poly inst      Term inst       ← O(1) compile-time dispatch
   (static 型が specific のとき)
                       │
                       ▼ (型が MathValue のとき)
              MathValue inst
              ┌─────────────────────────────┐
              │ match f as mathValue with    │ ← O(N) runtime linear shape match
              │   | poly $ts -> ...          │
              │   | $p1 / $p2 -> ...         │
              │   | ...                       │
              └─────────────────────────────┘
```

### 1.2 問題

1. **O(N) 線形探索**: MathValue instance 内の `match` は patterns を順に試すため、N 個の case で worst-case O(N) になる。型クラス本来の O(1) 性が失われている。

2. **ネスト型の表現力**: 現状 `Differentiable (Frac MathValue)` しか書けず、`Differentiable (Frac (Poly Integer [x, y]))` のように内側の型まで指定した specialization ができない。

3. **書く必要のあるコード重複**: ユーザーは specific 型の instance に加えて MathValue の switchboard も意識する必要がある (現状は手書き)。

### 1.3 達成したい姿

```egison
-- 1) ユーザーは specific 型のみ instance を書く
instance Differentiable (Frac (Poly Integer [x, y])) where
  partialDiff f x := -- 2変数有理式の最適化された規則

instance Differentiable (Frac MathValue) where
  partialDiff f x := -- 一般 frac の規則

instance Differentiable (Term MathValue) where
  partialDiff f x := -- 単一項の規則

-- 2) MathValue の switchboard はコンパイラが自動生成
--    (ユーザーは何も書かない)

-- 3) 呼び出し時:
--    static 型が specific      → コンパイル時に直接 dispatch (O(1))
--    static 型が MathValue     → 値の outer 構造から runtime 型を割り出して
--                                  対応 instance の辞書を引く (O(1))
```

---

## 2. 設計の概要

### 2.1 3 つの構成要素

```
┌──────────────────────────────────────────────────────────────┐
│  (A) runtimeType :: CASValue -> Type                          │
│      値の構造を再帰的に走査し、最も specific な型を返す       │
└──────────────────────────────────────────────────────────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────────┐
│  (B) Specificity-aware instance lookup                        │
│      target type にマッチする全 instance を集め、              │
│      specificity 降順 (最も specific を先頭) でソート → 先頭  │
└──────────────────────────────────────────────────────────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────────┐
│  (C) Auto-generated MathValue instance                        │
│      `class Differentiable a` の宣言時に                      │
│      `instance Differentiable MathValue` を自動生成し、       │
│      その body は `runtimeType v` → 動的 dictionary 参照     │
└──────────────────────────────────────────────────────────────┘
```

---

## 3. 構成要素 (A) `runtimeType :: CASValue -> Type`

CAS 値から型を計算する。**最大ネスト深さ 2** で stop することで O(1) を達成。

### 3.1 規則 (shallow 版)

```haskell
runtimeType :: CASValue -> Type

-- 整数定数
runtimeType (CASInteger _) = TInt

-- 単独 factor (シンボル, apply1 等)
runtimeType (CASFactor _) = TFactor

-- 空 poly (= 0)
runtimeType (CASPoly []) = TInt

-- 単一定数項 (係数のみ、factors 空)
runtimeType (CASPoly [CASTerm c []]) | isInt c = TInt

-- 単一項 (Term) — coef は 1 レベル下まで見る
runtimeType (CASPoly [CASTerm coef _]) =
  TTerm (shallowType coef)

-- 多項式 (Poly) — atoms は集めるが coef は浅く
runtimeType (CASPoly terms) =
  let coefType = shallowJoinCoefs terms
      atoms    = extractAtomsAsTypeAtoms terms
  in TPoly coefType (SymbolSetClosed atoms)

-- 分数 (Frac) — 内側は浅く
runtimeType (CASFrac n d) =
  TFrac (shallowJoinTypes (shallowType n) (shallowType d))

-- ----------------------------------------------------------------------
-- shallowType: 1 レベル下の型のみを返す (再帰しない)
-- ----------------------------------------------------------------------
shallowType :: CASValue -> Type
shallowType (CASInteger _) = TInt
shallowType (CASFactor _)  = TFactor
shallowType (CASPoly _)    = TMathValue        -- ← 内部に降りない
shallowType (CASFrac _ _)  = TMathValue

-- ↑ 内側まで詳しく見たい場合は、ユーザーが手動で型注釈すれば良い
```

### 3.2 例

| 値 | runtimeType (shallow) | 旧版 (再帰) |
|---|---|---|
| `42` | `TInt` | `TInt` |
| `'sin x` | `TFactor` | `TFactor` |
| `x^2` | `TTerm TInt` | `TTerm TInt` |
| `x + y` | `TPoly TInt [x, y]` | `TPoly TInt [x, y]` |
| `(x+1)/(y-1)` | `TFrac TMathValue` | `TFrac (TPoly TInt [x, y])` |
| `2 * x / 3` | `TFrac TMathValue` | `TFrac (TPoly TInt [x])` |

shallow 版では Frac の内側は `TMathValue` 固定。**O(1)** を保つ代償。

### 3.3 既存コードとの関係

- `Math/CAS.hs:prettyTypeOf` の **outer constructor 部分のみ** を Type に移植
- 内部再帰は持ち込まない (← shallow 化のキー)
- `Math/CAS.hs:casAtomSet` で Poly の atoms を抽出
- `Type/Join.hs:joinTypes` で coefficient 型を浅く join (1 レベル)

---

## 4. 構成要素 (B) Specificity Ordering

複数の instance が同一の target にマッチしたとき、**最も specific** な instance を選ぶ。

### 4.1 設計原則

CAS 型の subtype 関係 (`Type/Join.hs:isSubtype`) を活用:

> **MORE SPECIFIC = SUBTYPE (より制約が強い)**

CAS の階層:
```
Integer  ⊂  Factor  ⊂  Term  ⊂  Poly  ⊂  Frac  ⊂  MathValue
            ↑                              ↑
        atom 単位                       商の形
```

ネスト型は covariant (depth ≤ 2 の範囲で):
- `Frac t1 ⊂ Frac t2` iff `t1 ⊂ t2` (where t1, t2 は単純型)
- `Poly t1 s1 ⊂ Poly t2 s2` iff `t1 ⊂ t2 AND s1 ⊆ s2` (atom set inclusion)
- `Term t1 ⊂ Term t2` iff `t1 ⊂ t2`

### 4.2 採用方針: 純粋な subtype 順序 (partial order)

```haskell
moreSpecific :: Type -> Type -> Maybe Ordering
moreSpecific t1 t2
  | t1 == t2          = Just EQ
  | isSubtype t1 t2   = Just LT   -- t1 の方が specific
  | isSubtype t2 t1   = Just GT
  | otherwise         = Nothing   -- 比較不能 (= 同時マッチで ambiguity error)
```

**採用の理由**:

1. **数学的に厳密**: CAS 型の subtype 半順序 (`Type/Join.hs:isSubtype`) を直接使うので、型システム本体の意味論と一貫する。
2. **Ambiguity 検出が自然**: 比較不能なら `Nothing` → そのままエラー。ユーザーに「型注釈で曖昧性を解消してください」と知らせる。
3. **実装の単純さ**: 既存の `isSubtype` を使うだけ。新たな score 関数のチューニング不要。
4. **実用上の効率**: candidate 数 N は通常 5〜10 程度の小定数。O(N²) でも問題なし。

**部分順序であることの帰結**:
- 全順序が必要な操作 (sort) は使えない
- 代わりに「**候補集合の最大元 (= 他のすべての候補の subtype)**」を探す
- 最大元が複数 (互いに比較不能) → ambiguity error
- 最大元が 0 個 (全 candidate が比較不能) → ambiguity error

### 4.3 アルゴリズム実装

```haskell
-- target に compatible (subtype unification で一致) な instance を集める
candidates :: Type -> [InstanceInfo] -> [InstanceInfo]
candidates target = filter (\inst -> isCompatibleWith target (instType inst))

-- 候補から最も specific な instance を 1 つ選ぶ
findMostSpecific :: [InstanceInfo] -> Either AmbiguityError InstanceInfo
findMostSpecific [] = Left NoMatchingInstance
findMostSpecific [x] = Right x
findMostSpecific xs =
  let isMaxOf x = all (\y -> y == x || isSubtype (instType x) (instType y)) xs
      maxima = filter isMaxOf xs
   in case maxima of
        [unique] -> Right unique
        []       -> Left AmbiguousIncomparable     -- 比較不能ペアあり
        _        -> Left AmbiguousMultipleMaxima   -- 複数の最大元 (本来は起こらない)
```

### 4.4 例 (shallow runtimeType を使用)

値 `(x+1)/(y-1)` (商) → `runtimeType v = TFrac TMathValue` (shallow なので内側は不可視)。

候補:
1. `Frac MathValue` — exact match
2. `Frac Integer` — `TInt ⊆ TMathValue` ?  ← **逆方向**: `Frac MathValue` (target) → `Frac Integer` の subtype か。NO (Integer は MathValue の subtype だが、Frac MathValue は Frac Integer の subtype ではない)
3. `MathValue` — ✓ (Frac ⊂ MathValue)

subtype 関係:
- 1 < 3 (Frac MathValue ⊂ MathValue)
- 2 は target にマッチしない (Integer は MathValue の subtype だが、ここでは `target = Frac MathValue` で `inst = Frac Integer` の compatibility が逆向き)

最も specific = `Frac MathValue` (1)。

別の例: 値 `x + y` → `runtimeType v = TPoly TInt [x, y]`。

候補:
1. `Poly MathValue [..]` — ✓ (Int ⊂ MathValue, [x,y] ⊆ [..])
2. `Poly Integer [..]` — ✓ ([x,y] ⊆ [..])
3. `Poly Integer [x, y]` — ✓ (exact match)
4. `Poly Integer [x, y, z]` — ✓ ([x,y] ⊆ [x,y,z])
5. `MathValue` — ✓

subtype 関係:
- 3 < 4 < 2 < 1 < 5 (より制約 → 弱い)

最も specific = `Poly Integer [x, y]` (3)。

### 4.5 ambiguity の例

target = `Poly Integer [x]`、候補に `Poly Integer [x, y]` と `Poly Integer [x, z]` がある場合:
- 両者とも target を含む (`[x] ⊆ [x, y]`, `[x] ⊆ [x, z]`)
- `[x, y]` と `[x, z]` は互いに subtype 関係なし (atom set として不包含)
- `findMostSpecific` は `Nothing` (ambiguity error)
- ユーザーは `Poly Integer [x]` を直接書く / 型注釈で具体化する必要

---

## 5. 構成要素 (C) Auto-generated MathValue Instance

`class Differentiable a where ...` の宣言時に:

```egison
class Differentiable a where
  partialDiff (f: a) (x: MathValue) : MathValue
```

コンパイラが自動的に追加する:

```egison
-- 自動生成 (擬似コード)
instance Differentiable MathValue where
  partialDiff f x :=
    let realType := runtimeType f                       -- (A) 構成要素
        dict     := lookupDictionary "Differentiable" realType  -- (B) 構成要素
     in dict_("partialDiff") f x
```

`lookupDictionary` は:
1. ClassEnv から `Differentiable` の全 instance を取得
2. specificity ordering で best match を選ぶ
3. その instance の dictionary を返す

これは既存の `Type/Instance.hs:findMatchingInstanceForTypes` を拡張したもの。

### 5.1 注意: 自動生成と手書き instance の共存

ユーザーが `instance Differentiable MathValue where ...` を**手書きした場合は**:
- 自動生成は抑制 (重複 dictionary のため)
- 手書き instance がそのまま使われる (現状維持)

これにより**段階的移行**が可能 (新機能を opt-in で使い始められる)。

---

## 6. 実装フェーズ

shallow runtimeType により実装が単純化、合計 3 日に圧縮:

| Phase | 内容 | 主な変更ファイル | 工数 |
|---|---|---|---|
| 1 | `runtimeType :: CASValue -> Type` (shallow) プリミティブ実装 | `Primitives.hs`、`Math/CAS.hs` | 0.5 日 |
| 2 | `findMostSpecificInstance` を `Type/Instance.hs` に追加 + `isSubtype` の depth-2 ケース追加 | `Type/Instance.hs`、`Type/Join.hs` | 0.5 日 |
| 3 | `class` 宣言時に MathValue 自動 instance を emit | `EnvBuilder.hs`、`Desugar.hs` | 0.5 日 |
| 4 | `TypeClassExpand` で `findMostSpecificInstance` を使うよう変更 | `Type/TypeClassExpand.hs` | 0.5 日 |
| 5 | テスト: 浅い instance / specificity 競合 / ambiguity error | `mini-test/` 追加 | 0.5 日 |
| 6 | lib 整理: `Differentiable MathValue` 手書き instance 削除、ベンチマーク | `lib/math/analysis/derivative.egi` | 0.5 日 |
| **合計** | | | **3 日** |

---

## 7. 拡張点・周辺事項

### 7.1 深いネスト型への対応 (将来)

本設計はネスト深さ 2 までに制限する。深いネスト (例: `Frac (Poly Integer [x, y])` のような 3 レベル以上) が必要になった場合の選択肢:

**(a) `runtimeType` を再帰版に拡張**:
- 値全体を走査して完全なネスト型を計算
- コスト: O(値サイズ)
- 実装: 元設計の §3 を流用 (本設計の前バージョン参照)

**(b) instance 側で内側を `match` で分解**:
- ユーザーが `instance Differentiable (Frac _)` の body 内で内側を pattern match
- 既存の Egison 流儀
- 設計変更不要

**(c) 値構築時の型タグキャッシュで O(1) 維持**:
- `casPoly`/`casFrac` 等で構築時に型を計算してキャッシュ
- メモリオーバーヘッド: 値ごとに `Maybe Type` フィールド
- 実装は構築 API 全体に伝播

(b) は今すぐ可能、(a)/(c) は将来必要なら検討。

### 7.2 観察型 (`inspect`) との関係

注意: shallow `runtimeType` と `prettyTypeOf` (`inspect`) が**異なる**結果を返す可能性。

```egison
def v := (x + 1) / (y - 1)
inspect v
-- > Frac (Poly Integer [x, y])    (prettyTypeOf — 再帰版)

-- 一方、dispatch 用:
runtimeType v
-- > Frac MathValue                  (shallow 版)
```

ユーザーには「inspect は観察、dispatch には別の (shallower な) 型計算が使われる」と説明する必要。あるいは将来 `prettyTypeOf` も shallow 版に統一する選択もある。

### 7.3 動的な dictionary lookup の cache

頻出パターンに対しては LRU cache 等で `runtimeType → dictionary` をキャッシュ可能。実装は後回し (まずベンチマークで必要性を判断)。Shallow 版では `runtimeType` 自体が O(1) なのでキャッシュは不要かもしれない。

### 7.4 ユーザー定義型への一般化

CAS 専用の機構ではなく、一般のユーザー定義 inductive 型にも拡張可能 (将来検討):

```egison
inductive Tree a := Leaf | Node a (Tree a) (Tree a)

class TreeOp a where
  combine : Tree a -> a

instance TreeOp Integer where ...
instance TreeOp (Tree Integer) where ...
```

このとき `runtimeType` は inductive 型の constructor を outer + 1 レベルで判別する関数を自動生成。

---

## 8. 関連設計との比較

### 8.1 Haskell 型クラス + GADTs

Haskell では `Show a => a -> String` は静的に dispatch される (辞書渡し)。runtime 型 (= 値の構造) で dispatch するには `Typeable`/`Data.Dynamic` のような機構が必要。

本設計は CAS 型に限定して、**静的 + 動的**の両方を一つの type-class 機構で扱う点が異なる。

### 8.2 CLOS / Julia の multiple dispatch

multiple dispatch: 複数引数の型で最適 method を選ぶ。本設計は (現状は) 第一引数の型のみで dispatch する点が異なる。

multi-param TC dispatch (§2.1) と組み合わせれば multiple dispatch 相当になる。

### 8.3 OCaml の polymorphic variants

OCaml の poly-variant (`` `Foo of int | `Bar of string ``) は subtyping を持つが、type class はない。本設計は subtype + dictionary を統合。

### 8.4 Rust の trait + enum dispatch

`enum Shape { Circle, Square }` の trait 実装は match を使う (= O(N))。本設計は dictionary lookup で O(1)。

---

## 9. リスクと未解決問題

### 9.1 ambiguity の頻度

Specificity ordering で「比較不能」なケースが頻発するとユーザー体験が悪化。実装後にユースケースで頻度を計測し、必要なら案 3 (ハイブリッド) に切り替え。

### 9.2 自動生成と手書きの優先順位

ユーザーが手書きした `instance Differentiable MathValue` がある時、自動生成を抑制する判定をどこで行うか。

- 案 a: EnvBuilder で「`MathValue` instance が存在する」かチェック
- 案 b: 自動生成は常に行い、duplicate instance としてエラーにする (= 手書き存在時はユーザーが除去する)

案 a の方がユーザフレンドリ。

### 9.3 表現力の制限

ネスト深さ 2 の制限により、以下のような instance は書けるが**特別扱いされない**:

```egison
instance Differentiable (Frac (Poly Integer [x, y])) where ...
```

shallow runtimeType では値が `Frac _` までしか分類されないので、上の instance は `Differentiable (Frac _)` (= `Frac MathValue` instance) と同等扱いされる。

**回避策**:
- ユーザーが必要なら instance body で `match` を使って内側を分解 (現状と同じ)
- または将来 §7.1 の (a)/(c) で深い対応を追加

### 9.4 type-level symbol set のマッチング

`Poly Integer [x]` vs `Poly Integer [..]` の specificity 比較は明確だが、`Poly Integer [x, y]` vs `Poly Integer [y, z]` のような無関係な閉集合の比較はどうする?

- 値の atom set が `[y]` なら、両者ともマッチ。specific は `[x, y]` (より小さい) ?
- 値の atom set が `[x, y]` なら、`[x, y]` のみマッチ。`[y, z]` は対象外。

ルール: instance.atoms ⊇ target.atoms のもののみ candidate に入れる。candidates 内では atom set の cardinality が小さいものが specific。

→ 4.1 で示した subtype 順序にすでに含まれる (`symbolSetSubset`)。

---

## 10. 段階的移行計画

新方式は既存コードと共存可能 (5.1 参照) なので、段階的に移行できる:

1. **Phase 1-4 (実装)**: 機能を追加。既存 lib は変更しない。
2. **Phase 5 (テスト)**: 新機能のテストを追加。既存テストはそのまま pass することを確認。
3. **Phase 6 (lib 整理)**: `derivative.egi` 等の MathValue switchboard を削除。動作確認。
4. **将来**: 一般のユーザー定義型への拡張 (7.4)。

---

## 11. 改訂履歴

- 2026-04-30: 初版起案。runtimeType 再帰化、任意ネスト instance、specificity ordering の 3 案、自動 MathValue instance 生成。
- 2026-04-30: specificity ordering を案 1 (subtype 半順序) に確定。任意ネスト型を許す以上 O(1) は理論上不可能であることを §0 と §9.3 で明記。最適化案 (型タグキャッシュ、shallow 評価、LRU) を §9.3 に追加。案 2/3 と複雑度の混在記述を削除して焦点を案 1 に絞った。
- 2026-04-30: **ネスト深さ 2 に制限する方針に確定**。`runtimeType` を shallow 版に変更し O(1) を達成。深いネスト型対応は将来検討項目に降格。実装フェーズを 5 日 → 3 日に圧縮。§0/§3/§4/§6/§7/§9 を全面改訂。
