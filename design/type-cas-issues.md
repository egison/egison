# Egison CAS 型システム 実装課題と解決案

[type-cas.md](./type-cas.md) の設計に基づき、実装時に詰まりそうな課題と解決案を整理する。

優先度:

- **高**: 実装を止めうる構造的問題。Phase 着手前に決着が必要
- **中**: 設計の穴。実装開始後に決定しても間に合うが、先に決めた方が効率的
- **低**: 実装時の細かい障害。解決案は比較的明確

---

## 目次

### 高優先度

- [A. `term m` マッチャーへの型情報ルーティング](#a-term-m-マッチャーへの型情報ルーティング)
- [B. `declare apply` のブランチ型と `MathValue` 戻り値](#b-declare-apply-のブランチ型と-mathvalue-戻り値)
- [C. 正規化の demote ポリシー](#c-正規化の-demote-ポリシー)
- [D. `declare rule` LHS シンボル参照の型付け](#d-declare-rule-lhs-シンボル参照の型付け)
- [E. `SymbolExpr` 正規形比較のタイミング](#e-symbolexpr-正規形比較のタイミング)

### 中優先度

- [F. 型クラスインスタンスの特化メカニズム](#f-型クラスインスタンスの特化メカニズム)
- [G. `declare derivative` 未登録関数の微分](#g-declare-derivative-未登録関数の微分)
- [H. パースの曖昧性](#h-パースの曖昧性)
- [I. `declare symbol sin x` と `declare mathfunc sin` の同一性](#i-declare-symbol-sin-x-と-declare-mathfunc-sin-の同一性)
- [J. インスタンス優先順位の具体化](#j-インスタンス優先順位の具体化)
- [P. 関数シンボル (`function (x)`) の CAS 型システム統合](#p-関数シンボル-function-x-の-cas-型システム統合)

### 低優先度

- [K. `Term → Poly` embed のラップコスト](#k-term--poly-embed-のラップコスト)
- [L. `Rewrite.hs` 移行の回帰テスト](#l-rewritehs-移行の回帰テスト)
- [M. テンソル内の異種 Poly 型](#m-テンソル内の異種-poly-型)
- [N. `coerce` 失敗のエラーメッセージ設計](#n-coerce-失敗のエラーメッセージ設計)
- [O. `MathExpr` 既存パターンとパラメトリックマッチャーの共存](#o-mathexpr-既存パターンとパラメトリックマッチャーの共存)

---

## 高優先度

### ~~A. `term m` マッチャーへの型情報ルーティング~~ (解決済み)

#### 課題

`term m` マッチャーの 3 スロット分解 `($c, ($mc, $ms, $mf))` は、flat ランタイム Monomial を型注釈の `cs`/`ss`/`fs` 名前集合でルックアップして振り分ける必要があり、プリミティブに静的型情報をランタイム引数として渡す機構が必要だった。

#### 解決済み(2026-04-21)

**`term m` マッチャーを 1 スロット分解にする**。ランタイムが flat である以上、マッチャーで 3 スロット分解する必要はない。

```egison
-- 旧(3 スロット、型情報ルーティングが必要):
match t as term integer with | ($c, ($mc, $ms, $mf)) -> ...

-- 新(1 スロット = flat、型情報ルーティング不要):
match t as term integer with | ($c, $m) -> ...
-- $m : [(Factor, Integer)]  -- flat な (原子, 冪指数) のリスト
```

プリミティブは `termCoeff` / `termMonomial` の 2 つのみ。型情報は不要で、`CASTerm` の各フィールドを取り出すだけ。

分類したいユーザーは `$m` の各要素に対して `symbol`/`constantFactor`/`appliedFactor` マッチャーを自分で適用する:

```egison
match t as term integer with
  | ($c, multiset (symbol $name $idx, $exp) $syms) -> ...
  -- syms : [(Symbol, Integer)] のみ抽出
```

設計書 [type-cas.md](./type-cas.md) の「`:+` パターンの意味論」および「Step 5.3: `term` パラメトリックマッチャーの実装」セクション参照。

---

### ~~B. `declare apply` のブランチ型と `MathValue` 戻り値~~ (解決済み)

#### 課題

`declare apply` の RHS 各ブランチに型注釈(`(isqrt x : Integer)` 等)を書いたとき、宣言型 `MathValue` との関係が曖昧だった。

#### 解決済み(2026-04-21)

**`declare apply` の RHS は常に `MathValue` として評価される**。ブランチ別の型注釈は書かない(書いても関数インタフェースには漏れない)。呼び出し側で具体的な型が必要な場合は、式レベル型注釈で coerce を挿入する。

```egison
-- 定義側: 型注釈なし
declare apply sqrt x :=
  if isInteger x then
    if isPerfectSquare x then isqrt x
    else if hasPerfectSquareFactor x then
      let (k, m) = extractPerfectSquareFactor x in k * sqrt m
    else 'sqrt x
  else 'sqrt x

-- 呼び出し側で型絞り込み
(sqrt 4 : Integer)                         -- 2 : Integer
(sqrt 8 : Poly Integer [sqrt 2] [] [])     -- 2 * sqrt 2
```

設計書 [type-cas.md](./type-cas.md) の「数学関数の適用規則 (`declare apply`)」および「sqrt の定義全体像」セクション参照。

---

### ~~C. 正規化の demote ポリシー~~ (解決済み)

#### 課題

「demote は明示のみ」方針では `CASFrac (CASInteger 3) (CASInteger 1)` と `CASInteger 3` が別表現になり、等価判定や規則マッチが壊れる懸念があった。

#### 解決済み(2026-04-21)

`==` を `x - y = 0` で定義することで、demote ポリシーから完全に独立した。`x - y` の計算過程で `casNormalize` が適用され、差が数学的に 0 なら常に `CASInteger 0` に到達する(現実装で既に保証)。demote の有無は `==` の結果に影響しない。

```egison
instance Eq MathValue where
  (==) x y := (x - y) = 0

-- 例: (x+1)/1 == x+1
-- → ((x+1)/1 - (x+1)) = 0
-- → 内部で CASFrac と CASPoly の減算、normalize を経て CASInteger 0
-- → 0 = 0 → true
```

さらに、現実装の `casNormalize` は既に自動 demote を行っている([CAS.hs:409, 477](../hs-src/Language/Egison/Math/CAS.hs))ので、設計書の「demote は明示のみ」方針は現実装と矛盾していた。現実装の**自動 demote を採用**することで設計と実装が一致する。

設計書 [type-cas.md](./type-cas.md) の「等価性: `Eq MathValue` と subtype からの fallback」セクション参照。

---

### ~~D. `declare rule` LHS シンボル参照の型付け~~ (解決済み)

#### 課題

`declare rule auto term i^2 = -1` の `i` はパターン変数ではなく特定シンボルへの参照。規則の適用スコープ(どの型の値に対して試すか)が未定だった。

#### 解決済み(2026-04-21)

**規則は意味論的には常に MathValue レベルで登録される**が、**型情報で適用を最適化**する。

1. `T = MathValue` → 規則適用
2. `T` のどこかのスロットに `[..]` がある → 規則適用
3. 全スロット閉じている → `triggerSymbols(R) ∩ atoms(T) ≠ ∅` のときのみ適用

```egison
declare rule auto term i^2 = -1   -- triggerSymbols = {i}

def p : Poly Integer [] [x] [] := x^2 + 1     -- i 不在 → 規則スキップ
def q : GaussianInt := 1 + i                   -- i ∈ atoms → 規則適用
def r : MathValue := ...                       -- 常に適用
def s : Poly Integer [..] [..] [..] := ...     -- [..] あり → 常に適用
```

これは**正しさを保存する最適化**: 型フィルタで省かれた規則が適用されてもマッチしないはずなので結果は変わらない。

`ReductionRule` に `triggerSymbols :: [SymbolExpr]` フィールドを追加し、`casNormalizeWithRules :: ReductionEnv -> Type -> CASValue -> CASValue` で型を受け取るようにする。

設計書 [type-cas.md](./type-cas.md) の「規則適用の型情報フィルタ」セクション参照。

---

### ~~E. `SymbolExpr` 正規形比較のタイミング~~ (解決済み)

#### 課題

`SymbolSetClosed [SymbolExpr]` で注釈を照合するため、`sqrt 2` 等の正規化タイミングが未定だった。前方参照の扱いも未決。

#### 解決済み(2026-04-21)

**宣言環境 (Declaration Environment) の導入**により解決。プログラム読み込み時のプリパスで `declare mathfunc`, `declare symbol` 等を全て先に収集してから型推論・評価に入るので、**宣言順序の制約がなくなる**。

```egison
-- この順序で書いても有効(プリパスで declare mathfunc を先に収集するので)
def s : Poly Integer [sqrt 2] [] [] := sqrt 2
declare mathfunc sqrt : MathValue -> MathValue
declare apply sqrt x := ...
```

正規化のタイミング: 型注釈 `[sqrt 2]` を評価する時点で `MathFuncEnv` と `ApplyRuleEnv` が揃っているので、`Apply1 sqrt_id (CASInteger 2)` に正規化して `SymbolSetClosed [SymbolExpr]` に格納できる。

`sqrt (1 + 1)` も `declare apply` によって `sqrt 2` に正規化されるので、注釈との比較が一貫する。

設計書 [type-cas.md](./type-cas.md) の「宣言環境 (Declaration Environment)」セクション参照。

---

## 中優先度

### ~~F. 型クラスインスタンスの特化メカニズム~~ (解決済み)

#### 課題

`instance Ring (Poly a [..] [..] [..])` のテンプレートから `Ring (Poly Integer [] [i] [])` を解決時に自動派生する仕組みが必要だった。部分特化(`[sqrt 2] [..] [..]` 等)の扱いも未決。

#### 解決済み(2026-04-22)

**インスタンスヘッドの `[..]` をフレッシュ型変数として扱う**標準 Haskell 的な instance resolution を採用。

```
instance {Ring a} Ring (Poly a [..] [..] [..]) where ...
-- 内部表現: instance {Ring a} Ring (Poly a [α] [β] [γ]) where ...  (α, β, γ は fresh)

-- a + b : GaussianInt の解決
-- 目標: Ring (Poly Integer [] [i] [])
-- 単一化: a = Integer, α = [], β = [i], γ = []
-- → 解決成功、型は Poly Integer [] [i] [] が保存される
```

閉じたスロットと `[..]` の混在(部分特化)も同じ単一化で自然に動く。subtype への fallback も同じ仕組みで(embed 挿入により)解決する。

設計書 [type-cas.md](./type-cas.md) の「インスタンス解決メカニズム」セクション参照。

---

### ~~G. `declare derivative` 未登録関数の微分~~ (解決済み)

#### 課題

`declare mathfunc f` で宣言されたが `declare derivative f` が未登録の関数の微分をどう扱うかが未決だった。

#### 解決済み(2026-04-22)

**`declare derivative` 未登録はエラー**。`declare mathfunc f` + 未登録で `∂/∂ (f x) x` を計算しようとすると実行時エラーとなる。

シンボリックに未知関数を扱いたい場合は、Egison の**関数シンボル**(`function (x)`)を使う。これは `declare mathfunc` とは別機構で、具体実装を持たず連鎖律で自動微分できる:

```egison
-- declare mathfunc なしで、値レベルでシンボリック関数を定義
def f := function (x)
∂/∂ (f x) x   -- = f|1 x (偏微分インデックス付きの新しい function を返す)
```

この 2 系統の役割分担により、「`declare mathfunc` は具体実装のある関数専用、シンボリック操作は関数シンボル」という明確な使い分けができる。

設計書 [type-cas.md](./type-cas.md) の「`AppliedFactor` の導関数定義」および「関数シンボル (`function (x)`) との使い分け」セクション参照。

---

### ~~H. パースの曖昧性~~ (解決済み)

#### 課題

- 式を原子として注釈に書きたい(`[sin x, sqrt (x+1)]`)
- `Poly Integer [x, y]`(スロット集合)と `[Integer]`(リスト型)のレキサ上の曖昧性
- `[..]` 構文の特別扱い

#### 解決済み(2026-04-23)

**`Poly T [S1] [S2] [S3]` の 3 スロット構文を専用パスでパース**。3 つの角括弧グループを必須とすることで、コレクション型 `[T]` との曖昧性を解消。

```
Poly Integer [sqrt 2] [x, y] [sin x, log (x+1)]
            ^^^^^^^^ ^^^^^^^ ^^^^^^^^^^^^^^^^^^^
             atom 式  atom 式  atom 式のリスト
-- 3 つの角括弧を必須 → [Integer] リスト型とは区別
```

スロット内は atom 式として読む(関数適用 `sin x` 等を許容)。`[..]` はレキサ段階で専用トークン化する。

1 スロット記法(`Poly T [x, y]`)は糖衣構文(`ConstantPoly`, `SymbolPoly` 等)のみで許可し、生の `Poly` は常に 3 スロットを要求する。

設計書 [type-cas.md](./type-cas.md) の「型構文の実装」セクション参照(文法 BNF を記載)。

---

### ~~I. `declare symbol sin x` と `declare mathfunc sin` の同一性~~ (解決済み)

#### 課題

`declare symbol sin x` で登録した合成原子と、`declare mathfunc sin` 経由の `sin x` 評価結果が構造的に等しい `SymbolExpr` を生成する必要があった。両経路が別表現を生成すると、`SymbolSetClosed` との照合で失敗する。

#### 解決済み(2026-04-22)

**`declare symbol sin x` を廃止**し、合成原子は `declare mathfunc sin` のみで登録する。型注釈に `sin x` 等の合成原子が出現した時点で、宣言環境(`MathFuncEnv`)を参照して `SymbolExpr` に正規化される。

```egison
-- 旧(廃止):
declare symbol sin x  -- 合成原子登録

-- 新:
declare mathfunc sin : MathValue -> MathValue
-- Poly 型注釈に sin x が現れた時点で AppliedFactor 原子として自動登録
def f : Poly Integer [] [] [sin x] := sin x  -- パーサが [sin x] を見て MathFuncEnv で正規化
```

登録経路が 1 つに統一され、SymbolExpr の同一性問題が消える。

設計書 [type-cas.md](./type-cas.md) の「シンボルと不定元」および「Factor の各分類から Poly への埋め込み」セクション参照。

---

### ~~J. インスタンス優先順位の具体化~~ (解決済み)

#### 課題

複数のインスタンスが適用可能な場合(汎用 / 部分特化 / 完全特化)に「より具体的なもの」の判定アルゴリズムが未定だった。

#### 解決済み(2026-04-22)

**2 段階の具体性比較**:

1. **型引数の具体性**: `a`(型変数) < `Integer`(具体)
2. **原子集合の具体性**: `[..]`(fresh 変数) < `[..]` に制約付き(`[cs]` where ...) < `[]` や `[i]`(リテラル)

両方で「より具体的」なインスタンスを選ぶ。競合時は型エラー(OverlappingInstances は認めない)。

```
Ring (Poly Integer [] [i] []) を解決するとき:
- Ring GaussianInt: 両方具体 → 最も具体的
- Ring (Poly Integer [..] [..] [..]): 係数具体、原子変数
- Ring (Poly a [..] [..] [..]): 両方変数 → 最も汎用

→ Ring GaussianInt が選ばれる
```

設計書 [type-cas.md](./type-cas.md) の「インスタンス解決の優先順位」セクション参照(F の解決時に統合記述済み)。

---

## 低優先度

### K. `Term → Poly` embed のラップコスト

#### 課題

`Term a [cs] [ss] [fs] ⊂ Poly a [cs] [ss] [fs]` は `CASTerm → CASPoly [CASTerm]` のラッピングを伴う。「embed は常に安全でゼロコスト」という前提が崩れる。`poly m` の `$a :+ $rest` で得た `a : Term` を `Poly` 引数に渡すたびに wrap 発生。

#### 解決案

**受け入れる**。O(1) アロケーションで、Term/Poly 境界でのみ発生。性能的影響は無視できる。設計書に「Term→Poly embed はラップを伴う」と明記するだけ。

---

### L. `Rewrite.hs` 移行の回帰テスト

#### 課題

Phase 7.1–7.7 で `casRewriteRtu`, `casRewriteW`, `casRewriteSinCos`(新規)等を `declare rule` に移行する。既存の sample/ の数式出力と一致させる検証が必須。

#### 解決案

**ゴールデンテスト**。既存 sample/ の出力を snapshot し、移行後の出力と diff を取る。`mini-test/` に golden pair のテストを追加。

---

### M. テンソル内の異種 Poly 型

#### 課題

`Tensor (Poly Integer [sqrt 2] [] [])` と `Tensor ConstantFactor` の join が `Tensor MathValue` になる。成分ごとに異なる型の値を持つテンソルの演算ディスパッチが未定義。

#### 解決案

**Tensor は homogeneous を要求**。`Tensor a` は全成分が `a` 型。異種混在は明示的に `embed` で統一型にしてから Tensor に入れる。

```
[| 1 + i, sqrt 2 |] : Tensor (Poly Integer [..] [..] [..])
-- 両成分を Poly Integer [..] [..] [..] に embed 済みで構築
```

`Tensor MathValue` は「全成分を共通の MathValue 型として扱う」という意味になり、成分ごとの具体型情報は失われる(ユーザーが明示すれば情報保持可能)。

---

### N. `coerce` 失敗のエラーメッセージ設計

#### 課題

ランタイムで `Poly Integer [sqrt 2] [] [] := sqrt 3` が失敗したとき、どの原子がどの集合に含まれないかを示す必要がある。

#### 解決案

**詳細原子レポート**。期待集合と見つかった違反原子を位置情報付きで表示。

```
CoerceError at foo.egi:42:5
  Expected: Poly Integer [sqrt 2] [x] []
  Found unexpected atom: sqrt 3 (in term: sqrt 3 * x)
  Hint: consider using Poly Integer [sqrt 2, sqrt 3] [x] []
     or Poly Integer [..] [x] []
```

`SymbolExpr` の pretty-printer 整備が前提条件。

---

### O. `MathExpr` 既存パターンとパラメトリックマッチャーの共存

#### 課題

Phase 4 で導入した `inductive pattern MathExpr`(`poly $`, `div $ $`, `term $ $`)と Phase 5 のパラメトリックマッチャー(`poly m`, `frac m`, `term m`)の名前衝突。

#### 解決案

**Egison の名前空間分離を利用**。パターン名前空間と値名前空間は独立なので、パターン `poly` と関数 `poly` は共存可能。`div` → `frac` のリネーム(既存設計)でさらに安全化。実装上は既存のパーサ機構で解決済み。

---

### P. 関数シンボル (`function (x)`) の CAS 型システム統合

#### 課題

Egison の既存機構である関数シンボル(`def f := function (x)`, [function-symbol.md](./function-symbol.md))を、新しい CAS 型システム(`Poly`, `Factor`, タワー, 型クラス etc.)にどう統合するかが未定義。

具体的な未決事項:

1. **型**: `function (x)` の値の型は何か?
   - `AppliedFactor`? `MathValue`? 独自型 `FunctionSymbol`?
   - 関数シンボル全般の抽象化 `Func (a -> b)` のような高階型?
2. **`Poly` の原子集合への出現**: `Poly Integer [] [] [f x]` のように原子集合に書けるか?
3. **`SymbolExpr` との関係**: 既存実装では `FunctionData` として別枠だが、新 `SymbolExpr` / `CASValue` でどう表現するか
4. **偏微分インデックス `f|1`, `f|2` の型**: 導関数が `function` のまま残るとき、その型は?
5. **`declare mathfunc` との相互作用**: 同じ名前で両方定義したときの挙動は禁止すべきか?
6. **関数シンボルの適用(`f a`)**: 引数数チェックをどう行うか。型で表現するか、実行時エラーに留めるか

#### 解決案(暫定)

Phase 6 以降で個別に設計。現段階では「関数シンボルは新 CAS 型システム下でも既存の挙動を保持する」を最低限の目標とし、具体的な型付けは後回し。

- `FunctionData` を `SymbolExpr` のバリアントとして維持
- 型レベルでは関数シンボルの値は `MathValue`(あるいは `AppliedFactor`)として扱う
- 型注釈の原子集合(`Poly ... [f x]`)への出現は当面禁止(明確な型が決まってから解放)

---

## まとめ

| # | 課題 | 優先度 | 解決案の要点 |
|---|---|---|---|
| ~~A~~ | ~~`term m` 型情報~~ | ~~高~~ | **解決済み**: マッチャーを 1 スロット(flat)分解にする |
| ~~B~~ | ~~`declare apply` ブランチ型~~ | ~~高~~ | **解決済み**: RHS は常に MathValue、ブランチ注釈なし、呼出側で coerce |
| ~~C~~ | ~~demote ポリシー~~ | ~~高~~ | **解決済み**: `==` を `x - y = 0` で定義し demote 独立、現実装の自動 demote を採用 |
| ~~D~~ | ~~`declare rule` LHS 型~~ | ~~高~~ | **解決済み**: 型情報で適用をフィルタ(triggerSymbols) |
| ~~E~~ | ~~SymbolExpr 正規化~~ | ~~高~~ | **解決済み**: 宣言環境のプリパスで先に収集、順序制約なし |
| ~~F~~ | ~~型クラス特化~~ | ~~中~~ | **解決済み**: `[..]` を fresh 型変数として単一化、subtype は embed fallback |
| ~~G~~ | ~~未登録 derivative~~ | ~~中~~ | **解決済み**: 未登録はエラー、シンボリック微分は `function (x)` 機構を使う |
| ~~H~~ | ~~パース曖昧性~~ | ~~中~~ | **解決済み**: 3 スロット専用構文(3 つの `[...]` 必須)+ atom 式 |
| ~~I~~ | ~~symbol/mathfunc 同一性~~ | ~~中~~ | **解決済み**: `declare symbol sin x` 廃止、`declare mathfunc` のみで登録 |
| ~~J~~ | ~~インスタンス優先度~~ | ~~中~~ | **解決済み**: 2 段階具体性比較、競合はエラー |
| K | Term→Poly ラップ | 低 | 受け入れ |
| L | Rewrite.hs 移行 | 低 | ゴールデンテスト |
| M | 異種 Tensor | 低 | homogeneous 要求、明示 embed |
| N | coerce エラー | 低 | 詳細原子レポート |
| O | パターン/マッチャー共存 | 低 | 名前空間分離(既存機構で解決) |
| P | 関数シンボルの型統合 | 中 | 暫定: 既存挙動維持、型注釈原子集合への出現は当面禁止 |

---

## 推奨される着手順

### Phase 2(型システム 3 スロット化)着手前

- ~~**E**(SymbolExpr 正規化タイミング)~~ — 解決済み(宣言環境のプリパス)
- ~~**F**(型クラス特化メカニズム)~~ — 解決済み(`[..]` を fresh 型変数として単一化)
- ~~**J**(インスタンス優先順位)~~ — 解決済み(2 段階具体性比較)

### Phase 5(パラメトリックマッチャー)着手前

- ~~**A**(`term m` の型情報ルーティング)~~ — 解決済み(1 スロット分解)
- ~~**C**(demote ポリシー)~~ — 解決済み(`x - y = 0` で demote 独立)
- ~~**H**(パース曖昧性)~~ — 解決済み(3 スロット専用構文)

### Phase 6/7(ライブラリ/規則)着手前

- ~~**B**(`declare apply` ブランチ型)~~ — 解決済み(RHS は MathValue)
- ~~**D**(`declare rule` LHS 型付け)~~ — 解決済み(triggerSymbols フィルタ)
- ~~**G**(未登録 `declare derivative`)~~ — 解決済み(エラー、シンボリック微分は関数シンボル)
- ~~**I**(`declare symbol` と `declare mathfunc` の同一性)~~ — 解決済み(`declare symbol sin x` 廃止)

### 実装時に個別解決

K, L, M, N, O は実装フェーズで個別に対応可能。設計の大枠に影響しない。
