# Egison 型クラスシステム：現状と改善計画

## 概要

Egisonの型クラスシステムの現在の実装状況を整理し、CAS型システム（`type-cas.md`）で必要とされる代数的型クラス階層（`Num` クラスの分割）に向けた改善計画をまとめる。

---

## 現在の型クラスシステムの構成

### 処理の流れ

```
Parser (NonS.hs)
  ↓  ClassDeclExpr / InstanceDeclExpr を生成
EnvBuilder (EnvBuilder.hs)
  ↓  ClassInfo / InstanceInfo を ClassEnv に登録
     メソッドの型を TypeEnv に登録
Type Inference (Infer.hs)
  ↓  制約 [Constraint] を収集
TypeClassExpand (TypeClassExpand.hs)
  ↓  メソッド呼び出しを辞書ベースのディスパッチに変換
Evaluation (Core.hs)
```

### 主要なデータ構造

**型クラス情報**（`Types.hs:102-107`）:
```haskell
data ClassInfo = ClassInfo
  { classSupers  :: [String]           -- スーパークラス名のリスト
  , classParam   :: TyVar              -- 型パラメータ
  , classMethods :: [(String, Type)]   -- メソッド名と型
  }
```

**インスタンス情報**（`Types.hs:109-115`）:
```haskell
data InstanceInfo = InstanceInfo
  { instContext :: [Constraint]        -- インスタンス制約
  , instClass   :: String              -- クラス名
  , instType    :: Type                -- インスタンスの型
  , instMethods :: [(String, ())]      -- メソッド実装（プレースホルダー）
  }
```

**クラス環境**（`Env.hs:224-228`）:
```haskell
data ClassEnv = ClassEnv
  { classEnvClasses   :: Map String ClassInfo
  , classEnvInstances :: Map String [InstanceInfo]
  }
```

---

## 現在の言語機能

### 動作している機能

**型クラス定義**:
```egison
class Eq a where
  (==) (x: a) (y: a) : Bool
  (/=) (x: a) (y: a) : Bool
```

**単一スーパークラス**（構文のみ。意味論は不完全）:
```egison
class Ord a extends Eq a where
  compare (x: a) (y: a) : Ordering
```

**インスタンス定義（制約つき）**:
```egison
instance {Eq a} Eq (Tensor a) where
  (==) t1 t2 := t1 = t2
  (/=) t1 t2 := not (t1 == t2)
```

**制約つき関数**:
```egison
def sum {Num a} (xs: [a]) : a := foldl (+) 0 xs
```

**メソッドなしクラス定義**:
```egison
class Ring a extends AddGroup a where
  -- メソッドなしでも構文上は可能
```

**空メソッドインスタンス**（`where` は必要）:
```egison
instance Ring Integer where
  -- メソッドがなくても where があればパース可能
```

### 現在のクラス階層

```
Eq a                    (base.egi)
  └─ Ord a              (order.egi)
Num a                   (base.egi) ← スーパークラスなし
```

**Num のインスタンス**:
- `instance Num MathExpr` — `plusForMathExpr` 等の関数経由で実装
- `instance Num Float` — プリミティブ `f.+` 等で実装
- `instance Num Integer` — コメントアウト（MathExpr = Integer のため不要）

**Num を使う関数**:
- `def sum {Num a} (xs: [a]) : a` （`arithmetic.egi`）
- `def product {Num a} (xs: [a]) : a` （`arithmetic.egi`）

---

## 実装の不備

### 1. 複数スーパークラスのパースが未対応 → **解消済み**

(下の TODO 表 1-1 のとおり実装済み。`class MyRing a extends MyAdd a, MyMul a` が
パース・推論・ディスパッチまで通ることを実測確認 (2026-07-07)。歴史的記述として残す。)

**当時の現状**: パーサーが `extends` の後に単一のクラス名しか受け付けない。

**該当箇所**: `NonS.hs:294-295`
```haskell
superClassName <- upperId
superTypeArgs <- manyTill typeVarIdent (lookAhead (reserved "where"))
let constraints = [ConstraintExpr superClassName (map TEVar superTypeArgs)]
```

**必要な構文**:
```egison
class Ring a extends AddGroup a, MulMonoid a where ...
```

**影響**: AST（`ClassDecl.classSuperclasses :: [ConstraintExpr]`）と EnvBuilder は複数スーパークラスに対応済み。パーサーのみが制限。

### 2. スーパークラス制約の伝播が未実装 → **解消済み**

(下の TODO 表 1-2 のとおり `expandSuperclasses` で実装済み。歴史的記述として残す。)

**当時の現状**: `ClassInfo.classSupers` に格納されたスーパークラス情報が、型推論時に一切参照されていない。

**期待される動作**:
```egison
def f {Ord a} (x: a) (y: a) : Bool := x == y
-- Ord a → Eq a が自動で利用可能であるべき
-- 現在は {Ord a} だけでは (==) が使えない
```

**該当箇所**: `Infer.hs` の `addConstraints` は制約を蓄積するのみで、スーパークラスの展開を行わない。

### 3. `where` なしインスタンスがパースできない → **解消済み**

(下の TODO 表 1-3 のとおり実装済み。class/instance とも `where` は `option` になっており、
`class Marker a` + `instance Marker Integer` のマーカー運用が通ることを実測確認 (2026-07-07)。
歴史的記述として残す。)

**当時の現状**: `reserved "where"` が必須（`NonS.hs:342`）。

**必要な構文**:
```egison
instance Ring Integer   -- where なし（マーカーインスタンス）
```

**回避策**: `instance Ring Integer where` + 空のメソッドリストは動作する。設計ドキュメント側でこの記法を採用すれば実装変更は不要。

### 4. デフォルトメソッドが未使用（実装の不備で唯一残るもの）

**現状**: パーサーは `:=` によるデフォルト実装を読み取り、AST の `ClassMethod.methodDefault :: Maybe Expr` に格納するが、型推論・展開フェーズで使われない (2026-07-07 時点で parser/AST 以外に `methodDefault` の参照ゼロを確認)。

**実装計画 (据え置き、中規模)**: `Types.ClassInfo` は型しか持たないので (surface `Expr`
を Type 層に置くのは層違反)、Desugar が `ClassDeclExpr` を処理する際にデフォルト本体を
`EvalState` のマップ (メソッド名 → (クラス名, パラメータ, 本体 Expr)) に登録し、
`InstanceDeclExpr` の desugar (`makeDictDef`) で「インスタンスが提供しないメソッド」に
ついてこのマップから通常のインスタンスメソッドと同じ経路 (`desugarInstanceMethod`) で
定義を生成して辞書に詰める。辞書のキーが常に全メソッドを持つようになるので、
ディスパッチ時の欠落キー実行時エラーも同時に消える。型付けはインスタンス型での通常推論に
乗る。後続バッチのインスタンスにも効かせるため EvalState 永続が必要。

**将来必要になる場面**:
```egison
class AddGroup a extends AddMonoid a where
  neg (x: a) : a

-- (-) を AddGroup から自動導出
def (-) {AddGroup a} (x: a) (y: a) : a := x + neg y
```

デフォルトメソッドがなくてもトップレベル関数として定義すれば代用できるため、優先度は低い。

### 5. トップレベル def によるメソッド名 shadowing → **警告実装済み (2026-07-07)**

**現象**: `def one : GF4 := ...` のようにクラスメソッド名を素の def で再定義すると、
ディスパッチ束縛がプログラムの残り全体で置き換わり、**遠い場所の不可解な型エラー**
として現れる (GF(4) の作業中に発見した footgun)。

**対策**: クラス/インスタンス宣言は `IDefineMany` (レジストリ・ラッパー・辞書) に
降ろされ、素のメソッド名の `IDefine` を生成しない。よって「メソッド名と一致する
`IDefine` = 常にユーザーによる shadowing」が構造的に成立し、`inferITopExpr` が
`warnOnClassMethodShadow` で `ClassMethodShadowWarning` を出す
(`Type/Infer.hs` / `Type/Error.hs`、検収 mini-test/143)。

**警告が導入直後に検出した実物 2 件 (修正済み)**:

- `lib/core/order.egi` の min/max — Ord のメソッド宣言と総称 `def` の二重定義。
  呼び出し側はメソッド機構 (制約+メソッド名所有) で解決されており def は重複だった
  ため削除し、min/max は純粋な Ord メソッドに一本化 (全インスタンス+高階
  `foldl1 min` を実測)。
- `test/syntax.egi` の `def gcd` — GCDDomain のメソッドを shadow。しかもその
  「再帰」呼び出しは実際には Integer インスタンスへのディスパッチだった
  (自己参照ではなかった)。`euclid` に改名+シグネチャ付与。

### 6. 無注釈トップレベル再帰はシグネチャ必須 → **解消済み (2026-07-07、単相再帰)**

**当時の現状**: 型シグネチャのないトップレベル def の本体推論では自己名が
typeEnv に居らず、再帰呼び出しは `Unbound variable` 警告 + `Any` になり、実行時の
dispatch 失敗 (Pattern match failed) まで進みえた。`DefineWithType` は EnvBuilder
が署名を先に登録するので再帰できる — lib の再帰関数が全てシグネチャ付きなのは
このため。上記 gcd → euclid の改名で顕在化した (旧 `def gcd` はメソッドスキーム
経由で「たまたま」型が付いていた)。

**解消 (ML 流の単相再帰)**: `inferITopExpr` の IDefine 署名なし分岐が、本体推論の
**前に**自己名を fresh 型変数の単相スキーム `∀[]. t_rec` で束縛し、推論後に本体の
型と t_rec を単一化してから一般化する。`def euclid m n := ... euclid ...` が
警告なしに型付く (検収: test/syntax.egi の無注釈 euclid・mini-test/144)。付随する
明確化 2 点:

- **多相再帰** (本体内で自分を異なる型で使う) は単一化エラーになり、エラー文脈に
  「polymorphic recursion needs an explicit type signature」のヒントが付く
  (Haskell/OCaml と同じ扱い; permissive モードでは従来どおり無型評価へ
  フォールバックして実行は継続)。
- **前方参照** (無注釈の相互再帰を含む) は単相自己束縛では救えない (相手がまだ
  環境に居ない)。従来の generic な Unbound 警告の代わりに、参照先が同一ロード
  単位内の定義であることを検出して **ForwardReferenceWarning** を出す —
  「署名はプリパスで収集されるので、参照先に型シグネチャを付ければ前方参照は
  型付く」という修正方法つき (Eval が batch の定義名集合を
  `inferBatchDefNames` として推論状態に種付けする)。注釈付きの後方定義への
  前方参照は従来どおり無警告で型付く。

---

## Num クラス分割の計画

### 目標の階層

```
AddSemigroup a          (+)
  └─ AddMonoid a        zero
       └─ AddGroup a    neg

MulSemigroup a          (*)
  └─ MulMonoid a        one
       └─ MulGroup a    inv

Ring a = AddGroup a + MulMonoid a
Field a = Ring a + MulGroup a

GCDDomain a extends Ring a        gcd
EuclideanDomain a extends GCDDomain a    divMod
```

### 現在の Num との対応

```
現在の Num           →   新しい階層
─────────────────────────────────────
(+) : a -> a -> a    →   AddSemigroup.+
(-) : a -> a -> a    →   AddGroup.neg から導出（x + neg y）
(*) : a -> a -> a    →   MulSemigroup.*
(/) : a -> a -> a    →   Field（MulGroup.inv から導出）
```

### 移行の影響範囲

| 変更対象 | 内容 |
|---|---|
| `lib/core/base.egi` | `Num` クラスを階層に分割。互換のため `Num` をエイリアスとして残す案もあり |
| `lib/math/common/arithmetic.egi` | `{Num a}` → `{Ring a}` 等に変更。`sum`, `product` の制約を更新 |
| `instance Num MathExpr` | `instance AddSemigroup MathExpr`, `instance Ring MathExpr` 等に分割 |
| `instance Num Float` | 同上 |

---

## TODO（優先順）

### Phase 1: 型クラスの基盤強化

| # | 項目 | 該当ファイル | 状態 | 内容 |
|---|---|---|---|---|
| 1-1 | 複数スーパークラスのパース | `NonS.hs` | **完了** | `classHeader` を修正し、カンマ区切りの複数制約をパースできるようにした。インデントガードも追加 |
| 1-2 | スーパークラス制約の伝播 | `Infer.hs` | **完了** | `expandSuperclasses` を実装。`addConstraints` と `instConstraints` の両方でスーパークラスを再帰的に展開する |
| 1-3 | `where` なしインスタンス | `NonS.hs` | **完了** | `reserved "where"` を `option` にし、`where` なしマーカーインスタンス（`instance MyRing Integer`）をサポート |

### Phase 2: Num クラスの分割

| # | 項目 | 該当ファイル | 状態 | 内容 |
|---|---|---|---|---|
| 2-1 | 代数的型クラス階層の定義 | `lib/core/base.egi` | **完了** | `AddSemigroup` → `AddMonoid` → `AddGroup`、`MulSemigroup` → `MulMonoid` → `MulGroup`、`Ring`、`Field` を定義 |
| 2-2 | 既存インスタンスの分割 | `lib/core/base.egi` | **完了** | `Num MathExpr` / `Num Float` を各クラスのインスタンスに分割（8クラス×2型） |
| 2-3 | 制約の更新 | 全 `.egi` ライブラリ | **完了** | `{Num a}` を `{AddMonoid a}`, `{MulSemigroup a}`, `{Ring a}`, `{Field a}` 等の最小制約に変更 |
| 2-4 | `(-)` と `(/)` の導出 | `lib/core/base.egi` | **完了** | `(-) := x + neg y`、`(/) := x * inv y` をトップレベル関数として定義 |
| 2-5 | 0-arity メソッド対応 | `TypeClassExpand.hs` | **完了** | `zero`, `one` 等の定数メソッドが空ラムダにラップされる問題を修正。辞書アクセスを直接返すように変更 |

### Phase 2.5: Haskellスタイルのネスト辞書パッシングへの移行

| # | 項目 | 該当ファイル | 状態 | 内容 |
|---|---|---|---|---|
| 2.5-1 | 辞書にスーパークラス参照を追加 | `Desugar.hs` | **完了** | `makeDictDef` を修正し、各辞書に `("__super_ClassName", superDictExpr)` エントリを追加。マーカー型クラス（Ring, Field）でもスーパークラス参照を含む辞書を生成するようになった |
| 2.5-2 | 制約の脱展開 | `TypeClassExpand.hs` | **完了** | `deExpandConstraints` を実装。展開された制約 `{Field a, Ring a, AddGroup a, ...}` から最小セット `{Field a}` を算出する。`addDictionaryParametersT` 等で使用 |
| 2.5-3 | スーパークラスチェーンアクセス | `TypeClassExpand.hs` | **完了** | `findSuperclassPath` と `buildSuperclassChain` を実装。メソッドアクセスを `(dict_Field)_("__super_Ring")_("__super_MulMonoid")_("__super_MulSemigroup")_("times")` のようなチェーンに変換 |
| 2.5-4 | 辞書パラメータの最小化 | `TypeClassExpand.hs` | **完了** | `addDictionaryParametersT`, `checkConstrainedVariable`, `applyConcreteConstraintDictionaries` で脱展開した制約を使用。1制約につき1辞書パラメータのみ生成 |
| 2.5-5 | TITest/TIExecute の辞書適用 | `TypedDesugar.hs` | **完了** | `TITest` と `TIExecute` にも `applyConcreteConstraintDictionaries` を適用。制約が最小化されているため二重適用は発生しない |
| 2.5-6 | 辞書参照のリマッピング | `TypeClassExpand.hs` | **完了** | `remapDictRefsInBody` を実装。Step 1（expandTypeClassMethodsT）で展開済みの辞書参照名（dict_MulSemigroup等）を、Step 3（addDictionaryParametersT）の脱展開パラメータ名（dict_Field等）からのスーパークラスチェーンに置換 |

**設計の概要**:

以前のフラットな辞書パッシング（全スーパークラスを展開して並列に渡す）から、Haskellスタイルのネスト辞書パッシング（1制約につき1辞書、スーパークラスは辞書内の `__super_*` 参照で辿る）に移行した。

- 旧: `\dict_Field dict_Ring dict_AddGroup dict_AddMonoid dict_AddSemigroup dict_MulMonoid dict_MulSemigroup dict_MulGroup x y -> (dict_MulSemigroup)_("times") x ((dict_MulGroup)_("inv") y)`
- 新: `\dict_Field x y -> (dict_Field)_("__super_Ring")_("__super_MulMonoid")_("__super_MulSemigroup")_("times") x ((dict_Field)_("__super_MulGroup")_("inv") y)`

これにより以下の問題が解決された:
- Ring/Field のようなメソッドなしマーカー型クラスの辞書が存在しなかった問題
- 展開された制約の順序が定義と呼び出しで不一致になる問題
- TIDefine と TITest で辞書適用フェーズが異なり二重適用が起きる問題

### Phase 3: CAS 向け拡張

| # | 項目 | 該当ファイル | 状態 | 内容 |
|---|---|---|---|---|
| 3-1 | `GCDDomain` / `EuclideanDomain` 定義 | `lib/core/base.egi` | **完了** | `GCDDomain a extends Ring a` と `EuclideanDomain a extends GCDDomain a` を定義。導出関数 `modulo`, `quotient` も追加 |
| 3-2 | MathExpr インスタンス | `lib/core/base.egi` | **完了** | `gcd` は既存の `gcdForMathExpr` を使用、`divMod` は `i.quotient`/`i.modulo` を使用 |
| 3-3 | 既存 `gcd` のリネーム | `arithmetic.egi`, `root.egi`, `derivative.egi` | **完了** | 型クラスメソッド `gcd` との衝突を回避するため `gcdForMathExpr` にリネーム |
| 3-4 | `Poly` / `Div` 型のインスタンス | 新規 `.egi` | 未着手 | `type-cas.md` で設計した各インスタンスを実装 |
