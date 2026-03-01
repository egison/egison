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

### 1. 複数スーパークラスのパースが未対応

**現状**: パーサーが `extends` の後に単一のクラス名しか受け付けない。

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

### 2. スーパークラス制約の伝播が未実装

**現状**: `ClassInfo.classSupers` に格納されたスーパークラス情報が、型推論時に一切参照されていない。

**期待される動作**:
```egison
def f {Ord a} (x: a) (y: a) : Bool := x == y
-- Ord a → Eq a が自動で利用可能であるべき
-- 現在は {Ord a} だけでは (==) が使えない
```

**該当箇所**: `Infer.hs` の `addConstraints` は制約を蓄積するのみで、スーパークラスの展開を行わない。

### 3. `where` なしインスタンスがパースできない

**現状**: `reserved "where"` が必須（`NonS.hs:342`）。

**必要な構文**:
```egison
instance Ring Integer   -- where なし（マーカーインスタンス）
```

**回避策**: `instance Ring Integer where` + 空のメソッドリストは動作する。設計ドキュメント側でこの記法を採用すれば実装変更は不要。

### 4. デフォルトメソッドが未使用

**現状**: パーサーは `:=` によるデフォルト実装を読み取り、AST の `ClassMethod.methodDefault :: Maybe Expr` に格納するが、型推論・展開フェーズで使われない。

**将来必要になる場面**:
```egison
class AddGroup a extends AddMonoid a where
  neg (x: a) : a

-- (-) を AddGroup から自動導出
def (-) {AddGroup a} (x: a) (y: a) : a := x + neg y
```

デフォルトメソッドがなくてもトップレベル関数として定義すれば代用できるため、優先度は低い。

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

### Phase 3: CAS 向け拡張

| # | 項目 | 該当ファイル | 内容 |
|---|---|---|---|
| 3-1 | `GCDDomain` / `EuclideanDomain` 定義 | 新規 `.egi` | CAS 用の型クラスを追加 |
| 3-2 | `Poly` / `Div` 型のインスタンス | 新規 `.egi` | `type-cas.md` で設計した各インスタンスを実装 |
