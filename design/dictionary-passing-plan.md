# 辞書渡し実装計画

## 現状

### 実装済み
- ✅ 制約の収集と型スキームへの保存 (IInfer.hs, EnvBuilder.hs)
- ✅ 単純なメソッド呼び出しの置き換え (TypeClassExpand.hs)
- ✅ 辞書の生成 (Desugar.hs - ClassDeclExpr, InstanceDeclExpr)

### 未実装
- ❌ 制約付き関数の辞書パラメータ化
- ❌ 関数適用時の辞書渡し
- ❌ メソッド呼び出しの辞書アクセスへの変換

## 問題の例

```egison
class Num a where
  (+) (x: a) (y: a) : a

instance Num Integer where
  (+) x y := (b.+ x y)

def double {Num a} (x: a) : a := x + x

double 2  -- 現在: 評価されない（+ 2 2 のまま）
          -- 期待: 4
```

## 辞書渡しの変換

### ステップ1: 制約を辞書パラメータに変換

```egison
-- 元のコード
def double {Num a} (x: a) : a := x + x

-- 変換後（内部表現）
def double (dict_Num_a : Dict_Num a) (x: a) : a 
  := dict_Num_a."plus" x x
```

### ステップ2: 関数適用時に辞書を渡す

```egison
-- 元のコード
double 2

-- 変換後（内部表現）
double numInteger 2
-- ここで numInteger は Num Integer のインスタンス辞書
```

## 実装アプローチ

### アプローチA: 完全な辞書渡し（Haskell/GHC方式）

**利点**:
- 完全な多相性をサポート
- 型安全
- 将来の最適化が可能

**欠点**:
- 実装が複雑
- 大規模な変更が必要
- IExprに辞書パラメータの概念を追加する必要がある

### アプローチB: 単相化（Monomorphization）

**利点**:
- 実装が比較的簡単
- 既存のコードへの変更が少ない

**欠点**:
- 多相関数が使用される型ごとに複製される
- コードサイズが増加

### アプローチC: ハイブリッド

**利点**:
- 段階的に実装できる
- 具体型の場合は最適化、多相の場合は辞書渡し

**欠点**:
- 複雑性が増す

## 推奨アプローチ: 段階的実装

### フェーズ1: 具体型の単相化（短期）

具体的な型引数の場合のみ動作させる：

```egison
double 2  -- Integer に特殊化
```

実装箇所:
- TypeClassExpand.hs の `expandTypeClassMethodsT` を拡張
- 関数適用時に型を調べ、具体型の場合はインスタンスメソッドに直接置き換え

### フェーズ2: 辞書渡しの基本実装（中期）

制約付き関数を辞書パラメータ化：

実装箇所:
1. IExpr に辞書パラメータの概念を追加（または既存のlambdaで表現）
2. TypeClassExpand.hs で制約を辞書引数に変換
3. 関数適用時に辞書を挿入

### フェーズ3: 最適化（長期）

- 辞書のインライン化
- 特殊化（具体型の場合は辞書渡しを省略）
- Higher-order辞書（型クラスの型クラス）

## フェーズ1の詳細設計

### 変換例

```egison
-- ソースコード
def double {Num a} (x: a) : a := x + x
double 2

-- Desugar後の IExpr (現在)
def double := \x -> + x x
double 2

-- TypeClassExpand後の IExpr (目標)
def double := \x -> case typeof x of
  Integer -> numIntegerPlus x x
  Float -> numFloatPlus x x
  ...

double 2
-- ↓
numIntegerPlus 2 2
```

### 実装ステップ

1. **型情報の取得**: TIExpr から関数の制約と引数の型を取得
2. **インスタンス解決**: ClassEnv から適切なインスタンスを検索
3. **メソッド呼び出しの置き換え**: `+` を `numIntegerPlus` に置き換え

### 実装ファイル

- `TypeClassExpand.hs`:
  - `expandConstrainedFunction :: TIExpr -> EvalM TIExpr`
  - `resolveMethodCall :: ClassEnv -> Constraint -> String -> EvalM IExpr`

## 参考: 既存のDesugar実装

現在、型クラス宣言から以下が生成されます：

```egison
class Num a where
  (+) (x: a) (y: a) : a

-- ↓ Desugar時に生成
registryNum := {| |}
classNumPlus := \dict x y -> dict."plus" x y

instance Num Integer where
  (+) x y := (b.+ x y)

-- ↓ Desugar時に生成
numInteger := {| ("plus", numIntegerPlus) |}
numIntegerPlus := \x y -> (b.+ x y)
```

これらを活用して、メソッド呼び出しを解決します。

## 実装の優先順位

1. **高**: フェーズ1 - 具体型の単相化（mini-test/83を動作させる）
2. **中**: フェーズ2 - 辞書渡しの基本実装
3. **低**: フェーズ3 - 最適化

## テストケース

### テスト1: 単純な制約付き関数
```egison
class Num a where
  (+) (x: a) (y: a) : a

instance Num Integer where
  (+) x y := (b.+ x y)

def double {Num a} (x: a) : a := x + x

double 2  -- 期待: 4
```

### テスト2: 複数の制約
```egison
def compare {Eq a, Ord a} (x: a) (y: a) : Bool 
  := x == y && x < y

compare 1 2  -- 期待: False
```

### テスト3: 多相リスト関数
```egison
def sum {Num a} (xs: [a]) : a 
  := foldl (+) 0 xs

sum [1, 2, 3]  -- 期待: 6
```
