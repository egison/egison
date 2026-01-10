# 辞書渡し実装の進捗

## 実装日: 2026-01-10

## 完了した作業

### 1. 制約の収集と保存 ✅
**ファイル**: `hs-src/Language/Egison/EnvBuilder.hs`

**変更内容**:
- `DefineWithType` を処理する際に、`typedVarConstraints` を型スキームに含めるように修正
- 修正前: `Types.Forall freeVars [] funType`
- 修正後: `Types.Forall freeVars constraints funType`

**効果**:
```
def double : ∀a. Num a => a -> a
```
制約が型スキームに正しく保存され、Pretty printing で表示されるようになりました。

### 2. 制約を使ったメソッド解決の基本実装 ✅
**ファイル**: `hs-src/Language/Egison/Type/TypeClassExpand.hs`

**変更内容**:
- `expandTypeClassMethodsT` を拡張して、型スキームから制約を取得
- `expandTIExprWithConstraints` 関数を追加
- 制約付き関数の本体を処理する際に、制約情報を伝播

**効果**:
トップレベルのメソッド呼び出しは正しく解決されます：
```egison
2 + 2  →  numIntegerPlus 2 2  →  4  ✅
```

## 未解決の問題

### Lambda式内のメソッド呼び出し ❌

**問題**:
```egison
def double {Num a} (x: a) : a := x + x
double 2  →  + 2 2  (numIntegerPlus 2 2 にならない)
```

**原因**:
1. **型推論のスコープ問題**: Lambda式 `\x -> + x x` の中で `x` の型を推論しようとすると、`x` が型環境に含まれていないため `UnboundVariable` エラーになる
2. **評価タイミング**: `double 2` は評価時に `+ 2 2` に展開されるが、TypeClassExpand は評価前に実行されるため、評価時に生成される式は処理されない

**デバッグ出力**:
```
DEBUG: Method +, matching constraint: Just (Constraint {constraintClass = "Num", constraintType = TVar (TyVar "a")})
DEBUG: Arg type inference result: Left (UnboundVariable "x" ...)
```

## 解決策の検討

### アプローチA: 完全な辞書渡し（推奨）

制約付き関数を辞書パラメータを受け取る関数に変換する。

**変換例**:
```haskell
-- 元のコード
def double {Num a} (x: a) : a := x + x

-- 変換後の内部表現
def double := \dict_Num_a x -> (dict_Num_a."plus") x x

-- 呼び出し
double 2  →  double numInteger 2
```

**必要な変更**:
1. **Desugar段階**: 制約付き関数定義を辞書パラメータ化
   - `DefineWithType` の処理を変更
   - 制約を辞書パラメータに変換
   - メソッド呼び出しを辞書アクセスに変換

2. **TypeClassExpand段階**: 関数適用時に辞書を挿入
   - 制約付き関数の呼び出しを検出
   - 適切な辞書（インスタンス）を引数として挿入

3. **評価段階**: 辞書のサポート
   - 辞書をレコードとして実装
   - フィールドアクセス演算子のサポート

**利点**:
- 完全な多相性をサポート
- 型安全
- Haskell/GHCと同じアプローチ

**欠点**:
- 実装が複雑
- 大規模な変更が必要

### アプローチB: 関数のインライン展開

関数呼び出しサイトで関数をインライン展開してから、メソッド呼び出しを解決する。

**変換例**:
```haskell
-- 元のコード
double 2

-- ステップ1: インライン展開
(\x -> + x x) 2

-- ステップ2: Beta簡約
+ 2 2

-- ステップ3: メソッド解決
numIntegerPlus 2 2
```

**問題**:
- Beta簡約は評価器の仕事であり、TypeClassExpand段階では実行されない
- 評価時に生成される式はTypeClassExpandを通過しない

**可能な解決策**:
- 評価器にメソッド解決機能を組み込む
- または、TypeClassExpand時に簡単なBeta簡約を実行する

### アプローチC: 評価器の拡張

評価器がメソッド呼び出しを動的に解決する。

**利点**:
- TypeClassExpandの変更が最小限

**欠点**:
- 実行時オーバーヘッド
- 型情報が評価時に必要

## 次のステップ

### 短期（フェーズ1完了）
1. ✅ 制約の収集と保存
2. ✅ トップレベルのメソッド解決
3. ❌ Lambda式内のメソッド解決

### 中期（フェーズ2）
完全な辞書渡しの実装：

1. **Desugar段階の拡張**
   - `DefineWithType` で制約を辞書パラメータに変換
   - メソッド呼び出しを辞書アクセスに変換

2. **TypeClassExpand段階の拡張**
   - 関数適用時に辞書を挿入
   - インスタンス辞書の選択

3. **評価段階の拡張**
   - 辞書のサポート（既存のHashを使用可能）

### 長期（フェーズ3）
最適化：
- 辞書のインライン化
- 特殊化（具体型の場合は辞書渡しを省略）

## テスト結果

### 動作するケース ✅
```egison
class Num a where
  (+) (x: a) (y: a) : a

instance Num Integer where
  (+) x y := (b.+ x y)

2 + 2  -- 結果: 4 ✅
```

### 動作しないケース ❌
```egison
def double {Num a} (x: a) : a := x + x
double 2  -- 結果: + 2 2 (期待: 4) ❌
```

## 技術的な詳細

### 型スキームの構造
```haskell
data TypeScheme = Forall [TyVar] [Constraint] Type

-- 例: double の型
Forall ["a"] [Constraint "Num" (TVar "a")] (TFun (TVar "a") (TVar "a"))
```

### 制約の構造
```haskell
data Constraint = Constraint
  { constraintClass :: String  -- "Num"
  , constraintType  :: Type    -- TVar "a"
  }
```

### デバッグ出力の解釈
```
DEBUG: Expanding with constraints: [Constraint {constraintClass = "Num", constraintType = TVar (TyVar "a")}]
```
- 制約は正しく伝播している

```
DEBUG: Arg type inference result: Left (UnboundVariable "x" ...)
```
- Lambda式内の変数 `x` が型環境にない

```
DEBUG: Resolved to: numIntegerPlus
```
- トップレベルのメソッド呼び出しは正しく解決される

## 参考資料

- `design/dictionary-passing-plan.md`: 実装計画
- `design/FILE_MAPPING.md`: ファイルマッピング
- `design/implementation.md`: 処理フロー

## まとめ

制約の収集と保存は完了し、トップレベルのメソッド呼び出しは動作しています。しかし、Lambda式内のメソッド呼び出しを解決するには、完全な辞書渡しの実装が必要です。これは大規模な変更を伴いますが、Egisonの型クラスシステムを完全にするために不可欠です。

次の実装セッションでは、アプローチAの完全な辞書渡しを実装することを推奨します。
