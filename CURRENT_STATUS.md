# 現在のビルド状態と次のアクション（方針修正版）

**最終更新**: 2026年1月6日  
**重要**: 方針変更により、`TypedIExpr`は不要。既存の`TIExpr`を使用する。

## 🚦 現在の状態

### ビルド: ❌ 失敗

```bash
$ cabal build
# エラー: TypedDesugar.hs と Check.hs に旧API参照
```

### 完成度: 約 60% → 方針修正が必要

- ✅ 型推論モジュール実装完了 (`IInfer.hs`)
- ⚠️ **方針変更**: `TypedIAST.hs`は不要（削除予定）
- ⚠️ `IInfer.hs`の戻り値を`(Type, Subst)`に修正必要
- ✅ 古いファイル削除完了 (`Infer.hs`, `TypeInfer.hs`, `TypedAST.hs`)
- ⚠️ パイプライン修正必要（`TIExpr`を直接使用）

## 📋 次に実行すべきコマンド

### 1. エラー確認
```bash
cd /Users/egisatoshi/egison
cabal build 2>&1 | grep -A 3 "error:"
```

### 2. エラー箇所の一覧取得
```bash
# TypedDesugar.hs のエラー
grep -n "TDefine\|TDefineWithType\|TTest\|TExecute" \
  hs-src/Language/Egison/Type/TypedDesugar.hs

# Check.hs のエラー  
grep -n "FileLoader\|cfgFileLoader" \
  hs-src/Language/Egison/Type/Check.hs
```

## 🔧 修正すべきファイル

### 1. `hs-src/Language/Egison/Type/TypedDesugar.hs`

**問題**: 旧`TypedTopExpr`（Exprベース）を参照

**必要な変更**:
```haskell
-- 旧
import Language.Egison.Type.TypedAST
desugarTypedTopExprT (TDefine name expr) = ...
desugarTypedTopExprT (TDefineWithType name ...) = ...

-- 新
import Language.Egison.Type.TypedIAST  
desugarTypedTopExprT (TypedIDefine name constraints ty expr) = ...
desugarTypedTopExprT (TypedITest expr) = ...
```

**変更箇所**: 約20箇所のパターンマッチ

### 2. `hs-src/Language/Egison/Type/Check.hs`

**問題**: `FileLoader`と`cfgFileLoader`への参照

**オプションA**: `FileLoader`を削除
```haskell
-- 削除する行
, cfgFileLoader = Nothing

-- 関数シグネチャから削除
typeCheckWithLoader :: TypeCheckConfig -> FileLoader -> ...
```

**オプションB**: `FileLoader`を`IInfer.hs`に追加
```haskell
-- IInfer.hs に追加
type FileLoader = FilePath -> IO (Either String [TopExpr])

-- InferConfig に追加
data InferConfig = InferConfig
  { ...
  , cfgFileLoader :: Maybe FileLoader
  }
```

**推奨**: オプションA（シンプル）

## 📝 詳細な修正手順（方針修正版）

### Step 0: 方針変更の理解 ⚠️ 重要

**新しい方針**:
- `TypedIExpr`（`TypedIAST.hs`）は不要 → 削除
- 既存の`TIExpr`（`IExpr.hs`に定義済み）を使用
- `IInfer.hs`は`(Type, Subst)`のみ返す（TypedIExprを生成しない）

詳細は`REFACTORING_IEXPR_V2.md`を参照。

### Step 1: IInfer.hsの修正

**修正内容**:
1. 戻り値の型を変更:
   ```haskell
   -- 旧
   inferIExpr :: IExpr -> Infer (TypedIExpr, Subst)
   
   -- 新
   inferIExpr :: IExpr -> Infer (Type, Subst)
   ```

2. `TypedIAST.hs`へのimportを削除

3. `toTypedIExpr`などのヘルパー関数を削除

### Step 2: TypedIAST.hsの削除

**修正内容**:
1. ファイル削除: `hs-src/Language/Egison/Type/TypedIAST.hs`
2. `egison.cabal`から削除
3. すべての参照を削除

### Step 3: Eval.hsの修正

**修正内容**:
1. 型推論結果を`(Type, Subst)`として受け取る
2. `IExpr + Type → TIExpr`変換関数を追加
3. `TypedDesugar`に`TIExpr`を渡す

### Step 4: TypedDesugar.hsの修正

**修正内容**:
1. 入力型を`TITopExpr`に変更
2. テンソルDesugarの確認
3. TypeClassExpand呼び出しを追加

### Step 5: Check.hsの修正

```bash
# ファイルを開く
open hs-src/Language/Egison/Type/Check.hs
```

**修正内容**:
1. `InferConfig`の初期化から`cfgFileLoader`を削除:
   ```haskell
   -- 旧
   defaultInferConfig { cfgFileLoader = Nothing }
   
   -- 新
   defaultInferConfig
   ```

2. `FileLoader`を使用する関数を削除または修正

### Step 3: ビルドテスト

```bash
cd /Users/egisatoshi/egison
cabal clean
cabal build
```

### Step 4: 機能テスト

```bash
# 型推論テスト
cabal run egison -- --dump-typed --no-prelude -t mini-test/20-dump-typed.egi

# 標準ライブラリの型チェック
cabal run egison -- --load-only lib/core/base.egi
```

## 📊 修正の影響範囲

### 低リスク
- ✅ `Check.hs`: 2箇所の削除のみ
- ✅ import文の変更: 機械的な置換

### 中リスク  
- ⚠️ `TypedDesugar.hs`: 20箇所のパターンマッチ変更
- ⚠️ データ構造の違いによる調整が必要な可能性

### 高リスク
- ❌ なし（既存の動作コードには影響なし）

## 🎯 成功の定義

### 最小限の成功
1. ✅ `cabal build`が成功
2. ✅ エラーなく起動
3. ✅ `--dump-typed`が動作

### 完全な成功  
1. ✅ 最小限の成功
2. ✅ 既存テストがパス
3. ✅ `lib/core/base.egi`が型チェック通過
4. ✅ 型クラス機能が動作（`TypedDesugar`完成後）

## 📚 参考資料

- `REFACTORING_IEXPR.md`: 詳細な進捗報告
- `design/implementation.md`: 全体設計
- `design/FILE_MAPPING.md`: ファイル対応表

## ⏱️ 推定所要時間（方針修正版）

- IInfer.hs修正: 1-2時間
- TypedIAST.hs削除: 30分
- Eval.hs修正: 1-2時間
- TypedDesugar.hs修正: 2-3時間
- Check.hs修正: 30分
- テスト: 1-2時間
- **合計**: 6.5-10.5時間

---

**次回セッション開始時**: このファイルを読んでから作業開始

