# Design Documentation

このディレクトリには、Egisonインタプリタの設計ドキュメントが含まれています。

## 📁 ファイル一覧

### 主要ドキュメント

- **`implementation.md`** - 全体の実装設計と処理フロー
- **`FILE_MAPPING.md`** - ファイルとフェーズの対応表

### 型システム関連

- **`pattern.md`** - パターンマッチング設計
- **`pattern-function-implementation.md`** - パターン関数実装詳細 ✨ NEW (2026-02-08)
- **`type-tensor.md`** - テンソル型システム
- **`type-tensor-simple.md`** - テンソル型システム（簡易版）

## 🎯 クイックスタート

### 実装について知りたい

1. **全体の流れ** → `implementation.md`
2. **ファイル構成** → `FILE_MAPPING.md`

### 型システムについて知りたい

1. **パターンマッチング** → `pattern.md`（仕様）
2. **パターン関数の実装** → `pattern-function-implementation.md`（実装詳細）
3. **テンソル型** → `type-tensor-simple.md`（まずはこちら）

## 📝 最近の更新

### 2026-02-08: パターン関数の実装完了 ✅
- パターン関数の型推論を実装
- 型クラス展開のサポート（パターン内で`+`などの演算子が使用可能に）
- 環境管理の統一化（`recursiveBind`で一括処理）
- 詳細は `pattern-function-implementation.md` と `COMPLETED.md` を参照

