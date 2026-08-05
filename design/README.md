# Design Documentation

このディレクトリには、Egisonインタプリタの設計ドキュメントが含まれています。

## 📁 ファイル一覧

### 主要ドキュメント

- **`implementation.md`** - 全体の実装設計と処理フロー
- **`FILE_MAPPING.md`** - ファイルとフェーズの対応表

### 型システム関連

- **`pattern.md`** - パターンマッチング設計
- **`pattern-function-implementation.md`** - パターン関数の canonical `DualScheme` 設計と実装詳細
- **`type-tensor.md`** - テンソル型システム
- **`type-tensor-simple.md`** - テンソル型システム（簡易版）

### CAS 型システム

- **`type-cas.md`** - CAS 型システム設計（メイン、実装到達点と未解決課題も末尾に統合）
- **`type-cas-tower.md`** - ユーザ拡張可能な CAS タワー（将来構想）

## 🎯 クイックスタート

### 実装について知りたい

1. **全体の流れ** → `implementation.md`
2. **ファイル構成** → `FILE_MAPPING.md`

### 型システムについて知りたい

1. **パターンマッチング** → `pattern.md`（仕様）
2. **パターン関数の実装** → `pattern-function-implementation.md`（実装詳細）
3. **テンソル型** → `type-tensor-simple.md`（まずはこちら）

### CAS 型システムについて知りたい

1. **設計と実装到達点** → `type-cas.md` (末尾の「既知の制限と未解決課題」も参照)
2. **将来のタワー拡張構想** → `type-cas-tower.md`

## 📝 最近の更新

### 2026-08-05: パターン関数を二-sort設計へ同期

- 引数と結果の capability/target を一つの canonical `DualScheme` に保存
- 適用時に capability binders と target binders を同時に fresh instantiate
- header-only の前方・相互参照，expression-headed application，および body に残る非 core パターン形式を outside-core warning で明示（直接自己参照は定義時に拒否）
- 再定義時は古い finalized scheme を先に無効化し，同一 load unit 内の重複宣言は拒否
- runtime は引き続き `IPatternFuncExpr` と `recursiveBind` による統一的な環境 capture を使用
- 詳細は `pattern-function-implementation.md` と `type-pm-compatibility.md` を参照

### 2026-08-05: Egison core 境界 warning を用途別に分離

- 一般の core 外拡張は `--outside-egison-core-warnings` で報告
- primitive-pattern pattern を DFS 左から右へ走査して `$` より後に `#$x` があれば，`--pattern-hole-before-primitive-value-pattern-warnings` で報告
- nested structured primitive-pattern pattern は `--nested-structured-primitive-pattern-pattern-warnings` で独立に報告
- 同じ matcher atom の左 hole が束縛する変数を user value pattern が参照する場合は，従来どおり hard type error
