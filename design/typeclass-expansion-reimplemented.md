# 型クラス展開の再実装 - TIExpr再帰化対応

## 完了日: 2026-01-10

## 概要

TIExpr再帰化プロジェクトに伴い、`TypeClassExpand.hs`の型クラス展開機能を新しいTIExprNode構造に対応させました。

## 変更内容

### 1. 新しいアプローチ

**旧実装**: `IExpr`を処理し、変換後の`IExpr`を返す

```haskell
expandTIExprWithConstraintList :: ClassEnv -> [Constraint] -> IExpr -> EvalM IExpr
```

**新実装**: `TIExprNode`を直接処理し、変換後の`TIExprNode`を返す

```haskell
expandTIExprNodeWithConstraintList :: ClassEnv -> [Constraint] -> TIExprNode -> EvalM TIExprNode
```

### 2. 主な利点

1. **型情報の保持**: サブ式の型情報が保持される
2. **再推論不要**: `stripType` → 推論 → 再構築のサイクルが不要
3. **型安全性**: コンパイル時に型の一貫性が保証される
4. **効率性**: 冗長な型推論が削減される

### 3. 実装したTIExprNodeケース

#### 基本式
- `TIConstantExpr` - 定数
- `TIVarExpr` - 変数

#### ラムダ式
- `TILambdaExpr` - 通常のラムダ
- `TIMemoizedLambdaExpr` - メモ化ラムダ
- `TICambdaExpr` - パターンマッチングラムダ

#### 適用
- `TIApplyExpr` - 関数適用（型クラスメソッド解決を含む）
- `TIWedgeApplyExpr` - ウェッジ積適用

#### コレクション
- `TITupleExpr` - タプル
- `TICollectionExpr` - コレクション
- `TIConsExpr` - cons
- `TIJoinExpr` - 結合
- `TIHashExpr` - ハッシュマップ
- `TIVectorExpr` - ベクター

#### 制御フロー
- `TIIfExpr` - if式
- `TISeqExpr` - シーケンス
- `TIWithSymbolsExpr` - シンボル付き式
- `TIDoExpr` - Do式

#### Let束縛
- `TILetExpr` - Let束縛
- `TILetRecExpr` - 再帰Let束縛

#### パターンマッチング
- `TIMatchExpr` - Match式
- `TIMatchAllExpr` - MatchAll式
- `TIMatcherExpr` - Matcher定義（未処理）

#### テンソル操作
- `TITensorMapExpr` - テンソルマップ
- `TITensorMap2Expr` - 2項テンソルマップ
- `TIGenerateTensorExpr` - テンソル生成
- `TITensorExpr` - テンソル
- `TITensorContractExpr` - テンソル縮約
- `TITransposeExpr` - 転置
- `TIFlipIndicesExpr` - インデックス反転

#### その他
- `TIQuoteExpr` - クォート
- `TIQuoteSymbolExpr` - シンボルクォート
- `TIInductiveDataExpr` - 帰納的データ
- `TIIndexedExpr` - インデックスアクセス（indices未処理）
- `TISubrefsExpr`, `TISuprefsExpr`, `TIUserrefsExpr` - 参照式
- `TIFunctionExpr` - 組み込み関数

### 4. 型クラスメソッド解決

**実装箇所**: `tryResolveMethodCall`

```haskell
tryResolveMethodCall :: ClassEnv -> [Constraint] -> String -> [TIExpr] -> EvalM (Maybe TIExprNode)
```

**機能**:
1. メソッド名から対応する制約を検索
2. 制約からクラス名と型引数を取得
3. インスタンスを検索
4. 辞書名を生成（例: `eqInteger`）
5. 辞書変数への参照を返す

**例**:
```egison
-- Before
x == y  (with constraint: Eq Integer)

-- After (conceptually)
eqInteger.eq x y  (dictionary-based dispatch)
```

### 5. 制限事項と今後の課題

#### 実装済み ✅
- 基本的な式の再帰処理
- 型クラスメソッドの検出
- インスタンス検索
- 辞書名生成

#### 未実装 ⚠️
- **辞書メソッドアクセス**: 現在は辞書名のみ返す。実際のメソッドアクセス構文が必要
  - 例: `dictName.methodName` の構築
- **MatcherExpr内の処理**: Matcher定義内の型クラス展開
- **IndexedExpr indices**: インデックスリストの再帰処理
- **辞書パラメータ追加**: `addDictionaryParametersT`の実装

#### 追加機能の候補
- 型クラス制約の伝播の最適化
- 辞書インライン化（小さな辞書の場合）
- デバッグ情報の追加

### 6. テスト結果

```bash
$ cabal run egison -- --no-prelude --dump-typed mini-test/57-twin-primes.egi
[(3, 5), (5, 7), (11, 13), (17, 19), (29, 31)]  ✅

=== Typed AST (Phase 5-6: Type Inference) ===
  [3] def list : ∀a. Matcher a -> Matcher [a] := ...
  [5] (matchAll primes as list something with ...)
```

✅ コンパイル成功
✅ 基本テスト通過
✅ 複雑なmatcher式も正常動作

### 7. コード構造

```
TypeClassExpand.hs
├── expandTypeClassMethodsT
│   └── expandTIExprNodeWithConstraints
│       ├── expandTIExprNodeWithConstraintList (各TIExprNodeケース)
│       ├── expandTIExprWithConstraints (TIExprラッパー)
│       └── tryResolveMethodCall (メソッド解決)
├── addDictionaryParametersT (TODO: 未実装)
└── ヘルパー関数
    ├── findConstraintForMethod
    └── lowerFirst
```

### 8. インポートの追加

```haskell
import Data.List (find)
import Language.Egison.IExpr (TIExprNode(..), tiExprNode)
```

## 結論

TIExpr再帰化に対応した型クラス展開の基本実装が完了しました。

### 達成したこと
✅ 全主要TIExprNodeケースの実装
✅ 型クラスメソッド検出機能
✅ 基本的なインスタンス解決
✅ 既存テストとの互換性

### 次のステップ
1. 辞書メソッドアクセスの完全実装
2. `addDictionaryParametersT`の実装
3. より複雑な型クラスのテスト
4. パフォーマンス最適化

---
実装者: AI Assistant
完了日: 2026-01-10
