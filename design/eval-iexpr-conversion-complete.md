# Eval.hs IExpr to TIExpr 変換関数の完全実装

## 完了日: 2026-01-10

## 概要

`Eval.hs`の`iexprToTIExprNodeSimple`関数に、全ての主要なIExprコンストラクタのケースを追加しました。

## 追加したケース

### Collections
- `IConsExpr` - リストのcons
- `IJoinExpr` - リストの結合
- `IHashExpr` - ハッシュマップ
- `IVectorExpr` - ベクター

### Functions
- `IMemoizedLambdaExpr` - メモ化ラムダ
- `ICambdaExpr` - パターンマッチングラムダ

### Control Flow
- `ISeqExpr` - シーケンス
- `ILetRecExpr` - 再帰的let束縛

### Pattern Matching
- `IMatchExpr` - match式
- `IMatchAllExpr` - matchAll式 ⭐️ (twin-primesで必要だったケース)
- `IMatcherExpr` - matcher定義（簡易版）

### Inductive Data
- `IInductiveDataExpr` - 帰納的データ構築子

### Tensor Operations
- `IGenerateTensorExpr`
- `ITensorExpr`
- `ITensorContractExpr`
- `ITensorMapExpr`
- `ITensorMap2Expr`
- `ITransposeExpr`
- `IFlipIndicesExpr`

### Other
- `IWithSymbolsExpr` - シンボル付き式
- `IDoExpr` - Do式
- `IQuoteExpr` - クォート
- `IQuoteSymbolExpr` - シンボルクォート
- `IWedgeApplyExpr` - ウェッジ積適用
- `IFunctionExpr` - 組み込み関数参照

### Indexed Expressions
- `IIndexedExpr` - インデックスアクセス
- `ISubrefsExpr` - 下付き参照
- `ISuprefsExpr` - 上付き参照
- `IUserrefsExpr` - ユーザー定義参照

## テスト結果

### ✅ 成功したテスト

1. `mini-test/01-simple-constant.egi` - 定数
2. `mini-test/02-simple-function.egi` - 関数
3. `mini-test/05-list.egi` - リスト
4. `mini-test/08-if.egi` - if式
5. `mini-test/57-twin-primes.egi` - matchAll式 ⭐️

### 実行例

```bash
$ cabal run egison -- --no-prelude --dump-typed -t mini-test/57-twin-primes.egi
=== Typed AST (Phase 5-6: Type Inference) ===
  [5] (matchAll primes as list something with
    | ++ _ (:: p (:: (#(+ p 2)) _)) -> (p, + p 2) : [(Integer, Integer)])
=== End of Typed AST ===
```

## 制限事項

1. **MatcherExpr**: データ節の変換を省略（空リスト）
   - 完全な実装には`[(IPrimitiveDataPattern, IExpr)]`を`[TIBindingExpr]`に変換する必要がある
   
2. **IndexedExpr**: インデックスは`IExpr`のまま
   - 将来的に`Index TIExpr`に変更する必要がある

## コード統計

- **追加したケース数**: 約30個
- **変更行数**: 約100行

## 次のステップ

1. MatcherExprのデータ節を完全に変換
2. IndexedExprのインデックスをTIExpr化
3. より複雑なテストケースでの検証

## 結論

主要なIExprコンストラクタの全てに対応し、matchAll式を含む複雑なパターンマッチングも動作することを確認しました。

---
実装者: AI Assistant
完了日: 2026-01-10
