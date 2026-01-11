# Declare Symbol Feature

## 概要

未定義変数に対する型推論の警告を抑制するため、トップレベルでシンボルを宣言する機能を実装しました。

## 構文

```egison
declare symbol <name1>, <name2>, ...
declare symbol <name1>, <name2>, ... : <Type>
```

### 例

```egison
-- デフォルト型（Integer）で宣言
declare symbol a11, a12, a21, a22

-- 明示的な型指定
declare symbol x, y, z : Float
```

## 実装詳細

### 1. AST への追加 (`Language/Egison/AST.hs`)

```haskell
data TopExpr
  = ...
  | DeclareSymbol [String] (Maybe TypeExpr)
```

### 2. IExpr への追加 (`Language/Egison/IExpr.hs`)

```haskell
data ITopExpr
  = ...
  | IDeclareSymbol [String] (Maybe Type)

data TITopExpr
  = ...
  | TIDeclareSymbol [String] Type
```

### 3. パーサーの追加 (`Language/Egison/Parser/NonS.hs`)

- `declare` を予約語に追加
- `symbol` は既存コードへの影響を避けるため予約語にせず、パーサー内でチェック
- `declareSymbolExpr` パーサー関数を追加

### 4. デシュガー処理 (`Language/Egison/Desugar.hs`)

- `DeclareSymbol` を `IDeclareSymbol` に変換
- デフォルト型は `TInt` (Integer/MathExpr)

### 5. 環境構築 (`Language/Egison/EnvBuilder.hs`)

- `processTopExpr` に `DeclareSymbol` のケースを追加
- 宣言されたシンボルを型環境に登録

### 6. 型推論 (`Language/Egison/Type/IInfer.hs`)

#### `InferState` の拡張

```haskell
data InferState = InferState
  { ...
  , declaredSymbols :: Map.Map String Type
  }
```

#### `inferITopExpr` の拡張

- `IDeclareSymbol` のケースを追加
- declared symbols マップと型環境の両方に登録

#### `lookupVar` の修正

- 型環境で見つからない場合、declared symbols をチェック
- declared symbols に登録されている場合は警告なしでその型を返す

#### `lookupVarWithConstraints` の修正

- `lookupVar` と同様の変更

### 7. 型付きデシュガー (`Language/Egison/Type/TypedDesugar.hs`)

- `desugarTypedTopExprT` に `TIDeclareSymbol` のケースを追加
- 特別な変換は不要（そのまま返す）

### 8. 評価器 (`Language/Egison/Eval.hs`)

- `evalTopExpr'` に `IDeclareSymbol` のケースを追加
- 実行時は何もしない（型推論時のみ使用）

## 使用例

### Before（警告あり）

```egison
def m1 : Tensor Integer := [| [| a11, a12 |], [| a21, a22 |] |]
def m2 : Tensor Integer := [| [| b11, b12 |], [| b21, b22 |] |]
```

実行時の警告：
```
Warning: Unbound variable 'a11' (assuming type 'Any')
Warning: Unbound variable 'a12' (assuming type 'Any')
...
```

### After（警告なし）

```egison
declare symbol a11, a12, a21, a22
declare symbol b11, b12, b21, b22

def m1 : Tensor Integer := [| [| a11, a12 |], [| a21, a22 |] |]
def m2 : Tensor Integer := [| [| b11, b12 |], [| b21, b22 |] |]
```

警告なしで実行可能。

## テスト

- テストファイル: `mini-test/91-index-notation.egi`
- 行列計算でシンボリック変数を使用する例をテスト
- 警告が出ないことを確認

## 設計上の選択

### シンボルと型のマッピング保持

`declaredSymbols` を `Map String Type` として実装し、各シンボルに対して宣言された型を保持します。これにより：

- 型指定が可能（`declare symbol x : Float`）
- デフォルト型は `Integer` (MathExpr)
- 型環境とは独立して管理

### `symbol` を予約語にしない

既存のコードで `symbol` という変数名が使われている可能性があるため、`symbol` を予約語にせず、パーサー内で `declare` の後に続く識別子が `symbol` かどうかをチェックする方式を採用しました。

## 今後の拡張可能性

1. **スコープ付きシンボル宣言**: ローカルスコープでのシンボル宣言
2. **型変数のサポート**: `declare symbol f : a -> a`
3. **制約付き宣言**: `declare symbol x : Num a => a`
4. **一括宣言**: `declare symbol x, y, z : Float` の簡略記法の拡張

## 関連ファイル

- `hs-src/Language/Egison/AST.hs`
- `hs-src/Language/Egison/IExpr.hs`
- `hs-src/Language/Egison/Parser/NonS.hs`
- `hs-src/Language/Egison/Desugar.hs`
- `hs-src/Language/Egison/EnvBuilder.hs`
- `hs-src/Language/Egison/Type/IInfer.hs`
- `hs-src/Language/Egison/Type/TypedDesugar.hs`
- `hs-src/Language/Egison/Eval.hs`
- `mini-test/91-index-notation.egi`
