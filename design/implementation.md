## 処理フロー

```
TopExpr (構文解析結果)
  ↓
expandLoads (ファイル読み込み)
  - load文で指定されたファイルの読み込み
  - ファイルの存在チェック
  - 同じファイルの重複読み込み防止（キャッシング）
  - (将来: モジュールシステム導入時は依存関係の解決と循環参照チェックも必要)
  ↓
環境構築フェーズ
  ├─ データコンストラクタ定義収集
  ├─ 型クラス定義収集  
  ├─ インスタンス定義収集
  └─ 型シグネチャ収集
  ↓
Desugar (構文糖衣展開)
  ├─ 演算子の脱糖
  ├─ 無名関数の展開 (1#($1 + $2) など)
  ├─ Match-lambda の展開 (matchAll 式への変換)
  └─ その他の糖衣構文展開
  注: パターンマッチ自体はEval段階で処理される
  ↓
型推論フェーズ
  ├─ 制約生成 (型変数の割り当て)
  ├─ 制約解決 (Unification)
  └─ 型クラス制約の処理
  ↓
型チェックフェーズ
  ├─ 型注釈との整合性確認
  └─ 型クラス制約の充足性確認
  ↓
TypedTopExpr (型付きAST)
  ↓
TypedDesugar (型駆動の変換)
  ├─ 型クラス辞書渡し変換
  │   - インスタンス選択
  │   - メソッド呼び出しの具体化
  ├─ tensorMap 自動挿入
  │   - Tensor MathExpr と MathExpr の不一致検出
  │   - 適切な位置に tensorMap を挿入
  └─ 型情報の最適化・埋め込み
  ↓
TITopExpr (評価可能な型付きIR)
  ↓
評価 (Evaluation)
  - パターンマッチングの実行 (patternMatch関数)
  - 式の評価と値の計算
  - IOアクションの実行
```

## 各段階で発生しうるエラー

### expandLoads
- ファイルが見つからない
- (将来: モジュールシステム導入時は循環参照エラーも)

### 環境構築
- 重複定義
- 型クラスの循環継承
- インスタンスの重複

### Desugar
- 不正な糖衣構文
- 不正な演算子

### 型推論
- 型制約が解決できない
- 無限型
- 多相再帰の型推論失敗

### 型チェック
- 型注釈と推論結果の不一致
- 型クラス制約が満たされない

### TypedDesugar
- インスタンスが見つからない
- tensorMap 挿入失敗（不正なテンソル操作）

### 評価 (Evaluation)
- パターンマッチの失敗
- 未定義変数の参照
- 型エラー（実行時）
- ゼロ除算などの算術エラー
- IOエラー

## 段階的実装の戦略

### Phase 1: 基本的な型チェック
```
TopExpr → Desugar → 単純な型チェック → 評価
```
- 型クラスなし
- tensorMapなし
- 基本的な型のみ (Integer, Bool, String, [a], (a, b) など)

### Phase 2: 型推論の追加
```
TopExpr → Desugar → 型推論 → 型チェック → 評価
```
- Hindley-Milner型推論
- まだ型クラスなし
- let多相の実装

### Phase 3: 型クラスの追加
```
TopExpr → Desugar → 型推論+制約 → TypedDesugar(辞書渡し) → 評価
```
- 型クラス制約の推論
- 辞書渡しの実装
- Eq, Ord, Num などの基本型クラス

### Phase 4: tensorMapの自動挿入
```
完全なフロー (TypedDesugarでtensorMapも処理)
```
- Tensor型の処理
- 自動tensorMap挿入
- テンソル添字記法のサポート

## 中間表現 (IR) の設計

各段階で適切な中間表現を定義：

```haskell
-- 構文解析後
data TopExpr = ...

-- Desugar後 (糖衣構文が展開されている)
data DesugaredExpr = ...

-- 型推論後 (型注釈が付いている)
data TypedExpr = ...

-- TypedDesugar後 (実行可能形式)
data TIExpr = ...  -- Type-Inferred Expression
```

## 型情報の保持戦略

### TypedDesugar後も型情報を保持する

TypedDesugar後の中間表現（TIExpr）は**型情報を保持**します。

#### 理由

1. **より良いエラーメッセージ**
   - パターンマッチ失敗時に期待される型と実際の値の型を表示できる
   - テンソル計算での次元不一致を明確に報告できる
   
2. **デバッグの容易性**
   - 実行時の型情報により、問題の特定が容易
   - スタックトレースに型情報を含められる

3. **Egisonの特性に適合**
   - パターンマッチング中心の言語として、実行時の型情報が有用
   - 数学的計算（テンソル、微分幾何など）では詳細なエラーが重要
   - 教育・研究用途では分かりやすいエラーメッセージが必要

#### 型情報の用途

```haskell
-- TIExpr は各ノードに型情報を持つ
data TIExpr 
  = TIVar Type Name
  | TIApp Type TIExpr TIExpr
  | TILam Type Name TIExpr
  | ...
```

評価中のエラーメッセージ例：

```
❌ 型消去した場合:
Error: Pattern match failed at line 42
  in function 'processData'

✅ 型情報を保持した場合:
Error: Pattern match failed at line 42
  in function 'processData'
  Expected type: List Integer
  Actual value: [1, 2, "three"]
  Type mismatch at index 2: expected Integer, got String
```

#### 最適化との両立

型情報の保持はパフォーマンスに影響を与える可能性がありますが、以下の戦略で対応できます：

1. **デバッグモードとリリースモード**
   ```bash
   egison --debug file.egi    # 型情報保持
   egison --release file.egi  # 型情報を部分的に消去して最適化
   ```

2. **段階的な型消去**
   - 頻繁に実行される内側のループでは型情報を削減
   - トップレベルや複雑な関数では型情報を保持

3. **遅延型消去**
   - 初期段階では型情報を保持
   - プロファイリング結果に基づいて必要に応じて消去

#### 型情報の内容

TIExpr が保持すべき型情報：

- **基本型**: Integer, Bool, String, Float など
- **複合型**: List, Tuple, Function など
- **テンソル型**: Tensor (次元情報を含む)
- **ユーザ定義型**: データ型、型コンストラクタ
- **型クラス制約**: すでに解決済みなので保持不要（辞書に展開済み）

注意: 型クラス制約は TypedDesugar 段階で辞書渡しに変換されるため、TIExpr では型クラス情報を保持する必要はありません。

## 現在の実装の確認

### パターンマッチの処理

現在の実装（hs-src/Language/Egison/）を確認した結果：

**Desugar段階** (`Desugar.hs`):
- `MatchExpr` → `IMatchExpr` への単純な変換
- `MatchAllExpr` → `IMatchAllExpr` への単純な変換
- `MatchLambdaExpr` → lambda + `IMatchExpr` への展開
- パターンマッチの意味的な展開は行わない

**Eval段階** (`Core.hs`):
- `evalExprShallow` 内で `IMatchExpr` と `IMatchAllExpr` を評価
- 実際のパターンマッチングは `patternMatch` 関数で実行
- マッチャーの評価とパターンマッチングのアルゴリズム実行

この設計は合理的：
- Desugarは構文的な変換のみ
- パターンマッチの複雑なロジックは評価時に処理
- Egisonの強力なパターンマッチング機能を評価器で実現

## デバッグサポート

各段階の出力を確認できるオプション：

```bash
egison --dump-loads file.egi       # モジュール読み込み後
egison --dump-env file.egi         # 環境構築後
egison --dump-desugared file.egi   # 脱糖後
egison --dump-typed file.egi       # 型推論・チェック後
egison --dump-ti file.egi          # TypedDesugar後
egison --verbose file.egi          # 全段階の詳細出力
```
