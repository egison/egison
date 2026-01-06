## 処理フロー

```
TopExpr (構文解析結果)
  ↓
Phase 1: expandLoads (ファイル読み込み)
  - load文で指定されたファイルの読み込み
  - ファイルの存在チェック
  - 同じファイルの重複読み込み防止（キャッシング）
  - (将来: モジュールシステム導入時は依存関係の解決と循環参照チェックも必要)
  ↓
Phase 2: 環境構築フェーズ
  ├─ データコンストラクタ定義収集
  ├─ 型クラス定義収集  
  ├─ インスタンス定義収集
  └─ 型シグネチャ収集
  ↓
Phase 3-4: Desugar (構文糖衣展開) TopExpr → ITopExpr, Expr → IExpr
  入力: TopExpr/Expr (高レベルAST)
  出力: ITopExpr/IExpr (内部表現、糖衣構文が展開済み)
  
  処理内容:
  ├─ 中置演算子の展開 (InfixExpr → ApplyExpr)
  ├─ 無名関数の展開 (2#($1 + $2) など → Lambda)
  ├─ Match-lambda の展開 (matchAll式への変換)
  ├─ Cambda展開 (ICambdaExpr)
  └─ その他の糖衣構文展開
  
  注意:
  - パターンマッチ自体は展開せず、IMatchExpr/IMatchAllExprとして保持
  - 評価時に強力なパターンマッチエンジンで処理
  - 型情報はまだ付与されない（型推論前）
  
  実装: Language.Egison.Desugar (desugar :: Expr -> EvalM IExpr)
  ↓
Phase 5-6: 型推論フェーズ IExpr → TypedIExpr
  入力: IExpr (内部表現、型情報なし)
  出力: TypedIExpr (型情報付き内部表現)
  
  処理内容:
  ├─ 制約生成 (型変数の割り当て)
  ├─ 制約解決 (Unification)
  ├─ 型クラス制約の収集
  └─ 型注釈との整合性確認
  
  TypedIExpr の構造:
  - 各ノードに型情報が付与された IExpr
  - 例: IApplyExpr → TIApplyExpr Type IExpr IExpr
  - 型クラス制約は収集されるが、まだ解決されない
  
  実装: 新規作成予定 (inferIExpr :: IExpr -> Infer (TypedIExpr, Subst))
  ↓
Phase 7-8: TypedDesugar (型駆動の変換) TypedIExpr → TIExpr
  入力: TypedIExpr (型情報あり、型クラス未解決)
  出力: TIExpr (型情報あり、型クラス解決済み、実行可能)
  
  処理内容:
  ├─ 型クラス辞書渡し変換
  │   - インスタンス選択（型情報を使用）
  │   - メソッド呼び出しの具体化
  │   - 辞書パラメータの追加
  ├─ tensorMap 自動挿入
  │   - Tensor型とスカラー型の不一致検出
  │   - 適切な位置に tensorMap/tensorMap2 を挿入
  └─ 型情報の最適化
      - デバッグ用に型情報を保持
      - 必要に応じて部分的に型消去
  
  TIExpr の構造:
  - TypedIExpr とほぼ同じだが、型クラスメソッド呼び出しが辞書に解決済み
  - 例: TClassMethodCall → 具体的な辞書関数呼び出し
  
  実装: Language.Egison.Type.TypedDesugar
  ↓
Phase 9-10: 評価 (Evaluation) TIExpr → EgisonValue
  入力: TIExpr (実行可能な型付き内部表現)
  出力: EgisonValue (評価結果)
  
  処理内容:
  - パターンマッチングの実行 (patternMatch関数)
  - 式の評価と値の計算
  - IOアクションの実行
  - 型情報を使った詳細なエラーメッセージ
  
  実装: Language.Egison.Core (evalExpr)
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

### 現状: Phase 3 (型クラス実装中)
```
TopExpr → Desugar(Expr→IExpr) → 型推論(Expr→TypedExpr) → TypedDesugar → 評価
```
**問題点**: 
- 型推論が`Expr`ベース（糖衣構文が残っている）
- `PreDesugar`と`Desugar`が分離していて複雑

### リファクタリング計画: IExprベースの型推論

#### ステップ1: 型推論をIExprベースに移行
```
TopExpr → Desugar(Expr→IExpr) → 型推論(IExpr→TypedIExpr) → TypedDesugar → 評価
```
**変更内容**:
- `Infer.hs`を`IExpr`用に書き換え
- `TypedIExpr`データ型を新規作成
- `TypeInfer.hs`を削除または統合
- `PreDesugar.hs`を削除（不要）

**メリット**:
- 構文糖衣展開が1箇所に集約
- 型推論がシンプルな構造に対して動作
- 保守性向上

#### ステップ2: 型クラス辞書渡しの完成
```
完全なフロー: すべて動作
```
- 型クラスメソッド呼び出しを辞書に解決
- インスタンス選択ロジックの完成
- エラーメッセージの改善

#### ステップ3: tensorMapの自動挿入
```
完全なフロー + tensorMap最適化
```
- Tensor型の検出
- 自動tensorMap挿入
- テンソル添字記法のサポート

### 実装の優先順位

1. **最優先: ステップ1（IExprベース型推論）**
   - 設計の根本を正す
   - 以降の実装が容易になる
   
   **作業内容**:
   - [ ] `Language.Egison.Type.IInfer`を新規作成（IExpr用の型推論）
   - [ ] `TypedIExpr`データ型を`Language.Egison.Type.TypedIAST`に定義
   - [ ] `Language.Egison.Type.Infer`を`IExpr`対応に拡張
   - [ ] `Language.Egison.Eval`のパイプラインを変更
   - [ ] `Language.Egison.PreDesugar`を削除
   - [ ] `Language.Egison.Type.TypeInfer`を削除または統合
   - [ ] `Language.Egison.Type.TypedAST`を削除または`TypedIAST`に置き換え

2. **次: ステップ2（型クラス完成）**
   - 既存の型クラス実装を完成させる
   
   **作業内容**:
   - [ ] `Language.Egison.Type.TypedDesugar`を`TypedIExpr → TIExpr`に対応
   - [ ] `Language.Egison.Type.TypeClassExpand`の辞書渡し変換を完成
   - [ ] インスタンス選択ロジックの実装
   - [ ] エラーメッセージの改善

3. **最後: ステップ3（tensorMap）**
   - 高度な機能
   
   **作業内容**:
   - [ ] Tensor型の検出ロジック
   - [ ] 自動tensorMap挿入
   - [ ] テンソル添字記法のサポート

## 中間表現 (IR) の設計

各段階で適切な中間表現を定義：

```haskell
-- Phase 0: 構文解析後 (Language.Egison.AST)
data TopExpr = Define VarWithIndices Expr
             | DefineWithType TypedVarWithIndices Expr
             | Test Expr
             | Execute Expr
             | ...

data Expr = ConstantExpr ConstantExpr
          | VarExpr String
          | InfixExpr Op Expr Expr  -- 中置演算子（Desugarで展開）
          | ApplyExpr Expr [Expr]
          | LambdaExpr [Arg] Expr
          | IfExpr Expr Expr Expr
          | MatchExpr PMMode Expr Expr [MatchClause]
          | ...

-- Phase 3-4: Desugar後 (Language.Egison.IExpr)
-- 糖衣構文が展開され、内部表現に変換されている
data ITopExpr = IDefine Var IExpr
              | ITest IExpr
              | IExecute IExpr
              | ...

data IExpr = IConstantExpr ConstantExpr
           | IVarExpr Var
           | IApplyExpr IExpr [IExpr]        -- InfixExprは展開済み
           | ILambdaExpr (Maybe String) [Var] IExpr
           | ICambdaExpr String IExpr        -- 無名関数展開の中間形式
           | IIfExpr IExpr IExpr IExpr
           | IMatchExpr PMMode IExpr IExpr [IMatchClause]  -- パターンマッチは保持
           | ...

-- Phase 5-6: 型推論後 (新規作成予定)
-- IExpr に型情報が付与されている
data TypedIExpr = TypedIExpr 
  { tiExprType :: Type        -- この式の型
  , tiExprNode :: TypedINode  -- 型付きノード
  }

data TypedINode 
  = TIConstantExpr ConstantExpr
  | TIVarExpr Var
  | TIApplyExpr TypedIExpr [TypedIExpr]
  | TILambdaExpr (Maybe String) [(Var, Type)] TypedIExpr
  | TIIfExpr TypedIExpr TypedIExpr TypedIExpr
  | TIMatchExpr PMMode TypedIExpr TypedIExpr [TypedIMatchClause]
  -- 型クラスメソッド呼び出し（まだ辞書に解決されていない）
  | TIClassMethodCall String String [TypedIExpr]  -- className methodName args
  | ...

-- Phase 7-8: TypedDesugar後 (実行可能形式)
-- 型クラスが辞書に解決され、tensorMapが挿入されている
data TIExpr = TIExpr
  { tieType :: Type           -- デバッグ用に型情報を保持
  , tieNode :: TINode
  }

data TINode
  = TICon ConstantExpr
  | TIVar Var
  | TIApp TIExpr [TIExpr]
  | TILam (Maybe String) [(Var, Type)] TIExpr
  | TIIf TIExpr TIExpr TIExpr
  | TIMatch PMMode TIExpr TIExpr [TIMatchClause]
  -- 型クラスメソッド呼び出しは辞書パラメータ付き関数呼び出しに変換済み
  | TIDictApp TIExpr TIExpr [TIExpr]  -- dict method args
  -- tensorMapが必要な箇所に自動挿入される
  | TITensorMap TIExpr TIExpr         -- tensorMap fn tensor
  | ...

-- Phase 9-10: 評価結果
data EgisonValue = ...
```

### 各表現の役割

| 表現 | 段階 | 型情報 | 糖衣構文 | 型クラス | 特徴 |
|------|------|--------|----------|----------|------|
| `Expr` | パース後 | ❌ | ⭕ | ❌ | 高レベルAST |
| `IExpr` | Desugar後 | ❌ | ❌ | ❌ | 糖衣構文展開済み |
| `TypedIExpr` | 型推論後 | ⭕ | ❌ | 未解決 | 型情報付き |
| `TIExpr` | 実行前 | ⭕ | ❌ | 解決済み | 実行可能 |
| `EgisonValue` | 実行後 | ⭕ | ❌ | ❌ | 評価結果 |

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
egison --dump-loads file.egi       # Phase 1: モジュール読み込み後
egison --dump-env file.egi         # Phase 2: 環境構築後
egison --dump-desugared file.egi   # Phase 3-4: Desugar後 (IExpr)
egison --dump-typed file.egi       # Phase 5-6: 型推論後 (TypedIExpr)
egison --dump-ti file.egi          # Phase 7-8: TypedDesugar後 (TIExpr)
egison --verbose file.egi          # 全段階の詳細出力
```

### 各ダンプの出力例

```bash
# IExprのダンプ例
$ egison --dump-desugared test.egi
=== Desugared IR (Phase 3-4) ===
def add := ILambdaExpr Nothing [x, y] 
  (IApplyExpr (IVarExpr "+") [IVarExpr "x", IVarExpr "y"])

# TypedIExprのダンプ例  
$ egison --dump-typed test.egi
=== Typed IR (Phase 5-6) ===
def add : Integer -> Integer -> Integer
  := TILambdaExpr [(x, Integer), (y, Integer)]
       (TIApplyExpr (Integer -> Integer -> Integer)
         (TIVarExpr "+")
         [TIVarExpr "x", TIVarExpr "y"])

# TIExprのダンプ例
$ egison --dump-ti test.egi
=== Executable IR (Phase 7-8) ===
def add : Integer -> Integer -> Integer
  := TILam [(x, Integer), (y, Integer)]
       (TIApp (TIVar "+") [TIVar "x", TIVar "y"])
```

## 実装の手順の流れ

既存の実装と上記の処理フローでは異なる部分があるので、上記の処理フローに合うように実装を直す。
現在のlib以下のコードをパースし、実行できるようにするのが目標。
現在のテストコードを動かすのは、大規模な変更であるため無謀であるので、小さいテストを作成して動かす。
小さいテストはmini-testディレクトリを作り、そこの下に適宜生成。
一つ一つの構文を実行できるか--no-preludesオプションを使ってライブラリをロードせずにテストする。

stackは使わずにcabalを使ってください。

コマンドは全て１分でタイムアウト(gtimeoutコマンドを使う)をするようにしてください。