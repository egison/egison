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
Phase 5-6: 型推論フェーズ IExpr → (Type, Subst) → TIExpr
  入力: IExpr (内部表現、型情報なし)
  出力: TIExpr (型情報付き内部表現) ※--dump-typed用に作成
  
  処理内容:
  ├─ IInfer.hsで型推論 (IExpr → (Type, Subst))
  │   ├─ 制約生成 (型変数の割り当て)
  │   ├─ 制約解決 (Unification)
  │   ├─ 型クラス制約の収集
  │   └─ 型注釈との整合性確認
  └─ Eval.hsでTIExpr生成 (IExpr + TypeScheme → TIExpr)
      └─ IExprの各ノードに型スキーム（型変数・制約・型）を付与
  
  TIExpr の構造:
  - 各ノードに型スキーム（TypeScheme）が付与された IExpr
  - TypeScheme = Forall [TyVar] [Constraint] Type
  - 型変数、型クラス制約、型情報を保持
  - 例: IApplyExpr + TypeScheme → TIExpr { tiScheme, tiExpr }
  - 型クラス制約は収集されるが、まだ解決されない
  
  実装: Language.Egison.Type.IInfer (inferITopExpr :: ITopExpr -> Infer (Maybe (ITopExpr, Type), Subst))
        Language.Egison.Eval (iTopExprToTITopExprFromScheme :: ITopExpr -> TypeScheme -> TITopExpr)
  
  注: TIExprは--dump-typedやTypedDesugarのためだけに作成され、
      実際の評価には型情報を抜いたIExprが使用されます。
  ↓
Phase 7: Type Attachment (型情報の付与)
  入力: IExpr + TypeScheme
  出力: TIExpr (型スキーム付き内部表現)
  
  処理内容:
  - 型推論で得られた型スキームをIExprに付与
  - 環境から型スキームを取得（型変数名を保持）
  - TIExpr = TIExpr { tiScheme :: TypeScheme, tiExpr :: IExpr }
  
  実装: Language.Egison.Eval (iTopExprToTITopExprFromScheme)
  
Phase 8: TypedDesugar (型駆動の変換) 【基本フレームワーク実装済み】
  入力: TIExpr (型情報あり、型クラス未解決)
  出力: TIExpr (型情報あり、変換後)
  
  処理内容:
  ├─ TypedDesugar.hs (オーケストレーション)
  │   └─ 各変換モジュールを順次呼び出し
  ├─ TypeTensorExpand.hs (テンソル変換)
  │   └─ expandTensorApplications :: TIExpr -> EvalM TIExpr
  │       - ✅ フレームワーク実装済み（型情報を保持しながら再帰処理）
  │       - ⏳ tensorMap自動挿入ロジックは未実装
  │       - 関数適用時に型情報を取得して処理
  └─ TypeClassExpand.hs (型クラス展開)
      - ✅ Expr版は実装済み
      - ⏳ TIExpr版は未実装
  
  実装: Language.Egison.Type.TypedDesugar (desugarTypedExprT :: TIExpr -> EvalM TIExpr)
        Language.Egison.Type.TypeTensorExpand (expandTensorApplications :: TIExpr -> EvalM TIExpr)
  ↓
【型情報を抜く】IExpr (型なし) ← TIExpr (型あり)
  処理: Phase 8で変換したTIExprから型情報を抜いてIExprに戻す
  理由:
  - 元のevalExpr (Core.hs) がIExprベースで実装されている
  - 実行時に型情報は不要（最適化）
  - TIExprは--dump-typedやTypedDesugarのためにのみ必要
  
  実装: Language.Egison.IExpr (stripType :: TIExpr -> IExpr)
        Language.Egison.Eval (evalExpandedTopExprsTyped')
  ↓
Phase 9-10: 評価 (Evaluation) IExpr → EgisonValue
  入力: IExpr (型情報なし内部表現)
  出力: EgisonValue (評価結果)
  
  処理内容:
  - パターンマッチングの実行 (patternMatch関数)
  - 式の評価と値の計算
  - IOアクションの実行
  
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

## 実装の現状と今後の計画

### 現在の実装状態 (✅ 完了)

```
TopExpr → Desugar(Expr→IExpr) → 型推論(IExpr→TIExpr) → TypedDesugar(実装済み) → 評価
```

**完了した作業**:
- ✅ `Language.Egison.Type.IInfer`を作成（IExpr用の型推論）
- ✅ `TIExpr`をIExpr.hsに定義（TypeSchemeを保持）
- ✅ `Language.Egison.Eval`のパイプラインを変更
- ✅ `Language.Egison.PreDesugar`を削除
- ✅ `Language.Egison.Type.Infer`を削除（IInfer.hsに統合）
- ✅ `Language.Egison.Type.TypeInfer`を削除（IInfer.hsに統合）
- ✅ `Language.Egison.Type.TypedAST`を削除（TIExprに置き換え）
- ✅ `IExpr`, `TIExpr`のPretty Printing実装
- ✅ `--dump-desugared`, `--dump-typed`オプションの実装
- ✅ `Language.Egison.Type.TypedDesugar`の基本フレームワーク実装
- ✅ `Language.Egison.Type.TypeTensorExpand`の基本フレームワーク実装
- ✅ TIExprがTypeScheme（型変数・制約・型）を保持するように変更

**メリット**:
- 構文糖衣展開が1箇所に集約（Desugar.hs）
- 型推論がシンプルな構造に対して動作（IInfer.hs）
- 型駆動変換のフレームワークが整備済み
- 保守性の向上

### 今後の実装タスク

#### タスク1: tensorMapの自動挿入 (Phase 8)
**優先度**: 高

**現状**: フレームワーク実装済み（TypeTensorExpand.hs）

**作業内容**:
- [x] 基本フレームワーク実装（型情報を保持しながら再帰処理）
- [x] 関数適用時の型情報取得
- [ ] Tensor型とMathExpr型の不一致検出ロジック
- [ ] 自動tensorMap挿入ロジック
- [ ] テンソル添字記法のサポート
- [ ] エラーメッセージの改善

#### タスク2: 型クラス辞書渡しの実装 (Phase 8)
**優先度**: 高

**現状**: Expr版は実装済み（TypeClassExpand.hs）、TIExpr版は未実装

**作業内容**:
- [x] Expr版の実装（TypeClassExpand.hs）
- [ ] TIExpr版の実装
- [ ] 型クラスメソッド呼び出しを辞書に解決
- [ ] インスタンス選択ロジックの実装（型情報を使用）
- [ ] TypedDesugar.hsでの統合
- [ ] `--dump-ti`オプションの実装

#### タスク3: REPLでの型チェック機能の改善
**優先度**: 低

**作業内容**:
- [ ] REPL内でのTIExpr Pretty Printing
- [ ] より詳細な型エラーメッセージ

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

-- Phase 5-6: 型推論後 (Language.Egison.IExpr)
-- IInfer.hsで型推論を実行し、Eval.hsでIExprに型スキームを付与してTIExprに変換
data TIExpr = TIExpr
  { tiScheme :: TypeScheme    -- 型スキーム（型変数・制約・型を含む）
  , tiExpr   :: IExpr         -- 内部表現
  }

-- TypeScheme = Forall [TyVar] [Constraint] Type
-- 例: Forall ["a", "b"] [Constraint "Eq" (TVar "a")] (TFun (TVar "a") (TVar "b"))
-- 型変数、型クラス制約、型情報を保持

-- Phase 7: Type Attachment
-- IExpr + TypeScheme → TIExpr
-- 型推論で得られた型スキームをIExprに付与

-- Phase 8: TypedDesugar後 (実行可能形式)
-- 型クラスが辞書に解決され、tensorMapが挿入される（基本フレームワーク実装済み）
-- 現在は基本フレームワーク実装で TIExpr → TIExpr (再帰処理、変換ロジックは未実装)
-- 
-- 将来の拡張:
-- - tensorMap自動挿入: 関数適用時にTensor型とMathExpr型の不一致を検出してtensorMapを挿入
-- - 型クラス辞書渡し: 型クラスメソッド呼び出しを辞書に解決

-- Phase 9-10: 評価結果
data EgisonValue = ...
```

### 各表現の役割

| 表現 | 段階 | 型情報 | 糖衣構文 | 型クラス | 特徴 |
|------|------|--------|----------|----------|------|
| `Expr` | パース後 | ❌ | ⭕ | ❌ | 高レベルAST |
| `IExpr` | Desugar後 | ❌ | ❌ | ❌ | 糖衣構文展開済み |
| `TIExpr` | 型推論後/実行前 | ⭕ | ❌ | 未解決※ | 型スキーム付き、実行可能 |
| `EgisonValue` | 実行後 | ⭕ | ❌ | ❌ | 評価結果 |

※ 型クラスは型推論時に収集されますが、辞書渡し変換はまだ実装されていません（Phase 8で実装中）。

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
-- TIExpr は各ノードに型スキーム（型変数・制約・型）を持つ
data TIExpr = TIExpr
  { tiScheme :: TypeScheme    -- Forall [TyVar] [Constraint] Type
  , tiExpr   :: IExpr
  }

-- 型スキームから型変数、制約、型を取得可能
tiExprTypeVars :: TIExpr -> [TyVar]
tiExprConstraints :: TIExpr -> [Constraint]
tiExprType :: TIExpr -> Type
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

TIExpr が保持する型スキーム（TypeScheme）の内容：

- **型変数**: 多相型の型パラメータ（例: `a`, `b`）
- **型クラス制約**: 型クラス制約のリスト（例: `[Constraint "Eq" (TVar "a")]`）
- **型**: 具体的な型情報（例: `TFun (TVar "a") (TVar "b")`）

TypeScheme = Forall [TyVar] [Constraint] Type

例:
- `Forall [] [] TInt` - 単相型（Integer）
- `Forall ["a"] [] (TCollection (TVar "a"))` - 多相型（[a]）
- `Forall ["a"] [Constraint "Eq" (TVar "a")] (TFun (TVar "a") (TVar "a"))` - 型クラス制約付き

注意: 型クラス制約は Phase 8 (TypedDesugar) で辞書渡しに変換される予定ですが、
      現時点では型スキーム内に制約として保持されています。

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
egison --dump-typed file.egi       # Phase 5-6: 型推論後 (TIExpr)
egison --dump-ti file.egi          # Phase 7-8: TypedDesugar後 (TIExpr) ※未実装
egison --verbose file.egi          # 全段階の詳細出力
```

### 各ダンプの出力例

```bash
# IExprのダンプ例
$ egison --dump-desugared test.egi
=== Desugared IR (Phase 3-4) ===
def add := λx y -> (+ x y)

# TIExprのダンプ例 (Phase 5-6: 型推論後)
$ egison --dump-typed test.egi
=== Typed IR (Phase 5-6) ===
def add : Integer -> Integer -> Integer
  := (λx y -> ((+ : Integer -> Integer -> Integer) (x : Integer) (y : Integer)) : Integer -> Integer -> Integer) : Integer -> Integer -> Integer

# TIExprのダンプ例 (Phase 8: TypedDesugar後) ※実装準備済み
$ egison --dump-ti test.egi
=== Executable IR (Phase 8) ===
def add : ∀. Integer -> Integer -> Integer
  := λx y -> (+ x y)
  (型クラス辞書渡しやtensorMap挿入後の形式)
  ※現在は基本フレームワークのみ実装、変換ロジックは未実装
```

## 実装作業の手順の流れ

また、以下の実装作業の流れに従ってください。

既存の実装と上記の処理フローでは異なる部分があるので、上記の処理フローに合うように実装を直す。
現在のlib以下のコードをパースし、実行できるようにするのが目標。
現在のテストコードを動かすのは、大規模な変更であるため無謀であるので、小さいテストを作成して動かす。
小さいテストはmini-testディレクトリを作り、そこの下に適宜生成。
一つ一つの構文を実行できるか--no-preludesオプションを使ってライブラリをロードせずにテストする。
-tオプションを使うとmain関数を呼ばないテストができる。
ライブラリをロードするときは、-lオプションを使う。
% cabal run egison -- --no-prelude --dump-typed -l lib/core/base.egi -l lib/core/typeclass.egi -t mini-test/34-tuple-matcher.egi

stackは使わずにcabalを使ってください。

コマンドは全て１分でタイムアウト(gtimeoutコマンドを使う)をするようにしてください。

自動でgit commitするのはやめてください。
