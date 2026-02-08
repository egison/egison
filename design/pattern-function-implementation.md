# Pattern Function Implementation

## 概要

パターン関数（Pattern Function）の型推論と評価の実装について説明する。パターン関数は`def pattern`キーワードで定義され、通常の関数と同様に型パラメータ、引数の型、戻り値の型を持つ。本ドキュメントでは、型推論時の課題と解決策、および評価時の環境管理について詳述する。

## パターン関数の構文

```egison
def pattern twin {a} (pat1 : a) (pat2 : [a]) : [a] := 
  ($pat & ~pat1) :: #pat :: ~pat2

def pattern shuntsu {a} (pat1 : a) (pat2 : [a]) : [a] :=
  (num $s $n & ~pat1) :: num #s #(n + 1) :: num #s #(n + 2) :: ~pat2
```

- **型パラメータ**: `{a}` で多相型を表現
- **引数**: `(name : Type)` の形式で型付き引数
- **戻り値の型**: `: [a]` などで指定
- **本体**: パターン式を直接記述（式内で型クラスメソッドの使用も可能）

## 実装アーキテクチャ

### 1. AST構造

#### TopExpr (Parser出力)
```haskell
data TopExpr
  = ...
  | PatternFunctionDecl String [String] [(String, TypeExpr)] TypeExpr Pattern
    -- PatternFunctionDecl name typeParams params returnType body
```

#### ITopExpr (Desugar後)
```haskell
data ITopExpr
  = ...
  | IPatternFunctionDecl String [TyVar] [(String, Type)] Type IPattern
```

#### TITopExpr (Type Inference後)
```haskell
data TITopExpr
  = ...
  | TIPatternFunctionDecl String TypeScheme [(String, Type)] Type TIPattern
```

#### IExpr (Evaluation用)
```haskell
data IExpr
  = ...
  | IPatternFuncExpr [String] IPattern  -- NEW: パラメータ名とパターン本体
```

#### PatternFunc (Runtime値)
```haskell
data EgisonValue
  = ...
  | PatternFunc Env [String] IPattern
    -- PatternFunc environment paramNames body
```

### 2. 処理フロー

```
TopExpr (PatternFunctionDecl)
  ↓ desugarTopExpr
ITopExpr (IPatternFunctionDecl)
  ↓ inferITopExpr (型推論)
TITopExpr (TIPatternFunctionDecl)
  ↓ desugarTypedTopExprT (型クラス展開)
TITopExpr (TIPatternFunctionDecl with expanded body)
  ↓ stripTypeTopExpr (削除: Phase 9aで処理していた)
  ↓ 収集: allBindingsへ変換
(Var, IExpr) where IExpr = IPatternFuncExpr
  ↓ recursiveBind (他の定義と一緒に処理)
Env with PatternFunc values
  ↓ パターンマッチング時
PatternFunc適用 → 本体評価
```

## 型推論の実装

### 型環境の管理

パターン関数の型情報は以下の環境で管理される：

1. **TypeEnv**: 通常の値・関数の型
2. **PatternEnv**: パターンコンストラクタとパターン関数の型（統合）
3. **PatternFuncEnv**: パターン関数の型（専用、型推論時の参照用）
4. **ClassEnv**: 型クラスとインスタンスの情報

### IInductiveOrPApplyPat の役割

パターン式内では、パターンコンストラクタとパターン関数の適用を構文的に区別できない。`IInductiveOrPApplyPat`は両者を表現し、型推論時に解決される。

```haskell
data IPattern
  = ...
  | IInductiveOrPApplyPat String [IPattern]  -- コンストラクタまたは関数適用
  | IInductivePat String [IPattern]          -- 確定したコンストラクタ
  | IPApplyPat IExpr [IPattern]              -- 確定した関数適用
```

#### 解決ロジック（inferIPattern）

```haskell
inferIPattern (IInductiveOrPApplyPat name args) = do
  patFuncEnv <- gets inferPatternFuncEnv
  case lookupPatternEnv name patFuncEnv of
    Just scheme ->
      -- パターン関数として解決
      inferIPattern (IPApplyPat (IVarExpr name) args)
    Nothing ->
      -- パターンコンストラクタとして解決
      inferIPattern (IInductivePat name args)
```

### パターン関数本体の型推論

```haskell
inferITopExpr (IPatternFunctionDecl name tyVars params retType body) = do
  -- 型変数と制約を準備
  let tyVarSet = Set.fromList tyVars
  constraints <- mapM (\tv -> newConstraintVar) tyVars
  
  -- パラメータを環境に追加
  let paramBindings = [(stringToVar pname, Forall [] [] pty) | (pname, pty) <- params]
  localEnv <- extendInferEnv paramBindings
  
  -- 本体の型推論
  (typedBody, bodyType) <- inferIPattern body
  
  -- 戻り値の型と統一
  unify retType bodyType
  
  -- 型スキームを構築
  let scheme = Forall tyVars constraints retType
  
  -- パターン関数環境に登録
  modifyPatternFuncEnv (extendPatternEnv name scheme)
  
  return $ TIPatternFunctionDecl name scheme params retType typedBody
```

## 型クラス展開の実装

パターン関数本体内で型クラスメソッド（例：`+`演算子）を使用する場合、型クラス辞書への展開が必要。

### 課題

パターン関数本体は`TIPattern`であり、従来の`expandTypeClassMethodsT`（`TIExpr`用）では処理できない。

### 解決策

`TIPattern`用の型クラス展開関数を実装：

```haskell
expandTypeClassMethodsInPattern :: TIPattern -> EvalM TIPattern
expandTypeClassMethodsInPattern (TIPattern scheme node) = do
  node' <- expandNode node
  return $ TIPattern scheme node'
  where
    expandNode :: TIPatternNode -> EvalM TIPatternNode
    expandNode pnode = case pnode of
      TIValuePat expr -> do
        expr' <- expandTypeClassMethodsT expr      -- 式を展開
        expr'' <- applyConcreteConstraintDictionaries expr'  -- 辞書適用
        return $ TIValuePat expr''
      
      TIPredPat expr -> do
        expr' <- expandTypeClassMethodsT expr
        expr'' <- applyConcreteConstraintDictionaries expr'
        return $ TIPredPat expr''
      
      -- 他のノードも再帰的に処理
      TIInductiveOrPApplyPat name pats -> do
        pats' <- mapM expandTypeClassMethodsInPattern pats
        return $ TIInductiveOrPApplyPat name pats'
      
      -- ... (その他のパターンノード)
```

### 辞書名の正規化

`Integer`は`MathExpr`の型エイリアスであるため、辞書検索時に正規化が必要：

```haskell
-- TypeClassExpand.hs
resolveDictionaryForConstraint classEnv (Constraint className tyArg) = do
  -- TIntをTMathExprに正規化（Integer = MathExpr）
  let normalizedType = case tyArg of
                         TInt -> TMathExpr
                         _ -> tyArg
  let instances = lookupInstances className classEnv
  case findMatchingInstanceForType normalizedType instances of
    Just inst -> generateDictionaryAccess inst
    Nothing -> throwError $ "No instance for " ++ className

-- Types.hs
typeConstructorName :: Type -> String
typeConstructorName TInt = "MathExpr"      -- 正規化
typeConstructorName TMathExpr = "MathExpr"
-- ...

typeToName :: Type -> String
typeToName TInt = "MathExpr"      -- 正規化
typeToName TMathExpr = "MathExpr"
-- ...
```

### 制約の削除

辞書ハッシュアクセス時に制約を削除することで、重複適用を防ぐ：

```haskell
-- 辞書アクセス作成時
let methodType = getMethodTypeFromClass classEnv className methodKey tyArg
    methodScheme = Forall [] [] methodType  -- 制約なし（解決済み）
    dictAccess = TIExpr methodScheme $
                 TIIndexedExpr False dictExpr [Sub indexExpr]
```

## 評価時の環境管理

### 問題: 環境の不足

初期実装では、パターン関数を通常の定義より先に評価していたため、型クラス辞書や他の定義にアクセスできなかった。

```haskell
-- 旧実装 (Phase 9a → Phase 9b)
-- Phase 9a: パターン関数を先に評価
envWithPatternFuncs <- foldM evalPatternFunc env patternFuncs

-- Phase 9b: 他の定義をバインド
env' <- recursiveBind envWithPatternFuncs allBindings
```

問題点：
- Phase 9aで`PatternFunc env ...`を作成
- この`env`には`numMathExpr`等の辞書が含まれていない（Phase 9bで追加）
- パターン関数本体の評価時に辞書が見つからず、エラー

### 解決策: IPatternFuncExprの導入

パターン関数を`IExpr`の一種として表現し、`recursiveBind`で一括処理：

#### 1. IExprへの追加
```haskell
data IExpr
  = ...
  | IPatternFuncExpr [String] IPattern
```

#### 2. 評価ロジック
```haskell
evalExprShallow env (IPatternFuncExpr paramNames body) =
  -- 現在の環境envをキャプチャ（辞書を含む完全な環境）
  return $ Value (PatternFunc env paramNames body)
```

#### 3. 収集ロジック変更
```haskell
-- Eval.hs
case iTopExprExpanded of
  IDefine name expr ->
    return ((bindings ++ [(name, expr)], nonDefs), ...)
  
  IPatternFunctionDecl name _tyVars params _retType body ->
    -- IPatternFuncExprに変換してbindingsに追加
    let paramNames = map fst params
        patternFuncExpr = IPatternFuncExpr paramNames body
        var = stringToVar name
    in return ((bindings ++ [(var, patternFuncExpr)], nonDefs), ...)
```

#### 4. 統一的な評価
```haskell
-- Phase 9: すべての定義を一括バインド
env' <- recursiveBind env allBindings
  where allBindings には:
    - 通常の定義 (name, IExpr)
    - パターン関数 (name, IPatternFuncExpr paramNames body)
    - 型クラス辞書 (numMathExpr, IHashExpr ...)
```

### 利点

1. **完全な環境**: `PatternFunc`が作成される時点で、すべての定義と辞書が環境に含まれる
2. **相互再帰**: パターン関数と通常の定義が相互に参照可能
3. **コードの簡潔化**: 特別な処理（Phase 9a）が不要
4. **一貫性**: すべての定義が同じフローで処理される

## パターン関数の適用

### マッチング時の処理

```haskell
-- Core.hs: processMState'
case pattern of
  IInductiveOrPApplyPat name args ->
    case refVar env (stringToVar name) of
      Just ref -> do
        whnf <- evalRef ref
        case whnf of
          Value PatternFunc{} ->
            -- パターン関数として処理
            processMState' (mstate { mTrees = MAtom (IPApplyPat (IVarExpr name) args) target matcher:trees })
          _ ->
            -- パターンコンストラクタとして処理
            processMState' (mstate { mTrees = MAtom (IInductivePat name args) target matcher:trees })
```

### パターン関数本体の評価

```haskell
-- Core.hs: processMState'
case pattern of
  IPApplyPat func args -> do
    func' <- evalExprShallow env' func
    case func' of
      Value (PatternFunc env'' names expr) ->
        -- 新しいマッチング状態を作成
        -- env''はPatternFuncがキャプチャした環境（辞書を含む）
        let penv = zip names args  -- パラメータバインディング
        return . msingleton $ mstate { 
          mTrees = MNode penv (MState env'' [] [] [] [MAtom expr target matcher]) : trees 
        }
```

重要な点：
- `env''`は`PatternFunc`が持つ環境（`recursiveBind`時にキャプチャ）
- すべての定義と型クラス辞書が含まれている
- `penv`はパラメータから引数へのマッピング

### 本体内の式評価

```haskell
-- Core.hs: processMState'
case pattern of
  IValuePat valExpr -> do
    -- env'はMState env''から（PatternFuncの環境 + パラメータバインディング）
    val <- evalExprDeep env' valExpr
    tgtVal <- evalWHNF target
    if val == tgtVal
      then return . msingleton $ mstate { mTrees = trees }
      else return MNil
```

## テストケース

### 基本的なパターン関数（twin）

```egison
def pattern twin {a} (pat1 : a) (pat2 : [a]) : [a] := 
  ($pat & ~pat1) :: #pat :: ~pat2

matchAll [1, 1, 2] as list integer with
| twin $x [] -> x
-- 結果: [1]
```

### 型クラスメソッドを使用（shuntsu）

```egison
def pattern shuntsu {a} (pat1 : a) (pat2 : [a]) : [a] :=
  (num $s $n & ~pat1) :: num #s #(n + 1) :: num #s #(n + 2) :: ~pat2

matchAll [Num Wan 3, Num Wan 4, Num Wan 5] as list tile with
| shuntsu ~x [] -> x
-- 結果: [Num Wan 3]
-- 注: n + 1 の評価で numMathExpr 辞書が必要
```

### グローバル定義へのアクセス

```egison
def myConstant := 100

def pattern testAdd {a} : [a] := #(myConstant + 42) :: []

matchAll [142] as list integer with
| testAdd -> "SUCCESS"
-- 結果: ["SUCCESS"]
-- 注: myConstant と + の両方にアクセス可能
```

## 変更ファイル一覧

### 1. AST定義
- `hs-src/Language/Egison/IExpr.hs`
  - `IPatternFuncExpr [String] IPattern`を追加

### 2. 型推論
- `hs-src/Language/Egison/Type/IInfer.hs`
  - `inferITopExpr`に`IPatternFunctionDecl`ケース追加
  - `inferIPattern`で`IInductiveOrPApplyPat`を解決
  - `inferPatternFuncEnv`を参照してパターン関数判定

### 3. 型クラス展開
- `hs-src/Language/Egison/Type/TypeClassExpand.hs`
  - `expandTypeClassMethodsInPattern`を実装
  - `applyConcreteConstraintDictionariesInPattern`を実装
  - `resolveDictionaryForConstraint`でTInt→TMathExpr正規化

- `hs-src/Language/Egison/Type/Types.hs`
  - `typeConstructorName`でTInt→"MathExpr"正規化
  - `typeToName`でTInt→"MathExpr"正規化

- `hs-src/Language/Egison/Type/TypedDesugar.hs`
  - `TIPatternFunctionDecl`に型クラス展開適用

### 4. 評価
- `hs-src/Language/Egison/Core.hs`
  - `evalExprShallow`に`IPatternFuncExpr`ケース追加
  - `IInductiveOrPApplyPat`の解決ロジック（既存）

- `hs-src/Language/Egison/Eval.hs`
  - `IPatternFunctionDecl`を`IPatternFuncExpr`に変換
  - Phase 9aを削除
  - すべてを`recursiveBind`で処理

### 5. Pretty Printing
- `hs-src/Language/Egison/Pretty.hs`
  - `IPatVar`と`IVarPat`の表示を修正（`~x`と`$x`）

## 既知の制限事項

1. **型推論エラー時の fallback**: パターン関数の型推論に失敗した場合、permissiveモードでも適切にフォールバックできない可能性がある

2. **相互再帰の検証**: パターン関数間の相互再帰や、パターン関数と通常の関数の相互再帰が正しく動作するかは、より複雑なテストケースで検証が必要

3. **エラーメッセージ**: パターン関数本体での型エラー時のエラーメッセージが分かりにくい可能性がある

## 今後の拡張

1. **高階パターン関数**: パターン関数を引数として受け取るパターン関数

2. **パターン関数の部分適用**: 一部のパラメータのみを適用したパターン関数

3. **パターン関数のオーバーロード**: 同名で異なる型のパターン関数

4. **型クラス制約付きパターン関数**: パターン関数自体が型クラス制約を持つ

## まとめ

パターン関数の実装は以下の要素から構成される：

1. **型推論**: `IInductiveOrPApplyPat`による遅延解決、専用の環境管理
2. **型クラス展開**: パターン専用の展開関数、型エイリアスの正規化
3. **評価**: `IPatternFuncExpr`による統一的な処理、`recursiveBind`での一括バインド

これにより、パターン関数は通常の関数と同等の機能（型パラメータ、型クラス制約、グローバル定義へのアクセス）を持ちながら、パターンマッチングの文脈で使用できる強力な機能となった。
