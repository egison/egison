inductive pattern [a] := | nil | cons a [a] | join [a] [a]

def twin (p1 : Pattern a) (p2 : Pattern [a]) : Pattern [a] := 
  cons ($pat & ~p1) (cons #pat :: ~p2)

## 実装方針

### パターンコンストラクタの制限

`IInductivePat` は宣言されたパターンコンストラクタのみを使用可能にする。

- **宣言されたパターンコンストラクタ**: `inductive pattern` で宣言されたコンストラクタ（例: `nil`, `cons`, `join`）
- **マッチャー内のコンストラクタ**: マッチャー定義内で使用されるコンストラクタは `PPInductivePat` として処理し、`IInductivePat` とは区別する

### 実装上の変更点

1. **パターンコンストラクタ環境の追加**
   - `EvalState` に `patternConstructorEnv :: PatternConstructorEnv` を追加
   - `EnvBuildResult` に `ebrPatternConstructorEnv :: PatternConstructorEnv` を追加
   - `PatternInductiveDecl` を処理してパターンコンストラクタを登録

2. **`IInductivePat` の解決時の検証**
   - `IInductivePat` の解決時に、パターンコンストラクタ環境のみを参照
   - 宣言されていないコンストラクタの使用はエラーとする

3. **マッチャー内のコンストラクタ処理**
   - マッチャー定義内の `PPInductivePat` は既存の処理を維持
   - `primitivePatPatternMatch` で `PPInductivePat` と `IInductivePat` をマッチングする処理は、宣言されたパターンコンストラクタのみを許可するように変更

4. **型チェック時の検証**
   - 型推論・型チェック時に、`IInductivePat` で使用されるコンストラクタが宣言されているかを検証

5. **`PrimitivePatPattern` の型検査**
   - `PPInductivePat` の型検査を `inductive pattern` のコンストラクタ定義に基づいて実施
   - パターンコンストラクタ環境から、コンストラクタの型情報（引数の型）を取得
   - `PPInductivePat` の引数パターンの型を、宣言されたコンストラクタの引数の型と照合
   - 型推論時に、`PPInductivePat` の引数パターンの型を推論

### 実装上の注意点

1. **マッチャー内のコンストラクタとの関係**
   - マッチャー定義内では `PPInductivePat` を使用してパターンコンストラクタを定義する
   - マッチャー内の `PPInductivePat` は、宣言されたパターンコンストラクタを参照できる
   - 例: `matcher` 式内で `PPInductivePat "cons" ...` を使用する場合、`inductive pattern` で宣言された `cons` を参照する

2. **既存のマッチャーとの互換性**
   - 既存のマッチャー（`list`, `multiset` など）は、内部で `PPInductivePat` を使用している
   - これらのマッチャーは、宣言されたパターンコンストラクタ（`cons`, `nil`, `join` など）を参照するように変更する必要がある

3. **エラーメッセージ**
   - 宣言されていないパターンコンストラクタの使用時は、明確なエラーメッセージを表示
   - 例: "Pattern constructor 'xxx' is not declared. Use 'inductive pattern' to declare it."

### `PrimitivePatPattern` の型検査の詳細

#### 実装方針

1. **パターンコンストラクタ情報の定義**
   ```haskell
   data PatternConstructorInfo = PatternConstructorInfo
     { patternCtorName      :: String      -- コンストラクタ名（例: "cons", "nil"）
     , patternCtorArgTypes  :: [TypeExpr]  -- 引数の型（パターン型）
     , patternCtorTypeParams :: [String]   -- 型パラメータ（例: ["a"]）
     } deriving (Show, Eq)
   
   type PatternConstructorEnv = HashMap String PatternConstructorInfo
   ```

2. **`checkPatternDef` の拡張**
   - `PPInductivePat` の型検査時に、パターンコンストラクタ環境から型情報を取得
   - コンストラクタの引数の数と型を検証
   - 引数パターンの型を推論・検証

3. **型推論の拡張**
   - `PPInductivePat name args` の場合:
     - パターンコンストラクタ環境から `name` の型情報を取得
     - 各引数パターンに対して、対応する型を推論
     - 型パラメータの統一を行う

4. **型検査の例**
   ```egison
   inductive pattern [a] := | nil | cons a [a]
   
   -- マッチャー定義内で使用
   matcher
     | cons $ $ as (integer, list integer) with ...
   ```
   - `PPInductivePat "cons" [PPPatVar, PPPatVar]` の場合
   - パターンコンストラクタ環境から `cons : Pattern a -> Pattern [a] -> Pattern [a]` を取得
   - 第1引数は `Pattern a`、第2引数は `Pattern [a]` として型推論
   - `as (integer, list integer)` と照合して型を統一

5. **実装の詳細**
   - `checkPatternDef` 関数内で `PPInductivePat` を処理する際:
     ```haskell
     checkPrimitivePatPattern :: PrimitivePatPattern -> Infer ()
     checkPrimitivePatPattern (PPInductivePat name args) = do
       -- パターンコンストラクタ環境から型情報を取得
       patternCtorEnv <- getPatternConstructorEnv
       case HashMap.lookup name patternCtorEnv of
         Nothing -> throwError $ "Pattern constructor '" ++ name ++ "' is not declared"
         Just (PatternConstructorInfo _ argTypes typeParams) -> do
           -- 引数の数を検証
           when (length args /= length argTypes) $
             throwError $ "Pattern constructor '" ++ name ++ "' expects " ++ 
                         show (length argTypes) ++ " arguments, but got " ++ 
                         show (length args)
           -- 各引数パターンの型を推論・検証
           zipWithM_ checkArgPattern args argTypes
     ```
   - 型推論時に、`PPInductivePat` の引数パターンの型を推論:
     ```haskell
     inferPrimitivePatPattern :: PrimitivePatPattern -> Infer [Type]
     inferPrimitivePatPattern (PPInductivePat name args) = do
       patternCtorEnv <- getPatternConstructorEnv
       case HashMap.lookup name patternCtorEnv of
         Nothing -> throwError $ "Pattern constructor '" ++ name ++ "' is not declared"
         Just (PatternConstructorInfo _ argTypes _) -> do
           -- 各引数パターンの型を推論
           mapM inferArgPatternType (zip args argTypes)
     ```