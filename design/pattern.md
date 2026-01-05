inductive pattern MyList a := | myNil | myCons a (MyList a) | myJoin (MyList a) (MyList a)

def pattern twin {a} (p1 : a) (p2 : MyList a) : MyList a := 
  myCons ($pat & ~p1) (myCons #pat :: ~p2)

## 実装方針

### パターン関数の定義

パターン関数は `def pattern` キーワードで定義する。

- **型パラメータ**: `{a}` で明示的に指定可能
- **引数の型**: パターンの型として扱う（`Pattern a` への変換は不要）
- **戻り値の型**: パターンの型として扱う（`Pattern (MyList a)` への変換は不要）
- **本体**: パターン式を直接記述（パターン関数リテラル `\pat1 pat2 => ...` は不要）

### パターン型環境の分離

パターンコンストラクタとパターン関数の型環境は、値の型環境と分離する。

- **パターン型環境**: パターンコンストラクタとパターン関数の型情報を保持
- **値型環境**: 通常の関数や値の型情報を保持
- **型推論**: パターンの型推論は値の型推論と独立して行う

### パターンコンストラクタの制限

`IInductivePat` は宣言されたパターンコンストラクタのみを使用可能にする。

- **宣言されたパターンコンストラクタ**: `inductive pattern` で宣言されたコンストラクタ（例: `nil`, `cons`, `join`）
- **マッチャー内のコンストラクタ**: マッチャー定義内で使用されるコンストラクタは `PPInductivePat` として処理し、`IInductivePat` とは区別する

### 実装上の変更点

1. **パターン型環境の追加**
   - `EvalState` に `patternTypeEnv :: PatternTypeEnv` を追加（パターンコンストラクタとパターン関数の型情報）
   - `EnvBuildResult` に `ebrPatternTypeEnv :: PatternTypeEnv` を追加
   - `PatternInductiveDecl` を処理してパターンコンストラクタを登録
   - `PatternFunctionDecl` を処理してパターン関数を登録

2. **`IInductivePat` の解決時の検証**
   - `IInductivePat` の解決時に、パターンコンストラクタ環境のみを参照
   - 宣言されていないコンストラクタの使用はエラーとする

3. **マッチャー内のコンストラクタ処理**
   - マッチャー定義内の `PPInductivePat` は宣言されたパターンコンストラクタのみを許可するように変更
   - `primitivePatPatternMatch` で `PPInductivePat` と `IInductivePat` をマッチングする処理は既存の処理を維持

4. **型チェック時の検証**
   - 型推論・型チェック時に、マッチ節のパターン内の `IInductivePat` で使用されるコンストラクタが inductive pattern かパターン関数として宣言されているかを検証
   - マッチャー節のprimitivePatternPatternは inductive patternで宣言されたパターンコンストラクタのみが使える
   - マッチ節では、inductive patternで宣言されたパターンコンストラクタとパターン関数の両方が使える
   - パターン関数の型推論は、パターン型環境を使用して独立して行う

5. **パターン関数の型推論**
   - パターン関数の引数型は `Pattern a` への変換は行わない（`a` のまま）
   - パターン関数の戻り値型も `Pattern (MyList a)` への変換は行わない（`MyList a` のまま）
   - パターン型環境で型推論を行うため、値の型環境とは独立

6. **`PrimitivePatPattern` の型検査**
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

1. **パターン型環境の定義**
   ```haskell
   -- パターンコンストラクタとパターン関数の型情報を保持
   -- 値の型環境とは分離されている
   newtype PatternTypeEnv = PatternTypeEnv { unPatternTypeEnv :: Map String TypeScheme }
   
   -- パターンコンストラクタ環境（PatternTypeEnvと同じ形式）
   type PatternConstructorEnv = PatternTypeEnv
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
   inductive pattern MyList a := | nil | cons a MyList a
   
   -- マッチャー定義内で使用
   matcher
     | cons $ $ as (integer, myList integer) with ...
   ```
   - `PPInductivePat "cons" [PPPatVar, PPPatVar]` の場合
   - パターンコンストラクタ環境から `cons : a -> MyList a -> MyList a` を取得
   - 第1引数は `a`、第2引数は `MyList a` として型推論
   - `as (integer, myList integer)` と照合して型を統一

### パターン関数の実装詳細

#### 構文

```egison
-- 型パラメータあり
def pattern twin {a} (p1 : a) (p2 : MyList a) : MyList a := 
  myCons ($pat & ~p1) (myCons #pat :: ~p2)

-- 型パラメータなし（型変数は引数から推論）
def pattern pair (p1 : Integer) (p2 : Integer) : (Integer, Integer) := 
  (p1, p2)
```

#### 実装方針

1. **ASTへの追加**
   ```haskell
   data TopExpr
     = ...
     | PatternFunctionDecl String [String] [(String, TypeExpr)] TypeExpr Pattern
     -- String: 関数名
     -- [String]: 型パラメータ（例: ["a"]）
     -- [(String, TypeExpr)]: 引数（名前と型）
     -- TypeExpr: 戻り値の型
     -- Pattern: 本体（パターン式）
   ```

2. **パーサーの拡張**
   - `def pattern` キーワードを追加
   - 型パラメータ `{a}` のパース
   - 本体をパターン式として解析

3. **環境構築フェーズ**
   - `PatternFunctionDecl` を処理してパターン関数をパターン型環境に登録
   - 型パラメータを考慮した型スキームを作成

4. **型推論**
   - パターン型環境を使用して型推論
   - 引数の型は `Pattern a` への変換は行わない
   - 戻り値の型も `Pattern (MyList a)` への変換は行わない
   - 型パラメータの統一を行う

5. **評価時の処理**
   - パターン関数は `PatternFunc` 値として評価
   - `IInductivePat` の解決時に、パターン型環境からパターン関数を検索


# Value patternの扱い

primitive value patternを含むmatcher節は、mがvalue patternを処理できることを要求している。
aがEqクラスに属することは要求していない。

```
def multiset {a} (m: Matcher a) : Matcher [a] :=
  matcher
    | [] as () with
      | [] -> [()]
      | _ -> []
    | $ :: _ as (m) with
      | $tgt -> tgt
    | $ :: $ as (m, multiset m) with
      | $tgt ->
        matchAll tgt as list m with
          | $hs ++ $x :: $ts -> (x, hs ++ ts)
    | #$pxs ++ $ as (multiset m) with
      | $tgt ->
        match (pxs, tgt) as (list m, multiset m) with
          | loop $i (1, length pxs, _)
              {($x_i :: @, #x_i :: @), ...}
              ([], $rs) -> [rs]
          | _ -> []
    | $ ++ $ as (multiset m, multiset m) with
      | $tgt ->
        matchAll tgt as list m with
          | loop $i (1, $n)
              ($rs_i ++ $x_i :: ...)
              $ts ->
            (map (\i -> x_i) [1..n], concat (map (\i -> rs_i) [1..n] ++ [ts]))
    | #$val as () with
      | $tgt ->
        match (val, tgt) as (list m, multiset m) with
          | ([], []) -> [()]
          | ($x :: $xs, #x :: #xs) -> [()]
          | (_, _) -> []
    | $ as (something) with
      | $tgt -> [tgt]
```

この問題に対処するために、something matcherがvalue patternも扱えるようにする。
something matcherはvalue patternを処理するために、組み込みの投下演算子である `=` を使う。
eq matcherではEqクラスのメソッドであるユーザ定義の `==` を使うようにライブラリで定義する。
