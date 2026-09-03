# Pattern Matching Design

```egison
inductive pattern MyList a :=
  | myNil
  | myCons a (MyList a)
  | myJoin (MyList a) (MyList a)

def pattern twin {a} (p1 : a) (p2 : MyList a) : MyList a :=
  myCons ($pat & ~p1) (myCons #pat :: ~p2)
```

## パターン宣言と環境

パターンコンストラクタの signature とパターン関数の canonical `DualScheme` は値の型環境から
分離する。一方，body 検査前の target-only header と，検査済み `DualScheme` から射影した target
型は式側の名前解決・型推論のため `TypeEnv` にも登録する。パターンコンストラクタ環境と
パターン関数の header-only 環境も，一つの `PatternTypeEnv` 値に混在させない。

| 環境 | 格納する情報 |
|---|---|
| `patternEnv :: PatternTypeEnv` | `inductive pattern` が宣言したパターンコンストラクタの `TypeScheme` |
| `patternFuncDeclEnv :: PatternTypeEnv` | body 検査前のパターン関数 target-only header |
| `patternFuncEnv :: PatternFunctionEnv` | body 検査済みパターン関数の canonical `DualScheme` |
| `typeEnv :: TypeEnv` | 通常の値・関数の型と，必要な target projection |

`PatternTypeEnv` は `Map String TypeScheme` という共通の容器だが，パターンコンストラクタ
環境と header-only 環境は別の値として保持する。`PatternFunctionEnv` は
`Map String DualScheme` の専用環境である。

```haskell
newtype PatternTypeEnv = PatternTypeEnv
  { unPatternTypeEnv :: Map String TypeScheme }

type PatternConstructorEnv = PatternTypeEnv

newtype PatternFunctionEnv = PatternFunctionEnv
  { unPatternFunctionEnv :: Map String DualScheme }
```

## パターンコンストラクタ

`IInductivePat` は，原則として `inductive pattern` で宣言されたパターンコンストラクタを
参照する。マッチャー定義内の primitive-pattern pattern は `PPInductivePat` であり，通常の
マッチ節に現れる `IInductivePat` と区別する。

- `PatternInductiveDecl` は constructor signature を `patternEnv` に登録する。
- `PPInductivePat` の引数個数と target 型は，この frozen signature に対して検査する。
- `IInductiveOrPApplyPat` の名前が finalized/header-only のいずれのパターン関数環境にも
  なければ，パターンコンストラクタとして解決する。
- frozen signature を使わず generic inference へ進む production extension は，core と同期した
  直接経路ではない。詳細は `type-pm-compatibility.md` を参照する。

例えば，次の primitive-pattern pattern の `cons` は宣言から引数型を得る。

```egison
inductive pattern MyList a := | nil | cons a (MyList a)

matcher
  | cons $ $ as (integer, myList integer) with ...
```

`cons : a -> MyList a -> MyList a` を fresh instantiate し，各 `$` の target を matcher clause
の `(integer, myList integer)` と照合する。

## パターン関数

### 構文

パターン関数は `def pattern` で定義する。

- `{a}` で通常型パラメータを明示できる。
- 引数型と結果型は target 型であり，`Pattern` wrapper を付けない。
- capability は本体のパターン推論で求める。
- 本体はパターン式を直接記述する。

### canonical type

検査済みパターン関数の型は，引数と結果それぞれの capability/target を一つにまとめた
`DualScheme` である。

```haskell
data Dual = Dual
  { dualCapability :: Capability
  , dualTarget     :: Type
  }

data DualScheme = DualScheme
  { dualCapBinders :: [CapVar]
  , dualTyBinders  :: [TyVar]
  , dualArgs       :: [Dual]
  , dualResult     :: Dual
  }
```

定義時には，各パラメータへ fresh capability を割り当てて本体を推論し，最終 substitution
を引数 dual と結果 dual の双方へ適用する。その後，capability 変数と通常型変数を別々に
一般化し，canonical `DualScheme` を `patternFuncEnv` に保存する。

式側で必要な通常関数型は `dualSchemeTargetScheme` により canonical scheme から射影する。
binder と引数・結果の capability/target 相関は `DualScheme` を正本として維持する。

各パラメータは本体中でちょうど一回，宣言順，かつ分岐の外で使う。この線形性条件により，
適用時の引数パターンを左から右へ一回ずつ展開できる。

### 適用

finalized な named application では，`DualScheme` の capability binders と target binders を
一度の fresh instantiation で同時に置換し，同じ substitution を全引数と結果に使う。
結果 target，引数 target，引数 capability をそれぞれ対応する成分と照合し，result dual を
適用全体の型とする。

`IInductiveOrPApplyPat` の名前解決順は次のとおりである。

1. `patternFuncEnv` の finalized `DualScheme`
2. `patternFuncDeclEnv` の header-only 宣言
3. `patternEnv` のパターンコンストラクタ

header-only の前方・相互参照と，明示的な `IPApplyPat` で表される expression-headed application
には finalized capability 契約がない。後者は変数ヘッドであっても通常の lexical environment で
式として推論し，同名の top-level pattern function を理由に canonical dispatch へ切り替えない。
この二つの適用経路だけは target-only compatibility fallback を使用し，
`--outside-egison-core-warnings` が有効なら warning を出す。finalized named application は，
パターン関数であることを理由に warning を出さない。定義 body に残る
predicate，indexed，loop などの非 core パターン形式は定義時に warning を出す。
定義本体から同じパターン関数への直接または入れ子の自己呼び出しは，mechanized core の
nonrecursive side condition に従って定義時に拒否する。cross-load の再定義では新 header が
古い finalized scheme を body 検査前に無効化し，同じ expanded load unit 内の同名宣言は拒否する。

詳細な実装フローは `pattern-function-implementation.md` を参照する。

### 評価

パターン関数は runtime では `PatternFunc` 値として評価する。型を除去した宣言を
`IPatternFuncExpr` に変換し，通常の定義や型クラス辞書とともに `recursiveBind` する。
これにより，capture した環境から他の定義・辞書・パターン関数を参照できる。

## `match` と `matchDFS` の `else`

最初の解を返す `match` と `matchDFS` には，一本以上の通常節の後へ省略可能な `else` を置ける。
`else` は通常節の `|` と同じ字下げに置く。

```egison
match target as matcher with
  | pattern1 -> result1
  | pattern2 -> result2
  else fallback
```

通常節は従来どおりソース順に試す。どの節からも解が得られなかった場合だけ，
`fallback` を `match` の外側と同じ環境で評価する。したがって，通常節のパターン変数は
`else` から参照できない。`else` を省略してすべての節が失敗した場合は，従来どおり
実行時のパターンマッチ失敗となる。

`else` は matcher にパターンを渡さない。これは最終節の `| _ -> fallback` と重要な違いがある。
後者では wildcard も matcher によって照合されるため，ユーザー定義 matcher が空の結果を返せる。
前者はすべての通常節が空だったことに対する式自身のフォールバックである。

型検査では通常節と `else` が一つの結果型を共有する。通常節をソース順に検査した後，
`else` を外側の型環境で検査し，その結果型を通常節の結果型と単一化する。
`matchAll`，`matchAllDFS`，match-lambda は複数の結果を列挙する形式なので，この `else` は持たない。

## Value patternの扱い

primitive value patternを含むmatcher節は、mがvalue patternを処理できることを要求している。
aがEqクラスに属することは要求していない。

```
def multiset {a} (m: Matcher p a) : Matcher [p] [a] :=
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
          else []
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
          else []
    | $ as (something) with
      | $tgt -> [tgt]
```

この問題に対処するために、something matcherがvalue patternも扱えるようにする。
something matcherはvalue patternを処理するために、組み込みの投下演算子である `=` を使う。
eq matcherではEqクラスのメソッドであるユーザ定義の `==` を使うようにライブラリで定義する。
