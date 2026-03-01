# Egison 型推論アルゴリズムの設計

本ドキュメントは、Egison の型推論アルゴリズムが Hindley-Milner (HM) に基づいて設計・実装されていることを説明する。

## HM ベースを採用した理由

Egison は数学的なスクリプティング言語として使われており、ユーザーが自然に書くコード以上の型注釈を要求しないことが中心的な設計目標である。
HM 型推論を採用した理由は以下の通り:

1. **注釈不要の完全な型推論**: HM は principal type（主型）を注釈なしで推論できることが証明されている（Damas & Milner, 1982）。依存型や篩型（refinement types）を採用すると、証明項や不変条件の注釈が必須となり、数式を簡潔に記述するという目標と相容れない。
2. **決定可能性**: HM の型推論は決定可能であり、常に停止する。高ランク型や impredicativity を導入すると推論が決定不能になりうるが、Egison の拡張（パターンの型付け、マッチャー整合性、型クラス）はいずれも syntax-directed であり、この性質を保っている。
3. **エラーメッセージの品質**: principal type の存在により、型エラーの原因を特定しやすく、実用的なエラーメッセージを生成できる。
4. **実績と信頼性**: HM は ML, Haskell 98 等で数十年の実績があり、拡張の安全性について豊富な理論的知見が蓄積されている。

## 実装の概要

Egison の型推論は HM の構成要素をすべて備えており、その上にパターンマッチ固有の拡張を加えている。
以下、各構成要素と対応する実装を示す。

## 1. 型変数と代入（Substitution）

`Type/Types.hs` の `TyVar` と `Type/Subst.hs` の `Subst`:

- `Subst` は `Map TyVar Type`（型変数から型への写像）
- `composeSubst` で代入の合成（`s2 ∘ s1` = `s2 (s1 x)`）

```haskell
-- Type/Types.hs
data TypeScheme = Forall [TyVar] [Constraint] Type
```

これは HM の `∀a. σ` に対応する型スキームそのもの（型クラス制約付き）。

## 2. 単一化（Unification）

`Type/Unify.hs` に Robinson 単一化アルゴリズムが実装されている:

- **Occurs check**（無限型の防止）: `occursIn v t = v ∈ freeTyVars(t)`
- **型変数の束縛**: `singletonSubst v t`
- **構造的単一化**: `TFun a1 r1` と `TFun a2 r2` の場合、`a1 = a2` を単一化してから `r1 = r2` を単一化

```haskell
-- Type/Unify.hs
unifyVar :: TyVar -> Type -> Either UnifyError Subst
unifyVar v t
  | TVar v == t = Right emptySubst
  | occursIn v t = Left $ OccursCheck v t
  | otherwise = Right $ singletonSubst v t
```

## 3. 新しい型変数の生成（Fresh variable generation）

```haskell
-- Type/Infer.hs
freshVar :: String -> Infer Type
freshVar prefix = do
  st <- get
  let n = inferCounter st
  put st { inferCounter = n + 1 }
  return $ TVar $ TyVar $ prefix ++ show n
```

カウンターベースのフレッシュ変数生成で、Algorithm W の標準的実装。

## 4. instantiate と generalize

```haskell
-- Type/Env.hs
instantiate :: TypeScheme -> Int -> ([Constraint], Type, Int)
instantiate (Forall vs cs t) counter =
  let freshVars = zipWith (\v i -> (v, TVar (freshTyVar "t" (counter + i)))) vs [0..]
      substType = foldr (\(old, new) acc -> substVar old new acc) t freshVars
      substCs = map (substConstraint freshVars) cs
  in (substCs, substType, counter + length vs)
```

```haskell
-- Type/Env.hs
generalize :: TypeEnv -> Type -> TypeScheme
generalize env t =
  let envFreeVars = freeVarsInEnv env
      typeFreeVars = freeTyVars t
      genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
  in Forall genVars [] t
```

- `instantiate`: `Forall [a, b] t` → フレッシュ変数 `t0, t1` で `t[a:=t0, b:=t1]` を返す（HM の inst）
- `generalize`: 環境中の自由変数を除く型の自由変数を全称量化する（HM の gen）

## 5. 式レベルの推論構造（Algorithm W のパターン）

`Infer.hs` の `inferIExprWithContext` は Algorithm W の標準パターンに忠実:

- **ラムダ式**: 引数にフレッシュ型変数を割り当て → 本体を推論 → 関数型を構築
- **関数適用**: 関数を推論 → 引数を推論 → 単一化
- **let 式**: 右辺を推論 → generalize → 本体を推論
- **letrec 式**: プレースホルダ（フレッシュ変数）を用意 → 拡張環境で推論 → 単一化
- **トップレベル定義**: 推論 → generalize（型注釈がある場合は instantiate して照合）

## 6. HM からの拡張

HM をベースに、以下の拡張が加えられている:

- **型クラス制約** (`Constraint`): Haskell と同様の `{Num a} => ...` 形式の制約
- **テンソル型の特殊単一化** (`unifyWithConstraints`, `unifyWithTopLevel`): `Tensor a` と `a` の特殊ルール
- **パターンの型推論**: `matchAll` 式のパターンに対する型推論（syntax-directed）
- **マッチャー型** (`TMatcher`): パターンマッチの対象型を追跡

### 拡張が完全性を壊さない理由

これらの拡張はすべて HM の基本メカニズム（単一化 + generalize/instantiate）の上に構築されており、推論の decidability を壊す要因（高ランク型、impredicativity）は導入されていない。型クラスの処理も Haskell 98 と同レベルの単純なもの。

## 関連ファイル

| ファイル | 役割 |
|---|---|
| `hs-src/Language/Egison/Type/Types.hs` | `Type`, `TypeScheme`, `TyVar`, `Constraint` 等の定義 |
| `hs-src/Language/Egison/Type/Subst.hs` | `Subst`（代入）、`applySubst`、`composeSubst` |
| `hs-src/Language/Egison/Type/Unify.hs` | Robinson 単一化、occurs check |
| `hs-src/Language/Egison/Type/Env.hs` | `TypeEnv`、`generalize`、`instantiate` |
| `hs-src/Language/Egison/Type/Infer.hs` | メインの推論エンジン（Algorithm W） |
