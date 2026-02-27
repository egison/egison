---
title: Tensor contract 型推論の修正
date: 2026-01-18
---

# 概要

`contract : Tensor a -> [Tensor a]` の型推論と、型推論中の **中間式スキームの量化** を見直した。
結果として、`def (.)` の本体に残っていた `result7` や `contractElem8` が消え、
`t0` と `Tensor t0` が一貫して現れるようになった。

# 症状

以下のように、`contract` の戻り型が `[result8]` のまま残り、
`result7` や `contractElem8` が `t0` と統一されなかった。

```
def . : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 :=
  (\t1 t2 -> ... (t1 : result7) (t2 : result7) ... (contract ... : [result8]) ...)
```

# 原因

## 1) `contract` が引数を Tensor として強制していなかった

`ITensorContractExpr` は単に `tensorType` を `[tensorType]` に包んでおり、
引数が `Tensor a` であることを `unify` で保証していなかった。

結果として、`*` の推論で導入された型変数がそのまま残り、
`result7` / `result8` が消えなかった。

## 2) 型推論中にスキームを量化していた

`inferIApplicationWithContext` と `simplifyTensorConstraintsInScheme` が
推論途中の `TIExpr` に対して `Forall tvs` を再構築していた。

一方で `applySubstScheme` は量化済みの型変数に置換を適用しないため、
後続の置換 (`result7 = Tensor t0` など) が反映されず、
`result7` が残ったままになった。

# 修正内容

## A) `contract` の引数を Tensor として unify する

`ITensorContractExpr` で `tensorType` を `TTensor elemType` と unify し、
戻り型を `[Tensor elemType]` に固定した。

```
elemType <- freshVar "contractElem"
s2 <- unifyTypesWithContext (applySubst s1 tensorType) (TTensor elemType) exprCtx
resultType = TCollection (TTensor finalElemType)
```

## B) 型推論中のスキームを量化しない

推論中の中間式は `Forall []` を維持し、一般化は let/def 境界でのみ行う。

対象:
- `simplifyTensorConstraintsInScheme`
- `inferIApplicationWithContext` の `updatedFuncScheme`

# 結果

`def (.)` の本体が以下のように整合する。

```
def . : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 :=
  (\t1 t2 -> ... (t1 : Tensor t0) (t2 : Tensor t0) ... (contract ... : [Tensor t0]) ...)
```

# 関連ファイル

- `hs-src/Language/Egison/Type/Infer.hs`
- `design/type-tensor-simple.md`

# テスト

- `cabal run egison -- --dump-typed -t lib/math/algebra/tensor.egi`
- `cabal run egison -- --dump-typed -t mini-test/161-dot-product.egi`
