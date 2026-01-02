{- |
Module      : Language.Egison.Type.TypeClassExpand
Licence     : MIT

This module expands type class method calls using type information.
It transforms Expr to Expr, replacing type class method calls with
dictionary-based dispatch.

Pipeline: Parse → Infer → TypeClassExpand → Desugar → Eval
-}

module Language.Egison.Type.TypeClassExpand
  ( expandTypeClassMethods
  , expandTopExpr
  ) where

import           Data.Text              (pack)

import           Language.Egison.AST
import           Language.Egison.Type.Env (TypeEnv)
import           Language.Egison.Type.Types (Type(..), TyVar(..))

-- | Expand type class method calls in an expression
-- For now, this is a placeholder that returns the expression unchanged.
-- Type class expansion will be implemented when we have proper type class
-- method detection during type inference.
expandTypeClassMethods :: TypeEnv -> Expr -> Expr
expandTypeClassMethods _env expr = expandExpr expr

-- | Expand type class methods in a top-level expression
expandTopExpr :: TypeEnv -> TopExpr -> TopExpr
expandTopExpr env topExpr = case topExpr of
  Define vwi e -> Define vwi (expandTypeClassMethods env e)
  DefineWithType tvwi e -> DefineWithType tvwi (expandTypeClassMethods env e)
  Test e -> Test (expandTypeClassMethods env e)
  Execute e -> Execute (expandTypeClassMethods env e)
  -- Other top-level expressions don't contain expressions to expand
  _ -> topExpr

-- | Recursively expand type class methods in an expression
-- Currently just traverses the expression tree without modification.
-- Type class expansion will be added when type inference provides
-- the necessary type information.
expandExpr :: Expr -> Expr
expandExpr expr = case expr of
  -- Literals and variables - no expansion needed
  ConstantExpr c -> ConstantExpr c
  VarExpr v -> VarExpr v
  FreshVarExpr -> FreshVarExpr
  
  -- Indexed expressions
  IndexedExpr b e idxs -> IndexedExpr b (expandExpr e) (map (fmap expandExpr) idxs)
  SubrefsExpr b e1 e2 -> SubrefsExpr b (expandExpr e1) (expandExpr e2)
  SuprefsExpr b e1 e2 -> SuprefsExpr b (expandExpr e1) (expandExpr e2)
  UserrefsExpr b e1 e2 -> UserrefsExpr b (expandExpr e1) (expandExpr e2)
  
  -- Collections
  TupleExpr es -> TupleExpr (map expandExpr es)
  CollectionExpr es -> CollectionExpr (map expandExpr es)
  ConsExpr e1 e2 -> ConsExpr (expandExpr e1) (expandExpr e2)
  JoinExpr e1 e2 -> JoinExpr (expandExpr e1) (expandExpr e2)
  HashExpr pairs -> HashExpr [(expandExpr k, expandExpr v) | (k, v) <- pairs]
  VectorExpr es -> VectorExpr (map expandExpr es)
  
  -- Lambda expressions
  LambdaExpr args body -> LambdaExpr args (expandExpr body)
  LambdaExpr' args body -> LambdaExpr' args (expandExpr body)
  TypedLambdaExpr params retTy body -> TypedLambdaExpr params retTy (expandExpr body)
  MemoizedLambdaExpr params body -> MemoizedLambdaExpr params (expandExpr body)
  TypedMemoizedLambdaExpr params retTy body -> TypedMemoizedLambdaExpr params retTy (expandExpr body)
  CambdaExpr name body -> CambdaExpr name (expandExpr body)
  PatternFunctionExpr names pat -> PatternFunctionExpr names (expandPattern pat)
  
  -- Control flow
  IfExpr c t e -> IfExpr (expandExpr c) (expandExpr t) (expandExpr e)
  LetExpr binds body -> LetExpr (map expandBinding binds) (expandExpr body)
  LetRecExpr binds body -> LetRecExpr (map expandBinding binds) (expandExpr body)
  WithSymbolsExpr syms body -> WithSymbolsExpr syms (expandExpr body)
  
  -- Pattern matching
  MatchExpr mode tgt matcher clauses -> 
    MatchExpr mode (expandExpr tgt) (expandExpr matcher) (map expandClause clauses)
  MatchAllExpr mode tgt matcher clauses ->
    MatchAllExpr mode (expandExpr tgt) (expandExpr matcher) (map expandClause clauses)
  MatchLambdaExpr matcher clauses ->
    MatchLambdaExpr (expandExpr matcher) (map expandClause clauses)
  MatchAllLambdaExpr matcher clauses ->
    MatchAllLambdaExpr (expandExpr matcher) (map expandClause clauses)
  
  -- Matchers
  MatcherExpr patDefs -> 
    MatcherExpr [(pp, expandExpr e, [(dp, expandExpr b) | (dp, b) <- cs]) | (pp, e, cs) <- patDefs]
  AlgebraicDataMatcherExpr ctors ->
    AlgebraicDataMatcherExpr [(n, map expandExpr args) | (n, args) <- ctors]
  
  -- Quote/math expressions
  QuoteExpr e -> QuoteExpr (expandExpr e)
  QuoteSymbolExpr e -> QuoteSymbolExpr (expandExpr e)
  WedgeApplyExpr func args -> WedgeApplyExpr (expandExpr func) (map expandExpr args)
  
  -- IO
  DoExpr binds body -> DoExpr (map expandBinding binds) (expandExpr body)
  
  -- Operators
  PrefixExpr op e -> PrefixExpr op (expandExpr e)
  InfixExpr op e1 e2 -> InfixExpr op (expandExpr e1) (expandExpr e2)
  SectionExpr op mL mR -> SectionExpr op (fmap expandExpr mL) (fmap expandExpr mR)
  
  -- Sequence and apply
  SeqExpr e1 e2 -> SeqExpr (expandExpr e1) (expandExpr e2)
  ApplyExpr func args -> ApplyExpr (expandExpr func) (map expandExpr args)
  CApplyExpr f a -> CApplyExpr (expandExpr f) (expandExpr a)
  
  -- Anonymous parameters
  AnonParamFuncExpr n body -> AnonParamFuncExpr n (expandExpr body)
  AnonTupleParamFuncExpr n body -> AnonTupleParamFuncExpr n (expandExpr body)
  AnonListParamFuncExpr n body -> AnonListParamFuncExpr n (expandExpr body)
  AnonParamExpr n -> AnonParamExpr n
  
  -- Tensor operations
  GenerateTensorExpr gen shape -> GenerateTensorExpr (expandExpr gen) (expandExpr shape)
  TensorExpr d s -> TensorExpr (expandExpr d) (expandExpr s)
  TensorContractExpr e -> TensorContractExpr (expandExpr e)
  TensorMapExpr f t -> TensorMapExpr (expandExpr f) (expandExpr t)
  TensorMap2Expr f t1 t2 -> TensorMap2Expr (expandExpr f) (expandExpr t1) (expandExpr t2)
  TransposeExpr idxs t -> TransposeExpr (expandExpr idxs) (expandExpr t)
  FlipIndicesExpr e -> FlipIndicesExpr (expandExpr e)

-- | Expand type class methods in a binding
expandBinding :: BindingExpr -> BindingExpr
expandBinding (Bind pat e) = Bind pat (expandExpr e)
expandBinding (BindWithIndices vwi e) = BindWithIndices vwi (expandExpr e)
expandBinding (BindWithType tvwi e) = BindWithType tvwi (expandExpr e)

-- | Expand type class methods in a match clause
expandClause :: MatchClause -> MatchClause
expandClause (pat, body) = (expandPattern pat, expandExpr body)

-- | Expand type class methods in a pattern
expandPattern :: Pattern -> Pattern
expandPattern pat = case pat of
  WildCard -> WildCard
  PatVar v -> PatVar v
  ValuePat e -> ValuePat (expandExpr e)
  PredPat e -> PredPat (expandExpr e)
  IndexedPat p idxs -> IndexedPat (expandPattern p) (map expandExpr idxs)
  LetPat binds p -> LetPat (map expandBinding binds) (expandPattern p)
  NotPat p -> NotPat (expandPattern p)
  AndPat p1 p2 -> AndPat (expandPattern p1) (expandPattern p2)
  OrPat p1 p2 -> OrPat (expandPattern p1) (expandPattern p2)
  ForallPat p1 p2 -> ForallPat (expandPattern p1) (expandPattern p2)
  TuplePat ps -> TuplePat (map expandPattern ps)
  InductivePat name ps -> InductivePat name (map expandPattern ps)
  InfixPat op p1 p2 -> InfixPat op (expandPattern p1) (expandPattern p2)
  LoopPat v range p1 p2 -> LoopPat v range (expandPattern p1) (expandPattern p2)
  ContPat -> ContPat
  PApplyPat e ps -> PApplyPat (expandExpr e) (map expandPattern ps)
  VarPat name -> VarPat name
  SeqNilPat -> SeqNilPat
  SeqConsPat p1 p2 -> SeqConsPat (expandPattern p1) (expandPattern p2)
  LaterPatVar -> LaterPatVar
  DApplyPat p ps -> DApplyPat (expandPattern p) (map expandPattern ps)
  InductiveOrPApplyPat name ps -> InductiveOrPApplyPat name (map expandPattern ps)

