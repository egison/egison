{- |
Module      : Language.Egison.Type.TypeClassExpand
Licence     : MIT

This module expands type class method calls using type information.
It transforms Expr to Expr, replacing type class method calls with
dictionary-based dispatch.

Pipeline: Parse → Infer → TypeClassExpand → Desugar → Eval

For example, if we have:
  class Eq a where (==) : a -> a -> Bool
  instance Eq Integer where (==) x y := x = y

Then a call like:
  autoEq 1 2
becomes:
  eqIntegerEq 1 2

This eliminates the need for runtime dispatch functions like resolveEq.
-}

module Language.Egison.Type.TypeClassExpand
  ( expandTypeClassMethods
  , expandTopExpr
  ) where

import           Data.Char              (toLower, toUpper)
import qualified Data.Map.Strict        as Map

import           Language.Egison.AST
import           Language.Egison.Type.Check (TypeCheckEnv(..))
import           Language.Egison.Type.Env (TypeEnv, ClassEnv(..), ClassInfo(..), InstanceInfo(..))
import           Language.Egison.Type.Types (Type(..), TyVar(..))

-- | Expand type class method calls in an expression
expandTypeClassMethods :: TypeCheckEnv -> Expr -> Expr
expandTypeClassMethods tcEnv expr = expandExpr (tceClassEnv tcEnv) expr

-- | Expand type class methods in a top-level expression
expandTopExpr :: TypeCheckEnv -> TopExpr -> TopExpr
expandTopExpr tcEnv topExpr = case topExpr of
  Define vwi e -> Define vwi (expandTypeClassMethods tcEnv e)
  DefineWithType tvwi e -> DefineWithType tvwi (expandTypeClassMethods tcEnv e)
  Test e -> Test (expandTypeClassMethods tcEnv e)
  Execute e -> Execute (expandTypeClassMethods tcEnv e)
  -- Other top-level expressions don't contain expressions to expand
  _ -> topExpr

-- | Recursively expand type class methods in an expression
expandExpr :: ClassEnv -> Expr -> Expr
expandExpr classEnv expr = case expr of
  -- Literals and variables - no expansion needed
  ConstantExpr c -> ConstantExpr c
  VarExpr v -> VarExpr v
  FreshVarExpr -> FreshVarExpr
  
  -- Indexed expressions
  IndexedExpr b e idxs -> IndexedExpr b (go e) (map (fmap go) idxs)
  SubrefsExpr b e1 e2 -> SubrefsExpr b (go e1) (go e2)
  SuprefsExpr b e1 e2 -> SuprefsExpr b (go e1) (go e2)
  UserrefsExpr b e1 e2 -> UserrefsExpr b (go e1) (go e2)
  
  -- Collections
  TupleExpr es -> TupleExpr (map go es)
  CollectionExpr es -> CollectionExpr (map go es)
  ConsExpr e1 e2 -> ConsExpr (go e1) (go e2)
  JoinExpr e1 e2 -> JoinExpr (go e1) (go e2)
  HashExpr pairs -> HashExpr [(go k, go v) | (k, v) <- pairs]
  VectorExpr es -> VectorExpr (map go es)
  
  -- Lambda expressions
  LambdaExpr args body -> LambdaExpr args (go body)
  LambdaExpr' args body -> LambdaExpr' args (go body)
  TypedLambdaExpr params retTy body -> TypedLambdaExpr params retTy (go body)
  MemoizedLambdaExpr params body -> MemoizedLambdaExpr params (go body)
  TypedMemoizedLambdaExpr params retTy body -> TypedMemoizedLambdaExpr params retTy (go body)
  CambdaExpr name body -> CambdaExpr name (go body)
  PatternFunctionExpr names pat -> PatternFunctionExpr names (expandPattern classEnv pat)
  
  -- Control flow
  IfExpr c t e -> IfExpr (go c) (go t) (go e)
  LetExpr binds body -> LetExpr (map (expandBinding classEnv) binds) (go body)
  LetRecExpr binds body -> LetRecExpr (map (expandBinding classEnv) binds) (go body)
  WithSymbolsExpr syms body -> WithSymbolsExpr syms (go body)
  
  -- Pattern matching
  MatchExpr mode tgt matcher clauses -> 
    MatchExpr mode (go tgt) (go matcher) (map (expandClause classEnv) clauses)
  MatchAllExpr mode tgt matcher clauses ->
    MatchAllExpr mode (go tgt) (go matcher) (map (expandClause classEnv) clauses)
  MatchLambdaExpr matcher clauses ->
    MatchLambdaExpr (go matcher) (map (expandClause classEnv) clauses)
  MatchAllLambdaExpr matcher clauses ->
    MatchAllLambdaExpr (go matcher) (map (expandClause classEnv) clauses)
  
  -- Matchers
  MatcherExpr patDefs -> 
    MatcherExpr [PatternDef constraints pp (go e) [(dp, go b) | (dp, b) <- cs] | PatternDef constraints pp e cs <- patDefs]
  AlgebraicDataMatcherExpr ctors ->
    AlgebraicDataMatcherExpr [(n, map go args) | (n, args) <- ctors]
  
  -- Quote/math expressions
  QuoteExpr e -> QuoteExpr (go e)
  QuoteSymbolExpr e -> QuoteSymbolExpr (go e)
  WedgeApplyExpr func args -> WedgeApplyExpr (go func) (map go args)
  
  -- IO
  DoExpr binds body -> DoExpr (map (expandBinding classEnv) binds) (go body)
  
  -- Operators
  PrefixExpr op e -> PrefixExpr op (go e)
  InfixExpr op e1 e2 -> InfixExpr op (go e1) (go e2)
  SectionExpr op mL mR -> SectionExpr op (fmap go mL) (fmap go mR)
  
  -- Sequence and apply
  SeqExpr e1 e2 -> SeqExpr (go e1) (go e2)
  
  -- Function application - this is where we expand type class methods!
  ApplyExpr func args ->
    case tryExpandTypeClassCall classEnv func args of
      Just expanded -> expanded
      Nothing -> ApplyExpr (go func) (map go args)
  
  CApplyExpr f a -> CApplyExpr (go f) (go a)
  
  -- Anonymous parameters
  AnonParamFuncExpr n body -> AnonParamFuncExpr n (go body)
  AnonTupleParamFuncExpr n body -> AnonTupleParamFuncExpr n (go body)
  AnonListParamFuncExpr n body -> AnonListParamFuncExpr n (go body)
  AnonParamExpr n -> AnonParamExpr n
  
  -- Tensor operations
  GenerateTensorExpr gen shape -> GenerateTensorExpr (go gen) (go shape)
  TensorExpr d s -> TensorExpr (go d) (go s)
  TensorContractExpr e -> TensorContractExpr (go e)
  TensorMapExpr f t -> TensorMapExpr (go f) (go t)
  TensorMap2Expr f t1 t2 -> TensorMap2Expr (go f) (go t1) (go t2)
  TransposeExpr idxs t -> TransposeExpr (go idxs) (go t)
  FlipIndicesExpr e -> FlipIndicesExpr (go e)
  where
    go = expandExpr classEnv

-- | Try to expand a type class method call
-- For example: autoEq x y -> eqIntegerEq x y (if x has type Integer)
-- Currently expands calls to known auto-dispatch functions
tryExpandTypeClassCall :: ClassEnv -> Expr -> [Expr] -> Maybe Expr
tryExpandTypeClassCall _classEnv func args = case func of
  -- Expand autoEq x y -> eqIntegerEq x y (based on first argument's type)
  VarExpr "autoEq" | length args >= 2 ->
    case getTypeFromExpr (head args) of
      Just ty -> Just $ ApplyExpr 
        (VarExpr (resolveMethodName "Eq" "eq" ty))
        args
      Nothing -> Nothing
  
  VarExpr "autoNeq" | length args >= 2 ->
    case getTypeFromExpr (head args) of
      Just ty -> Just $ ApplyExpr 
        (VarExpr (resolveMethodName "Eq" "neq" ty))
        args
      Nothing -> Nothing
  
  _ -> Nothing

-- | Try to get a type from an expression with type annotation
getTypeFromExpr :: Expr -> Maybe String
getTypeFromExpr expr = case expr of
  -- Look for type-annotated expressions
  -- For now, we can infer types from literal constants
  ConstantExpr c -> getTypeFromConstant c
  -- For typed lambda arguments
  _ -> Nothing

-- | Get type name from a constant
getTypeFromConstant :: ConstantExpr -> Maybe String
getTypeFromConstant c = case c of
  IntegerExpr _ -> Just "Integer"
  FloatExpr _ -> Just "Float"
  StringExpr _ -> Just "String"
  BoolExpr _ -> Just "Bool"
  CharExpr _ -> Just "Char"
  _ -> Nothing

-- | Resolve a type class method name for a specific type
-- e.g., resolveMethodName "Eq" "eq" "Integer" -> "eqIntegerEq"
resolveMethodName :: String -> String -> String -> String
resolveMethodName className methodName typeName =
  lowerFirst className ++ typeName ++ capitalizeFirst methodName

-- | Capitalize first character
capitalizeFirst :: String -> String
capitalizeFirst []     = []
capitalizeFirst (c:cs) = toUpper c : cs

-- | Lowercase first character
lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (c:cs) = toLower c : cs

-- | Expand type class methods in a binding
expandBinding :: ClassEnv -> BindingExpr -> BindingExpr
expandBinding classEnv (Bind pat e) = Bind pat (expandExpr classEnv e)
expandBinding classEnv (BindWithIndices vwi e) = BindWithIndices vwi (expandExpr classEnv e)
expandBinding classEnv (BindWithType tvwi e) = BindWithType tvwi (expandExpr classEnv e)

-- | Expand type class methods in a match clause
expandClause :: ClassEnv -> MatchClause -> MatchClause
expandClause classEnv (pat, body) = (expandPattern classEnv pat, expandExpr classEnv body)

-- | Expand type class methods in a pattern
expandPattern :: ClassEnv -> Pattern -> Pattern
expandPattern classEnv pat = case pat of
  WildCard -> WildCard
  PatVar v -> PatVar v
  ValuePat e -> ValuePat (expandExpr classEnv e)
  PredPat e -> PredPat (expandExpr classEnv e)
  IndexedPat p idxs -> IndexedPat (go p) (map (expandExpr classEnv) idxs)
  LetPat binds p -> LetPat (map (expandBinding classEnv) binds) (go p)
  NotPat p -> NotPat (go p)
  AndPat p1 p2 -> AndPat (go p1) (go p2)
  OrPat p1 p2 -> OrPat (go p1) (go p2)
  ForallPat p1 p2 -> ForallPat (go p1) (go p2)
  TuplePat ps -> TuplePat (map go ps)
  InductivePat name ps -> InductivePat name (map go ps)
  InfixPat op p1 p2 -> InfixPat op (go p1) (go p2)
  LoopPat v range p1 p2 -> LoopPat v range (go p1) (go p2)
  ContPat -> ContPat
  PApplyPat e ps -> PApplyPat (expandExpr classEnv e) (map go ps)
  VarPat name -> VarPat name
  SeqNilPat -> SeqNilPat
  SeqConsPat p1 p2 -> SeqConsPat (go p1) (go p2)
  LaterPatVar -> LaterPatVar
  DApplyPat p ps -> DApplyPat (go p) (map go ps)
  InductiveOrPApplyPat name ps -> InductiveOrPApplyPat name (map go ps)
  where
    go = expandPattern classEnv
