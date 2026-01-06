{- |
Module      : Language.Egison.Type.TypedIAST
Licence     : MIT

This module defines the Typed Internal AST (TypedIExpr) - Phase 5-6 output.

TypedIExpr is the result of type inference on IExpr and input to TypedDesugar.
It differs from TIExpr (Phase 9) in that:
  - Type class method calls are NOT yet resolved to dictionaries
  - Type class constraints are collected but not resolved
  - This is the intermediate form before dictionary passing transformation

Processing Flow:
  IExpr (Phase 3-4: Desugared, no types)
    ↓ Type Inference (Phase 5-6)
  TypedIExpr (Typed, type classes unresolved)
    ↓ TypedDesugar (Phase 7-8)
  TIExpr (Typed, type classes resolved, executable)
-}

module Language.Egison.Type.TypedIAST
  ( -- * Typed internal expressions
    TypedIExpr(..)
  , TypedINode(..)
  , TypedITopExpr(..)
  , TypedIPattern(..)
  , TypedIPatternNode(..)
  , TypedIBinding
  , TypedIMatchClause
  , TypedIPatternDef
  , TypedILoopRange(..)
  -- * Utility functions
  , tiexprType
  , tiexprNode
  , applySubstToTypedIExpr
  , applySubstToTypedITopExpr
  -- * Pretty printing
  , prettyTypedIExpr
  , prettyTypedITopExpr
  ) where

import           Data.List                    (intercalate)

import           Language.Egison.AST          (ConstantExpr, PMMode, PrimitivePatPattern, PDPatternBase)
import           Language.Egison.IExpr        (Var, Index, IPrimitiveDataPattern)
import           Language.Egison.Type.Types   (Type (..), Constraint (..))
import           Language.Egison.Type.Subst   (Subst, applySubst)
import           Language.Egison.Type.Pretty  (prettyType)

-- | Typed top-level expression (Phase 5-6: After type inference)
-- Carries type information and type class constraints (unresolved)
data TypedITopExpr
  = TypedIDefine String [Constraint] Type TypedIExpr
    -- ^ Define with type: name, constraints, type, body
    -- Constraints are collected but not yet resolved
  | TypedITest TypedIExpr
    -- ^ Test expression (REPL)
  | TypedIExecute TypedIExpr
    -- ^ Execute IO expression
  | TypedILoadFile FilePath
    -- ^ Load file (should not appear after expandLoads)
  | TypedILoad String
    -- ^ Load library (should not appear after expandLoads)
  deriving (Show)

-- | Typed internal expression (Phase 5-6: After type inference)
-- Each node carries its inferred type and may include type class method calls
data TypedIExpr = TypedIExpr
  { tiexprType :: Type                  -- ^ Inferred type
  , tiexprNode :: TypedINode            -- ^ Typed expression node
  } deriving (Show)

-- | Typed expression nodes
data TypedINode
  = TIConstantExpr ConstantExpr
  | TIVarExpr String
  | TIInductiveDataExpr String [TypedIExpr]
  | TITupleExpr [TypedIExpr]
  | TICollectionExpr [TypedIExpr]
  | TIConsExpr TypedIExpr TypedIExpr
  | TIJoinExpr TypedIExpr TypedIExpr
  | TIHashExpr [(TypedIExpr, TypedIExpr)]
  | TIVectorExpr [TypedIExpr]
  
  -- Functions
  | TILambdaExpr (Maybe Var) [(Var, Type)] TypedIExpr
    -- ^ Lambda with typed parameters
  | TIMemoizedLambdaExpr [String] TypedIExpr
  | TICambdaExpr String TypedIExpr
  | TIPatternFunctionExpr [String] TypedIPattern
  
  -- Function application
  | TIApplyExpr TypedIExpr [TypedIExpr]
  | TICApplyExpr TypedIExpr TypedIExpr
  | TIWedgeApplyExpr TypedIExpr [TypedIExpr]
  
  -- Type class method call (NOT yet resolved to dictionary)
  -- This is the key difference from TIExpr (Phase 9)
  | TIClassMethodCall String String [Constraint] [TypedIExpr]
    -- ^ className methodName constraints arguments
    -- Constraints indicate which type variables need which type classes
    -- TypedDesugar will resolve this to dictionary passing
  
  -- Control flow
  | TIIfExpr TypedIExpr TypedIExpr TypedIExpr
  | TILetExpr [TypedIBinding] TypedIExpr
  | TILetRecExpr [TypedIBinding] TypedIExpr
  | TIWithSymbolsExpr [String] TypedIExpr
  | TIDoExpr [TypedIBinding] TypedIExpr
  | TISeqExpr TypedIExpr TypedIExpr
  
  -- Pattern matching (kept as-is, evaluated at runtime)
  | TIMatchExpr PMMode TypedIExpr TypedIExpr [TypedIMatchClause]
  | TIMatchAllExpr PMMode TypedIExpr TypedIExpr [TypedIMatchClause]
  
  -- Matchers
  | TIMatcherExpr [TypedIPatternDef]
  
  -- Tensor operations
  | TIGenerateTensorExpr TypedIExpr TypedIExpr
  | TITensorExpr TypedIExpr TypedIExpr
  | TITensorContractExpr TypedIExpr
  | TITensorMapExpr TypedIExpr TypedIExpr
  | TITensorMap2Expr TypedIExpr TypedIExpr TypedIExpr
  | TITransposeExpr TypedIExpr TypedIExpr
  | TIFlipIndicesExpr TypedIExpr
  
  -- Indexed expressions
  | TIIndexedExpr Bool TypedIExpr [Index TypedIExpr]
  | TISubrefsExpr Bool TypedIExpr TypedIExpr
  | TISuprefsExpr Bool TypedIExpr TypedIExpr
  | TIUserrefsExpr Bool TypedIExpr TypedIExpr
  
  -- Math/Symbolic
  | TIQuoteExpr TypedIExpr
  | TIQuoteSymbolExpr TypedIExpr
  | TIFunctionExpr [String]
  
  deriving (Show)

-- | Typed pattern
data TypedIPattern = TypedIPattern
  { tipType :: Type
  , tipNode :: TypedIPatternNode
  } deriving (Show)

data TypedIPatternNode
  = TIPWildCard
  | TIPPatVar String
  | TIPValuePat TypedIExpr
  | TIPPredPat TypedIExpr
  | TIPIndexedPat TypedIPattern [TypedIExpr]
  | TIPLetPat [TypedIBinding] TypedIPattern
  | TIPNotPat TypedIPattern
  | TIPAndPat TypedIPattern TypedIPattern
  | TIPOrPat TypedIPattern TypedIPattern
  | TIPForallPat TypedIPattern TypedIPattern
  | TIPTuplePat [TypedIPattern]
  | TIPInductivePat String [TypedIPattern]
  | TIPLoopPat String TypedILoopRange TypedIPattern TypedIPattern
  | TIPContPat
  | TIPApplyPat TypedIExpr [TypedIPattern]
  | TIPVarPat String
  | TIPInductiveOrPApplyPat String [TypedIPattern]
  | TIPSeqNilPat
  | TIPSeqConsPat TypedIPattern TypedIPattern
  | TIPLaterPatVar
  | TIPDApplyPat TypedIPattern [TypedIPattern]
  deriving (Show)

-- | Typed loop range
data TypedILoopRange = TypedILoopRange TypedIExpr TypedIExpr TypedIPattern
  deriving (Show)

-- | Typed binding
type TypedIBinding = (IPrimitiveDataPattern, TypedIExpr)

-- | Typed match clause
type TypedIMatchClause = (TypedIPattern, TypedIExpr)

-- | Typed pattern definition (for matchers)
type TypedIPatternDef = (PrimitivePatPattern, TypedIExpr, [(IPrimitiveDataPattern, TypedIExpr)])

--------------------------------------------------------------------------------
-- Substitution Application
--------------------------------------------------------------------------------

-- | Apply type substitution to TypedIExpr
applySubstToTypedIExpr :: Subst -> TypedIExpr -> TypedIExpr
applySubstToTypedIExpr s (TypedIExpr ty node) =
  TypedIExpr (applySubst s ty) (applySubstToNode s node)

applySubstToNode :: Subst -> TypedINode -> TypedINode
applySubstToNode s node = case node of
  TIConstantExpr c -> TIConstantExpr c
  TIVarExpr v -> TIVarExpr v
  TIInductiveDataExpr name exprs -> TIInductiveDataExpr name (map (applySubstToTypedIExpr s) exprs)
  TITupleExpr exprs -> TITupleExpr (map (applySubstToTypedIExpr s) exprs)
  TICollectionExpr exprs -> TICollectionExpr (map (applySubstToTypedIExpr s) exprs)
  TIConsExpr e1 e2 -> TIConsExpr (applySubstToTypedIExpr s e1) (applySubstToTypedIExpr s e2)
  TIJoinExpr e1 e2 -> TIJoinExpr (applySubstToTypedIExpr s e1) (applySubstToTypedIExpr s e2)
  TIHashExpr pairs -> TIHashExpr [(applySubstToTypedIExpr s k, applySubstToTypedIExpr s v) | (k, v) <- pairs]
  TIVectorExpr exprs -> TIVectorExpr (map (applySubstToTypedIExpr s) exprs)
  
  TILambdaExpr mvar params body ->
    TILambdaExpr mvar [(v, applySubst s t) | (v, t) <- params] (applySubstToTypedIExpr s body)
  TIMemoizedLambdaExpr names body -> TIMemoizedLambdaExpr names (applySubstToTypedIExpr s body)
  TICambdaExpr name body -> TICambdaExpr name (applySubstToTypedIExpr s body)
  TIPatternFunctionExpr names pat -> TIPatternFunctionExpr names (applySubstToPattern s pat)
  
  TIApplyExpr func args -> TIApplyExpr (applySubstToTypedIExpr s func) (map (applySubstToTypedIExpr s) args)
  TICApplyExpr func arg -> TICApplyExpr (applySubstToTypedIExpr s func) (applySubstToTypedIExpr s arg)
  TIWedgeApplyExpr func args -> TIWedgeApplyExpr (applySubstToTypedIExpr s func) (map (applySubstToTypedIExpr s) args)
  
  TIClassMethodCall className methodName constraints args ->
    TIClassMethodCall className methodName (map (applySubstToConstraint s) constraints) (map (applySubstToTypedIExpr s) args)
  
  TIIfExpr cond thenE elseE -> TIIfExpr (applySubstToTypedIExpr s cond) (applySubstToTypedIExpr s thenE) (applySubstToTypedIExpr s elseE)
  TILetExpr bindings body -> TILetExpr (map (applySubstToBinding s) bindings) (applySubstToTypedIExpr s body)
  TILetRecExpr bindings body -> TILetRecExpr (map (applySubstToBinding s) bindings) (applySubstToTypedIExpr s body)
  TIWithSymbolsExpr syms body -> TIWithSymbolsExpr syms (applySubstToTypedIExpr s body)
  TIDoExpr bindings body -> TIDoExpr (map (applySubstToBinding s) bindings) (applySubstToTypedIExpr s body)
  TISeqExpr e1 e2 -> TISeqExpr (applySubstToTypedIExpr s e1) (applySubstToTypedIExpr s e2)
  
  TIMatchExpr mode target matcher clauses ->
    TIMatchExpr mode (applySubstToTypedIExpr s target) (applySubstToTypedIExpr s matcher) (map (applySubstToMatchClause s) clauses)
  TIMatchAllExpr mode target matcher clauses ->
    TIMatchAllExpr mode (applySubstToTypedIExpr s target) (applySubstToTypedIExpr s matcher) (map (applySubstToMatchClause s) clauses)
  
  TIMatcherExpr patDefs -> TIMatcherExpr patDefs  -- TODO: apply subst to pattern defs if needed
  
  TIGenerateTensorExpr fn size -> TIGenerateTensorExpr (applySubstToTypedIExpr s fn) (applySubstToTypedIExpr s size)
  TITensorExpr size body -> TITensorExpr (applySubstToTypedIExpr s size) (applySubstToTypedIExpr s body)
  TITensorContractExpr tensor -> TITensorContractExpr (applySubstToTypedIExpr s tensor)
  TITensorMapExpr fn tensor -> TITensorMapExpr (applySubstToTypedIExpr s fn) (applySubstToTypedIExpr s tensor)
  TITensorMap2Expr fn t1 t2 -> TITensorMap2Expr (applySubstToTypedIExpr s fn) (applySubstToTypedIExpr s t1) (applySubstToTypedIExpr s t2)
  TITransposeExpr indices tensor -> TITransposeExpr (applySubstToTypedIExpr s indices) (applySubstToTypedIExpr s tensor)
  TIFlipIndicesExpr tensor -> TIFlipIndicesExpr (applySubstToTypedIExpr s tensor)
  
  TIIndexedExpr override base indices ->
    TIIndexedExpr override (applySubstToTypedIExpr s base) (map (fmap (applySubstToTypedIExpr s)) indices)
  TISubrefsExpr override base idx -> TISubrefsExpr override (applySubstToTypedIExpr s base) (applySubstToTypedIExpr s idx)
  TISuprefsExpr override base idx -> TISuprefsExpr override (applySubstToTypedIExpr s base) (applySubstToTypedIExpr s idx)
  TIUserrefsExpr override base idx -> TIUserrefsExpr override (applySubstToTypedIExpr s base) (applySubstToTypedIExpr s idx)
  
  TIQuoteExpr e -> TIQuoteExpr (applySubstToTypedIExpr s e)
  TIQuoteSymbolExpr e -> TIQuoteSymbolExpr (applySubstToTypedIExpr s e)
  TIFunctionExpr names -> TIFunctionExpr names

applySubstToConstraint :: Subst -> Constraint -> Constraint
applySubstToConstraint s (Constraint className ty) =
  Constraint className (applySubst s ty)

applySubstToBinding :: Subst -> TypedIBinding -> TypedIBinding
applySubstToBinding s (pat, expr) = (pat, applySubstToTypedIExpr s expr)

applySubstToMatchClause :: Subst -> TypedIMatchClause -> TypedIMatchClause
applySubstToMatchClause s (pat, expr) = (applySubstToPattern s pat, applySubstToTypedIExpr s expr)

applySubstToPattern :: Subst -> TypedIPattern -> TypedIPattern
applySubstToPattern s (TypedIPattern ty node) =
  TypedIPattern (applySubst s ty) node  -- TODO: apply subst to pattern node if needed

-- | Apply substitution to top-level expression
applySubstToTypedITopExpr :: Subst -> TypedITopExpr -> TypedITopExpr
applySubstToTypedITopExpr s topExpr = case topExpr of
  TypedIDefine name constraints ty body ->
    TypedIDefine name (map (applySubstToConstraint s) constraints) (applySubst s ty) (applySubstToTypedIExpr s body)
  TypedITest expr -> TypedITest (applySubstToTypedIExpr s expr)
  TypedIExecute expr -> TypedIExecute (applySubstToTypedIExpr s expr)
  TypedILoadFile path -> TypedILoadFile path
  TypedILoad lib -> TypedILoad lib

--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------

prettyTypedITopExpr :: TypedITopExpr -> String
prettyTypedITopExpr (TypedIDefine name constraints ty body) =
  let constraintStr = if null constraints
                      then ""
                      else "{" ++ intercalate ", " (map prettyConstraint constraints) ++ "} "
  in "def " ++ name ++ " " ++ constraintStr ++ ": " ++ prettyType ty ++ " := " ++ prettyTypedIExpr body
prettyTypedITopExpr (TypedITest expr) = "test " ++ prettyTypedIExpr expr
prettyTypedITopExpr (TypedIExecute expr) = "execute " ++ prettyTypedIExpr expr
prettyTypedITopExpr (TypedILoadFile path) = "loadFile \"" ++ path ++ "\""
prettyTypedITopExpr (TypedILoad lib) = "load \"" ++ lib ++ "\""

prettyConstraint :: Constraint -> String
prettyConstraint (Constraint className ty) = className ++ " " ++ prettyType ty

prettyTypedIExpr :: TypedIExpr -> String
prettyTypedIExpr (TypedIExpr ty node) =
  "(" ++ prettyNode node ++ " : " ++ prettyType ty ++ ")"

prettyNode :: TypedINode -> String
prettyNode (TIConstantExpr c) = show c
prettyNode (TIVarExpr v) = v
prettyNode (TIApplyExpr func args) =
  prettyTypedIExpr func ++ " (" ++ intercalate ", " (map prettyTypedIExpr args) ++ ")"
prettyNode (TILambdaExpr _ params body) =
  "\\(" ++ intercalate ", " (map (\(v, t) -> show v ++ ": " ++ prettyType t) params) ++ ") -> " ++ prettyTypedIExpr body
prettyNode (TIClassMethodCall className methodName _ args) =
  className ++ "." ++ methodName ++ " (" ++ intercalate ", " (map prettyTypedIExpr args) ++ ")"
prettyNode (TIIfExpr cond thenE elseE) =
  "if " ++ prettyTypedIExpr cond ++ " then " ++ prettyTypedIExpr thenE ++ " else " ++ prettyTypedIExpr elseE
prettyNode node = show node  -- Fallback for other cases

