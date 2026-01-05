{- |
Module      : Language.Egison.Type.TypedAST
Licence     : MIT

This module defines the typed Abstract Syntax Tree (AST) for Egison.
After type inference, the untyped AST (Expr) is transformed into a typed AST (TypedExpr)
where each node carries its inferred type.

The typed AST enables:
1. Type class instance resolution at compile time
2. Better error messages with type information
3. Type-directed optimizations
4. Type erasure for runtime efficiency
-}

module Language.Egison.Type.TypedAST
  ( -- * Typed expressions
    TypedExpr(..)
  , TypedExprNode(..)
  , TypedPattern(..)
  , TypedPatternNode(..)
  , TypedMatchClause
  , TypedBinding(..)
    -- * Typed top-level expressions
  , TypedTopExpr(..)
    -- * Utilities
  , getType
  , setType
  , mapTypedExpr
  , applySubstToTypedExpr
    -- * Pretty printing
  , prettyTypedTopExpr
  , prettyTypedExpr
  , prettyTypedPattern
  ) where

import           Data.List                    (intercalate, unwords)
import           Data.Text                    (Text)

import           Language.Egison.AST          (Arg(..), ConstantExpr, IndexExpr, LoopRange,
                                               Op, PMMode, PDPatternBase(..),
                                               PrimitiveDataPattern, PrimitivePatPattern(..),
                                               VarWithIndices)
import           Language.Egison.Type.Types   (Type (..), TypeScheme, Constraint(..))
import           Language.Egison.Type.Pretty  (prettyType)
import           Language.Egison.Type.Subst   (Subst, applySubst)

-- | A typed expression: pairs an expression node with its inferred type
data TypedExpr = TypedExpr
  { texprType :: Type          -- ^ The inferred type of this expression
  , texprNode :: TypedExprNode -- ^ The expression node
  } deriving (Show)

-- | Get the type of a typed expression
getType :: TypedExpr -> Type
getType = texprType

-- | Set the type of a typed expression
setType :: Type -> TypedExpr -> TypedExpr
setType t e = e { texprType = t }

-- | Apply a substitution to all types in a TypedExpr
applySubstToTypedExpr :: Subst -> TypedExpr -> TypedExpr
applySubstToTypedExpr s (TypedExpr ty node) = 
  TypedExpr (applySubst s ty) (applySubstToNode s node)
  where
    applySubstToNode :: Subst -> TypedExprNode -> TypedExprNode
    applySubstToNode _ (TConstantExpr c) = TConstantExpr c
    applySubstToNode _ (TVarExpr name) = TVarExpr name
    applySubstToNode s' (TIndexedExpr b e idxs) = 
      TIndexedExpr b (applySubstToTypedExpr s' e) (map (fmap (applySubstToTypedExpr s')) idxs)
    applySubstToNode s' (TTupleExpr es) = TTupleExpr (map (applySubstToTypedExpr s') es)
    applySubstToNode s' (TCollectionExpr es) = TCollectionExpr (map (applySubstToTypedExpr s') es)
    applySubstToNode s' (THashExpr pairs) = THashExpr [(applySubstToTypedExpr s' k, applySubstToTypedExpr s' v) | (k, v) <- pairs]
    applySubstToNode s' (TConsExpr e1 e2) = TConsExpr (applySubstToTypedExpr s' e1) (applySubstToTypedExpr s' e2)
    applySubstToNode s' (TJoinExpr e1 e2) = TJoinExpr (applySubstToTypedExpr s' e1) (applySubstToTypedExpr s' e2)
    applySubstToNode s' (TLambdaExpr params body) = TLambdaExpr params (applySubstToTypedExpr s' body)
    applySubstToNode s' (TTypedLambdaExpr params retTy body) = 
      TTypedLambdaExpr (map (\(n, t) -> (n, applySubst s' t)) params) (applySubst s' retTy) (applySubstToTypedExpr s' body)
    applySubstToNode s' (TMemoizedLambdaExpr params body) = TMemoizedLambdaExpr params (applySubstToTypedExpr s' body)
    applySubstToNode s' (TApplyExpr func args) = TApplyExpr (applySubstToTypedExpr s' func) (map (applySubstToTypedExpr s') args)
    applySubstToNode s' (TCApplyExpr f a) = TCApplyExpr (applySubstToTypedExpr s' f) (applySubstToTypedExpr s' a)
    applySubstToNode s' (TIfExpr cond thenE elseE) = 
      TIfExpr (applySubstToTypedExpr s' cond) (applySubstToTypedExpr s' thenE) (applySubstToTypedExpr s' elseE)
    applySubstToNode s' (TLetExpr bindings body) = 
      TLetExpr (map (applySubstToBinding s') bindings) (applySubstToTypedExpr s' body)
    applySubstToNode s' (TLetRecExpr bindings body) = 
      TLetRecExpr (map (applySubstToBinding s') bindings) (applySubstToTypedExpr s' body)
    applySubstToNode s' (TDoExpr bindings body) = 
      TDoExpr (map (applySubstToBinding s') bindings) (applySubstToTypedExpr s' body)
    applySubstToNode s' (TMatchExpr mode target matcher clauses) = 
      TMatchExpr mode (applySubstToTypedExpr s' target) (applySubstToTypedExpr s' matcher) 
        (map (\(p, e) -> (applySubstToPattern s' p, applySubstToTypedExpr s' e)) clauses)
    applySubstToNode s' (TMatchAllExpr mode target matcher clauses) = 
      TMatchAllExpr mode (applySubstToTypedExpr s' target) (applySubstToTypedExpr s' matcher)
        (map (\(p, e) -> (applySubstToPattern s' p, applySubstToTypedExpr s' e)) clauses)
    applySubstToNode s' (TMatchLambdaExpr matcher clauses) = 
      TMatchLambdaExpr (applySubstToTypedExpr s' matcher) 
        (map (\(p, e) -> (applySubstToPattern s' p, applySubstToTypedExpr s' e)) clauses)
    applySubstToNode s' (TMatchAllLambdaExpr matcher clauses) = 
      TMatchAllLambdaExpr (applySubstToTypedExpr s' matcher)
        (map (\(p, e) -> (applySubstToPattern s' p, applySubstToTypedExpr s' e)) clauses)
    applySubstToNode s' (TClassMethodExpr cls meth dict args) = 
      TClassMethodExpr cls meth (applySubstToTypedExpr s' dict) (map (applySubstToTypedExpr s') args)
    applySubstToNode s' (TMatcherExpr patDefs) = 
      TMatcherExpr (map (\(patPat, nextMatcher, clauses) -> 
        (patPat, applySubstToTypedExpr s' nextMatcher, 
         map (\(dataPat, body) -> (dataPat, applySubstToTypedExpr s' body)) clauses)) patDefs)
    applySubstToNode s' (TAlgebraicDataMatcherExpr constructors) = 
      TAlgebraicDataMatcherExpr (map (\(name, args) -> (name, map (applySubstToTypedExpr s') args)) constructors)
    applySubstToNode s' (TGenerateTensorExpr gen shape) = 
      TGenerateTensorExpr (applySubstToTypedExpr s' gen) (applySubstToTypedExpr s' shape)
    applySubstToNode s' (TTensorExpr dataE shapeE) = 
      TTensorExpr (applySubstToTypedExpr s' dataE) (applySubstToTypedExpr s' shapeE)
    applySubstToNode s' (TTensorContractExpr e) = TTensorContractExpr (applySubstToTypedExpr s' e)
    applySubstToNode s' (TTensorMapExpr f t) = TTensorMapExpr (applySubstToTypedExpr s' f) (applySubstToTypedExpr s' t)
    applySubstToNode s' (TTensorMap2Expr f t1 t2) = 
      TTensorMap2Expr (applySubstToTypedExpr s' f) (applySubstToTypedExpr s' t1) (applySubstToTypedExpr s' t2)
    applySubstToNode s' (TTransposeExpr indices tensor) = 
      TTransposeExpr (applySubstToTypedExpr s' indices) (applySubstToTypedExpr s' tensor)
    applySubstToNode s' (TQuoteExpr e) = TQuoteExpr (applySubstToTypedExpr s' e)
    applySubstToNode s' (TQuoteSymbolExpr e) = TQuoteSymbolExpr (applySubstToTypedExpr s' e)
    applySubstToNode s' (TWithSymbolsExpr syms body) = TWithSymbolsExpr syms (applySubstToTypedExpr s' body)
    applySubstToNode _ (TFunctionExpr args) = TFunctionExpr args
    applySubstToNode s' (TIOExpr e) = TIOExpr (applySubstToTypedExpr s' e)
    applySubstToNode s' (TSeqExpr e1 e2) = TSeqExpr (applySubstToTypedExpr s' e1) (applySubstToTypedExpr s' e2)
    applySubstToNode s' (TPatternFunctionExpr params body) = 
      TPatternFunctionExpr params (applySubstToPattern s' body)
    applySubstToNode s' (TSectionExpr op mLeft mRight) = 
      TSectionExpr op (fmap (applySubstToTypedExpr s') mLeft) (fmap (applySubstToTypedExpr s') mRight)
    applySubstToNode s' (TAnonParamFuncExpr arity body) = TAnonParamFuncExpr arity (applySubstToTypedExpr s' body)
    applySubstToNode _ (TAnonParamExpr n) = TAnonParamExpr n
    applySubstToNode _ TFreshVarExpr = TFreshVarExpr
    applySubstToNode s' (TPrefixExpr op e) = TPrefixExpr op (applySubstToTypedExpr s' e)
    applySubstToNode s' (TInfixExpr op e1 e2) = TInfixExpr op (applySubstToTypedExpr s' e1) (applySubstToTypedExpr s' e2)
    
    applySubstToBinding :: Subst -> TypedBinding -> TypedBinding
    applySubstToBinding s' (TBind pat e) = TBind pat (applySubstToTypedExpr s' e)
    applySubstToBinding s' (TBindWithType name ty e) = TBindWithType name (applySubst s' ty) (applySubstToTypedExpr s' e)
    
    applySubstToPattern :: Subst -> TypedPattern -> TypedPattern
    applySubstToPattern s' (TypedPattern ty node) = TypedPattern (applySubst s' ty) (applySubstToPatternNode s' node)
    
    applySubstToPatternNode :: Subst -> TypedPatternNode -> TypedPatternNode
    applySubstToPatternNode _ TPWildCard = TPWildCard
    applySubstToPatternNode _ (TPPatVar name) = TPPatVar name
    applySubstToPatternNode s' (TPValuePat e) = TPValuePat (applySubstToTypedExpr s' e)
    applySubstToPatternNode s' (TPPredPat e) = TPPredPat (applySubstToTypedExpr s' e)
    applySubstToPatternNode s' (TPIndexedPat pat idxs) = 
      TPIndexedPat (applySubstToPattern s' pat) (map (applySubstToTypedExpr s') idxs)
    applySubstToPatternNode s' (TPLetPat bindings pat) = 
      TPLetPat (map (applySubstToBinding s') bindings) (applySubstToPattern s' pat)
    applySubstToPatternNode s' (TPNotPat pat) = TPNotPat (applySubstToPattern s' pat)
    applySubstToPatternNode s' (TPAndPat p1 p2) = TPAndPat (applySubstToPattern s' p1) (applySubstToPattern s' p2)
    applySubstToPatternNode s' (TPOrPat p1 p2) = TPOrPat (applySubstToPattern s' p1) (applySubstToPattern s' p2)
    applySubstToPatternNode s' (TPForallPat p1 p2) = TPForallPat (applySubstToPattern s' p1) (applySubstToPattern s' p2)
    applySubstToPatternNode s' (TPTuplePat pats) = TPTuplePat (map (applySubstToPattern s') pats)
    applySubstToPatternNode s' (TPInductivePat name pats) = TPInductivePat name (map (applySubstToPattern s') pats)
    applySubstToPatternNode s' (TPInfixPat op p1 p2) = TPInfixPat op (applySubstToPattern s' p1) (applySubstToPattern s' p2)
    applySubstToPatternNode s' (TPLoopPat i range p1 p2) = 
      TPLoopPat i range (applySubstToPattern s' p1) (applySubstToPattern s' p2)
    applySubstToPatternNode _ TPContPat = TPContPat
    applySubstToPatternNode s' (TPPApplyPat func pats) = 
      TPPApplyPat (applySubstToTypedExpr s' func) (map (applySubstToPattern s') pats)
    applySubstToPatternNode _ (TPVarPat name) = TPVarPat name
    applySubstToPatternNode _ TPSeqNilPat = TPSeqNilPat
    applySubstToPatternNode s' (TPSeqConsPat p1 p2) = TPSeqConsPat (applySubstToPattern s' p1) (applySubstToPattern s' p2)
    applySubstToPatternNode _ TPLaterPatVar = TPLaterPatVar
    applySubstToPatternNode s' (TPDApplyPat pat pats) = 
      TPDApplyPat (applySubstToPattern s' pat) (map (applySubstToPattern s') pats)

-- | Map over a typed expression, applying a function to all sub-expressions
mapTypedExpr :: (TypedExpr -> TypedExpr) -> TypedExpr -> TypedExpr
mapTypedExpr f (TypedExpr t node) = TypedExpr t (mapNode node)
  where
    mapNode n = case n of
      TConstantExpr c -> TConstantExpr c
      TVarExpr name -> TVarExpr name
      TIndexedExpr b e idxs -> TIndexedExpr b (f e) (map (fmap f) idxs)
      TTupleExpr es -> TTupleExpr (map f es)
      TCollectionExpr es -> TCollectionExpr (map f es)
      THashExpr pairs -> THashExpr [(f k, f v) | (k, v) <- pairs]
      TLambdaExpr argParams body -> TLambdaExpr argParams (f body)
      TApplyExpr func args -> TApplyExpr (f func) (map f args)
      TIfExpr c th el -> TIfExpr (f c) (f th) (f el)
      TLetExpr binds body -> TLetExpr (map mapBinding binds) (f body)
      TMatchExpr m tgt mtch cls -> TMatchExpr m (f tgt) (f mtch) (mapClauses cls)
      TMatchAllExpr m tgt mtch cls -> TMatchAllExpr m (f tgt) (f mtch) (mapClauses cls)
      -- Class method call with resolved dictionary
      TClassMethodExpr cls meth dict args -> 
        TClassMethodExpr cls meth (f dict) (map f args)
      -- Other nodes...
      other -> other
    
    mapBinding (TBind pat e) = TBind pat (f e)
    mapBinding (TBindWithType name ty e) = TBindWithType name ty (f e)
    
    mapClauses = map (\(p, e) -> (p, f e))

-- | Typed expression nodes
-- Each constructor corresponds to an Expr constructor but with typed sub-expressions
data TypedExprNode
  -- Literals
  = TConstantExpr ConstantExpr
  | TVarExpr String
  | TIndexedExpr Bool TypedExpr [IndexExpr TypedExpr]
  
  -- Collections
  | TTupleExpr [TypedExpr]
  | TCollectionExpr [TypedExpr]
  | THashExpr [(TypedExpr, TypedExpr)]
  | TConsExpr TypedExpr TypedExpr
  | TJoinExpr TypedExpr TypedExpr
  
  -- Functions
  -- TLambdaExpr holds Arg String to preserve Arg/InvertedArg info
  | TLambdaExpr [Arg String] TypedExpr
  | TTypedLambdaExpr [(String, Type)] Type TypedExpr
  | TMemoizedLambdaExpr [String] TypedExpr
  | TApplyExpr TypedExpr [TypedExpr]
  | TCApplyExpr TypedExpr TypedExpr
  
  -- Control flow
  | TIfExpr TypedExpr TypedExpr TypedExpr
  | TLetExpr [TypedBinding] TypedExpr
  | TLetRecExpr [TypedBinding] TypedExpr
  | TDoExpr [TypedBinding] TypedExpr
  
  -- Pattern matching
  | TMatchExpr PMMode TypedExpr TypedExpr [TypedMatchClause]
  | TMatchAllExpr PMMode TypedExpr TypedExpr [TypedMatchClause]
  | TMatchLambdaExpr TypedExpr [TypedMatchClause]
  | TMatchAllLambdaExpr TypedExpr [TypedMatchClause]
  
  -- Type class method call (key for instance resolution)
  -- TClassMethodExpr className methodName resolvedDict arguments
  -- The dictionary is resolved during type inference based on the argument types
  | TClassMethodExpr String String TypedExpr [TypedExpr]
  
  -- Matcher
  | TMatcherExpr [(PrimitivePatPattern, TypedExpr, [(PrimitiveDataPattern, TypedExpr)])]
  | TAlgebraicDataMatcherExpr [(String, [TypedExpr])]
  
  -- Tensor operations
  | TGenerateTensorExpr TypedExpr TypedExpr
  | TTensorExpr TypedExpr TypedExpr
  | TTensorContractExpr TypedExpr
  | TTensorMapExpr TypedExpr TypedExpr
  | TTensorMap2Expr TypedExpr TypedExpr TypedExpr
  | TTransposeExpr TypedExpr TypedExpr
  
  -- Math/Symbolic
  | TQuoteExpr TypedExpr
  | TQuoteSymbolExpr TypedExpr
  | TWithSymbolsExpr [String] TypedExpr
  | TFunctionExpr [String]
  
  -- IO
  | TIOExpr TypedExpr
  | TSeqExpr TypedExpr TypedExpr
  
  -- Pattern functions
  | TPatternFunctionExpr [String] TypedPattern
  
  -- Sections
  | TSectionExpr Op (Maybe TypedExpr) (Maybe TypedExpr)
  
  -- Partial application / anonymous parameters
  | TAnonParamFuncExpr Int TypedExpr
  | TAnonParamExpr Int
  
  -- Fresh variable
  | TFreshVarExpr
  
  -- Prefix operator
  | TPrefixExpr String TypedExpr
  
  -- Infix operator
  | TInfixExpr Op TypedExpr TypedExpr
  
  deriving (Show)

-- | Typed pattern
data TypedPattern = TypedPattern
  { tpatType :: Type
  , tpatNode :: TypedPatternNode
  } deriving (Show)

-- | Typed pattern nodes
data TypedPatternNode
  = TPWildCard
  | TPPatVar String
  | TPValuePat TypedExpr
  | TPPredPat TypedExpr
  | TPIndexedPat TypedPattern [TypedExpr]
  | TPLetPat [TypedBinding] TypedPattern
  | TPNotPat TypedPattern
  | TPAndPat TypedPattern TypedPattern
  | TPOrPat TypedPattern TypedPattern
  | TPForallPat TypedPattern TypedPattern
  | TPTuplePat [TypedPattern]
  | TPInductivePat String [TypedPattern]
  | TPInfixPat Op TypedPattern TypedPattern
  | TPLoopPat String LoopRange TypedPattern TypedPattern
  | TPContPat
  | TPPApplyPat TypedExpr [TypedPattern]
  | TPVarPat String
  | TPSeqNilPat
  | TPSeqConsPat TypedPattern TypedPattern
  | TPLaterPatVar
  | TPDApplyPat TypedPattern [TypedPattern]
  deriving (Show)

-- | Typed match clause: (pattern, body)
type TypedMatchClause = (TypedPattern, TypedExpr)

-- | Typed binding expression
data TypedBinding
  = TBind PrimitiveDataPattern TypedExpr
  | TBindWithType String Type TypedExpr
  deriving (Show)

-- | Typed top-level expression
data TypedTopExpr
  = TDefine String TypedExpr
  | TDefineWithType String [Constraint] [(String, Type)] Type TypedExpr
  | TTest TypedExpr
  | TExecute TypedExpr
  | TLoadFile FilePath
  | TLoad String
  -- Type declarations
  | TInductiveDecl String [String] [(String, [Type])]
  | TClassDecl String [String] [String] [(String, [(String, Type)], Type)]
  | TInstanceDecl [Type] String [Type] [(String, [String], TypedExpr)]
  deriving (Show)

--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------

-- | Pretty print a TypedTopExpr
prettyTypedTopExpr :: TypedTopExpr -> String
prettyTypedTopExpr (TDefine varName typedExpr) =
  "def " ++ varName ++ " : " ++ prettyType (texprType typedExpr) ++ " := " ++ prettyTypedExpr typedExpr
prettyTypedTopExpr (TDefineWithType varName constraints params retType typedExpr) =
  let constraintStr = if null constraints
                      then ""
                      else "{" ++ intercalate ", " (map prettyConstraint constraints) ++ "} "
      paramStr = if null params
                 then ""
                 else "(" ++ intercalate ", " (map (\(n, t) -> n ++ ": " ++ prettyType t) params) ++ ") "
  in "def " ++ varName ++ " " ++ constraintStr ++ paramStr ++ ": " ++ prettyType retType ++ " := " ++ prettyTypedExpr typedExpr
  where
    prettyConstraint (Constraint className ty) = className ++ " " ++ prettyType ty
prettyTypedTopExpr (TTest typedExpr) =
  "test " ++ prettyTypedExpr typedExpr
prettyTypedTopExpr (TExecute typedExpr) =
  "execute " ++ prettyTypedExpr typedExpr
prettyTypedTopExpr (TLoadFile path) =
  "loadFile \"" ++ path ++ "\""
prettyTypedTopExpr (TLoad lib) =
  "load \"" ++ lib ++ "\""
prettyTypedTopExpr (TInductiveDecl name params constructors) =
  "inductive " ++ name ++ " " ++ unwords params ++ " := " ++
  intercalate " | " (map (\(n, ts) -> n ++ " " ++ unwords (map prettyType ts)) constructors)
prettyTypedTopExpr (TClassDecl name params supers methods) =
  "class " ++ name ++ " " ++ unwords params ++ " " ++
  (if null supers then "" else "extends " ++ intercalate ", " supers ++ " ") ++
  "where " ++ intercalate "; " (map (\(n, ps, t) -> n ++ " " ++ show ps ++ " : " ++ prettyType t) methods)
prettyTypedTopExpr (TInstanceDecl context className instTypes methods) =
  "instance " ++ show context ++ " " ++ className ++ " " ++ unwords (map prettyType instTypes) ++ " where ..."

-- | Check if a TypedExprNode is an atom (doesn't need parentheses)
isTypedAtom :: TypedExprNode -> Bool
isTypedAtom (TConstantExpr _) = True
isTypedAtom (TVarExpr _) = True
isTypedAtom (TApplyExpr _ []) = True  -- Function with no args is an atom
isTypedAtom _ = False

-- | Pretty print a TypedExpr with type annotation
prettyTypedExpr :: TypedExpr -> String
prettyTypedExpr (TypedExpr ty node) = 
  let exprStr = prettyTypedExpr' node
      typeStr = prettyType ty
  in exprStr ++ " : " ++ typeStr
  where
    prettyTypedExpr' n | isTypedAtom n = prettyTypedExprNode n
                       | otherwise = "(" ++ prettyTypedExprNode n ++ ")"

-- | Pretty print a TypedExprNode (simplified version)
prettyTypedExprNode :: TypedExprNode -> String
prettyTypedExprNode (TConstantExpr c) = show c
prettyTypedExprNode (TVarExpr name) = name
prettyTypedExprNode (TApplyExpr func args) =
  prettyTypedExpr func ++ " (" ++ intercalate ", " (map prettyTypedExpr args) ++ ")"
prettyTypedExprNode (TLambdaExpr params body) =
  "\\" ++ unwords (map (\(Arg n) -> n) params) ++ " -> " ++ prettyTypedExpr body
prettyTypedExprNode (TMatchExpr _ target matcher clauses) =
  "match " ++ prettyTypedExpr target ++ " as " ++ prettyTypedExpr matcher ++ " with " ++
  intercalate " | " (map (\(p, e) -> prettyTypedPattern p ++ " -> " ++ prettyTypedExpr e) clauses)
prettyTypedExprNode (TMatchAllExpr _ target matcher clauses) =
  "matchAll " ++ prettyTypedExpr target ++ " as " ++ prettyTypedExpr matcher ++ " with " ++
  intercalate " | " (map (\(p, e) -> prettyTypedPattern p ++ " -> " ++ prettyTypedExpr e) clauses)
prettyTypedExprNode (TIfExpr cond thenE elseE) =
  "if " ++ prettyTypedExpr cond ++ " then " ++ prettyTypedExpr thenE ++ " else " ++ prettyTypedExpr elseE
prettyTypedExprNode (TTupleExpr es) = "(" ++ intercalate ", " (map prettyTypedExpr es) ++ ")"
prettyTypedExprNode (TCollectionExpr es) = "[" ++ intercalate ", " (map prettyTypedExpr es) ++ "]"
prettyTypedExprNode (TMatcherExpr patDefs) =
  "matcher {" ++ intercalate "; " (map prettyMatcherDef patDefs) ++ "}"
  where
    prettyMatcherDef (patPat, nextMatcher, clauses) =
      prettyPrimitivePatPattern patPat ++ " -> " ++ prettyTypedExpr nextMatcher ++
      " with " ++ intercalate " | " (map (\(dataPat, body) -> 
        prettyPrimitiveDataPattern dataPat ++ " -> " ++ prettyTypedExpr body) clauses)
prettyTypedExprNode (TAlgebraicDataMatcherExpr constructors) =
  "algebraicDataMatcher " ++ intercalate " | " (map (\(name, args) ->
    name ++ " " ++ unwords (map prettyTypedExpr args)) constructors)
prettyTypedExprNode node = show node  -- Fallback for other cases

-- | Pretty print a PrimitivePatPattern
prettyPrimitivePatPattern :: PrimitivePatPattern -> String
prettyPrimitivePatPattern PPWildCard = "_"
prettyPrimitivePatPattern PPPatVar = "$"
prettyPrimitivePatPattern (PPValuePat x) = "#$" ++ x
prettyPrimitivePatPattern (PPInductivePat x pppats) = 
  x ++ " " ++ unwords (map prettyPrimitivePatPattern pppats)
prettyPrimitivePatPattern (PPTuplePat pppats) = 
  "(" ++ intercalate ", " (map prettyPrimitivePatPattern pppats) ++ ")"

-- | Pretty print a PrimitiveDataPattern
prettyPrimitiveDataPattern :: PrimitiveDataPattern -> String
prettyPrimitiveDataPattern PDWildCard = "_"
prettyPrimitiveDataPattern (PDPatVar x) = x
prettyPrimitiveDataPattern (PDInductivePat x pdpats) = 
  x ++ " (" ++ intercalate ", " (map prettyPrimitiveDataPattern pdpats) ++ ")"
prettyPrimitiveDataPattern (PDTuplePat pdpats) = 
  "(" ++ intercalate ", " (map prettyPrimitiveDataPattern pdpats) ++ ")"
prettyPrimitiveDataPattern PDEmptyPat = "[]"
prettyPrimitiveDataPattern (PDConsPat pdp1 pdp2) = 
  prettyPrimitiveDataPattern' pdp1 ++ " :: " ++ prettyPrimitiveDataPattern' pdp2
prettyPrimitiveDataPattern (PDSnocPat pdp1 pdp2) = 
  prettyPrimitiveDataPattern' pdp1 ++ " *: " ++ prettyPrimitiveDataPattern' pdp2
prettyPrimitiveDataPattern (PDConstantPat expr) = show expr  -- ConstantExpr

-- | Helper for pretty printing PrimitiveDataPattern with parentheses when needed
prettyPrimitiveDataPattern' :: PrimitiveDataPattern -> String
prettyPrimitiveDataPattern' p@PDWildCard = prettyPrimitiveDataPattern p
prettyPrimitiveDataPattern' p@(PDPatVar _) = prettyPrimitiveDataPattern p
prettyPrimitiveDataPattern' p@(PDInductivePat _ []) = prettyPrimitiveDataPattern p
prettyPrimitiveDataPattern' p@PDEmptyPat = prettyPrimitiveDataPattern p
prettyPrimitiveDataPattern' p = "(" ++ prettyPrimitiveDataPattern p ++ ")"

-- | Pretty print a TypedPattern (simplified version)
prettyTypedPattern :: TypedPattern -> String
prettyTypedPattern (TypedPattern _ node) = case node of
  TPWildCard -> "_"
  TPPatVar name -> "$" ++ name
  TPValuePat expr -> "#" ++ prettyTypedExpr expr
  TPTuplePat pats -> "(" ++ intercalate ", " (map prettyTypedPattern pats) ++ ")"
  TPInductivePat name pats -> name ++ " " ++ unwords (map prettyTypedPattern pats)
  _ -> show node  -- Fallback

