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
  ) where

import           Data.Text                    (Text)

import           Language.Egison.AST          (Arg(..), ConstantExpr, IndexExpr, LoopRange,
                                               Op, PMMode, PrimitiveDataPattern,
                                               PrimitivePatPattern, VarWithIndices)
import           Language.Egison.Type.Types   (Type (..), TypeScheme)

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
  -- TLambdaExpr holds Arg String to preserve ScalarArg/TensorArg info
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
  | TDefineWithType String [(String, Type)] Type TypedExpr
  | TTest TypedExpr
  | TExecute TypedExpr
  | TLoadFile FilePath
  | TLoad String
  -- Type declarations
  | TInductiveDecl String [String] [(String, [Type])]
  | TClassDecl String [String] [String] [(String, [(String, Type)], Type)]
  | TInstanceDecl [Type] String [Type] [(String, [String], TypedExpr)]
  deriving (Show)

