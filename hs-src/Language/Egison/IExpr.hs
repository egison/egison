{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Language.Egison.IExpr
Licence     : MIT

This module defines internal representation of Egison language.
-}

module Language.Egison.IExpr
  ( ITopExpr (..)
  , IExpr (..)
  , IPattern (..)
  , ILoopRange (..)
  , IBindingExpr
  , IMatchClause
  , IPatternDef
  , IPrimitiveDataPattern
  -- Typed versions
  , TITopExpr (..)
  , TIExpr (..)
  , TIPattern (..)
  , TILoopRange (..)
  , TIBindingExpr
  , TIMatchClause
  , tiExprType
  , stripType
  , stripTypeTopExpr
  , Var (..)
  , stringToVar
  , extractNameFromVar
  , Index (..)
  , extractSupOrSubIndex
  , extractIndex
  , makeIApply
  -- Re-export from AST
  , ConstantExpr (..)
  , PMMode (..)
  , PrimitivePatPattern (..)
  , PDPatternBase (..)
  ) where

import           Data.Hashable
import           GHC.Generics        (Generic)

import           Language.Egison.AST (ConstantExpr (..), PDPatternBase (..), PMMode (..), PrimitivePatPattern (..))
import           Language.Egison.Type.Types (Type(..))

data ITopExpr
  = IDefine Var IExpr
  | IDefineMany [(Var, IExpr)]  -- Multiple definitions (for type class instances)
  | ITest IExpr
  | IExecute IExpr
  | ILoadFile String
  | ILoad String
  deriving Show

data IExpr
  = IConstantExpr ConstantExpr
  | IVarExpr String
  | IIndexedExpr Bool IExpr [Index IExpr]
  | ISubrefsExpr Bool IExpr IExpr
  | ISuprefsExpr Bool IExpr IExpr
  | IUserrefsExpr Bool IExpr IExpr
  | IInductiveDataExpr String [IExpr]
  | ITupleExpr [IExpr]
  | ICollectionExpr [IExpr]
  | IConsExpr IExpr IExpr
  | IJoinExpr IExpr IExpr
  | IHashExpr [(IExpr, IExpr)]
  | IVectorExpr [IExpr]
  | ILambdaExpr (Maybe Var) [Var] IExpr
  | IMemoizedLambdaExpr [String] IExpr
  | ICambdaExpr String IExpr
  | IPatternFunctionExpr [String] IPattern
  | IIfExpr IExpr IExpr IExpr
  | ILetRecExpr [IBindingExpr] IExpr
  | ILetExpr [IBindingExpr] IExpr
  | IWithSymbolsExpr [String] IExpr
  | IMatchExpr PMMode IExpr IExpr [IMatchClause]
  | IMatchAllExpr PMMode IExpr IExpr [IMatchClause]
  | IMatcherExpr [IPatternDef]
  | IQuoteExpr IExpr
  | IQuoteSymbolExpr IExpr
  | IWedgeApplyExpr IExpr [IExpr]
  | IDoExpr [IBindingExpr] IExpr
  | ISeqExpr IExpr IExpr
  | IApplyExpr IExpr [IExpr]
  | ICApplyExpr IExpr IExpr
  | IGenerateTensorExpr IExpr IExpr
  | ITensorExpr IExpr IExpr
  | ITensorContractExpr IExpr
  | ITensorMapExpr IExpr IExpr
  | ITensorMap2Expr IExpr IExpr IExpr
  | ITransposeExpr IExpr IExpr
  | IFlipIndicesExpr IExpr
  | IFunctionExpr [String]
  deriving Show

type IBindingExpr = (IPrimitiveDataPattern, IExpr)
type IMatchClause = (IPattern, IExpr)
type IPatternDef  = (PrimitivePatPattern, IExpr, [(IPrimitiveDataPattern, IExpr)])
type IPrimitiveDataPattern = PDPatternBase Var

data IPattern
  = IWildCard
  | IPatVar String
  | IValuePat IExpr
  | IPredPat IExpr
  | IIndexedPat IPattern [IExpr]
  | ILetPat [IBindingExpr] IPattern
  | INotPat IPattern
  | IAndPat IPattern IPattern
  | IOrPat IPattern IPattern
  | IForallPat IPattern IPattern
  | ITuplePat [IPattern]
  | IInductivePat String [IPattern]
  | ILoopPat String ILoopRange IPattern IPattern
  | IContPat
  | IPApplyPat IExpr [IPattern]
  | IVarPat String
  | IInductiveOrPApplyPat String [IPattern]
  | ISeqNilPat
  | ISeqConsPat IPattern IPattern
  | ILaterPatVar
  -- For symbolic computing
  | IDApplyPat IPattern [IPattern]
  deriving Show

data ILoopRange = ILoopRange IExpr IExpr IPattern
  deriving Show

data Index a
  = Sub a
  | Sup a
  | MultiSub a Integer a
  | MultiSup a Integer a
  | SupSub a
  | User a
  | DF Integer Integer
  deriving (Show, Eq, Functor, Foldable, Generic, Traversable)

extractSupOrSubIndex :: Index a -> Maybe a
extractSupOrSubIndex (Sub x)    = Just x
extractSupOrSubIndex (Sup x)    = Just x
extractSupOrSubIndex (SupSub x) = Just x
extractSupOrSubIndex _          = Nothing

extractIndex :: Index a -> a
extractIndex (Sub x)    = x
extractIndex (Sup x)    = x
extractIndex (SupSub x) = x
extractIndex (User x)   = x
extractIndex DF{}       = undefined

data Var = Var String [Index (Maybe Var)]
  deriving (Generic, Show)

-- for eq and hashable
data Var' = Var' String [Index ()]
  deriving (Eq, Generic, Show)

instance Eq Var where
  Var name (MultiSup _ _ _:_) == Var name' is' = Var name [] == Var name' is'
  Var name (MultiSub _ _ _:_) == Var name' is' = Var name [] == Var name' is'
  Var name is == Var name' (MultiSup _ _ _:_)  = Var name is == Var name' []
  Var name is == Var name' (MultiSub _ _ _:_)  = Var name is == Var name' []
  Var name is == Var name' is'                 = Var' name (map (fmap (\_ -> ())) is) == Var' name' (map (fmap (\_ -> ())) is')

instance Hashable a => Hashable (Index a)
instance Hashable Var'
instance Hashable Var where
  hashWithSalt salt (Var name (MultiSup _ _ _:_)) = hashWithSalt salt (Var' name [])
  hashWithSalt salt (Var name (MultiSub _ _ _:_)) = hashWithSalt salt (Var' name [])
  hashWithSalt salt (Var name is) = hashWithSalt salt (Var' name (map (fmap (\_ -> ())) is))

stringToVar :: String -> Var
stringToVar name = Var name []

extractNameFromVar :: Var -> String
extractNameFromVar (Var name _) = name

makeIApply :: String -> [IExpr] -> IExpr
makeIApply fn args = IApplyExpr (IVarExpr fn) args

--
-- Typed Internal Expressions
--
-- TIExpr carries type information alongside the expression.
-- This allows type-based dispatch during evaluation.
--

-- | Typed top-level expression
data TITopExpr
  = TIDefine Type Var TIExpr           -- ^ Typed definition
  | TIDefineMany [(Var, TIExpr)]       -- ^ Multiple definitions
  | TITest TIExpr                      -- ^ Test expression
  | TIExecute TIExpr                   -- ^ Execute expression
  | TILoadFile String                  -- ^ Load file
  | TILoad String                      -- ^ Load library
  deriving Show

-- | Typed internal expression
-- Each expression carries its inferred type
data TIExpr = TIExpr
  { tiType :: Type      -- ^ The type of this expression
  , tiExpr :: IExpr     -- ^ The underlying untyped expression
  } deriving Show

-- | Get the type of a typed expression
tiExprType :: TIExpr -> Type
tiExprType = tiType

-- | Strip type information, returning the untyped expression
stripType :: TIExpr -> IExpr
stripType = tiExpr

-- | Strip type information from top-level expression
stripTypeTopExpr :: TITopExpr -> ITopExpr
stripTypeTopExpr (TIDefine _ var expr) = IDefine var (stripType expr)
stripTypeTopExpr (TIDefineMany bindings) = IDefineMany [(v, stripType e) | (v, e) <- bindings]
stripTypeTopExpr (TITest expr) = ITest (stripType expr)
stripTypeTopExpr (TIExecute expr) = IExecute (stripType expr)
stripTypeTopExpr (TILoadFile file) = ILoadFile file
stripTypeTopExpr (TILoad file) = ILoad file

-- | Typed pattern (for future use)
data TIPattern = TIPattern
  { tipType :: Type
  , tipPattern :: IPattern
  } deriving Show

-- | Typed loop range
data TILoopRange = TILoopRange TIExpr TIExpr TIPattern
  deriving Show

-- | Typed binding expression
type TIBindingExpr = (IPrimitiveDataPattern, TIExpr)

-- | Typed match clause
type TIMatchClause = (TIPattern, TIExpr)

instance {-# OVERLAPPING #-} Show (Index String) where
  show (Sup s)    = "~" ++ s
  show (Sub s)    = "_" ++ s
  show (SupSub s) = "~_" ++ s
  show (User s)   = "|" ++ s
  show (DF _ _)   = ""
