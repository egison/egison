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
  , Var (..)
  , stringToVar
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

data ITopExpr
  = IDefine Var IExpr
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
  | ILambdaExpr (Maybe String) [String] IExpr
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
  deriving (Eq, Generic, Show)

-- for hashing
data Var' = Var' String [Index ()]
  deriving (Eq, Generic, Show)

instance Hashable a => Hashable (Index a)
instance Hashable Var'
instance Hashable Var where
  hashWithSalt salt (Var name is) = hashWithSalt salt (Var' name (map (fmap (\_ -> ())) is))

stringToVar :: String -> Var
stringToVar name = Var name []

makeIApply :: String -> [IExpr] -> IExpr
makeIApply func args = IApplyExpr (IVarExpr func) args

instance {-# OVERLAPPING #-} Show (Index String) where
  show (Sup s)    = "~" ++ s
  show (Sub s)    = "_" ++ s
  show (SupSub s) = "~_" ++ s
  show (User s)   = "|" ++ s
  show (DF _ _)   = ""
