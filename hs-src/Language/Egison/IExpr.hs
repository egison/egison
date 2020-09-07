{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Language.Egison.IExpr
  ( ITopExpr (..)
  , IExpr (..)
  , IPattern
  , IBindingExpr
  , IMatchClause
  , IPatternDef
  , Var (..)
  , Index (..)
  , extractSupOrSubIndex
  , makeIApply
  -- Re-export from AST
  , ConstantExpr (..)
  , VarWithIndices (..)
  , stringToVarWithIndices
  , PatternBase (..)
  , LoopRange (..)
  , stringToVar
  , extractIndex
  , PMMode (..)
  , PrimitivePatPattern (..)
  , PrimitiveDataPattern (..)
  ) where

import           Data.Hashable       (Hashable)
import           GHC.Generics        (Generic)

import           Language.Egison.AST ( ConstantExpr (..)
                                     , VarWithIndices (..)
                                     , stringToVarWithIndices
                                     , PatternBase (..)
                                     , LoopRange (..)
                                     , PMMode (..)
                                     , PrimitivePatPattern (..)
                                     , PrimitiveDataPattern (..)
                                     )

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
  | IIoExpr IExpr
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

type IPattern = PatternBase IExpr
type IBindingExpr = (PrimitiveDataPattern, IExpr)
type IMatchClause = (IPattern, IExpr)
type IPatternDef  = (PrimitivePatPattern, IExpr, [(PrimitiveDataPattern, IExpr)])

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

data Var = Var String [Index ()]
  deriving (Eq, Generic, Show)

instance Hashable (Index ())
instance Hashable Var

stringToVar :: String -> Var
stringToVar name = Var name []

makeIApply :: String -> [IExpr] -> IExpr
makeIApply func args = IApplyExpr (IVarExpr func) args

-- instance Show (Index ()) where
--   show (Sup ())    = "~"
--   show (Sub ())    = "_"
--   show (SupSub ()) = "~_"
--   show (User _)    = "|"
--   show (DF _ _)    = ""

instance {-# OVERLAPPING #-} Show (Index String) where
  show (Sup s)    = "~" ++ s
  show (Sub s)    = "_" ++ s
  show (SupSub s) = "~_" ++ s
  show (User s)   = "|" ++ s
  show (DF _ _)   = ""
