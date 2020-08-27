{-# LANGUAGE FlexibleInstances  #-}

module Language.Egison.IExpr
  ( ITopExpr (..)
  , IExpr (..)
  , IBindingExpr
  , IMatchClause
  , IPatternDef
  , IPattern (..)
  , ILoopRange (..)
  , stringToIVarExpr
  , makeIApply
  ) where

import           Language.Egison.AST  hiding (TopExpr, Expr, Pattern, LoopRange)

data ITopExpr
  = IDefine Var IExpr
  | ITest IExpr
  | IExecute IExpr
  | ILoadFile String
  | ILoad String
 deriving (Show, Eq)

data IExpr
  = IConstantExpr ConstantExpr
  | IVarExpr Var
  | IIndexedExpr Bool IExpr [Index IExpr]  -- True -> delete old index and append new one
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
  | IWedgeApplyExpr IExpr IExpr
  | IDoExpr [IBindingExpr] IExpr
  | IIoExpr IExpr
  | ISeqExpr IExpr IExpr
  | IApplyExpr IExpr IExpr
  | ICApplyExpr IExpr IExpr
  | IGenerateTensorExpr IExpr IExpr
  | ITensorExpr IExpr IExpr
  | ITensorContractExpr IExpr
  | ITensorMapExpr IExpr IExpr
  | ITensorMap2Expr IExpr IExpr IExpr
  | ITransposeExpr IExpr IExpr
  | IFlipIndicesExpr IExpr                              -- Does not appear in user program
  | IFunctionExpr [IExpr]
 deriving (Eq, Show)

type IBindingExpr = (PrimitiveDataPattern, IExpr)
type IMatchClause = (IPattern, IExpr)
type IPatternDef  = (PrimitivePatPattern, IExpr, [(PrimitiveDataPattern, IExpr)])

data IPattern
  = IWildCard
  | IPatVar Var
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
  | ILoopPat Var ILoopRange IPattern IPattern
  | IContPat
  | IPApplyPat IExpr [IPattern]
  | IVarPat String
  | IInductiveOrPApplyPat String [IPattern]
  | ISeqNilPat
  | ISeqConsPat IPattern IPattern
  | ILaterPatVar
  -- For symbolic computing
  | IDApplyPat IPattern [IPattern]
 deriving (Eq, Show)

data ILoopRange = ILoopRange IExpr IExpr IPattern
 deriving (Eq, Show)

instance Show (Index IExpr) where
  show (Superscript i)  = "~" ++ show i
  show (Subscript i)    = "_" ++ show i
  show (SupSubscript i) = "~_" ++ show i
  show (DFscript _ _)   = ""
  show (Userscript i)   = "|" ++ show i

stringToIVarExpr :: String -> IExpr
stringToIVarExpr = IVarExpr . stringToVar

makeIApply :: String -> [IExpr] -> IExpr
makeIApply func args = IApplyExpr (stringToIVarExpr func) (ITupleExpr args)
