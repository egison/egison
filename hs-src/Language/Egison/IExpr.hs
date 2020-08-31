{-# LANGUAGE FlexibleInstances  #-}

module Language.Egison.IExpr
  ( ITopExpr (..)
  , IExpr (..)
  , IPattern
  , IBindingExpr
  , IMatchClause
  , IPatternDef
  , stringToIVarExpr
  , makeIApply
  -- Re-export from AST
  , ConstantExpr (..)
  , Var (..)
  , VarWithIndices (..)
  , PatternBase (..)
  , LoopRange (..)
  , stringToVar
  , varToVarWithIndices
  , extractIndex
  , Index (..)
  , PMMode (..)
  , PrimitivePatPattern (..)
  , PrimitiveDataPattern (..)
  ) where

import           Language.Egison.AST ( ConstantExpr (..)
                                     , Var (..)
                                     , VarWithIndices (..)
                                     , PatternBase (..)
                                     , LoopRange (..)
                                     , stringToVar
                                     , varToVarWithIndices
                                     , extractIndex
                                     , Index (..)
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
  | IVarExpr Var
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
  | IFunctionExpr [Var]
  deriving Show

type IPattern = PatternBase IExpr
type IBindingExpr = (PrimitiveDataPattern, IExpr)
type IMatchClause = (IPattern, IExpr)
type IPatternDef  = (PrimitivePatPattern, IExpr, [(PrimitiveDataPattern, IExpr)])

instance Show (Index IExpr) where
  show (Superscript i)  = "~" ++ show i
  show (Subscript i)    = "_" ++ show i
  show (SupSubscript i) = "~_" ++ show i
  show (DFscript _ _)   = ""
  show (Userscript i)   = "|" ++ show i

stringToIVarExpr :: String -> IExpr
stringToIVarExpr = IVarExpr . stringToVar

makeIApply :: String -> [IExpr] -> IExpr
makeIApply func args = IApplyExpr (stringToIVarExpr func) args
