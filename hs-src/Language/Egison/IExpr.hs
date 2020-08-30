{-# LANGUAGE FlexibleInstances  #-}

module Language.Egison.IExpr
  ( ITopExpr (..)
  -- Re-export from AST
  , ConstantExpr (..)
  , Expr (..)
  , BindingExpr
  , MatchClause
  , PatternDef
  , stringToVarExpr
  , makeApply
  , Var (..)
  , VarWithIndices (..)
  , PatternCore (..)
  , Pattern
  , LoopRange (..)
  , stringToVar
  , varToVarWithIndices
  , extractIndex
  , Index (..)
  , PMMode (..)
  , PrimitivePatPattern (..)
  , PrimitiveDataPattern (..)
  , Arg (..)
  ) where

import           Language.Egison.AST ( ConstantExpr (..)
                                     , Expr (..)
                                     , BindingExpr
                                     , MatchClause
                                     , PatternDef
                                     , stringToVarExpr
                                     , makeApply
                                     , Var (..)
                                     , VarWithIndices (..)
                                     , PatternCore (..)
                                     , Pattern
                                     , LoopRange (..)
                                     , stringToVar
                                     , varToVarWithIndices
                                     , extractIndex
                                     , Index (..)
                                     , PMMode (..)
                                     , PrimitivePatPattern (..)
                                     , PrimitiveDataPattern (..)
                                     , Arg (..)
                                     )

data ITopExpr
  = IDefine Var Expr
  | ITest Expr
  | IExecute Expr
  | ILoadFile String
  | ILoad String
  deriving Show
