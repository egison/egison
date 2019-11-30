{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Language.Egison.AST
Copyright   : Satoshi Egi
Licence     : MIT

This module defines the syntax of Egison.
-}

module Language.Egison.AST
  ( EgisonTopExpr (..)
  , EgisonExpr (..)
  , EgisonPattern (..)
  , Var (..)
  , VarWithIndices (..)
  , varToVarWithIndices
  , Arg (..)
  , Index (..)
  , PMMode (..)
  , InnerExpr (..)
  , BindingExpr
  , MatchClause
  , PatternDef
  , LoopRange (..)
  , PrimitivePatPattern (..)
  , PrimitiveDataPattern (..)
  , EgisonBinOp (..)
  , BinOpAssoc (..)
  , reservedBinops
  , stringToVar
  , stringToVarExpr
  ) where

import           Data.Hashable   (Hashable)
import           Data.List       (intercalate)
import           Data.List.Split (splitOn)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

data EgisonTopExpr =
    Define Var EgisonExpr
  | DefineWithIndices VarWithIndices EgisonExpr
  | Redefine Var EgisonExpr
  | Test EgisonExpr
  | Execute EgisonExpr
    -- temporary : we will replace load to import and export
  | LoadFile String
  | Load String
 deriving (Show, Eq)

data EgisonExpr =
    CharExpr Char
  | StringExpr Text
  | BoolExpr Bool
  | IntegerExpr Integer
  | FloatExpr Double
  | VarExpr Var
  | FreshVarExpr
  | IndexedExpr Bool EgisonExpr [Index EgisonExpr]  -- True -> delete old index and append new one
  | SubrefsExpr Bool EgisonExpr EgisonExpr
  | SuprefsExpr Bool EgisonExpr EgisonExpr
  | UserrefsExpr Bool EgisonExpr EgisonExpr
  | PowerExpr EgisonExpr EgisonExpr           -- TODO: delete this in v4.0.0
  | InductiveDataExpr String [EgisonExpr]
  | TupleExpr [EgisonExpr]
  | CollectionExpr [InnerExpr]                -- TODO: InnerExpr should be EgisonExpr from v4.0.0
  | ArrayExpr [EgisonExpr]
  | HashExpr [(EgisonExpr, EgisonExpr)]
  | VectorExpr [EgisonExpr]

  | LambdaExpr [Arg] EgisonExpr
  | LambdaArgExpr [Char]
  | MemoizedLambdaExpr [String] EgisonExpr
  | MemoizeExpr [(EgisonExpr, EgisonExpr, EgisonExpr)] EgisonExpr
  | CambdaExpr String EgisonExpr
  | ProcedureExpr [String] EgisonExpr
  | PatternFunctionExpr [String] EgisonPattern

  | IfExpr EgisonExpr EgisonExpr EgisonExpr
  | LetRecExpr [BindingExpr] EgisonExpr
  | LetExpr [BindingExpr] EgisonExpr
  | LetStarExpr [BindingExpr] EgisonExpr
  | WithSymbolsExpr [String] EgisonExpr

  | MatchExpr PMMode EgisonExpr EgisonExpr [MatchClause]
  | MatchAllExpr PMMode EgisonExpr EgisonExpr [MatchClause]
  | MatchLambdaExpr EgisonExpr [MatchClause]
  | MatchAllLambdaExpr EgisonExpr [MatchClause]

  | MatcherExpr [PatternDef]
  | AlgebraicDataMatcherExpr [(String, [EgisonExpr])]

  | QuoteExpr EgisonExpr
  | QuoteSymbolExpr EgisonExpr
  | WedgeApplyExpr EgisonExpr EgisonExpr

  | DoExpr [BindingExpr] EgisonExpr
  | IoExpr EgisonExpr

  | UnaryOpExpr String EgisonExpr
  | BinaryOpExpr EgisonBinOp EgisonExpr EgisonExpr

  | SeqExpr EgisonExpr EgisonExpr
  | ApplyExpr EgisonExpr EgisonExpr
  | CApplyExpr EgisonExpr EgisonExpr
  | PartialExpr Integer EgisonExpr
  | PartialVarExpr Integer

  | GenerateArrayExpr EgisonExpr (EgisonExpr, EgisonExpr)
  | ArrayBoundsExpr EgisonExpr
  | ArrayRefExpr EgisonExpr EgisonExpr

  | GenerateTensorExpr EgisonExpr EgisonExpr
  | TensorExpr EgisonExpr EgisonExpr EgisonExpr EgisonExpr
  | TensorContractExpr EgisonExpr EgisonExpr
  | TensorMapExpr EgisonExpr EgisonExpr
  | TensorMap2Expr EgisonExpr EgisonExpr EgisonExpr
  | TransposeExpr EgisonExpr EgisonExpr
  | FlipIndicesExpr EgisonExpr

  | FunctionExpr [EgisonExpr]

  | SomethingExpr
  | UndefinedExpr
 deriving (Eq, Show)

data Var = Var [String] [Index ()]
  deriving (Eq, Generic)

data VarWithIndices = VarWithIndices [String] [Index String]
 deriving (Eq)

data Arg =
    ScalarArg String
  | InvertedScalarArg String
  | TensorArg String
 deriving (Eq, Show)

data Index a =
    Subscript a
  | Superscript a
  | SupSubscript a
  | MultiSubscript a a
  | MultiSuperscript a a
  | DFscript Integer Integer -- DifferentialForm
  | Userscript a
  | DotSubscript a
  | DotSupscript a
 deriving (Eq, Functor, Generic)

data InnerExpr =
    ElementExpr EgisonExpr
  | SubCollectionExpr EgisonExpr
 deriving (Show, Eq)

data PMMode = BFSMode | DFSMode
 deriving (Eq, Show)

type BindingExpr = ([Var], EgisonExpr)
type MatchClause = (EgisonPattern, EgisonExpr)
type PatternDef  = (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])

-- TODO(momohatt): AndPat and OrPat take only 2 arguments in new syntax
data EgisonPattern =
    WildCard
  | PatVar Var
  | ValuePat EgisonExpr
  | PredPat EgisonExpr
  | IndexedPat EgisonPattern [EgisonExpr]
  | LetPat [BindingExpr] EgisonPattern
  | NotPat EgisonPattern
  | AndPat [EgisonPattern]
  | OrPat [EgisonPattern]
  | TuplePat [EgisonPattern]
  | InductivePat String [EgisonPattern]
  | LoopPat Var LoopRange EgisonPattern EgisonPattern
  | ContPat
  | PApplyPat EgisonExpr [EgisonPattern]
  | VarPat String
  | SeqNilPat
  | SeqConsPat EgisonPattern EgisonPattern
  | LaterPatVar
  -- For symbolic computing
  | DApplyPat EgisonPattern [EgisonPattern]
  | DivPat EgisonPattern EgisonPattern
  | PlusPat [EgisonPattern]
  | MultPat [EgisonPattern]
  | PowerPat EgisonPattern EgisonPattern
 deriving (Eq, Show)

data LoopRange = LoopRange EgisonExpr EgisonExpr EgisonPattern
 deriving (Eq, Show)

data PrimitivePatPattern =
    PPWildCard
  | PPPatVar
  | PPValuePat String
  | PPInductivePat String [PrimitivePatPattern]
  | PPTuplePat [PrimitivePatPattern]
 deriving (Show, Eq)

data PrimitiveDataPattern =
    PDWildCard
  | PDPatVar String
  | PDInductivePat String [PrimitiveDataPattern]
  | PDTuplePat [PrimitiveDataPattern]
  | PDEmptyPat
  | PDConsPat PrimitiveDataPattern PrimitiveDataPattern
  | PDSnocPat PrimitiveDataPattern PrimitiveDataPattern
  | PDConstantPat EgisonExpr
 deriving (Show, Eq)

data EgisonBinOp
  = EgisonBinOp { repr     :: String  -- syntastic representation
                , func     :: String  -- semantics
                , priority :: Int
                , assoc    :: BinOpAssoc
                , isWedge  :: Bool    -- True if operator is prefixed with '!'
                }
  deriving (Eq, Ord, Show)

data BinOpAssoc
  = LeftAssoc
  | RightAssoc
  | NonAssoc
  deriving (Eq, Ord)

instance Show BinOpAssoc where
  show LeftAssoc  = "infixl"
  show RightAssoc = "infixr"
  show NonAssoc   = "infix"

reservedBinops :: [EgisonBinOp]
reservedBinops =
  [ makeBinOp "^"  "**"        8 LeftAssoc
  , makeBinOp "*"  "*"         7 LeftAssoc
  , makeBinOp "/"  "/"         7 LeftAssoc
  , makeBinOp "%"  "remainder" 7 LeftAssoc
  , makeBinOp "+"  "+"         6 LeftAssoc
  , makeBinOp "-"  "-"         6 LeftAssoc
  , makeBinOp "++" "append"    5 RightAssoc
  , makeBinOp "::" "cons"      5 RightAssoc
  , makeBinOp "="  "eq?"       4 LeftAssoc
  , makeBinOp "<=" "lte?"      4 LeftAssoc
  , makeBinOp ">=" "gte?"      4 LeftAssoc
  , makeBinOp "<"  "lt?"       4 LeftAssoc
  , makeBinOp ">"  "gt?"       4 LeftAssoc
  , makeBinOp "&&" "and"       3 RightAssoc
  , makeBinOp "||" "or"        2 RightAssoc
  ]
  where
    makeBinOp r f p a =
      EgisonBinOp { repr = r, func = f, priority = p, assoc = a, isWedge = False }

instance Hashable (Index ())
instance Hashable Var

stringToVar :: String -> Var
stringToVar name = Var (splitOn "." name) []

stringToVarExpr :: String -> EgisonExpr
stringToVarExpr = VarExpr . stringToVar

instance Show Var where
  show (Var xs is) = intercalate "." xs ++ concatMap show is

instance Show VarWithIndices where
  show (VarWithIndices xs is) = intercalate "." xs ++ concatMap show is

varToVarWithIndices :: Var -> VarWithIndices
varToVarWithIndices (Var xs is) = VarWithIndices xs $ map f is
 where
   f :: Index () -> Index String
   f index = (\() -> "") <$> index

instance Show (Index ()) where
  show (Superscript ())  = "~"
  show (Subscript ())    = "_"
  show (SupSubscript ()) = "~_"
  show (DFscript _ _)    = ""
  show (Userscript _)    = "|"

instance Show (Index String) where
  show (Superscript s)  = "~" ++ s
  show (Subscript s)    = "_" ++ s
  show (SupSubscript s) = "~_" ++ s
  show (DFscript _ _)   = ""
  show (Userscript i)   = "|" ++ show i

instance Show (Index EgisonExpr) where
  show (Superscript i)  = "~" ++ show i
  show (Subscript i)    = "_" ++ show i
  show (SupSubscript i) = "~_" ++ show i
  show (DFscript _ _)   = ""
  show (Userscript i)   = "|" ++ show i
