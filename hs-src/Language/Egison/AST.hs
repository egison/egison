{-# LANGUAGE DeriveGeneric     #-}
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
  , Arg (..)
  , Index (..)
  , PMMode (..)
  , InnerExpr (..)
  , BindingExpr (..)
  , MatchClause (..)
  , PatternDef (..)
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
import qualified Data.Text       as T
import           GHC.Generics    (Generic)

data EgisonTopExpr =
    Define Var EgisonExpr
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
  | MacroExpr [String] EgisonExpr
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
 deriving (Eq)

data Var = Var [String] [Index ()]
  deriving (Eq, Generic)

data Arg =
    ScalarArg String
  | InvertedScalarArg String
  | TensorArg String
 deriving (Eq)

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
 deriving (Eq, Generic)

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
  | LaterPat EgisonPattern
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
 deriving Eq

data LoopRange = LoopRange EgisonExpr EgisonExpr EgisonPattern
 deriving Eq

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
  deriving (Eq, Ord)

instance Show EgisonBinOp where
  show = repr

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

instance Show EgisonExpr where
  show (CharExpr c) = "c#" ++ [c]
  show (StringExpr str) = "\"" ++ T.unpack str ++ "\""
  show (BoolExpr True) = "#t"
  show (BoolExpr False) = "#f"
  show (IntegerExpr n) = show n
  show (FloatExpr x) = show x
  show (VarExpr name) = show name
  show (PartialVarExpr n) = "%" ++ show n
  show (FunctionExpr args) = "(function [" ++ unwords (map show args) ++ "])"
  show (IndexedExpr True expr idxs) = show expr ++ concatMap show idxs
  show (IndexedExpr False expr idxs) = show expr ++ "..." ++ concatMap show idxs
  show (TupleExpr exprs) = "[" ++ unwords (map show exprs) ++ "]"
  show (CollectionExpr ls) = "{" ++ unwords (map show ls) ++ "}"

  show (UnaryOpExpr op e) = op ++ " " ++ show e
  show (BinaryOpExpr op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"

  show (QuoteExpr e) = "'" ++ show e
  show (QuoteSymbolExpr e) = "`" ++ show e

  show (ApplyExpr fn (TupleExpr [])) = "(" ++ show fn ++ ")"
  show (ApplyExpr fn (TupleExpr args)) = "(" ++ show fn ++ " " ++ unwords (map show args) ++ ")"
  show (ApplyExpr fn arg) = "(" ++ show fn ++ " " ++ show arg ++ ")"
  show (VectorExpr xs) = "[| " ++ unwords (map show xs) ++ " |]"
  show (WithSymbolsExpr xs e) = "(withSymbols {" ++ unwords (map show xs) ++ "} " ++ show e ++ ")"
  show _ = "(not supported)"

instance Show Var where
  show (Var xs is) = intercalate "." xs ++ concatMap show is

instance Show Arg where
  show (ScalarArg name)         = "$" ++ name
  show (InvertedScalarArg name) = "*$" ++ name
  show (TensorArg name)         = "%" ++ name

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

instance Show EgisonPattern where
  show WildCard = "_"
  show (PatVar var) = "$" ++ show var
  show (ValuePat expr) = "," ++ show expr
  show (PredPat expr) = "?" ++ show expr
  show (IndexedPat pat exprs) = show pat ++ concatMap (("_" ++) . show) exprs
  show (LetPat bexprs pat) = "(let {" ++ unwords (map (\(vars, expr) -> "[" ++ showVarsHelper vars ++ " " ++ show expr ++ "]") bexprs) ++
                             "} " ++ show pat ++ ")"
    where showVarsHelper [] = ""
          showVarsHelper [v] = "$" ++ show v
          showVarsHelper vs = "[" ++ unwords (map (("$" ++) . show) vs) ++ "]"
  show (LaterPat pat) = "(later " ++ show pat ++ ")"
  show (NotPat pat) = "!" ++ show pat
  show (AndPat pats) = "(&" ++ concatMap ((" " ++) . show) pats ++ ")"
  show (OrPat pats) = "(|" ++ concatMap ((" " ++) . show) pats ++ ")"
  show (TuplePat pats) = "[" ++ unwords (map show pats) ++ "]"
  show (InductivePat name pats) = "<" ++ name ++ concatMap ((" " ++) . show) pats ++ ">"
  show (LoopPat var range pat endPat) = "(loop $" ++ unwords [show var, show range, show pat, show endPat] ++ ")"
  show ContPat = "..."
  show (PApplyPat expr pats) = "(" ++ unwords (show expr : map show pats) ++ ")"
  show (VarPat name) = name
  show SeqNilPat = "{}"
  show (SeqConsPat pat pat') = "{" ++ show pat ++ showSeqPatHelper pat' ++ "}"
    where showSeqPatHelper SeqNilPat = ""
          showSeqPatHelper (SeqConsPat pat pat') = " " ++ show pat ++ showSeqPatHelper pat'
          showSeqPatHelper pat = " " ++ show pat
  show LaterPatVar = "#"

  show (DApplyPat pat pats) = "(" ++ unwords (show pat : map show pats) ++ ")"
  show (DivPat pat pat') = "(/ " ++ show pat ++ " " ++ show pat' ++ ")"
  show (PlusPat pats) = "(+" ++ concatMap ((" " ++) . show) pats
  show (MultPat pats) = "(*" ++ concatMap ((" " ++) . show) pats
  show (PowerPat pat pat') = "(" ++ show pat ++ " ^ " ++ show pat' ++ ")"

instance Show LoopRange where
  show (LoopRange start (ApplyExpr (VarExpr (Var ["from"] [])) (ApplyExpr _ (TupleExpr (x:_)))) endPat) =
    "[" ++ show start ++ " (from " ++ show x ++ ") " ++ show endPat ++ "]"
  show (LoopRange start ends endPat) = "[" ++ show start ++ " " ++ show ends ++ " " ++ show endPat ++ "]"
