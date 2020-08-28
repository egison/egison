{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Language.Egison.AST
Licence     : MIT

This module defines the syntax of Egison.
-}

module Language.Egison.AST
  ( TopExpr (..)
  , Expr (..)
  , ConstantExpr (..)
  , Pattern (..)
  , Var (..)
  , VarWithIndices (..)
  , varToVarWithIndices
  , makeApply
  , Arg (..)
  , Index (..)
  , extractIndex
  , extractSupOrSubIndex
  , PMMode (..)
  , BindingExpr
  , MatchClause
  , PatternDef
  , LoopRange (..)
  , PrimitivePatPattern (..)
  , PrimitiveDataPattern (..)
  , Op (..)
  , Assoc (..)
  , reservedExprOp
  , reservedPatternOp
  , findOpFrom
  , stringToVar
  , stringToVarExpr
  , stringToVarWithIndices
  ) where

import           Data.Hashable   (Hashable)
import           Data.List       (find, intercalate)
import           Data.Maybe      (fromJust)
import           Data.List.Split (splitOn)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

data TopExpr
  = Define VarWithIndices Expr
  | Test Expr
  | Execute Expr
    -- temporary : we will replace load to import and export
  | LoadFile String
  | Load String
  | InfixDecl Bool Op -- True for pattern infix; False for expression infix
 deriving (Show, Eq)

data ConstantExpr
  = CharExpr Char
  | StringExpr Text
  | BoolExpr Bool
  | IntegerExpr Integer
  | FloatExpr Double
  | SomethingExpr
  | UndefinedExpr
 deriving (Show, Eq)

data Expr
  = ConstantExpr ConstantExpr
  | VarExpr Var
  | FreshVarExpr
  | IndexedExpr Bool Expr [Index Expr]  -- True -> delete old index and append new one
  | SubrefsExpr Bool Expr Expr
  | SuprefsExpr Bool Expr Expr
  | UserrefsExpr Bool Expr Expr
  | TupleExpr [Expr]
  | CollectionExpr [Expr]
  | ConsExpr Expr Expr
  | JoinExpr Expr Expr
  | HashExpr [(Expr, Expr)]
  | VectorExpr [Expr]

  | LambdaExpr [Arg] Expr
  | MemoizedLambdaExpr [String] Expr
  | CambdaExpr String Expr
  | PatternFunctionExpr [String] Pattern

  | IfExpr Expr Expr Expr
  | LetRecExpr [BindingExpr] Expr
  | WithSymbolsExpr [String] Expr

  | MatchExpr PMMode Expr Expr [MatchClause]
  | MatchAllExpr PMMode Expr Expr [MatchClause]
  | MatchLambdaExpr Expr [MatchClause]
  | MatchAllLambdaExpr Expr [MatchClause]

  | MatcherExpr [PatternDef]
  | AlgebraicDataMatcherExpr [(String, [Expr])]

  | QuoteExpr Expr
  | QuoteSymbolExpr Expr
  | WedgeApplyExpr Expr Expr

  | DoExpr [BindingExpr] Expr
  | IoExpr Expr

  | PrefixExpr String Expr
  | InfixExpr Op Expr Expr
  | SectionExpr Op (Maybe Expr) (Maybe Expr) -- There cannot be 'SectionExpr op (Just _) (Just _)'

  | SeqExpr Expr Expr
  | ApplyExpr Expr Expr
  | CApplyExpr Expr Expr
  | AnonParamFuncExpr Integer Expr
  | AnonParamExpr Integer

  | GenerateTensorExpr Expr Expr
  | TensorExpr Expr Expr
  | TensorContractExpr Expr
  | TensorMapExpr Expr Expr
  | TensorMap2Expr Expr Expr Expr
  | TransposeExpr Expr Expr

  | FunctionExpr [Var]
 deriving (Eq, Show)

data Var = Var [String] [Index ()]
  deriving (Eq, Generic)

data VarWithIndices = VarWithIndices [String] [Index String]
 deriving (Eq)

data Arg
  = ScalarArg String
  | InvertedScalarArg String
  | TensorArg String
 deriving (Eq, Show)

data Index a
  = Subscript a
  | Superscript a
  | SupSubscript a
  | MultiSubscript a a
  | MultiSuperscript a a
  | DFscript Integer Integer -- DifferentialForm
  | Userscript a
 deriving (Eq, Functor, Foldable, Generic, Traversable)

extractIndex :: Index a -> a
extractIndex (Subscript x)    = x
extractIndex (Superscript x)  = x
extractIndex (SupSubscript x) = x
extractIndex (Userscript x)   = x
extractIndex _                = error "extractIndex: Not supported"

extractSupOrSubIndex :: Index a -> Maybe a
extractSupOrSubIndex (Subscript x)    = Just x
extractSupOrSubIndex (Superscript x)  = Just x
extractSupOrSubIndex (SupSubscript x) = Just x
extractSupOrSubIndex _                = Nothing

data PMMode = BFSMode | DFSMode
 deriving (Eq, Show)

type BindingExpr = (PrimitiveDataPattern, Expr)
type MatchClause = (Pattern, Expr)
type PatternDef  = (PrimitivePatPattern, Expr, [(PrimitiveDataPattern, Expr)])

data Pattern
  = WildCard
  | PatVar Var
  | ValuePat Expr
  | PredPat Expr
  | IndexedPat Pattern [Expr]
  | LetPat [BindingExpr] Pattern
  | InfixPat Op Pattern Pattern -- Includes AndPat,OrPat,InductivePat(cons/join)
  | NotPat Pattern
  | AndPat Pattern Pattern
  | OrPat Pattern Pattern
  | ForallPat Pattern Pattern
  | TuplePat [Pattern]
  | InductivePat String [Pattern]
  | LoopPat Var LoopRange Pattern Pattern
  | ContPat
  | PApplyPat Expr [Pattern]
  | VarPat String
  | InductiveOrPApplyPat String [Pattern]
  | SeqNilPat
  | SeqConsPat Pattern Pattern
  | LaterPatVar
  -- For symbolic computing
  | DApplyPat Pattern [Pattern]
 deriving (Eq, Show)

data LoopRange = LoopRange Expr Expr Pattern
 deriving (Eq, Show)

data PrimitivePatPattern
  = PPWildCard
  | PPPatVar
  | PPValuePat String
  | PPInductivePat String [PrimitivePatPattern]
  | PPTuplePat [PrimitivePatPattern]
 deriving (Show, Eq)

data PrimitiveDataPattern
  = PDWildCard
  | PDPatVar Var
  | PDInductivePat String [PrimitiveDataPattern]
  | PDTuplePat [PrimitiveDataPattern]
  | PDEmptyPat
  | PDConsPat PrimitiveDataPattern PrimitiveDataPattern
  | PDSnocPat PrimitiveDataPattern PrimitiveDataPattern
  | PDConstantPat ConstantExpr
 deriving (Show, Eq)

data Op
  = Op { repr     :: String  -- syntastic representation
       , priority :: Int
       , assoc    :: Assoc
       , isWedge  :: Bool    -- True if operator is prefixed with '!'. Only used for expression infix.
       }
  deriving (Eq, Ord, Show)

data Assoc
  = InfixL
  | InfixR
  | InfixN
  | Prefix
  deriving (Eq, Ord)

instance Show Assoc where
  show InfixL = "infixl"
  show InfixR = "infixr"
  show InfixN = "infix"
  show Prefix = "prefix"

reservedExprOp :: [Op]
reservedExprOp =
  [ Op "!"  8 Prefix False -- Wedge
  , Op "-"  7 Prefix False -- Negate
  , Op "%"  7 InfixL False -- primitive function
  , Op "*$" 7 InfixL False -- For InvertedScalarArg
  , Op "++" 5 InfixR False
  , Op "::" 5 InfixR False
  , Op "="  4 InfixL False -- primitive function
  , Op "<=" 4 InfixL False -- primitive function
  , Op ">=" 4 InfixL False -- primitive function
  , Op "<"  4 InfixL False -- primitive function
  , Op ">"  4 InfixL False -- primitive function
  ]

reservedPatternOp :: [Op]
reservedPatternOp =
  [ Op "::" 5 InfixR False  -- cons (desugared)
  , Op "++" 5 InfixR False  -- join (desugared)
  , Op "&"  3 InfixR False
  , Op "|"  2 InfixR False
  ]

findOpFrom :: String -> [Op] -> Op
findOpFrom op table = fromJust $ find ((== op) . repr) table

instance Hashable (Index ())
instance Hashable Var

stringToVar :: String -> Var
stringToVar name = Var (splitOn "." name) []

stringToVarExpr :: String -> Expr
stringToVarExpr = VarExpr . stringToVar

stringToVarWithIndices :: String -> VarWithIndices
stringToVarWithIndices name = VarWithIndices (splitOn "." name) []

instance Show Var where
  show (Var xs is) = intercalate "." xs ++ concatMap show is

instance Show VarWithIndices where
  show (VarWithIndices xs is) = intercalate "." xs ++ concatMap show is

varToVarWithIndices :: Var -> VarWithIndices
varToVarWithIndices (Var xs is) = VarWithIndices xs $ map f is
 where
   f :: Index () -> Index String
   f index = (\() -> "") <$> index

makeApply :: String -> [Expr] -> Expr
makeApply func args = ApplyExpr (stringToVarExpr func) (TupleExpr args)

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

instance Show (Index Expr) where
  show (Superscript i)  = "~" ++ show i
  show (Subscript i)    = "_" ++ show i
  show (SupSubscript i) = "~_" ++ show i
  show (DFscript _ _)   = ""
  show (Userscript i)   = "|" ++ show i
