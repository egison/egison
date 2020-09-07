{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}

{- |
Module      : Language.Egison.AST
Licence     : MIT

This module defines the syntax of Egison.
-}

module Language.Egison.AST
  ( TopExpr (..)
  , ConstantExpr (..)
  , Expr (..)
  , PatternBase (..)
  , Pattern
  , VarWithIndices (..)
  , makeApply
  , Arg (..)
  , ArgPattern (..)
  , IndexExpr (..)
  , extractIndexExpr
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
  , stringToVarWithIndices
  ) where

import           Data.List       (find)
import           Data.Maybe      (fromJust)
import           Data.Text       (Text)

data TopExpr
  = Define VarWithIndices Expr
  | Test Expr
  | Execute Expr
    -- temporary : we will replace load to import and export
  | LoadFile String
  | Load String
  | InfixDecl Bool Op -- True for pattern infix; False for expression infix
 deriving Show

data ConstantExpr
  = CharExpr Char
  | StringExpr Text
  | BoolExpr Bool
  | IntegerExpr Integer
  | FloatExpr Double
  | SomethingExpr
  | UndefinedExpr
  deriving Show

data Expr
  = ConstantExpr ConstantExpr
  | VarExpr String
  | FreshVarExpr
  | IndexedExpr Bool Expr [IndexExpr Expr]  -- True -> delete old index and append new one
  | SubrefsExpr Bool Expr Expr
  | SuprefsExpr Bool Expr Expr
  | UserrefsExpr Bool Expr Expr
  | TupleExpr [Expr]
  | CollectionExpr [Expr]
  | ConsExpr Expr Expr
  | JoinExpr Expr Expr
  | HashExpr [(Expr, Expr)]
  | VectorExpr [Expr]

  | LambdaExpr [Arg ArgPattern] Expr
  | LambdaExpr' [Arg String] Expr
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
  | WedgeApplyExpr Expr [Expr]

  | DoExpr [BindingExpr] Expr
  | IoExpr Expr

  | PrefixExpr String Expr
  | InfixExpr Op Expr Expr
  | SectionExpr Op (Maybe Expr) (Maybe Expr) -- There cannot be 'SectionExpr op (Just _) (Just _)'

  | SeqExpr Expr Expr
  | ApplyExpr Expr [Expr]
  | CApplyExpr Expr Expr
  | AnonParamFuncExpr Integer Expr
  | AnonParamExpr Integer

  | GenerateTensorExpr Expr Expr
  | TensorExpr Expr Expr
  | TensorContractExpr Expr
  | TensorMapExpr Expr Expr
  | TensorMap2Expr Expr Expr Expr
  | TransposeExpr Expr Expr
  | FlipIndicesExpr Expr                              -- Does not appear in user program

  | FunctionExpr [String]
  deriving Show

data VarWithIndices = VarWithIndices String [IndexExpr String]
  deriving Show

data Arg a
  = ScalarArg a
  | InvertedScalarArg a
  | TensorArg a
  deriving Show

data ArgPattern
  = APWildCard
  | APPatVar String
  | APInductivePat String [Arg ArgPattern]
  | APTuplePat [Arg ArgPattern]
  | APEmptyPat
  | APConsPat (Arg ArgPattern) (Arg ArgPattern)
  | APSnocPat (Arg ArgPattern) (Arg ArgPattern)
  deriving Show

data IndexExpr a
  = Subscript a
  | Superscript a
  | SupSubscript a
  | MultiSubscript a a
  | MultiSuperscript a a
  | Userscript a
  deriving (Show, Eq, Functor, Foldable, Traversable)

extractIndexExpr :: IndexExpr a -> a
extractIndexExpr (Subscript x)    = x
extractIndexExpr (Superscript x)  = x
extractIndexExpr (SupSubscript x) = x
extractIndexExpr (Userscript x)   = x
extractIndexExpr _                = error "extractIndexExpr: Not supported"

data PMMode = BFSMode | DFSMode
  deriving Show

type Pattern = PatternBase Expr
type BindingExpr = (PrimitiveDataPattern, Expr)
type MatchClause = (Pattern, Expr)
type PatternDef  = (PrimitivePatPattern, Expr, [(PrimitiveDataPattern, Expr)])

data PatternBase expr
  = WildCard
  | PatVar String
  | ValuePat expr
  | PredPat expr
  | IndexedPat (PatternBase expr) [expr]
  | LetPat [(PrimitiveDataPattern, expr)] (PatternBase expr)
  | InfixPat Op (PatternBase expr) (PatternBase expr) -- Includes AndPat,OrPat,InductivePat(cons/join)
  | NotPat (PatternBase expr)
  | AndPat (PatternBase expr) (PatternBase expr)
  | OrPat (PatternBase expr) (PatternBase expr)
  | ForallPat (PatternBase expr) (PatternBase expr)
  | TuplePat [PatternBase expr]
  | InductivePat String [PatternBase expr]
  | LoopPat String (LoopRange expr) (PatternBase expr) (PatternBase expr)
  | ContPat
  | PApplyPat expr [PatternBase expr]
  | VarPat String
  | InductiveOrPApplyPat String [PatternBase expr]
  | SeqNilPat
  | SeqConsPat (PatternBase expr) (PatternBase expr)
  | LaterPatVar
  -- For symbolic computing
  | DApplyPat (PatternBase expr) [PatternBase expr]
  deriving Show

data LoopRange expr = LoopRange expr expr (PatternBase expr)
  deriving Show

data PrimitivePatPattern
  = PPWildCard
  | PPPatVar
  | PPValuePat String
  | PPInductivePat String [PrimitivePatPattern]
  | PPTuplePat [PrimitivePatPattern]
  deriving Show

data PrimitiveDataPattern
  = PDWildCard
  | PDPatVar String
  | PDInductivePat String [PrimitiveDataPattern]
  | PDTuplePat [PrimitiveDataPattern]
  | PDEmptyPat
  | PDConsPat PrimitiveDataPattern PrimitiveDataPattern
  | PDSnocPat PrimitiveDataPattern PrimitiveDataPattern
  | PDConstantPat ConstantExpr
  deriving Show

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
  , Op "*$" 7 Prefix False -- For InvertedScalarArg
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

stringToVarWithIndices :: String -> VarWithIndices
stringToVarWithIndices name = VarWithIndices name []

makeApply :: String -> [Expr] -> Expr
makeApply func args = ApplyExpr (VarExpr func) args
