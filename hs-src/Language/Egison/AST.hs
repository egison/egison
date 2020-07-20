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
  ( EgisonTopExpr (..)
  , EgisonExpr (..)
  , EgisonPattern (..)
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
  , Infix (..)
  , Assoc (..)
  , reservedExprInfix
  , reservedPatternInfix
  , findOpFrom
  , stringToVar
  , stringToVarExpr
  ) where

import           Data.Hashable   (Hashable)
import           Data.List       (find, intercalate)
import           Data.Maybe      (fromJust)
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
  | InfixDecl Bool Infix -- True for pattern infix; False for expression infix
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
  | InductiveDataExpr String [EgisonExpr]
  | TupleExpr [EgisonExpr]
  | CollectionExpr [EgisonExpr]
  | ConsExpr EgisonExpr EgisonExpr
  | JoinExpr EgisonExpr EgisonExpr
  | HashExpr [(EgisonExpr, EgisonExpr)]
  | VectorExpr [EgisonExpr]

  | LambdaExpr [Arg] EgisonExpr
  | MemoizedLambdaExpr [String] EgisonExpr
  | CambdaExpr String EgisonExpr
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

  | PrefixExpr String EgisonExpr
  | InfixExpr Infix EgisonExpr EgisonExpr
  | SectionExpr Infix (Maybe EgisonExpr) (Maybe EgisonExpr) -- There cannot be 'SectionExpr op (Just _) (Just _)'

  | SeqExpr EgisonExpr EgisonExpr
  | ApplyExpr EgisonExpr EgisonExpr
  | CApplyExpr EgisonExpr EgisonExpr
  | AnonParamFuncExpr Integer EgisonExpr
  | AnonParamExpr Integer

  | GenerateTensorExpr EgisonExpr EgisonExpr
  | TensorExpr EgisonExpr EgisonExpr
  | TensorContractExpr EgisonExpr
  | TensorMapExpr EgisonExpr EgisonExpr
  | TensorMap2Expr EgisonExpr EgisonExpr EgisonExpr
  | TransposeExpr EgisonExpr EgisonExpr
  | FlipIndicesExpr EgisonExpr                              -- Does not appear in user program

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

type BindingExpr = ([Var], EgisonExpr)
type MatchClause = (EgisonPattern, EgisonExpr)
type PatternDef  = (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])

data EgisonPattern =
    WildCard
  | PatVar Var
  | ValuePat EgisonExpr
  | PredPat EgisonExpr
  | IndexedPat EgisonPattern [EgisonExpr]
  | LetPat [BindingExpr] EgisonPattern
  | InfixPat Infix EgisonPattern EgisonPattern -- Includes AndPat,OrPat,InductivePat(cons/join)
  | NotPat EgisonPattern
  | AndPat EgisonPattern EgisonPattern
  | OrPat EgisonPattern EgisonPattern
  | ForallPat EgisonPattern EgisonPattern
  | TuplePat [EgisonPattern]
  | InductivePat String [EgisonPattern]
  | LoopPat Var LoopRange EgisonPattern EgisonPattern
  | ContPat
  | PApplyPat EgisonExpr [EgisonPattern]
  | VarPat String
  | InductiveOrPApplyPat String [EgisonPattern]
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

data Infix
  = Infix { repr     :: String  -- syntastic representation
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

reservedExprInfix :: [Infix]
reservedExprInfix =
  [ Infix "!"  8 Prefix False -- Wedge
  , Infix "-"  7 Prefix False -- Negate
  , Infix "%"  7 InfixL False -- primitive function
  , Infix "*$" 7 InfixL False -- For InvertedScalarArg
  , Infix "++" 5 InfixR False
  , Infix "::" 5 InfixR False
  , Infix "="  4 InfixL False -- primitive function
  , Infix "<=" 4 InfixL False -- primitive function
  , Infix ">=" 4 InfixL False -- primitive function
  , Infix "<"  4 InfixL False -- primitive function
  , Infix ">"  4 InfixL False -- primitive function
  ]

reservedPatternInfix :: [Infix]
reservedPatternInfix =
  [ Infix "^"  8 InfixL False  -- PowerPat
  , Infix "*"  7 InfixL False  -- MultPat
  , Infix "/"  7 InfixL False  -- DivPat
  , Infix "+"  6 InfixL False  -- PlusPat
  , Infix "::" 5 InfixR False  -- cons (desugared)
  , Infix "++" 5 InfixR False  -- join (desugared)
  , Infix "&"  3 InfixR False
  , Infix "|"  2 InfixR False
  ]

findOpFrom :: String -> [Infix] -> Infix
findOpFrom op table = fromJust $ find ((== op) . repr) table

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

makeApply :: String -> [EgisonExpr] -> EgisonExpr
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

instance Show (Index EgisonExpr) where
  show (Superscript i)  = "~" ++ show i
  show (Subscript i)    = "_" ++ show i
  show (SupSubscript i) = "~_" ++ show i
  show (DFscript _ _)   = ""
  show (Userscript i)   = "|" ++ show i
