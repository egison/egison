{-# Language TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Language.Egison.Types where

import Control.Monad.Error

import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import System.IO
import Text.Parsec (ParseError)
--
-- Expressions
--
data EgisonTopExpr =
    Define Binding
  | Test EgisonExpr
  | Execute [String]
    -- temporary : we will replace load to import and export
  | LoadFile String
  | Load String
 deriving (Show)

data EgisonExpr =
    CharExpr Char
  | StringExpr String
  | BoolExpr Bool
  | IntegerExpr Integer
  | FloatExpr Double
  | VarExpr String [EgisonExpr]

  | InductiveDataExpr String [EgisonExpr]
  | TupleExpr [EgisonExpr]
  | CollectionExpr [InnerExpr]

  | WildCardExpr
  | PatVarExpr String [EgisonExpr]
  | PatVarOmitExpr String [EgisonExpr]
  | OmitExpr String [EgisonExpr]
  | ValuePatExpr EgisonExpr
  | PredPatExpr EgisonExpr
  | CutPatExpr EgisonExpr
  | NotPatExpr EgisonExpr
  | AndPatExpr [EgisonExpr]
  | OrPatExpr [EgisonExpr]

  | LambdaExpr [String] EgisonExpr
  
  | IfExpr EgisonExpr EgisonExpr EgisonExpr
  | LetExpr [Binding] EgisonExpr
  | LetRecExpr [Binding] EgisonExpr
    
  | MatchExpr EgisonExpr EgisonExpr [MatchClause]
  | MatchAllExpr EgisonExpr EgisonExpr MatchClause

  | FunctionExpr EgisonExpr [MatchClause]

  | MatcherExpr MatcherInfoExpr
  
  | DoExpr [Binding] EgisonExpr
    
  | ApplyExpr EgisonExpr EgisonExpr

  | SomethingExpr
  | UndefinedExpr
 deriving (Show)

type MatchClause = (EgisonExpr, EgisonExpr)

data PrimitivePatPattern =
    PPWildCard
  | PPPatVar
  | PPValuePat String
  | PPInductivePat String [PrimitivePatPattern]
 deriving (Show)

data PrimitiveDataPattern =
    PDWildCard
  | PDPatVar String
  | PDInductivePat String [PrimitiveDataPattern]
  | PDEmptyPat
  | PDConsPat PrimitiveDataPattern PrimitiveDataPattern
  | PDSnocPat PrimitiveDataPattern PrimitiveDataPattern
  | PDConstantPat EgisonExpr
 deriving (Show)

data InnerExpr =
    ElementExpr EgisonExpr
  | SubCollectionExpr EgisonExpr
 deriving (Show)

type Binding = ([String], EgisonExpr)

type MatcherInfoExpr = [(PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])]

--
-- Values
--
type ObjectRef = IORef Object

data Object =
    Closure Env EgisonExpr
  | Intermidiate EgisonIntermidiate
  | Value EgisonValue
  
data EgisonPattern =
    WildCard
  | PatVar String [Integer]
  | ValuePat Env EgisonExpr
  | PredPat Env EgisonExpr [EgisonExpr]
  | CutPat EgisonPattern
  | NotPat EgisonPattern
  | AndPat [EgisonPattern]
  | OrPat [EgisonPattern]
  | TuplePat [EgisonExpr]
  | InductivePat String [EgisonExpr]

data EgisonIntermidiate =
    IInductiveData String [ObjectRef]
  | ITuple [ObjectRef]
  | ICollection [InnerObject]

data EgisonValue =
    World [Action]
  | Char Char
  | String String
  | Bool Bool
  | Integer Integer
  | Float Double
  | InductiveData String [EgisonValue]
  | Tuple [EgisonValue]
  | Collection [EgisonValue]
  | Matcher MatcherInfo
  | Func ObjectRef EgisonExpr Env
  | PrimitiveFunc ([EgisonValue] -> EgisonM EgisonValue)
  | IOFunc ([EgisonValue] -> EgisonM EgisonValue)
  | Port String Handle
  | Something
  | EOF

data InnerObject =
    IElement ObjectRef
  | ISubCollection ObjectRef

data Action =
    OpenInputPort String
  | OpenOutputPort String
  | ClosePort String
  | FlushPort String
  | ReadFromPort String String
  | WriteToPort String String
 deriving (Show)

type MatcherInfo = [(PrimitivePatPattern, ObjectRef, [(Env, PrimitiveDataPattern, EgisonExpr)])]

instance Show EgisonValue where
 show = undefined

--
-- Internal Data
--
type Var = (String, [Integer])

type Frame = HashMap Var ObjectRef

type Env = [Frame]

data MatchFlag = MAll | MOne
  
data MatchAtom = MAtom {maPat :: ObjectRef,
                        maTyp :: ObjectRef,
                        maTgt :: ObjectRef
                         }

data MatchState = MState {msFrame :: Frame,
                          mAtoms :: [MatchAtom]
                          }

---
--- Types for Error Handling
---
data EgisonError =
    Parser ParseError
  | UnboundVariable Var
  | TypeMismatch String EgisonValue
  | NotImplemented String
  | Default String
    
instance Show EgisonError where
  show (Parser error) = "Parse error at: " ++ show error
  show (UnboundVariable (var, nums)) = "Unbound variable: $" ++ var ++
                                      concatMap (('_':) . show) nums
  show (TypeMismatch expected found) = "Expected " ++  expected ++
                                        " ,but found: " ++ show found
  show (NotImplemented message) = "Not implemented: " ++ message
  show (Default message) = "Error: " ++ message

instance Error EgisonError where
  noMsg = Default "An error has occurred"
  strMsg = Default

newtype EgisonM a = EgisonM {
    runEgisonM :: ErrorT EgisonError IO a
  } deriving (Functor, Monad, MonadIO, MonadError EgisonError)
