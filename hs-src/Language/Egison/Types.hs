{-# Language TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Language.Egison.Types where

import Control.Applicative
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
    Define String EgisonExpr
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
  | LetRecExpr [(String, EgisonExpr)] EgisonExpr
    
  | MatchExpr EgisonExpr EgisonExpr [MatchClause]
  | MatchAllExpr EgisonExpr EgisonExpr MatchClause

  | FunctionExpr EgisonExpr [MatchClause]

  | MatcherExpr MatcherInfoExpr
  
  | DoExpr [Binding] EgisonExpr
    
  | ApplyExpr EgisonExpr EgisonExpr

  | SomethingExpr
  | UndefinedExpr
 deriving (Show)

type Binding = ([String], EgisonExpr)
type MatchClause = (EgisonExpr, EgisonExpr)
type MatcherInfoExpr = [(PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])]

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

--
-- Values
--

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
  | Func Env [String] EgisonExpr
  | PrimitiveFunc PrimitiveFunc
  | IOFunc IOFunc
  | Port String Handle
  | Something
  | EOF

type PrimitiveFunc = [WHNFData] -> Either EgisonError EgisonValue
type IOFunc = [WHNFData] -> EgisonM EgisonValue
type MatcherInfo = [(PrimitivePatPattern, ObjectRef, [(Env, PrimitiveDataPattern, EgisonExpr)])]

instance Show EgisonValue where
  show (Char c) = return c
  show (String s) = s
  show (Bool b) = show b
  show (Integer i) = show i
  show (Float f) = show f
  show (InductiveData name vals) = "<" ++ name ++ " " ++ unwords (map show vals) ++ ">"
  show (Tuple vals) = "[" ++ unwords (map show vals) ++ "]"
  show (Collection vals) = "{" ++ unwords (map show vals) ++ "}"
  show (Func _ names _) = "(lambda [" ++ unwords names ++ "] ...)"
  show (PrimitiveFunc _) = "#<primitive>"
  show Something = "something"
  show _ = undefined

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

data Action =
    OpenInputPort String
  | OpenOutputPort String
  | ClosePort String
  | FlushPort String
  | ReadFromPort String String
  | WriteToPort String String
 deriving (Show)

--
-- Internal Data
--

data Object =
    Thunk Env EgisonExpr
  | WHNF WHNFData

type ObjectRef = IORef Object

data WHNFData =
    Intermediate Intermediate
  | Value EgisonValue

data Intermediate =
    IInductiveData String [ObjectRef]
  | ITuple [ObjectRef]
  | ICollection [Inner]

data Inner =
    IElement ObjectRef
  | ISubCollection ObjectRef

instance Show WHNFData where
  show (Value val) = show val 
  show (Intermediate (IInductiveData name _)) = "<" ++ name ++ " ...>"
  show (Intermediate (ITuple _)) = "[...]"
  show (Intermediate (ICollection _)) = "{...}"

--
-- Environment
--

type Var = (String, [Integer])
type Env = [HashMap Var ObjectRef]

nullEnv :: Env
nullEnv = []

extendEnv :: Env -> [(Var, ObjectRef)] -> Env
extendEnv env = (: env) . HashMap.fromList

refVar :: Env -> Var -> EgisonM ObjectRef
refVar env var = maybe (throwError $ UnboundVariable var) return
                       (msum $ map (HashMap.lookup var) env)

makeThunk :: Env -> EgisonExpr -> EgisonM ObjectRef
makeThunk = ((liftIO . newIORef) .) . Thunk

makeBindings :: [String] -> [ObjectRef] -> EgisonM [(Var, ObjectRef)]
makeBindings (name : names) (ref : refs) =
  (((name, []), ref) :) <$> makeBindings names refs
makeBindings [] [] = return []
makeBindings _ _ = throwError $ strMsg "invalid bindings"

--
-- Errors
--

data EgisonError =
    Parser ParseError
  | UnboundVariable Var
  | TypeMismatch String WHNFData
  | ArgumentsNum Int [WHNFData]
  | NotImplemented String
  | Default String
    
instance Show EgisonError where
  show (Parser error) = "Parse error at: " ++ show error
  show (UnboundVariable (var, nums)) = "Unbound variable: " ++ var ++
                                      concatMap (('_':) . show) nums
  show (TypeMismatch expected found) = "Expected " ++  expected ++
                                        ", but found: " ++ show found
  show (NotImplemented message) = "Not implemented: " ++ message
  show (Default message) = "Error: " ++ message

instance Error EgisonError where
  noMsg = Default "An error has occurred"
  strMsg = Default

newtype EgisonM a = EgisonM {
    runEgisonM :: ErrorT EgisonError IO a
  } deriving (Functor, Monad, MonadIO, MonadError EgisonError)

liftError :: Either EgisonError a -> EgisonM a
liftError = either throwError return
