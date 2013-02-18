{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Language.Egison.Types where

import qualified Data.Map
import Data.IORef
import System.IO
import Control.Monad.Error
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
  | SymExpr String
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
    PWildCard
  | PPatVar String
  | PInductivePat String [PrimitiveDataPattern]
  | PEmptyPat
  | PConsPat PrimitiveDataPattern PrimitiveDataPattern
  | PSnocPat PrimitiveDataPattern PrimitiveDataPattern
  | PConstantPat EgisonExpr
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
  | PrimitiveFunc ([EgisonValue] -> ThrowsError EgisonValue)
  | IOFunc ([EgisonValue] -> IOThrowsError EgisonValue)
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

--
-- Internal Data
--
type VarExpr = (String, [EgisonExpr])

type Var = (String, [Integer])

type MatchResult = [(Var, ObjectRef)]

type Frame = Data.Map.Map Var ObjectRef

type FrameRef = IORef Frame

data Env = Environment {
        parentEnv :: (Maybe Env), 
        topFrameRef :: FrameRef
    }

data MatchFlag = MAll | MOne
  
data MatchAtom = MAtom {maPat :: ObjectRef,
                        maTyp :: ObjectRef,
                        maTgt :: ObjectRef
                         }

data MatchState = MState {msFrame :: MatchResult,
                          mAtoms :: [MatchAtom]
                          }

---
--- Types for Error Handling
---
data EgisonError =
    Parser ParseError
  | NotImplemented String
  | Default String
    
instance Show EgisonError where
  show (Parser parseErr) = "Parse error at " ++ ": " ++ show parseErr
  show (NotImplemented message) = "Not implemented: " ++ message
  show (Default message) = "Error: " ++ message
instance Error EgisonError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either EgisonError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err) = error $ "Unexpected error(" ++ show err ++ ") in extractValue; "

type IOThrowsError = ErrorT EgisonError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrowsREPL :: IOThrowsError String -> IO String
runIOThrowsREPL action = runErrorT (trapError action) >>= return . extractValue

runIOThrows :: IOThrowsError String -> IO (Maybe String)
runIOThrows action = do
    runState <- runErrorT action
    case runState of
        Left err -> return $ Just (show err)
        Right _ -> return $ Nothing
