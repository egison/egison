{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Language.Egison.Types where

import qualified Data.Map
import Data.IORef
import System.IO
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

--
-- Expressions
--
data EgisonTopExpr =
    Define Binding
  | DefineType String EgisonType
  | DefineClass String EgisonClass
  | Instance String [String] [(String, String)]
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
  | PatVarExpr String [EgisonExpr]
  | VarExpr String [EgisonExpr]
  | SymExpr String
  | VarNameExpr String
  | PatVarOmitExpr String [EgisonExpr]
  | OmitExpr String [EgisonExpr]

  | WildCardExpr
  | ValuePatExpr EgisonExpr
  | PredPatExpr EgisonExpr
  | CutPatExpr EgisonExpr
  | NotPatExpr EgisonExpr
  | AndPatExpr [EgisonExpr]
  | OrPatExpr [EgisonExpr]

  | InductiveDataExpr String [EgisonExpr]
  | TupleExpr [EgisonExpr]
  | CollectionExpr [InnerExpr]
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
    
  | ContextExpr EgisonType [EgisonClass]
 deriving (Show)

data EgisonType =
    CharType
  | BoolType
  | IntegerType
  | FloatType
    
    -- @ Types for variable names
  | VarNameType EgisonType
    
    -- @ Functional types
    -- Example)
    --   (\ Integer Bool) -> FunType IntegerType BoolType
  | FunType EgisonType EgisonType

    -- (Matcher _), (Pattern _)
  | MatcherType EgisonType
  | PatternType EgisonType
    
    -- @ Type constructors for user-defined types
    -- Example)
    --   Nat          -> TypeCons "Nat" []
    --   Tree Integer -> TypeCons "Tree" [IntegerType]
  | TypeCons String [EgisonType]
    
    -- @ Tuples of types
  | TupleType [EgisonType]
    
    -- @ Types for collections
  | CollectionType EgisonType
    
    -- @ Type variable binded to some type
  | VarType String
    
    -- @ Types with contexts
    -- Example)
    --   ($a :: Show ,a) -> TypeContext (PatVarType "a") [ClassContext "Show" [VarType "a"]]
  | TypeContext EgisonType [EgisonContext]
 deriving (Show)
          
data EgisonContext = 
  -- @ Contexts defined by a class predicate
  ClassContext String       -- Names of classes
               [EgisonExpr] -- Type parameters
               deriving (Show)

data EgisonClass =
  -- @ Type classes of egison
  Class [String] ClassInfoExpr
                 deriving (Show)
        
type MatchClause = (EgisonExpr, EgisonExpr)

data PrimitivePatPattern =
    PPWildCard
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
  -- Are these really necessary?
  | PPatBool Bool
  | PPatChar Char
  | PPatInteger Integer
  | PPatFloat Double
 deriving (Show)

data InnerExpr =
    ElementExpr EgisonExpr
  | SubCollectionExpr EgisonExpr
 deriving (Show)

data InnerTypedExpr =
    ElementTypedExpr EgisonTypedExpr
  | SubCollectionTypedExpr EgisonTypedExpr
 deriving (Show)

type Binding = (EgisonExpr, EgisonExpr)

type MatcherInfoExpr = [(PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])]

type ClassInfoExpr = [(String, EgisonType)]

--
-- Typed Expression
--


data EgisonTypedExpr =
 Hoge
 deriving (Show)

data Environment =
  Poyo
 deriving (Show)

--
-- Values
--
type ObjectRef = IORef Object

data Object =
    Closure Env EgisonExpr
  | Pattern EgisonPattern
  | Intermidiate EgisonIntermidiate
  | Value EgisonValue
  
data EgisonPattern =
    WildCard
  | PatVar String [Integer]
  | ValuePat Environment EgisonExpr
  | PredPat Environment EgisonExpr [EgisonExpr]
  | CutPat EgisonPattern
  | NotPat EgisonPattern
  | AndPat [EgisonPattern]
  | OrPat [EgisonPattern]
  | TuplePat [EgisonPattern]
  | InductivePat String [EgisonPattern]

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

type FrameList = [(Var, ObjectRef)]

type Frame = Data.Map.Map Var ObjectRef

type FrameRef = IORef Frame

data Env = Environment {
        parentEnv :: (Maybe Env), 
        topFrameRef :: FrameRef
    }

data MatchFlag = MAll | MOne
  
data PClosure = PClosure {pcFrame :: FrameList,
                          pcBody :: ObjectRef
                          }

data MAtom = MAtom {pClosure :: PClosure,
                    maTyp :: ObjectRef,
                    maTarget :: ObjectRef
                    }

data MState = MState {msFrame :: FrameList,
                      mAtoms :: [MAtom]
                      }

---
--- Types for Error Handling
---
data EgisonError =
    Parser ParseError
  | TypeMismatch EgisonExpr EgisonType
  | NotImplemented String
  | Default String
    
instance Show EgisonError where
  show (Parser parseErr) = "Parse error at " ++ ": " ++ show parseErr
  show (TypeMismatch expr typ) = "Type error: The type of a expression '" ++ show expr ++ "' is expected to be '" ++ show typ ++ "'"
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
