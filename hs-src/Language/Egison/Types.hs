{-# Language TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, UndecidableInstances  #-}
module Language.Egison.Types where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans.Maybe

import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import System.IO
import Text.Parsec (ParseError)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

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
  | VarExpr String
  | IndexedExpr EgisonExpr [EgisonExpr]
  | InductiveDataExpr String [EgisonExpr]
  | TupleExpr [EgisonExpr]
  | CollectionExpr [InnerExpr]
  | ArrayExpr [EgisonExpr]

  | PatternExpr EgisonPattern

  | LambdaExpr [String] EgisonExpr
  
  | IfExpr EgisonExpr EgisonExpr EgisonExpr
  | LetExpr [BindingExpr] EgisonExpr
  | LetRecExpr [BindingExpr] EgisonExpr

  | IndexLoopExpr String String String EgisonExpr EgisonExpr EgisonExpr EgisonExpr
    
  | MatchExpr EgisonExpr EgisonExpr [MatchClause]
  | MatchAllExpr EgisonExpr EgisonExpr MatchClause

  | FunctionExpr EgisonExpr [MatchClause]

  | MatcherExpr MatcherInfo
  
  | DoExpr [BindingExpr] EgisonExpr
    
  | ApplyExpr EgisonExpr EgisonExpr

  | AlgebraicDataMatcherExpr [EgisonExpr]
  | GenerateArrayExpr [String] EgisonExpr EgisonExpr
  | ArraySizeExpr EgisonExpr
  | ArrayRefExpr EgisonExpr EgisonExpr

  | SomethingExpr
  | UndefinedExpr
 deriving (Show)

type BindingExpr = ([String], EgisonExpr)
type MatchClause = (EgisonExpr, EgisonExpr)
type MatcherInfo = [(PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])]

data EgisonPattern =
    WildCard
  | PatVar String
  | ValuePat EgisonExpr
  | PredPat EgisonExpr
  | CutPat EgisonExpr
  | NotPat EgisonExpr
  | AndPat [EgisonExpr]
  | OrPat [EgisonExpr]
  | IndexedPattern EgisonPattern EgisonExpr
  | InductivePattern String [EgisonExpr]
 deriving (Show)

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
    World
  | Char Char
  | String String
  | Bool Bool
  | Integer Integer
  | Float Double
  | InductiveData String [EgisonValue]
  | Tuple [EgisonValue]
  | Collection [EgisonValue]
  | Array (IntMap EgisonValue)
  | Pattern EgisonPattern
  | Matcher Matcher
  | Func Env [String] EgisonExpr
  | PrimitiveFunc PrimitiveFunc
  | IOFunc (EgisonM WHNFData)
  | Port Handle
  | Something
  | Undefined
  | EOF

type Matcher = (Env, MatcherInfo)
type PrimitiveFunc = [WHNFData] -> EgisonM EgisonValue

instance Show EgisonValue where
  show (Char c) = return c
  show (String s) = s
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Integer i) = show i
  show (Float f) = show f
  show (InductiveData name []) = "<" ++ name ++ ">"
  show (InductiveData name vals) = "<" ++ name ++ " " ++ unwords (map show vals) ++ ">"
  show (Tuple vals) = "[" ++ unwords (map show vals) ++ "]"
  show (Collection vals) = "{" ++ unwords (map show vals) ++ "}"
  show (Array vals) = "[|" ++ unwords (map show $ IntMap.elems vals) ++ "|]"
  show (Pattern _) = "#<pattern>"
  show (Matcher _) = "#<matcher>"
  show (Func _ names _) = "(lambda [" ++ unwords names ++ "] ...)"
  show (PrimitiveFunc _) = "#<primitive>"
  show (IOFunc _) = "#<io>"
  show (Port _) = "#<port>"
  show Something = "something"
  show Undefined = "undefined"
  show World = "#<world>"
  show EOF = "#<eof>"

instance Eq EgisonValue where
 (Char c) == (Char c') = c == c'
 (String s) == (String s') = s == s'
 (Bool b) == (Bool b') = b == b'
 (Integer i) == (Integer i') = i == i'
 (Float f) == (Float f') = f == f'
 (InductiveData name vals) == (InductiveData name' vals') = name == name' && vals == vals'
 (Tuple vals) == (Tuple vals') = vals == vals'
 (Array vals) == (Array vals') = vals == vals'
 (Collection vals) == (Collection vals') = vals == vals'
 _ == _ = False

--
-- Internal Data
--

data Object =
    Thunk (EgisonM WHNFData)
  | WHNF WHNFData

type ObjectRef = IORef Object

data WHNFData =
    Intermediate Intermediate
  | Value EgisonValue

data Intermediate =
    IInductiveData String [ObjectRef]
  | ITuple [ObjectRef]
  | ICollection [Inner]
  | IArray (IntMap ObjectRef)

data Inner =
    IElement ObjectRef
  | ISubCollection ObjectRef
  
instance Show WHNFData where
  show (Value val) = show val 
  show (Intermediate (IInductiveData name _)) = "<" ++ name ++ " ...>"
  show (Intermediate (ITuple _)) = "[...]"
  show (Intermediate (ICollection _)) = "{...}"
  show (Intermediate (IArray _)) = "[|...|]" 

fromCharValue :: WHNFData -> Either EgisonError Char
fromCharValue (Value (Char c)) = return c
fromCharValue val = throwError $ TypeMismatch "char" val

fromStringValue :: WHNFData -> Either EgisonError String
fromStringValue (Value (String s)) = return s
fromStringValue val = throwError $ TypeMismatch "string" val

fromBoolValue :: WHNFData -> Either EgisonError Bool
fromBoolValue (Value (Bool b)) = return b
fromBoolValue val = throwError $ TypeMismatch "bool" val

fromIntegerValue :: WHNFData -> Either EgisonError Integer
fromIntegerValue (Value (Integer i)) = return i
fromIntegerValue val = throwError $ TypeMismatch "integer" val

fromFloatValue :: WHNFData -> Either EgisonError Double
fromFloatValue (Value (Float f)) = return f
fromFloatValue val = throwError $ TypeMismatch "float" val

fromPortValue :: WHNFData -> Either EgisonError Handle
fromPortValue (Value (Port handle)) = return handle
fromPortValue val = throwError $ TypeMismatch "port" val

fromPatternValue :: WHNFData -> Either EgisonError EgisonPattern
fromPatternValue (Value (Pattern pattern)) = return pattern
fromPatternValue val = throwError $ TypeMismatch "pattern" val

fromMatcherValue :: WHNFData -> Either EgisonError Matcher
fromMatcherValue (Value (Matcher matcher)) = return matcher
fromMatcherValue val = throwError $ TypeMismatch "matcher" val

fromPrimitiveValue :: WHNFData -> Either EgisonError EgisonValue
fromPrimitiveValue (Value val@(Char _)) = return val
fromPrimitiveValue (Value val@(String _)) = return val
fromPrimitiveValue (Value val@(Bool _)) = return val
fromPrimitiveValue (Value val@(Integer _)) = return val
fromPrimitiveValue (Value val@(Float _)) = return val
fromPrimitiveValue val = throwError $ TypeMismatch "primitive value" val 

--
-- Environment
--

type Var = String
type Env = [HashMap Var ObjectRef]
type Binding = (Var, ObjectRef)

nullEnv :: Env
nullEnv = []

extendEnv :: Env -> [Binding] -> Env
extendEnv env = (: env) . HashMap.fromList

refVar :: Env -> Var -> EgisonM ObjectRef
refVar env var = maybe (throwError $ UnboundVariable var) return
                       (msum $ map (HashMap.lookup var) env)

--
-- Pattern Match
--

data MatchingState = MState Env [Binding] [MatchingTree]

data MatchingTree =
    MAtom EgisonExpr ObjectRef WHNFData
  | MNode [PatternBinding] MatchingState

type PatternBinding = (Var, EgisonExpr)

--
-- Errors
--

data EgisonError =
    Parser ParseError
  | UnboundVariable Var
  | TypeMismatch String WHNFData
  | ArgumentsNum Int Int
  | NotImplemented String
  | Assertion String
  | Match String
  | Desugar String
  | Default String
    
instance Show EgisonError where
  show (Parser error) = "Parse error at: " ++ show error
  show (UnboundVariable var) = "Unbound variable: " ++ var
  show (TypeMismatch expected found) = "Expected " ++  expected ++
                                        ", but found: " ++ show found
  show (ArgumentsNum expected got) = "Wrong number of arguments: expected " ++
                                     show expected ++ ", but got " ++  show got
  show (NotImplemented message) = "Not implemented: " ++ message
  show (Assertion message) = "Assertion failed: " ++ message
  show (Desugar message) = "Error: " ++ message
  show (Default message) = "Error: " ++ message

instance Error EgisonError where
  noMsg = Default "An error has occurred"
  strMsg = Default

liftError :: (MonadError e m) => Either e a -> m a
liftError = either throwError return

--
-- Monads
--

newtype FreshT m a = FreshT { unFreshT :: StateT Int m a }
  deriving (Functor, Applicative, Monad, MonadState Int, MonadTrans)

type Fresh = FreshT Identity

class (Applicative m, Monad m) => MonadFresh m where
  fresh :: m String

instance (Applicative m, Monad m) => MonadFresh (FreshT m) where
  fresh = FreshT $ do counter <- get; modify (+ 1)
                      return $ "$_" ++ show counter

instance (MonadError e m) => MonadError e (FreshT m) where
  throwError = lift . throwError
  catchError m h = FreshT $ catchError (unFreshT m) (unFreshT . h)

instance (MonadState s m) => MonadState s (FreshT m) where
  get = lift $ get
  put s = lift $ put s
  
instance MonadIO (FreshT IO) where
  liftIO = lift

runFreshT :: Monad m => Int -> FreshT m a -> m (a, Int)
runFreshT seed = flip (runStateT . unFreshT) seed

runFresh :: Int -> Fresh a -> (a, Int)
runFresh seed m = runIdentity $ flip runStateT seed $ unFreshT m

newtype EgisonM a = EgisonM {
    unEgisonM :: ErrorT EgisonError (FreshT IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError EgisonError)

runEgisonM :: EgisonM a -> FreshT IO (Either EgisonError a)
runEgisonM = runErrorT . unEgisonM

liftEgisonM :: Fresh (Either EgisonError a) -> EgisonM a
liftEgisonM m = EgisonM $ ErrorT $ FreshT $ do
  s <- get
  (a, s') <- return $ runFresh s m
  put s'
  return $ either throwError return $ a   
  
type MatchM = MaybeT EgisonM

matchFail :: MatchM a
matchFail = MaybeT $ return Nothing

data MList m a = MNil | MCons a (m (MList m a))  

fromList :: Monad m => [a] -> MList m a
fromList = foldr f MNil
 where f x xs = MCons x $ return xs

fromMList :: Monad m => MList m a -> m [a]
fromMList = mfoldr f $ return []
 where f x xs = xs >>= return . (x:)

msingleton :: Monad m => a -> MList m a
msingleton = flip MCons $ return MNil

mfoldr :: Monad m => (a -> m b -> m b) -> m b -> MList m a -> m b
mfoldr f init MNil = init
mfoldr f init (MCons x xs) = f x (xs >>= mfoldr f init)

mappend :: Monad m => MList m a -> m (MList m a) -> m (MList m a)
mappend xs ys = mfoldr ((return .) . MCons) ys xs

mconcat :: Monad m => MList m (MList m a) -> m (MList m a)
mconcat = mfoldr mappend $ return MNil

mmap :: Monad m => (a -> m b) -> MList m a -> m (MList m b)
mmap f = mfoldr g $ return MNil
 where g x xs = f x >>= return . flip MCons xs

mfor :: Monad m => MList m a -> (a -> m b) -> m (MList m b)
mfor = flip mmap

