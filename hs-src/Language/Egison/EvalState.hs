{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Language.Egison.EvalState
Licence     : MIT

This module defines the state during the evaluation.
-}

module Language.Egison.EvalState
  ( EvalState(..)
  , initialEvalState
  , MonadEval(..)
  , mLabelFuncName
  , InstanceEnv
  , MethodDict
  , ConstructorEnv
  , ConstructorInfo(..)
  , PatternConstructorEnv
  ) where

import           Control.Monad.Except
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict

import qualified Data.HashMap.Strict              as HashMap
import           Data.HashMap.Strict              (HashMap)

import           Language.Egison.IExpr
import           Language.Egison.Type.Types       (Type, TypeScheme)
import           Language.Egison.Type.Env          (TypeEnv, ClassEnv, PatternTypeEnv, emptyEnv, emptyClassEnv, emptyPatternEnv, extendEnv, extendPatternEnv)

-- | Instance environment: maps class name -> method name -> type -> implementation
-- The implementation is stored as a function reference (Var name)
type MethodDict = HashMap Type String  -- Type -> implementation function name
type InstanceEnv = HashMap String (HashMap String MethodDict)  -- ClassName -> MethodName -> Dict

-- | Constructor environment: maps constructor name -> constructor info
-- Used for type inference and pattern matching
data ConstructorInfo = ConstructorInfo
  { ctorTypeName :: String      -- ^ The inductive type name, e.g., "Maybe"
  , ctorArgTypes :: [Type]      -- ^ Constructor argument types
  , ctorTypeParams :: [String]  -- ^ Type parameters of the inductive type, e.g., ["a"]
  } deriving (Show, Eq)

type ConstructorEnv = HashMap String ConstructorInfo

-- | Pattern constructor environment: maps pattern constructor name -> type scheme
-- This uses the same format as PatternTypeEnv for consistency
type PatternConstructorEnv = PatternTypeEnv

data EvalState = EvalState
  { funcNameStack  :: [Var]          -- ^ Names of called functions for improved error message
  , instanceEnv    :: InstanceEnv    -- ^ Type class instance environment (runtime dispatch)
  , constructorEnv :: ConstructorEnv -- ^ Inductive data constructor environment
  , typeEnv        :: TypeEnv        -- ^ Type environment (for type inference)
  , classEnv       :: ClassEnv       -- ^ Class environment (for type inference)
  }

initialEvalState :: EvalState
initialEvalState = EvalState 
  { funcNameStack = [] 
  , instanceEnv = HashMap.empty
  , constructorEnv = HashMap.empty
  , typeEnv = emptyEnv
  , classEnv = emptyClassEnv
  }

class (Applicative m, Monad m) => MonadEval m where
  pushFuncName :: Var -> m ()
  topFuncName :: m Var
  popFuncName :: m ()
  getFuncNameStack :: m [Var]
  -- Instance environment operations
  getInstanceEnv :: m InstanceEnv
  registerInstance :: String -> String -> Type -> String -> m ()
  lookupInstance :: String -> String -> Type -> m (Maybe String)
  -- Constructor environment operations
  getConstructorEnv :: m ConstructorEnv
  registerConstructor :: String -> ConstructorInfo -> m ()
  lookupConstructor :: String -> m (Maybe ConstructorInfo)
  -- Type environment operations
  getTypeEnv :: m TypeEnv
  setTypeEnv :: TypeEnv -> m ()
  extendTypeEnv :: String -> TypeScheme -> m ()
  -- Class environment operations
  getClassEnv :: m ClassEnv
  setClassEnv :: ClassEnv -> m ()

instance Monad m => MonadEval (StateT EvalState m) where
  pushFuncName name = do
    st <- get
    put $ st { funcNameStack = name : funcNameStack st }
    return ()
  topFuncName = head . funcNameStack <$> get
  popFuncName = do
    st <- get
    put $ st { funcNameStack = tail $ funcNameStack st }
    return ()
  getFuncNameStack = funcNameStack <$> get
  
  getInstanceEnv = instanceEnv <$> get
  
  registerInstance className methodName ty implName = do
    st <- get
    let env = instanceEnv st
        classDict = HashMap.lookupDefault HashMap.empty className env
        methodDict = HashMap.lookupDefault HashMap.empty methodName classDict
        methodDict' = HashMap.insert ty implName methodDict
        classDict' = HashMap.insert methodName methodDict' classDict
        env' = HashMap.insert className classDict' env
    put $ st { instanceEnv = env' }
  
  lookupInstance className methodName ty = do
    env <- instanceEnv <$> get
    return $ do
      classDict <- HashMap.lookup className env
      methodDict <- HashMap.lookup methodName classDict
      HashMap.lookup ty methodDict
  
  getConstructorEnv = constructorEnv <$> get
  
  registerConstructor ctorName info = do
    st <- get
    let env = constructorEnv st
        env' = HashMap.insert ctorName info env
    put $ st { constructorEnv = env' }
  
  lookupConstructor ctorName = do
    env <- constructorEnv <$> get
    return $ HashMap.lookup ctorName env
  
  getTypeEnv = typeEnv <$> get
  setTypeEnv env = do
    st <- get
    put $ st { typeEnv = env }
  extendTypeEnv name scheme = do
    st <- get
    let env' = extendEnv name scheme (typeEnv st)
    put $ st { typeEnv = env' }
  
  getClassEnv = classEnv <$> get
  setClassEnv env = do
    st <- get
    put $ st { classEnv = env }

instance (MonadEval m) => MonadEval (ExceptT e m) where
  pushFuncName name = lift $ pushFuncName name
  topFuncName = lift topFuncName
  popFuncName = lift popFuncName
  getFuncNameStack = lift getFuncNameStack
  getInstanceEnv = lift getInstanceEnv
  registerInstance cn mn t i = lift $ registerInstance cn mn t i
  lookupInstance cn mn t = lift $ lookupInstance cn mn t
  getConstructorEnv = lift getConstructorEnv
  registerConstructor cn info = lift $ registerConstructor cn info
  lookupConstructor cn = lift $ lookupConstructor cn
  getTypeEnv = lift getTypeEnv
  setTypeEnv = lift . setTypeEnv
  extendTypeEnv name scheme = lift $ extendTypeEnv name scheme
  getClassEnv = lift getClassEnv
  setClassEnv = lift . setClassEnv

mLabelFuncName :: MonadEval m => Maybe Var -> m a -> m a
mLabelFuncName Nothing m = m
mLabelFuncName (Just name) m = do
  pushFuncName name
  v <- m
  popFuncName
  return v
