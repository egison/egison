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
import           Language.Egison.Type.Env          (TypeEnv, ClassEnv, PatternTypeEnv, emptyEnv, emptyClassEnv, emptyPatternEnv, extendEnv)

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
  , patternEnv     :: PatternTypeEnv -- ^ Pattern constructor environment (for type inference)
  , patternFuncEnv :: PatternTypeEnv -- ^ Pattern function environment (for disambiguation)
  , reductionRulesCount  :: Int      -- ^ Phase 7.4/7.5: number of `declare rule` declarations seen
  , derivativeRulesCount :: Int      -- ^ Phase 6.3: number of `declare derivative` declarations seen
  , reductionRuleNames   :: [String] -- ^ Names of named rules ("auto" rules are excluded)
  , derivativeRuleNames  :: [String] -- ^ Names of declared derivatives (the function names)
  , autoRuleVarNames     :: [String] -- ^ Phase 7.5: full var names of auto rules (e.g. "autoRule.0").
                                       --   Accumulated as `declare rule auto` declarations are desugared,
                                       --   used to rebuild `mathNormalize` to apply each rule in sequence.
  , autoRuleTriggers     :: [[String]] -- ^ Trigger-symbol set per auto rule (parallel to autoRuleVarNames).
                                       --   Each entry lists the literal symbols/functions referenced by the
                                       --   rule's LHS. Empty list means "no specific trigger" -> always run.
                                       --   Passed to iterateRulesCAS so the Haskell loop can skip rules whose
                                       --   triggers are absent from the value (single CAS-scan per iteration).
  , derivativesDesugared :: [String] -- ^ Phase 6.3: derivative names desugared so far (in declaration order).
                                       --   Each `declare derivative` redefines `chainPartialDiff` using only
                                       --   the names *up to and including* itself, avoiding forward references
                                       --   to derivatives declared later (which would emit warnings).
  }

initialEvalState :: EvalState
initialEvalState = EvalState
  { funcNameStack = []
  , instanceEnv = HashMap.empty
  , constructorEnv = HashMap.empty
  , typeEnv = emptyEnv
  , classEnv = emptyClassEnv
  , patternEnv = emptyPatternEnv
  , patternFuncEnv = emptyPatternEnv
  , reductionRulesCount = 0
  , derivativeRulesCount = 0
  , reductionRuleNames = []
  , derivativeRuleNames = []
  , autoRuleVarNames = []
  , autoRuleTriggers = []
  , derivativesDesugared = []
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
  extendTypeEnv :: Var -> TypeScheme -> m ()
  -- Class environment operations
  getClassEnv :: m ClassEnv
  setClassEnv :: ClassEnv -> m ()
  -- Pattern environment operations
  getPatternEnv :: m PatternTypeEnv
  setPatternEnv :: PatternTypeEnv -> m ()
  -- Pattern function environment operations
  getPatternFuncEnv :: m PatternTypeEnv
  setPatternFuncEnv :: PatternTypeEnv -> m ()
  -- Phase 7.4/7.5: reduction-rule and derivative-rule registration counts.
  -- Counts only — full data is held by EnvBuildResult during build phase
  -- and isn't currently threaded into the runtime state.
  getReductionRulesCount :: m Int
  setReductionRulesCount :: Int -> m ()
  getDerivativeRulesCount :: m Int
  setDerivativeRulesCount :: Int -> m ()
  getReductionRuleNames :: m [String]
  setReductionRuleNames :: [String] -> m ()
  getDerivativeRuleNames :: m [String]
  setDerivativeRuleNames :: [String] -> m ()
  -- Phase 7.5: auto-rule full var names (e.g. "autoRule.0", "autoRule.1").
  -- Used to rebuild `mathNormalize` per `declare rule auto`.
  getAutoRuleVarNames :: m [String]
  setAutoRuleVarNames :: [String] -> m ()
  appendAutoRuleVarName :: String -> m ()
  -- Trigger-symbol set per auto rule, parallel to autoRuleVarNames.
  -- Passed to iterateRulesCAS to skip rules whose triggers are absent.
  getAutoRuleTriggers :: m [[String]]
  appendAutoRuleTriggers :: [String] -> m ()
  -- Phase 6.3: derivative names already desugared (in declaration order).
  -- Lets each `declare derivative` see only the derivatives that come at or
  -- before it, avoiding forward references in the generated chainPartialDiff.
  getDerivativesDesugared :: m [String]
  setDerivativesDesugared :: [String] -> m ()
  appendDerivativeDesugared :: String -> m ()

instance Monad m => MonadEval (StateT EvalState m) where
  pushFuncName name = do
    st <- get
    put $ st { funcNameStack = name : funcNameStack st }
    return ()
  topFuncName = do
    stack <- funcNameStack <$> get
    case stack of
      (x:_) -> return x
      []    -> error "topFuncName: function name stack is empty"
  popFuncName = do
    st <- get
    case funcNameStack st of
      (_:rest) -> put st { funcNameStack = rest }
      []       -> error "popFuncName: function name stack is empty"
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
  
  getPatternEnv = patternEnv <$> get
  setPatternEnv env = do
    st <- get
    put $ st { patternEnv = env }
  
  getPatternFuncEnv = patternFuncEnv <$> get
  setPatternFuncEnv env = do
    st <- get
    put $ st { patternFuncEnv = env }

  getReductionRulesCount = reductionRulesCount <$> get
  setReductionRulesCount n = do
    st <- get
    put $ st { reductionRulesCount = n }

  getDerivativeRulesCount = derivativeRulesCount <$> get
  setDerivativeRulesCount n = do
    st <- get
    put $ st { derivativeRulesCount = n }

  getReductionRuleNames = reductionRuleNames <$> get
  setReductionRuleNames ns = do
    st <- get
    put $ st { reductionRuleNames = ns }

  getDerivativeRuleNames = derivativeRuleNames <$> get
  setDerivativeRuleNames ns = do
    st <- get
    put $ st { derivativeRuleNames = ns }

  getAutoRuleVarNames = autoRuleVarNames <$> get
  setAutoRuleVarNames ns = do
    st <- get
    put $ st { autoRuleVarNames = ns }
  appendAutoRuleVarName n = do
    st <- get
    put $ st { autoRuleVarNames = autoRuleVarNames st ++ [n] }

  getAutoRuleTriggers = autoRuleTriggers <$> get
  appendAutoRuleTriggers ts = do
    st <- get
    put $ st { autoRuleTriggers = autoRuleTriggers st ++ [ts] }

  getDerivativesDesugared = derivativesDesugared <$> get
  setDerivativesDesugared ns = do
    st <- get
    put $ st { derivativesDesugared = ns }
  appendDerivativeDesugared n = do
    st <- get
    put $ st { derivativesDesugared = derivativesDesugared st ++ [n] }

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
  getPatternEnv = lift getPatternEnv
  setPatternEnv = lift . setPatternEnv
  getPatternFuncEnv = lift getPatternFuncEnv
  setPatternFuncEnv = lift . setPatternFuncEnv
  getReductionRulesCount = lift getReductionRulesCount
  setReductionRulesCount = lift . setReductionRulesCount
  getDerivativeRulesCount = lift getDerivativeRulesCount
  setDerivativeRulesCount = lift . setDerivativeRulesCount
  getReductionRuleNames = lift getReductionRuleNames
  setReductionRuleNames = lift . setReductionRuleNames
  getDerivativeRuleNames = lift getDerivativeRuleNames
  setDerivativeRuleNames = lift . setDerivativeRuleNames
  getAutoRuleVarNames = lift getAutoRuleVarNames
  setAutoRuleVarNames = lift . setAutoRuleVarNames
  appendAutoRuleVarName = lift . appendAutoRuleVarName
  getDerivativesDesugared = lift getDerivativesDesugared
  setDerivativesDesugared = lift . setDerivativesDesugared
  appendDerivativeDesugared = lift . appendDerivativeDesugared

mLabelFuncName :: MonadEval m => Maybe Var -> m a -> m a
mLabelFuncName Nothing m = m
mLabelFuncName (Just name) m = do
  pushFuncName name
  v <- m
  popFuncName
  return v
