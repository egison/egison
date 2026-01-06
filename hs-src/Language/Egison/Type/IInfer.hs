{- |
Module      : Language.Egison.Type.IInfer
Licence     : MIT

This module provides type inference for IExpr (Internal Expression).
This is the unified type inference module for Phase 5-6 of the Egison compiler:
  IExpr (Desugared, no types) → TypedIExpr (Typed, constraints collected)

This module consolidates all type inference functionality, including:
  - Hindley-Milner type inference
  - Type class constraint collection
  - Infer monad and state management
  - All helper functions

Previous modules (Infer.hs for Expr, TypeInfer.hs for Expr→TypedExpr) are deprecated.
-}

module Language.Egison.Type.IInfer
  ( -- * Type inference
    inferIExpr
  , inferITopExpr
  , inferITopExprs
    -- * Infer monad
  , Infer
  , InferState(..)
  , InferConfig(..)
  , initialInferState
  , initialInferStateWithConfig
  , defaultInferConfig
  , permissiveInferConfig
  , runInfer
  , runInferWithWarnings
  , runInferWithWarningsAndState
    -- * Running inference
  , runInferI
  , runInferIWithEnv
    -- * Helper functions
  , freshVar
  , getEnv
  , setEnv
  , withEnv
  , lookupVar
  , unifyTypes
  , generalize
  , inferConstant
  , addWarning
  , clearWarnings
  ) where

import           Control.Monad              (foldM)
import           Control.Monad.Except       (ExceptT, runExceptT, throwError)
import           Control.Monad.State.Strict (StateT, evalStateT, runStateT, get, modify, put)
import           Language.Egison.AST        (ConstantExpr (..))
import           Language.Egison.IExpr      (IExpr (..), ITopExpr (..)
                                            , IBindingExpr
                                            , IPrimitiveDataPattern, PDPatternBase (..)
                                            , extractNameFromVar)
import           Language.Egison.Type.Env
import           Language.Egison.Type.Error
import           Language.Egison.Type.Subst
import           Language.Egison.Type.Types
import           Language.Egison.Type.Unify as TU
import           Language.Egison.Type.TypedIAST

--------------------------------------------------------------------------------
-- * Infer Monad and State
--------------------------------------------------------------------------------

-- | Inference configuration
data InferConfig = InferConfig
  { cfgPermissive      :: Bool  -- ^ Treat unbound variables as warnings, not errors
  , cfgCollectWarnings :: Bool  -- ^ Collect warnings during inference
  }

instance Show InferConfig where
  show cfg = "InferConfig { cfgPermissive = " ++ show (cfgPermissive cfg)
           ++ ", cfgCollectWarnings = " ++ show (cfgCollectWarnings cfg)
           ++ " }"

-- | Default configuration (strict mode)
defaultInferConfig :: InferConfig
defaultInferConfig = InferConfig
  { cfgPermissive = False
  , cfgCollectWarnings = False
  }

-- | Permissive configuration (for gradual adoption)
permissiveInferConfig :: InferConfig
permissiveInferConfig = InferConfig
  { cfgPermissive = True
  , cfgCollectWarnings = True
  }

-- | Inference state
data InferState = InferState
  { inferCounter  :: Int              -- ^ Fresh variable counter
  , inferEnv      :: TypeEnv          -- ^ Current type environment
  , inferWarnings :: [TypeWarning]    -- ^ Collected warnings
  , inferConfig   :: InferConfig      -- ^ Configuration
  , inferClassEnv :: ClassEnv         -- ^ Type class environment
  } deriving (Show)

-- | Initial inference state
initialInferState :: InferState
initialInferState = InferState 0 emptyEnv [] defaultInferConfig emptyClassEnv

-- | Create initial state with config
initialInferStateWithConfig :: InferConfig -> InferState
initialInferStateWithConfig cfg = InferState 0 emptyEnv [] cfg emptyClassEnv

-- | Inference monad (with IO for potential future extensions)
type Infer a = ExceptT TypeError (StateT InferState IO) a

-- | Run type inference
runInfer :: Infer a -> InferState -> IO (Either TypeError a)
runInfer m st = evalStateT (runExceptT m) st

-- | Run type inference and also return warnings
runInferWithWarnings :: Infer a -> InferState -> IO (Either TypeError a, [TypeWarning])
runInferWithWarnings m st = do
  (result, finalState) <- runStateT (runExceptT m) st
  return (result, inferWarnings finalState)

-- | Run inference and return result, warnings, and final state
runInferWithWarningsAndState :: Infer a -> InferState -> IO (Either TypeError a, [TypeWarning], InferState)
runInferWithWarningsAndState m st = do
  (result, finalState) <- runStateT (runExceptT m) st
  return (result, inferWarnings finalState, finalState)

--------------------------------------------------------------------------------
-- * Helper Functions
--------------------------------------------------------------------------------

-- | Add a warning
addWarning :: TypeWarning -> Infer ()
addWarning w = modify $ \st -> st { inferWarnings = w : inferWarnings st }

-- | Clear all accumulated warnings
clearWarnings :: Infer ()
clearWarnings = modify $ \st -> st { inferWarnings = [] }

-- | Check if we're in permissive mode
isPermissive :: Infer Bool
isPermissive = cfgPermissive . inferConfig <$> get

-- | Generate a fresh type variable
freshVar :: String -> Infer Type
freshVar prefix = do
  st <- get
  let n = inferCounter st
  put st { inferCounter = n + 1 }
  return $ TVar $ TyVar $ prefix ++ show n

-- | Get the current type environment
getEnv :: Infer TypeEnv
getEnv = inferEnv <$> get

-- | Set the type environment
setEnv :: TypeEnv -> Infer ()
setEnv env = modify $ \st -> st { inferEnv = env }

-- | Extend the environment temporarily
withEnv :: [(String, TypeScheme)] -> Infer a -> Infer a
withEnv bindings action = do
  oldEnv <- getEnv
  setEnv $ extendEnvMany bindings oldEnv
  result <- action
  setEnv oldEnv
  return result

-- | Look up a variable's type
lookupVar :: String -> Infer Type
lookupVar name = do
  env <- getEnv
  case lookupEnv name env of
    Just scheme -> do
      st <- get
      let (_constraints, t, newCounter) = instantiate scheme (inferCounter st)
      -- TODO: Track constraints for type class resolution
      modify $ \s -> s { inferCounter = newCounter }
      return t
    Nothing -> do
      permissive <- isPermissive
      if permissive
        then do
          -- In permissive mode, treat as a warning and return a fresh type variable
          addWarning $ UnboundVariableWarning name emptyContext
          freshVar "unbound"
        else throwError $ UnboundVariable name emptyContext

-- | Unify two types
unifyTypes :: Type -> Type -> Infer Subst
unifyTypes t1 t2 = case unify t1 t2 of
  Right s  -> return s
  Left err -> case err of
    TU.OccursCheck v t -> throwError $ OccursCheckError v t emptyContext
    TU.TypeMismatch a b -> throwError $ UnificationError a b emptyContext

-- | Infer type for constants
inferConstant :: ConstantExpr -> Infer Type
inferConstant c = case c of
  CharExpr _    -> return TChar
  StringExpr _  -> return TString
  BoolExpr _    -> return TBool
  IntegerExpr _ -> return TInt
  FloatExpr _   -> return TFloat
  -- something : Matcher a (polymorphic matcher that matches any type)
  SomethingExpr -> do
    elemType <- freshVar "a"
    return (TMatcher elemType)
  -- undefined has a fresh type variable (bottom-like, can be any type)
  UndefinedExpr -> freshVar "undefined"

--------------------------------------------------------------------------------
-- * Type Inference for IExpr
--------------------------------------------------------------------------------

-- | Infer type for IExpr
inferIExpr :: IExpr -> Infer (Type, Subst)
inferIExpr expr = case expr of
  -- Constants
  IConstantExpr c -> do
    ty <- inferConstant c
    return (ty, emptySubst)
  
  -- Variables
  IVarExpr name -> do
    ty <- lookupVar name
    return (ty, emptySubst)
  
  -- Tuples
  ITupleExpr elems -> do
    results <- mapM inferIExpr elems
    let elemTypes = map fst results
        s = foldr composeSubst emptySubst (map snd results)
    return (TTuple elemTypes, s)
  
  -- Collections (Lists)
  ICollectionExpr elems -> do
    elemType <- freshVar "elem"
    s <- foldM (inferListElem elemType) emptySubst elems
    return (TCollection (applySubst s elemType), s)
    where
      inferListElem eType s e = do
        (t, s') <- inferIExpr e
        s'' <- unifyTypes (applySubst s eType) t
        return $ composeSubst s'' (composeSubst s' s)
  
  -- Cons
  IConsExpr headExpr tailExpr -> do
    (headType, s1) <- inferIExpr headExpr
    (tailType, s2) <- inferIExpr tailExpr
    let s12 = composeSubst s2 s1
    s3 <- unifyTypes (TCollection (applySubst s12 headType)) (applySubst s12 tailType)
    let finalS = composeSubst s3 s12
    return (applySubst finalS tailType, finalS)
  
  -- Join (list concatenation)
  IJoinExpr leftExpr rightExpr -> do
    (leftType, s1) <- inferIExpr leftExpr
    (rightType, s2) <- inferIExpr rightExpr
    let s12 = composeSubst s2 s1
    s3 <- unifyTypes (applySubst s12 leftType) (applySubst s12 rightType)
    let finalS = composeSubst s3 s12
    return (applySubst finalS leftType, finalS)
  
  -- Hash (Map)
  IHashExpr pairs -> do
    keyType <- freshVar "hashKey"
    valType <- freshVar "hashVal"
    s <- foldM (inferHashPair keyType valType) emptySubst pairs
    return (THash (applySubst s keyType) (applySubst s valType), s)
    where
      inferHashPair kType vType s' (k, v) = do
        (kt, s1) <- inferIExpr k
        (vt, s2) <- inferIExpr v
        s3 <- unifyTypes (applySubst (composeSubst s2 s1) kType) kt
        s4 <- unifyTypes (applySubst (composeSubst s3 (composeSubst s2 s1)) vType) vt
        return $ foldr composeSubst s' [s4, s3, s2, s1]
  
  -- Vector (Tensor)
  IVectorExpr elems -> do
    elemType <- freshVar "vecElem"
    s <- foldM (inferListElem elemType) emptySubst elems
    return (TTensor (applySubst s elemType), s)
    where
      inferListElem eType s e = do
        (t, s') <- inferIExpr e
        s'' <- unifyTypes (applySubst s eType) t
        return $ composeSubst s'' (composeSubst s' s)
  
  -- Lambda
  ILambdaExpr _mVar params body -> do
    argTypes <- mapM (\_ -> freshVar "arg") params
    let bindings = zipWith makeBinding params argTypes
    (bodyType, s) <- withEnv (map toScheme bindings) $ inferIExpr body
    let finalArgTypes = map (applySubst s) argTypes
        funType = foldr TFun bodyType finalArgTypes
    return (funType, s)
    where
      makeBinding var t = (extractNameFromVar var, t)
      toScheme (name, t) = (name, Forall [] [] t)
  
  -- Function Application
  IApplyExpr func args -> do
    (funcType, s1) <- inferIExpr func
    inferIApplication funcType args s1
  
  -- If expression
  IIfExpr cond thenExpr elseExpr -> do
    (condType, s1) <- inferIExpr cond
    s2 <- unifyTypes condType TBool
    let s12 = composeSubst s2 s1
    (thenType, s3) <- inferIExpr thenExpr
    (elseType, s4) <- inferIExpr elseExpr
    s5 <- unifyTypes (applySubst s4 thenType) elseType
    let finalS = foldr composeSubst emptySubst [s5, s4, s3, s12]
    return (applySubst finalS elseType, finalS)
  
  -- Let expression
  ILetExpr bindings body -> do
    env <- getEnv
    (extendedEnv, s1) <- inferIBindings bindings env emptySubst
    (bodyType, s2) <- withEnv extendedEnv $ inferIExpr body
    let finalS = composeSubst s2 s1
    return (applySubst finalS bodyType, finalS)
  
  -- LetRec expression
  ILetRecExpr bindings body -> do
    env <- getEnv
    (extendedEnv, s1) <- inferIRecBindings bindings env emptySubst
    (bodyType, s2) <- withEnv extendedEnv $ inferIExpr body
    let finalS = composeSubst s2 s1
    return (applySubst finalS bodyType, finalS)
  
  -- Sequence expression
  ISeqExpr expr1 expr2 -> do
    (_, s1) <- inferIExpr expr1
    (t2, s2) <- inferIExpr expr2
    return (t2, composeSubst s2 s1)
  
  -- Inductive Data Constructor
  IInductiveDataExpr _name args -> do
    _ <- mapM inferIExpr args
    -- TODO: Look up constructor type in environment
    -- For now, return TAny
    return (TAny, emptySubst)
  
  -- Matchers (return Matcher type)
  IMatcherExpr _patDefs -> do
    matchedTy <- freshVar "matched"
    return (TMatcher matchedTy, emptySubst)
  
  -- Match expressions (pattern matching)
  IMatchExpr _mode target matcher _clauses -> do
    (targetType, s1) <- inferIExpr target
    (matcherType, s2) <- inferIExpr matcher
    
    -- Matcher should be TMatcher a, and target should be a
    matchedTy <- freshVar "matched"
    let s12 = composeSubst s2 s1
    s3 <- unifyTypes (applySubst s12 matcherType) (TMatcher matchedTy)
    
    let s123 = composeSubst s3 s12
    s4 <- unifyTypes (applySubst s123 targetType) (applySubst s123 matchedTy)
    
    -- TODO: Infer match clauses result type
    resultTy <- freshVar "matchResult"
    let finalS = composeSubst s4 s123
    return (applySubst finalS resultTy, finalS)
  
  -- MatchAll expressions
  IMatchAllExpr _mode target matcher _clauses -> do
    (targetType, s1) <- inferIExpr target
    (matcherType, s2) <- inferIExpr matcher
    
    matchedTy <- freshVar "matched"
    let s12 = composeSubst s2 s1
    s3 <- unifyTypes (applySubst s12 matcherType) (TMatcher matchedTy)
    
    let s123 = composeSubst s3 s12
    s4 <- unifyTypes (applySubst s123 targetType) (applySubst s123 matchedTy)
    
    -- MatchAll returns a collection of results
    resultElemTy <- freshVar "matchAllElem"
    let finalS = composeSubst s4 s123
    return (TCollection (applySubst finalS resultElemTy), finalS)
  
  -- Memoized Lambda
  IMemoizedLambdaExpr args body -> do
    argTypes <- mapM (\_ -> freshVar "memoArg") args
    let bindings = zip args argTypes  -- [(String, Type)]
        schemes = map (\(name, t) -> (name, Forall [] [] t)) bindings
    (bodyType, s) <- withEnv schemes $ inferIExpr body
    let finalArgTypes = map (applySubst s) argTypes
        funType = foldr TFun bodyType finalArgTypes
    return (funType, s)
  
  -- Do expression
  IDoExpr _bindings body -> do
    -- TODO: Properly handle IO monad bindings
    (bodyType, s) <- inferIExpr body
    return (TIO bodyType, s)
  
  -- Cambda (pattern matching lambda)
  ICambdaExpr _var body -> do
    argType <- freshVar "cambdaArg"
    (bodyType, s) <- inferIExpr body
    return (TFun argType bodyType, s)
  
  -- With symbols
  IWithSymbolsExpr _syms body -> inferIExpr body
  
  -- Quote expressions (symbolic math)
  IQuoteExpr _ -> return (TInt, emptySubst)
  IQuoteSymbolExpr _ -> return (TInt, emptySubst)
  
  -- Other cases: return TAny for now
  _ -> return (TAny, emptySubst)

-- | Infer application (helper)
inferIApplication :: Type -> [IExpr] -> Subst -> Infer (Type, Subst)
inferIApplication funcType args initSubst = do
  -- Infer argument types
  argResults <- mapM inferIExpr args
  let argTypes = map fst argResults
      argSubst = foldr composeSubst initSubst (map snd argResults)
  
  -- Create expected function type
  resultType <- freshVar "result"
  let expectedFuncType = foldr TFun resultType argTypes
  
  -- Unify
  s <- unifyTypes (applySubst argSubst funcType) expectedFuncType
  let finalS = composeSubst s argSubst
  return (applySubst finalS resultType, finalS)

-- | Infer let bindings (non-recursive)
inferIBindings :: [IBindingExpr] -> TypeEnv -> Subst -> Infer ([(String, TypeScheme)], Subst)
inferIBindings [] _env s = return ([], s)
inferIBindings ((pat, expr):bs) env s = do
  (exprType, s1) <- inferIExpr expr
  let bindings = extractIBindingsFromPattern pat (applySubst s1 exprType)
      s' = composeSubst s1 s
  _env' <- getEnv
  let extendedEnvList = bindings  -- Already a list of (String, TypeScheme)
  (restBindings, s2) <- withEnv extendedEnvList $ inferIBindings bs env s'
  return (bindings ++ restBindings, s2)

-- | Infer letrec bindings (recursive)
inferIRecBindings :: [IBindingExpr] -> TypeEnv -> Subst -> Infer ([(String, TypeScheme)], Subst)
inferIRecBindings bindings _env s = do
  -- Create placeholders with fresh type variables
  placeholders <- mapM (\(pat, _) -> do
    ty <- freshVar "rec"
    return (pat, ty)) bindings
  
  -- Extract bindings from placeholders
  let placeholderBindings = concatMap (\(pat, ty) -> extractIBindingsFromPattern pat ty) placeholders
  
  -- Infer expressions in extended environment
  results <- withEnv placeholderBindings $ mapM (\(_, expr) -> inferIExpr expr) bindings
  
  let exprTypes = map fst results
      substList = map snd results
      finalS = foldr composeSubst s substList
  
  -- Re-extract bindings with inferred types
  let finalBindings = concat $ zipWith (\(pat, _) ty -> extractIBindingsFromPattern pat (applySubst finalS ty)) bindings exprTypes
  
  return (finalBindings, finalS)

-- | Extract bindings from pattern
extractIBindingsFromPattern :: IPrimitiveDataPattern -> Type -> [(String, TypeScheme)]
extractIBindingsFromPattern pat ty = case pat of
  PDWildCard -> []
  PDPatVar var -> [(extractNameFromVar var, Forall [] [] ty)]
  PDInductivePat _ pats -> concatMap (\p -> extractIBindingsFromPattern p ty) pats
  PDTuplePat pats -> 
    case ty of
      TTuple tys -> concat $ zipWith extractIBindingsFromPattern pats tys
      _ -> []  -- Type mismatch
  PDEmptyPat -> []
  PDConsPat p1 p2 ->
    case ty of
      TCollection elemTy -> extractIBindingsFromPattern p1 elemTy ++ extractIBindingsFromPattern p2 ty
      _ -> []
  PDSnocPat p1 p2 ->
    case ty of
      TCollection elemTy -> extractIBindingsFromPattern p1 ty ++ extractIBindingsFromPattern p2 elemTy
      _ -> []
  _ -> []

-- | Convert IExpr inference result to TypedIExpr
toTypedIExpr :: IExpr -> Type -> Subst -> TypedIExpr
toTypedIExpr iexpr ty subst =
  TypedIExpr (applySubst subst ty) (toTypedINode iexpr ty subst)

toTypedINode :: IExpr -> Type -> Subst -> TypedINode
toTypedINode iexpr _ty _subst = case iexpr of
  IConstantExpr c -> TIConstantExpr c
  IVarExpr name -> TIVarExpr name
  _ -> TIVarExpr "<todo>"  -- TODO: implement all cases

-- | Infer top-level IExpr
inferITopExpr :: ITopExpr -> Infer (Maybe TypedITopExpr, Subst)
inferITopExpr topExpr = case topExpr of
  IDefine var expr -> do
    (exprType, subst) <- inferIExpr expr
    env <- getEnv
    let scheme = generalize env exprType
    -- Add to environment
    modify $ \s -> s { inferEnv = extendEnv (extractNameFromVar var) scheme (inferEnv s) }
    
    let finalType = applySubst subst exprType
    -- TODO: Convert expr to TypedIExpr properly
    let typedExpr = toTypedIExpr expr finalType subst
    return (Just (TypedIDefine (extractNameFromVar var) [] finalType typedExpr), subst)
  
  ITest expr -> do
    (exprType, subst) <- inferIExpr expr
    let typedExpr = toTypedIExpr expr exprType subst
    return (Just (TypedITest typedExpr), subst)
  
  IExecute expr -> do
    (exprType, subst) <- inferIExpr expr
    let typedExpr = toTypedIExpr expr exprType subst
    return (Just (TypedIExecute typedExpr), subst)
  
  ILoadFile path -> return (Just (TypedILoadFile path), emptySubst)
  ILoad lib -> return (Just (TypedILoad lib), emptySubst)
  
  _ -> return (Nothing, emptySubst)

-- | Infer multiple top-level IExprs
inferITopExprs :: [ITopExpr] -> Infer ([Maybe TypedITopExpr], Subst)
inferITopExprs [] = return ([], emptySubst)
inferITopExprs (e:es) = do
  (tyE, s1) <- inferITopExpr e
  (tyEs, s2) <- inferITopExprs es
  return (tyE : tyEs, composeSubst s2 s1)

--------------------------------------------------------------------------------
-- * Running Inference
--------------------------------------------------------------------------------

-- | Run type inference on IExpr
runInferI :: InferConfig -> TypeEnv -> IExpr -> IO (Either TypeError (Type, Subst, [TypeWarning]))
runInferI cfg env expr = do
  let initState = (initialInferStateWithConfig cfg) { inferEnv = env }
  (result, warnings) <- runInferWithWarnings (inferIExpr expr) initState
  return $ case result of
    Left err -> Left err
    Right (ty, subst) -> Right (ty, subst, warnings)

-- | Run type inference on IExpr with initial environment
runInferIWithEnv :: InferConfig -> TypeEnv -> IExpr -> IO (Either TypeError (Type, Subst, TypeEnv, [TypeWarning]))
runInferIWithEnv cfg env expr = do
  let initState = (initialInferStateWithConfig cfg) { inferEnv = env }
  (result, warnings, finalState) <- runInferWithWarningsAndState (inferIExpr expr) initState
  return $ case result of
    Left err -> Left err
    Right (ty, subst) -> Right (ty, subst, inferEnv finalState, warnings)
