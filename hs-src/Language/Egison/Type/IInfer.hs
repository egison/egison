{- |
Module      : Language.Egison.Type.IInfer
Licence     : MIT

This module provides type inference for IExpr (Internal Expression).
This is the unified type inference module for Phase 5-6 of the Egison compiler:
  IExpr (Desugared, no types) → (Type, Subst)

This module consolidates all type inference functionality, including:
  - Hindley-Milner type inference
  - Type class constraint collection
  - Infer monad and state management
  - All helper functions

Note: This module only performs type inference and returns Type information.
The typed AST (TIExpr) is created in a separate phase by combining IExpr with Type.

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

import           Control.Monad              (foldM, zipWithM)
import           Control.Monad.Except       (ExceptT, runExceptT, throwError)
import           Control.Monad.State.Strict (StateT, evalStateT, runStateT, get, modify, put)
import           Data.Maybe                  (catMaybes)
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import           Language.Egison.AST        (ConstantExpr (..), PrimitivePatPattern (..))
import           Language.Egison.IExpr      (IExpr (..), ITopExpr (..), TITopExpr (..)
                                            , TIExpr (..), TIExprNode (..)
                                            , IBindingExpr, TIBindingExpr
                                            , IMatchClause, TIMatchClause, IPatternDef, TIPatternDef
                                            , IPattern (..), ILoopRange (..)
                                            , TIPattern (..), TIPatternNode (..), TILoopRange (..)
                                            , IPrimitiveDataPattern, PDPatternBase (..)
                                            , extractNameFromVar, Var (..), Index (..)
                                            , tiExprType)
import           Language.Egison.Pretty     (prettyStr)
import           Language.Egison.Type.Env
import qualified Language.Egison.Type.Error as TE
import           Language.Egison.Type.Error (TypeError(..), TypeErrorContext(..), TypeWarning(..),
                                              emptyContext, withExpr)
import           Language.Egison.Type.Subst (Subst, applySubst, applySubstConstraint,
                                              applySubstScheme, composeSubst, emptySubst)
import           Language.Egison.Type.Tensor (normalizeTensorType)
import           Language.Egison.Type.Types
import           Language.Egison.Type.Unify as TU
import           Language.Egison.Type.Instance (findMatchingInstanceForType)

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
  { inferCounter     :: Int              -- ^ Fresh variable counter
  , inferEnv         :: TypeEnv          -- ^ Current type environment
  , inferWarnings    :: [TypeWarning]    -- ^ Collected warnings
  , inferConfig      :: InferConfig      -- ^ Configuration
  , inferClassEnv    :: ClassEnv         -- ^ Type class environment
  , inferPatternEnv  :: PatternTypeEnv   -- ^ Pattern constructor environment
  , inferConstraints :: [Constraint]     -- ^ Accumulated type class constraints
  , declaredSymbols  :: Map.Map String Type  -- ^ Declared symbols with their types
  } deriving (Show)

-- | Initial inference state
initialInferState :: InferState
initialInferState = InferState 0 emptyEnv [] defaultInferConfig emptyClassEnv emptyPatternEnv [] Map.empty

-- | Create initial state with config
initialInferStateWithConfig :: InferConfig -> InferState
initialInferStateWithConfig cfg = InferState 0 emptyEnv [] cfg emptyClassEnv emptyPatternEnv [] Map.empty

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

-- | Add type class constraints
addConstraints :: [Constraint] -> Infer ()
addConstraints cs = modify $ \st -> st { inferConstraints = inferConstraints st ++ cs }

-- | Get accumulated constraints
getConstraints :: Infer [Constraint]
getConstraints = inferConstraints <$> get

-- | Clear accumulated constraints
clearConstraints :: Infer ()
clearConstraints = modify $ \st -> st { inferConstraints = [] }

-- | Run an action with local constraint tracking
withLocalConstraints :: Infer a -> Infer (a, [Constraint])
withLocalConstraints action = do
  oldConstraints <- getConstraints
  clearConstraints
  result <- action
  newConstraints <- getConstraints
  modify $ \st -> st { inferConstraints = oldConstraints }
  return (result, newConstraints)

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

-- | Get the current pattern type environment
getPatternEnv :: Infer PatternTypeEnv
getPatternEnv = inferPatternEnv <$> get

-- | Set the pattern type environment
setPatternEnv :: PatternTypeEnv -> Infer ()
setPatternEnv penv = modify $ \st -> st { inferPatternEnv = penv }

-- | Get the current class environment
getClassEnv :: Infer ClassEnv
getClassEnv = inferClassEnv <$> get

-- | Resolve a constraint based on available instances
-- If the constraint type is a Tensor type and no instance exists for it,
-- try to use the element type's instance instead
-- | Resolve constraints in a TIExpr recursively
resolveConstraintsInTIExpr :: ClassEnv -> Subst -> TIExpr -> TIExpr
resolveConstraintsInTIExpr classEnv subst (TIExpr (Forall vars constraints ty) node) =
  let resolvedConstraints = map (resolveConstraintWithInstances classEnv subst) constraints
      resolvedNode = resolveConstraintsInNode classEnv subst node
  in TIExpr (Forall vars resolvedConstraints ty) resolvedNode

-- | Resolve constraints in a TIExprNode recursively
resolveConstraintsInNode :: ClassEnv -> Subst -> TIExprNode -> TIExprNode
resolveConstraintsInNode classEnv subst node = case node of
  TIConstantExpr c -> TIConstantExpr c
  TIVarExpr name -> TIVarExpr name
  TILambdaExpr mVar params body ->
    TILambdaExpr mVar params (resolveConstraintsInTIExpr classEnv subst body)
  TIApplyExpr func args ->
    TIApplyExpr (resolveConstraintsInTIExpr classEnv subst func)
                (map (resolveConstraintsInTIExpr classEnv subst) args)
  TITupleExpr exprs ->
    TITupleExpr (map (resolveConstraintsInTIExpr classEnv subst) exprs)
  TICollectionExpr exprs ->
    TICollectionExpr (map (resolveConstraintsInTIExpr classEnv subst) exprs)
  TIIfExpr cond thenExpr elseExpr ->
    TIIfExpr (resolveConstraintsInTIExpr classEnv subst cond)
             (resolveConstraintsInTIExpr classEnv subst thenExpr)
             (resolveConstraintsInTIExpr classEnv subst elseExpr)
  TILetExpr bindings body ->
    TILetExpr (map (\(p, e) -> (p, resolveConstraintsInTIExpr classEnv subst e)) bindings)
              (resolveConstraintsInTIExpr classEnv subst body)
  TILetRecExpr bindings body ->
    TILetRecExpr (map (\(p, e) -> (p, resolveConstraintsInTIExpr classEnv subst e)) bindings)
                 (resolveConstraintsInTIExpr classEnv subst body)
  TIIndexedExpr b expr indices ->
    TIIndexedExpr b (resolveConstraintsInTIExpr classEnv subst expr) indices
  TIGenerateTensorExpr shape func ->
    TIGenerateTensorExpr (resolveConstraintsInTIExpr classEnv subst shape)
                         (resolveConstraintsInTIExpr classEnv subst func)
  TITensorExpr shape elems ->
    TITensorExpr (resolveConstraintsInTIExpr classEnv subst shape)
                 (resolveConstraintsInTIExpr classEnv subst elems)
  TITensorContractExpr tensor ->
    TITensorContractExpr (resolveConstraintsInTIExpr classEnv subst tensor)
  TITensorMapExpr func tensor ->
    TITensorMapExpr (resolveConstraintsInTIExpr classEnv subst func)
                    (resolveConstraintsInTIExpr classEnv subst tensor)
  TITensorMap2Expr func t1 t2 ->
    TITensorMap2Expr (resolveConstraintsInTIExpr classEnv subst func)
                     (resolveConstraintsInTIExpr classEnv subst t1)
                     (resolveConstraintsInTIExpr classEnv subst t2)
  TIMatchExpr mode target matcher clauses ->
    TIMatchExpr mode
                (resolveConstraintsInTIExpr classEnv subst target)
                (resolveConstraintsInTIExpr classEnv subst matcher)
                (map (\(p, e) -> (p, resolveConstraintsInTIExpr classEnv subst e)) clauses)
  _ -> node

resolveConstraintWithInstances :: ClassEnv -> Subst -> Constraint -> Constraint
resolveConstraintWithInstances classEnv subst (Constraint className tyVar) =
  let resolvedType = applySubst subst tyVar
      instances = lookupInstances className classEnv
  in case resolvedType of
       TTensor elemType ->
         -- Tensor型の場合、まず要素型が型変数かチェック
         case elemType of
           TVar _ ->
             -- 要素が型変数の場合、無条件に要素型の制約を使う
             -- (Tensor t0 の場合、instance Num (Tensor t0) は存在しないが、
             --  instance Num t0 は型パラメータとして渡されるため)
             Constraint className elemType
           _ ->
             -- 要素が具体的な型の場合、インスタンスを探す
             case findMatchingInstanceForType resolvedType instances of
               Just _ -> 
                 -- Tensor自体のインスタンスがある場合はそれを使う
                 Constraint className resolvedType
               Nothing -> 
                 -- Tensorのインスタンスがなければ、要素型で試す
                 case findMatchingInstanceForType elemType instances of
                   Just _ -> 
                     -- 要素型のインスタンスがある場合はそれを使う
                     -- tensorMapで要素ごとに適用することを想定
                     Constraint className elemType
                   Nothing -> 
                     -- どちらもない場合は、解決された型をそのまま使う
                     -- (エラーは後のPhaseで検出される)
                     Constraint className resolvedType
       _ -> 
         -- 非Tensor型の場合は、単純に代入を適用
         Constraint className resolvedType

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
      let (constraints, t, newCounter) = instantiate scheme (inferCounter st)
      -- Track constraints for type class resolution
      modify $ \s -> s { inferCounter = newCounter }
      addConstraints constraints
      return t
    Nothing -> do
      -- Check if this is a declared symbol
      st <- get
      case Map.lookup name (declaredSymbols st) of
        Just ty -> return ty  -- Return the declared type without warning
        Nothing -> do
          permissive <- isPermissive
          if permissive
            then do
              -- In permissive mode, treat as a warning and return a fresh type variable
              addWarning $ UnboundVariableWarning name emptyContext
              freshVar "unbound"
            else throwError $ UnboundVariable name emptyContext

-- | Lookup variable and return type with constraints
lookupVarWithConstraints :: String -> Infer (Type, [Constraint])
lookupVarWithConstraints name = do
  env <- getEnv
  case lookupEnv name env of
    Just scheme -> do
      st <- get
      let (constraints, t, newCounter) = instantiate scheme (inferCounter st)
      -- Track constraints for type class resolution
      modify $ \s -> s { inferCounter = newCounter }
      addConstraints constraints
      return (t, constraints)
    Nothing -> do
      -- Check if this is a declared symbol
      st <- get
      case Map.lookup name (declaredSymbols st) of
        Just ty -> return (ty, [])  -- Return the declared type without warning
        Nothing -> do
          permissive <- isPermissive
          if permissive
            then do
              -- In permissive mode, treat as a warning and return a fresh type variable
              addWarning $ UnboundVariableWarning name emptyContext
              t <- freshVar "unbound"
              return (t, [])
            else throwError $ UnboundVariable name emptyContext

-- | Unify two types
unifyTypes :: Type -> Type -> Infer Subst
unifyTypes t1 t2 = unifyTypesWithContext t1 t2 emptyContext

-- | Unify two types with context information
unifyTypesWithContext :: Type -> Type -> TypeErrorContext -> Infer Subst
unifyTypesWithContext t1 t2 ctx = case unify t1 t2 of
  Right s  -> return s
  Left err -> case err of
    TU.OccursCheck v t -> throwError $ OccursCheckError v t ctx
    TU.TypeMismatch a b -> throwError $ UnificationError a b ctx

-- | Unify two types with context, allowing Tensor a to unify with a
-- This is used only for top-level definitions with type annotations
-- According to type-tensor-simple.md: "トップレベル定義のテンソルについてのみ、Tensor a型が a型とunifyするとa型になる。"
unifyTypesWithTopLevel :: Type -> Type -> TypeErrorContext -> Infer Subst
unifyTypesWithTopLevel t1 t2 ctx = case TU.unifyWithTopLevel t1 t2 of
  Right s  -> return s
  Left err -> case err of
    TU.OccursCheck v t -> throwError $ OccursCheckError v t ctx
    TU.TypeMismatch a b -> throwError $ UnificationError a b ctx

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

-- | Helper: Create a TIExpr with a simple monomorphic type (no type variables, no constraints)
mkTIExpr :: Type -> TIExprNode -> TIExpr
mkTIExpr ty node = TIExpr (Forall [] [] ty) node

-- | Apply a substitution to a TIExpr, updating both the type scheme and all subexpressions
applySubstToTIExpr :: Subst -> TIExpr -> TIExpr
applySubstToTIExpr s (TIExpr scheme node) =
  let updatedScheme = applySubstScheme s scheme
      updatedNode = applySubstToTIExprNode s node
  in TIExpr updatedScheme updatedNode

-- | Apply a substitution to a TIExprNode recursively
applySubstToTIExprNode :: Subst -> TIExprNode -> TIExprNode
applySubstToTIExprNode s node = case node of
  TIConstantExpr c -> TIConstantExpr c
  TIVarExpr name -> TIVarExpr name
  
  TILambdaExpr mVar params body ->
    TILambdaExpr mVar params (applySubstToTIExpr s body)
  
  TIApplyExpr func args ->
    TIApplyExpr (applySubstToTIExpr s func) (map (applySubstToTIExpr s) args)
  
  TITupleExpr exprs ->
    TITupleExpr (map (applySubstToTIExpr s) exprs)
  
  TICollectionExpr exprs ->
    TICollectionExpr (map (applySubstToTIExpr s) exprs)
  
  TIConsExpr h t ->
    TIConsExpr (applySubstToTIExpr s h) (applySubstToTIExpr s t)
  
  TIJoinExpr l r ->
    TIJoinExpr (applySubstToTIExpr s l) (applySubstToTIExpr s r)
  
  TIIfExpr cond thenE elseE ->
    TIIfExpr (applySubstToTIExpr s cond) (applySubstToTIExpr s thenE) (applySubstToTIExpr s elseE)
  
  TILetExpr bindings body ->
    TILetExpr (map (\(pat, expr) -> (pat, applySubstToTIExpr s expr)) bindings)
              (applySubstToTIExpr s body)
  
  TILetRecExpr bindings body ->
    TILetRecExpr (map (\(pat, expr) -> (pat, applySubstToTIExpr s expr)) bindings)
                 (applySubstToTIExpr s body)
  
  TISeqExpr e1 e2 ->
    TISeqExpr (applySubstToTIExpr s e1) (applySubstToTIExpr s e2)
  
  TIInductiveDataExpr name exprs ->
    TIInductiveDataExpr name (map (applySubstToTIExpr s) exprs)
  
  TIMatcherExpr patDefs ->
    TIMatcherExpr (map (\(pat, expr, bindings) -> (pat, applySubstToTIExpr s expr, bindings)) patDefs)
  
  TIMatchExpr mode target matcher clauses ->
    TIMatchExpr mode 
                (applySubstToTIExpr s target)
                (applySubstToTIExpr s matcher)
                (map (\(pat, body) -> (pat, applySubstToTIExpr s body)) clauses)
  
  TIMatchAllExpr mode target matcher clauses ->
    TIMatchAllExpr mode
                   (applySubstToTIExpr s target)
                   (applySubstToTIExpr s matcher)
                   (map (\(pat, body) -> (pat, applySubstToTIExpr s body)) clauses)
  
  TIMemoizedLambdaExpr params body ->
    TIMemoizedLambdaExpr params (applySubstToTIExpr s body)
  
  TIDoExpr bindings body ->
    TIDoExpr (map (\(pat, expr) -> (pat, applySubstToTIExpr s expr)) bindings)
             (applySubstToTIExpr s body)
  
  TICambdaExpr var body ->
    TICambdaExpr var (applySubstToTIExpr s body)
  
  TIWithSymbolsExpr syms body ->
    TIWithSymbolsExpr syms (applySubstToTIExpr s body)
  
  TIQuoteExpr e ->
    TIQuoteExpr (applySubstToTIExpr s e)
  
  TIQuoteSymbolExpr e ->
    TIQuoteSymbolExpr (applySubstToTIExpr s e)
  
  TIIndexedExpr isSupported base indices ->
    TIIndexedExpr isSupported (applySubstToTIExpr s base) indices
  
  TISubrefsExpr isSupported base ref ->
    TISubrefsExpr isSupported (applySubstToTIExpr s base) (applySubstToTIExpr s ref)
  
  TISuprefsExpr isSupported base ref ->
    TISuprefsExpr isSupported (applySubstToTIExpr s base) (applySubstToTIExpr s ref)
  
  TIUserrefsExpr isSupported base ref ->
    TIUserrefsExpr isSupported (applySubstToTIExpr s base) (applySubstToTIExpr s ref)
  
  TIWedgeApplyExpr func args ->
    TIWedgeApplyExpr (applySubstToTIExpr s func) (map (applySubstToTIExpr s) args)
  
  TIFunctionExpr names ->
    TIFunctionExpr names
  
  TIVectorExpr exprs ->
    TIVectorExpr (map (applySubstToTIExpr s) exprs)
  
  TIHashExpr pairs ->
    TIHashExpr (map (\(k, v) -> (applySubstToTIExpr s k, applySubstToTIExpr s v)) pairs)
  
  TIGenerateTensorExpr shape func ->
    TIGenerateTensorExpr (applySubstToTIExpr s shape) (applySubstToTIExpr s func)
  
  TITensorExpr shape elems ->
    TITensorExpr (applySubstToTIExpr s shape) (applySubstToTIExpr s elems)
  
  TITransposeExpr tensor perm ->
    TITransposeExpr (applySubstToTIExpr s tensor) (applySubstToTIExpr s perm)
  
  TIFlipIndicesExpr tensor ->
    TIFlipIndicesExpr (applySubstToTIExpr s tensor)
  
  TITensorMapExpr func tensor ->
    TITensorMapExpr (applySubstToTIExpr s func) (applySubstToTIExpr s tensor)
  
  TITensorMap2Expr func t1 t2 ->
    TITensorMap2Expr (applySubstToTIExpr s func) (applySubstToTIExpr s t1) (applySubstToTIExpr s t2)
  
  TITensorContractExpr tensor ->
    TITensorContractExpr (applySubstToTIExpr s tensor)

-- | Infer type for IExpr
-- NEW: Returns TIExpr (typed expression) instead of (IExpr, Type, Subst)
-- This builds the recursive TIExpr structure directly during type inference
inferIExpr :: IExpr -> Infer (TIExpr, Subst)
inferIExpr expr = inferIExprWithContext expr emptyContext

-- | Infer type for IExpr with context information
-- NEW: Returns TIExpr (typed expression) with type information embedded
inferIExprWithContext :: IExpr -> TypeErrorContext -> Infer (TIExpr, Subst)
inferIExprWithContext expr ctx = case expr of
  -- Constants
  IConstantExpr c -> do
    ty <- inferConstant c
    let scheme = Forall [] [] ty
    return (TIExpr scheme (TIConstantExpr c), emptySubst)
  
  -- Variables
  IVarExpr name -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (ty, constraints) <- lookupVarWithConstraints name
    let scheme = Forall [] constraints ty
    return (TIExpr scheme (TIVarExpr name), emptySubst)
  
  -- Tuples
  ITupleExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    case elems of
      [] -> do
        -- Empty tuple: unit type ()
        let scheme = Forall [] [] (TTuple [])
        return (TIExpr scheme (TITupleExpr []), emptySubst)
      [single] -> do
        -- Single element tuple: same as the element itself (parentheses are just grouping)
        inferIExprWithContext single exprCtx
      _ -> do
        results <- mapM (\e -> inferIExprWithContext e exprCtx) elems
        let elemTIExprs = map fst results
            elemTypes = map (tiExprType . fst) results
            s = foldr composeSubst emptySubst (map snd results)
        
        -- Check if all elements are Matcher types
        -- If so, return Matcher (Tuple ...) instead of (Matcher ..., Matcher ...)
        let appliedElemTypes = map (applySubst s) elemTypes
            matcherTypes = catMaybes (map extractMatcherType appliedElemTypes)
        
        if length matcherTypes == length appliedElemTypes && not (null appliedElemTypes)
          then do
            -- All elements are matchers: return Matcher (Tuple ...)
            let tupleType = TTuple matcherTypes
                resultType = TMatcher tupleType
                scheme = Forall [] [] resultType
            return (TIExpr scheme (TITupleExpr elemTIExprs), s)
          else do
            -- Not all elements are matchers: return regular tuple
            let resultType = TTuple appliedElemTypes
                scheme = Forall [] [] resultType
            return (TIExpr scheme (TITupleExpr elemTIExprs), s)
        where
          -- Extract the inner type from Matcher a -> Just a, otherwise Nothing
          extractMatcherType :: Type -> Maybe Type
          extractMatcherType (TMatcher t) = Just t
          extractMatcherType _ = Nothing
  
  -- Collections (Lists)
  ICollectionExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    elemType <- freshVar "elem"
    (elemTIExprs, s) <- foldM (inferListElem elemType exprCtx) ([], emptySubst) elems
    let resultType = TCollection (applySubst s elemType)
    return (mkTIExpr resultType (TICollectionExpr (reverse elemTIExprs)), s)
    where
      inferListElem eType exprCtx (accExprs, s) e = do
        (tiExpr, s') <- inferIExprWithContext e exprCtx
        let t = tiExprType tiExpr
        s'' <- unifyTypesWithContext (applySubst s eType) t exprCtx
        return (tiExpr : accExprs, composeSubst s'' (composeSubst s' s))
  
  -- Cons
  IConsExpr headExpr tailExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (headTI, s1) <- inferIExprWithContext headExpr exprCtx
    (tailTI, s2) <- inferIExprWithContext tailExpr exprCtx
    let headType = tiExprType headTI
        tailType = tiExprType tailTI
        s12 = composeSubst s2 s1
    s3 <- unifyTypesWithContext (TCollection (applySubst s12 headType)) (applySubst s12 tailType) exprCtx
    let finalS = composeSubst s3 s12
        resultType = applySubst finalS tailType
    return (mkTIExpr resultType (TIConsExpr headTI tailTI), finalS)
  
  -- Join (list concatenation)
  IJoinExpr leftExpr rightExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (leftTI, s1) <- inferIExprWithContext leftExpr exprCtx
    (rightTI, s2) <- inferIExprWithContext rightExpr exprCtx
    let leftType = tiExprType leftTI
        rightType = tiExprType rightTI
        s12 = composeSubst s2 s1
    s3 <- unifyTypesWithContext (applySubst s12 leftType) (applySubst s12 rightType) exprCtx
    let finalS = composeSubst s3 s12
        resultType = applySubst finalS leftType
    return (mkTIExpr resultType (TIJoinExpr leftTI rightTI), finalS)
  
  -- Hash (Map)
  IHashExpr pairs -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    keyType <- freshVar "hashKey"
    valType <- freshVar "hashVal"
    (pairTIs, s) <- foldM (inferHashPair keyType valType exprCtx) ([], emptySubst) pairs
    let resultType = THash (applySubst s keyType) (applySubst s valType)
    return (mkTIExpr resultType (TIHashExpr (reverse pairTIs)), s)
    where
      inferHashPair kType vType exprCtx (accPairs, s') (k, v) = do
        (kTI, s1) <- inferIExprWithContext k exprCtx
        (vTI, s2) <- inferIExprWithContext v exprCtx
        let kt = tiExprType kTI
            vt = tiExprType vTI
        s3 <- unifyTypesWithContext (applySubst (composeSubst s2 s1) kType) kt exprCtx
        s4 <- unifyTypesWithContext (applySubst (composeSubst s3 (composeSubst s2 s1)) vType) vt exprCtx
        return ((kTI, vTI) : accPairs, foldr composeSubst s' [s4, s3, s2, s1])
  
  -- Vector (Tensor)
  IVectorExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    elemType <- freshVar "vecElem"
    (elemTIs, s) <- foldM (inferListElem elemType exprCtx) ([], emptySubst) elems
    let resultType = normalizeTensorType (TTensor (applySubst s elemType))
    return (mkTIExpr resultType (TIVectorExpr (reverse elemTIs)), s)
    where
      inferListElem eType exprCtx (accExprs, s) e = do
        (tiExpr, s') <- inferIExprWithContext e exprCtx
        let t = tiExprType tiExpr
        s'' <- unifyTypesWithContext (applySubst s eType) t exprCtx
        return (tiExpr : accExprs, composeSubst s'' (composeSubst s' s))
  
  -- Lambda
  ILambdaExpr mVar params body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argTypes <- mapM (\_ -> freshVar "arg") params
    let bindings = zipWith makeBinding params argTypes
    (bodyTIExpr, s) <- withEnv (map toScheme bindings) $ inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTIExpr
        finalArgTypes = map (applySubst s) argTypes
        funType = foldr TFun bodyType finalArgTypes
    return (mkTIExpr funType (TILambdaExpr mVar params bodyTIExpr), s)
    where
      makeBinding var t = (extractNameFromVar var, t)
      toScheme (name, t) = (name, Forall [] [] t)
  
  -- Function Application
  IApplyExpr func args -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (funcTI, s1) <- inferIExprWithContext func exprCtx
    let funcType = tiExprType funcTI
    inferIApplicationWithContext funcTI funcType args s1 exprCtx
  
  -- If expression
  IIfExpr cond thenExpr elseExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (condTI, s1) <- inferIExprWithContext cond exprCtx
    let condType = tiExprType condTI
    s2 <- unifyTypesWithContext condType TBool exprCtx
    let s12 = composeSubst s2 s1
    (thenTI, s3) <- inferIExprWithContext thenExpr exprCtx
    (elseTI, s4) <- inferIExprWithContext elseExpr exprCtx
    let thenType = tiExprType thenTI
        elseType = tiExprType elseTI
    s5 <- unifyTypesWithContext (applySubst s4 thenType) elseType exprCtx
    let finalS = foldr composeSubst emptySubst [s5, s4, s3, s12]
        resultType = applySubst finalS elseType
    return (mkTIExpr resultType (TIIfExpr condTI thenTI elseTI), finalS)
  
  -- Let expression
  ILetExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    env <- getEnv
    (bindingTIs, extendedEnv, s1) <- inferIBindingsWithContext bindings env emptySubst exprCtx
    (bodyTI, s2) <- withEnv extendedEnv $ inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTI
        finalS = composeSubst s2 s1
        resultType = applySubst finalS bodyType
    return (mkTIExpr resultType (TILetExpr bindingTIs bodyTI), finalS)
  
  -- LetRec expression
  ILetRecExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    env <- getEnv
    (bindingTIs, extendedEnv, s1) <- inferIRecBindingsWithContext bindings env emptySubst exprCtx
    (bodyTI, s2) <- withEnv extendedEnv $ inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTI
        finalS = composeSubst s2 s1
        resultType = applySubst finalS bodyType
    return (mkTIExpr resultType (TILetRecExpr bindingTIs bodyTI), finalS)
  
  -- Sequence expression
  ISeqExpr expr1 expr2 -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (expr1TI, s1) <- inferIExprWithContext expr1 exprCtx
    (expr2TI, s2) <- inferIExprWithContext expr2 exprCtx
    let t2 = tiExprType expr2TI
    return (mkTIExpr t2 (TISeqExpr expr1TI expr2TI), composeSubst s2 s1)
  
  -- Inductive Data Constructor
  IInductiveDataExpr name args -> do
    -- Look up constructor type in environment
    env <- getEnv
    case lookupEnv name env of
      Just scheme -> do
        -- Instantiate the type scheme
        st <- get
        let (_constraints, constructorType, newCounter) = instantiate scheme (inferCounter st)
        modify $ \s -> s { inferCounter = newCounter }
        -- Treat constructor as a function application
        inferIApplication name constructorType args emptySubst
      Nothing -> do
        -- Constructor not found in environment
        let exprCtx = withExpr (prettyStr expr) ctx
        permissive <- isPermissive
        if permissive
          then do
            -- In permissive mode, treat as a warning and return a fresh type variable
            addWarning $ UnboundVariableWarning name exprCtx
            resultType <- freshVar "ctor"
            return (mkTIExpr resultType (TIInductiveDataExpr name []), emptySubst)
          else throwError $ UnboundVariable name exprCtx
  
  -- Matchers (return Matcher type)
  IMatcherExpr patDefs -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    -- Infer type of each pattern definition (matcher clause)
    -- Each clause has: (PrimitivePatPattern, nextMatcherExpr, [(primitiveDataPat, targetExpr)])
    results <- mapM (inferPatternDef exprCtx) patDefs
    
    -- Collect TIPatternDefs and substitutions
    let tiPatDefs = map fst results
        substs = concatMap (snd . snd) results  -- Extract [Subst] from (TIPatternDef, (Type, [Subst]))
        finalSubst = foldr composeSubst emptySubst substs
    
    -- All clauses should agree on the matched type
    -- Unify all matched types from each pattern definition
    let matchedTypes = [applySubst finalSubst ty | (_, (ty, _)) <- results]
    (matchedTy, s_matched) <- case matchedTypes of
      [] -> do
        ty <- freshVar "matched"
        return (ty, emptySubst)
      (firstTy:restTys) -> do
        -- Unify all matched types
        s <- foldM (\accS ty -> do
            s' <- unifyTypesWithContext (applySubst accS firstTy) (applySubst accS ty) exprCtx
            return $ composeSubst s' accS
          ) emptySubst restTys
        return (applySubst s firstTy, s)
    
    let allSubst = composeSubst s_matched finalSubst
    return (mkTIExpr (TMatcher matchedTy) (TIMatcherExpr tiPatDefs), allSubst)
    where
      -- Infer a single pattern definition (matcher clause)
      -- Returns (TIPatternDef, (matched type, [substitutions]))
      inferPatternDef :: TypeErrorContext -> IPatternDef -> Infer (TIPatternDef, (Type, [Subst]))
      inferPatternDef ctx (ppPat, nextMatcherExpr, dataClauses) = do
        -- Infer the type of next matcher expression
        -- It should be a Matcher type (possibly Matcher of tuple, like Matcher (a, b))
        -- Note: (integer, integer) is inferred as Matcher (Integer, Integer), not (Matcher Integer, Matcher Integer)
        (nextMatcherTI, s1) <- inferIExprWithContext nextMatcherExpr ctx
        let nextMatcherType = tiExprType nextMatcherTI
        
        -- nextMatcherType must be a Matcher type
        -- Unify with Matcher a to constrain it and detect errors early
        matcherInnerTy <- freshVar "matcherInner"
        s1' <- unifyTypesWithContext (applySubst s1 nextMatcherType) (TMatcher matcherInnerTy) ctx
        let nextMatcherType' = applySubst s1' nextMatcherType
        
        -- Infer PrimitivePatPattern type to get matched type, pattern hole types, and variable bindings
        (matchedType, patternHoleTypes, ppBindings, s_pp) <- inferPrimitivePatPattern ppPat ctx
        let s1'' = composeSubst s_pp s1'
            matchedType' = applySubst s1'' matchedType
            -- Apply substitution to variable bindings
            ppBindings' = [(var, applySubstScheme s1'' scheme) | (var, scheme) <- ppBindings]
        
        -- Apply substitution to pattern hole types (keep as inner types)
        let patternHoleTypes' = map (applySubst s1'') patternHoleTypes
        
        -- Extract inner type(s) from next matcher type
        -- If multiple pattern holes, combine them into a tuple to match ITupleExpr behavior
        nextMatcherInnerTypes <- extractInnerTypesFromMatcher nextMatcherType' (length patternHoleTypes') ctx
        
        -- Unify pattern hole types (inner types) with next matcher inner types
        s_unify <- checkPatternHoleConsistency patternHoleTypes' nextMatcherInnerTypes ctx
        let s1''' = composeSubst s_unify s1''
        
        -- Infer the type of data clauses with pp variables in scope
        -- Each data clause: (primitiveDataPattern, targetListExpr)
        dataClauseResults <- withEnv ppBindings' $ 
          mapM (inferDataClauseWithCheck ctx nextMatcherInnerTypes matchedType') dataClauses
        let s2 = foldr composeSubst emptySubst dataClauseResults
        
        -- Build TIPatternDef: need to convert dataClauses to TIBindingExpr
        -- For each data clause, infer the pattern to get bindings, then infer the expression with those bindings
        dataClauseTIs <- withEnv ppBindings' $ 
          mapM (\(pdPat, targetExpr) -> do
            -- Infer primitive data pattern to get variable bindings
            (_, pdBindings, _) <- inferPrimitiveDataPattern pdPat matchedType' ctx
            -- Infer target expression with both pp variables and pd pattern variables in scope
            (targetTI, _) <- withEnv pdBindings $ inferIExprWithContext targetExpr ctx
            return (pdPat, targetTI)) dataClauses
        
        let tiPatDef = (ppPat, nextMatcherTI, dataClauseTIs)
        
        return (tiPatDef, (matchedType', [s1''', s2]))
      
      -- Infer PrimitivePatPattern type
      -- Returns (matched type, pattern hole types, variable bindings, substitution)
      -- Pattern hole types are the inner types (without TMatcher wrapper)
      -- The caller should wrap them with TMatcher when unifying with next matcher types
      -- Variable bindings are for PPValuePat variables (#$val)
      -- Note: Pattern hole types are determined by the pattern constructor, not by external context
      inferPrimitivePatPattern :: PrimitivePatPattern -> TypeErrorContext -> Infer (Type, [Type], [(String, TypeScheme)], Subst)
      inferPrimitivePatPattern ppPat ctx = case ppPat of
        PPWildCard -> do
          -- Wildcard pattern: no pattern holes, no bindings
          matchedTy <- freshVar "matched"
          return (matchedTy, [], [], emptySubst)
        
        PPPatVar -> do
          -- Pattern variable ($): one pattern hole, no binding
          -- Returns the matched type as the pattern hole type
          -- The caller will wrap it with TMatcher when unifying with next matcher type
          matchedTy <- freshVar "matched"
          return (matchedTy, [matchedTy], [], emptySubst)
        
        PPValuePat var -> do
          -- Value pattern (#$val): no pattern holes, binds variable to matched type
          matchedTy <- freshVar "matched"
          let binding = (var, Forall [] [] matchedTy)
          return (matchedTy, [], [binding], emptySubst)
        
        PPTuplePat ppPats -> do
          -- Tuple pattern: ($p1, $p2, ...)
          -- Recursively infer each sub-pattern
          results <- mapM (\pp -> inferPrimitivePatPattern pp ctx) ppPats
          let matchedTypes = [mt | (mt, _, _, _) <- results]
              patternHoleLists = [phs | (_, phs, _, _) <- results]
              bindingLists = [bs | (_, _, bs, _) <- results]
              substs = [s | (_, _, _, s) <- results]
              allPatternHoles = concat patternHoleLists
              allBindings = concat bindingLists
              finalSubst = foldr composeSubst emptySubst substs
          
          -- Matched type is tuple of matched types
          let matchedTy = TTuple (map (applySubst finalSubst) matchedTypes)
          return (matchedTy, map (applySubst finalSubst) allPatternHoles, allBindings, finalSubst)
        
        PPInductivePat name ppPats -> do
          -- Inductive pattern: look up pattern constructor type from pattern environment
          patternEnv <- getPatternEnv
          case lookupPatternEnv name patternEnv of
            Just scheme -> do
              -- Found in pattern environment: use the declared type
              st <- get
              let (_constraints, ctorType, newCounter) = instantiate scheme (inferCounter st)
              modify $ \s -> s { inferCounter = newCounter }
              
              -- Pattern constructor type: arg1 -> arg2 -> ... -> resultType
              -- Extract argument types and result type
              let (argTypes, resultType) = extractFunctionArgs ctorType
              
              -- Check argument count matches
              if length argTypes /= length ppPats
                then throwError $ TE.TypeMismatch
                       (foldr TFun resultType (replicate (length ppPats) (TVar (TyVar "a"))))
                       ctorType
                       ("Pattern constructor " ++ name ++ " expects " ++ show (length argTypes) 
                        ++ " arguments, but got " ++ show (length ppPats))
                       ctx
                else do
                  -- Recursively infer each sub-pattern
                  results <- mapM (\pp -> inferPrimitivePatPattern pp ctx) ppPats
                  
                  let matchedTypes = [mt | (mt, _, _, _) <- results]
                      patternHoleLists = [phs | (_, phs, _, _) <- results]
                      bindingLists = [bs | (_, _, bs, _) <- results]
                      substs = [s | (_, _, _, s) <- results]
                      allPatternHoles = concat patternHoleLists
                      allBindings = concat bindingLists
                      s = foldr composeSubst emptySubst substs
                  
                  -- Verify that inferred matched types match expected argument types
                  -- Extract inner types from Matcher types in argTypes
                  let expectedMatchedTypes = map (\ty -> case ty of
                        TMatcher inner -> inner
                        _ -> ty) argTypes
                  s' <- foldM (\accS (inferredTy, expectedTy) -> do
                      s'' <- unifyTypesWithContext (applySubst accS inferredTy) (applySubst accS expectedTy) ctx
                      return $ composeSubst s'' accS
                    ) s (zip matchedTypes expectedMatchedTypes)
                  
                  return (applySubst s' resultType, map (applySubst s') allPatternHoles, allBindings, s')
            
            Nothing -> do
              -- Not found in pattern environment: use generic inference
              -- This is for backward compatibility
              results <- mapM (\pp -> inferPrimitivePatPattern pp ctx) ppPats
              let matchedTypes = [mt | (mt, _, _, _) <- results]
                  patternHoleLists = [phs | (_, phs, _, _) <- results]
                  bindingLists = [bs | (_, _, bs, _) <- results]
                  substs = [s | (_, _, _, s) <- results]
                  allPatternHoles = concat patternHoleLists
                  allBindings = concat bindingLists
                  s = foldr composeSubst emptySubst substs
              
              -- Result type is inductive type
              let resultType = TInductive name (map (applySubst s) matchedTypes)
              return (resultType, map (applySubst s) allPatternHoles, allBindings, s)
      
      -- Extract function argument types and result type
      -- e.g., a -> b -> c -> d  =>  ([a, b, c], d)
      extractFunctionArgs :: Type -> ([Type], Type)
      extractFunctionArgs (TFun arg rest) = 
        let (args, result) = extractFunctionArgs rest
        in (arg : args, result)
      extractFunctionArgs t = ([], t)
      
      -- Extract matched type from Matcher type
      -- Check consistency between pattern hole types and next matcher types
      checkPatternHoleConsistency :: [Type] -> [Type] -> TypeErrorContext -> Infer Subst
      checkPatternHoleConsistency [] [] _ctx = return emptySubst
      checkPatternHoleConsistency patternHoles nextMatchers ctx
        | length patternHoles /= length nextMatchers = 
            throwError $ TE.TypeMismatch
              (TTuple nextMatchers)
              (TTuple patternHoles)
              ("Inconsistent number of pattern holes (" ++ show (length patternHoles) 
               ++ ") and next matchers (" ++ show (length nextMatchers) ++ ")")
              ctx
        | otherwise = do
            -- Unify each pattern hole type with corresponding next matcher type
            foldM (\accS (holeTy, matcherTy) -> do
                s <- unifyTypesWithContext (applySubst accS holeTy) (applySubst accS matcherTy) ctx
                return $ composeSubst s accS
              ) emptySubst (zip patternHoles nextMatchers)
      
      -- Extract inner types from next matcher type
      -- Given Matcher a, returns [a]
      -- Given Matcher (a, b, ...) and n pattern holes, returns [a, b, ...] if n > 1, or [(a, b, ...)] if n = 1
      -- Special case: (Matcher a, Matcher b, ...) should be converted to Matcher (a, b, ...) first
      -- Note: Even when numHoles = 0, we extract inner types to detect mismatches in checkPatternHoleConsistency
      extractInnerTypesFromMatcher :: Type -> Int -> TypeErrorContext -> Infer [Type]
      extractInnerTypesFromMatcher matcherType numHoles ctx = case numHoles of
        0 -> case matcherType of
          -- No pattern holes, but extract inner type to allow error detection
          TMatcher innerType -> return [innerType]
          TTuple types -> do
            let matcherInners = mapM extractMatcherInner types
            case matcherInners of
              Just inners -> return inners
              Nothing -> return []  -- Not matcher types, return empty
          _ -> return []  -- Not a matcher type
        1 -> case matcherType of
          TMatcher innerType -> return [innerType]  -- Single hole: return inner type as-is
          -- Special case: (Matcher a, Matcher b, ...) from ITupleExpr that failed to convert
          -- This can happen when matcher parameters are used before ITupleExpr conversion
          TTuple types -> do
            let matcherInners = mapM extractMatcherInner types
            case matcherInners of
              Just inners -> return [TTuple inners]  -- Return as single tuple type
              Nothing -> throwError $ TE.TypeMismatch
                           (TMatcher (TVar (TyVar "a")))
                           matcherType
                           "Expected Matcher type or tuple of Matcher types"
                           ctx
          _ -> throwError $ TE.TypeMismatch
                 (TMatcher (TVar (TyVar "a")))
                 matcherType
                 "Expected Matcher type"
                 ctx
        n -> case matcherType of
          -- Multiple holes: expect Matcher (tuple) and extract each element
          TMatcher (TTuple innerTypes) ->
            if length innerTypes == n
              then return innerTypes
              else throwError $ TE.TypeMismatch
                     (TMatcher (TTuple (replicate n (TVar (TyVar "a")))))
                     matcherType
                     ("Expected Matcher with tuple of " ++ show n ++ " elements, but got " ++ show (length innerTypes))
                     ctx
          -- Special case: (Matcher a, Matcher b, ...) - extract inner types directly
          TTuple types -> do
            let matcherInners = mapM extractMatcherInner types
            case matcherInners of
              Just inners | length inners == n -> return inners
              _ -> throwError $ TE.TypeMismatch
                     (TMatcher (TTuple (replicate n (TVar (TyVar "a")))))
                     matcherType
                     "Expected tuple of Matcher types with correct count"
                     ctx
          _ -> throwError $ TE.TypeMismatch
                 (TMatcher (TTuple (replicate n (TVar (TyVar "a")))))
                 matcherType
                 ("Expected Matcher of tuple with " ++ show n ++ " elements")
                 ctx
      
      -- Helper: Extract inner type from Matcher a -> Just a, otherwise Nothing
      extractMatcherInner :: Type -> Maybe Type
      extractMatcherInner (TMatcher t) = Just t
      extractMatcherInner _ = Nothing
      
      -- Infer a data clause with type checking
      -- Check that the target expression returns a list of values with types matching next matcher inner types
      -- Also uses matched type for validation
      -- nextMatcherInnerTypes: inner types extracted from next matcher (already without TMatcher wrapper)
      inferDataClauseWithCheck :: TypeErrorContext -> [Type] -> Type -> (IPrimitiveDataPattern, IExpr) -> Infer Subst
      inferDataClauseWithCheck ctx nextMatcherInnerTypes matchedType (pdPat, targetExpr) = do
        -- Extract expected element type from next matcher inner types (the target type)
        -- This is the type of elements in the list returned by the target expression
        targetType <- case nextMatcherInnerTypes of
          [] -> return (TTuple [])  -- No pattern holes: empty tuple () case
          [single] -> return single  -- Single pattern hole: use inner type directly
          multiple -> return (TTuple multiple)  -- Multiple holes: tuple of inner types
        
        -- Infer PrimitiveDataPattern with matched type
        -- Primitive data pattern matches against values of the matched type
        -- and produces bindings and next targets
        (pdTargetType, bindings, s_pd) <- inferPrimitiveDataPattern pdPat matchedType ctx
        
        -- The primitive data pattern should match the matched type
        -- No need to unify pdTargetType with targetType - they serve different purposes
        -- pdTargetType: type of data that pdPat matches (should be matchedType)
        -- targetType: type of next targets returned by the target expression
        
        -- Verify that pdTargetType is consistent with matchedType
        s_match <- unifyTypesWithContext (applySubst s_pd pdTargetType) (applySubst s_pd matchedType) ctx
        let s_pd' = composeSubst s_match s_pd
        
        -- Infer the target expression with pattern variables in scope
        (targetTI, s1) <- withEnv bindings $ inferIExprWithContext targetExpr ctx
        let exprType = tiExprType targetTI
            s_combined = composeSubst s1 s_pd'
        
        -- Unify with actual expression type
        -- Expected: [targetType]
        let expectedType = TCollection (applySubst s_combined targetType)
        
        s2 <- unifyTypesWithContext (applySubst s_combined exprType) expectedType ctx
        return $ composeSubst s2 s_combined
      
      -- Helper to check if a pattern is a pattern variable
      isPDPatVar :: IPrimitiveDataPattern -> Bool
      isPDPatVar (PDPatVar _) = True
      isPDPatVar _ = False
      
      -- Infer PrimitiveDataPattern type
      -- Returns (inferred target type, variable bindings, substitution)
      -- This is similar to pattern matching in Haskell for algebraic data types
      inferPrimitiveDataPattern :: IPrimitiveDataPattern -> Type -> TypeErrorContext -> Infer (Type, [(String, TypeScheme)], Subst)
      inferPrimitiveDataPattern pdPat expectedType ctx = case pdPat of
        PDWildCard -> do
          -- Wildcard: matches any type, no bindings
          return (expectedType, [], emptySubst)
        
        PDPatVar var -> do
          -- Pattern variable: binds to the expected type
          let varName = extractNameFromVar var
          return (expectedType, [(varName, Forall [] [] expectedType)], emptySubst)
        
        PDConstantPat c -> do
          -- Constant pattern: must match the constant's type
          constTy <- inferConstant c
          s <- unifyTypesWithContext constTy expectedType ctx
          return (applySubst s expectedType, [], s)
        
        PDTuplePat pats -> do
          -- Tuple pattern: expected type should be a tuple
          case expectedType of
            TTuple types | length types == length pats -> do
              -- Types match: infer each sub-pattern
              results <- zipWithM (\p t -> inferPrimitiveDataPattern p t ctx) pats types
              let (_, bindingsList, substs) = unzip3 results
                  allBindings = concat bindingsList
                  s = foldr composeSubst emptySubst substs
              return (applySubst s expectedType, allBindings, s)
            
            TVar _ -> do
              -- Expected type is a type variable: create fresh types for each element
              elemTypes <- mapM (\_ -> freshVar "elem") pats
              let tupleTy = TTuple elemTypes
              s <- unifyTypesWithContext expectedType tupleTy ctx
              
              -- Recursively infer each sub-pattern
              results <- zipWithM (\p t -> inferPrimitiveDataPattern p (applySubst s t) ctx) pats elemTypes
              let (_, bindingsList, substs) = unzip3 results
                  allBindings = concat bindingsList
                  s' = foldr composeSubst s substs
              return (applySubst s' tupleTy, allBindings, s')
            
            _ -> do
              -- Type mismatch
              throwError $ TE.TypeMismatch
                (TTuple (replicate (length pats) (TVar (TyVar "a"))))
                expectedType
                "Tuple pattern but target is not a tuple type"
                ctx
        
        PDEmptyPat -> do
          -- Empty collection pattern: expected type should be [a] for some a
          elemTy <- freshVar "elem"
          s <- unifyTypesWithContext expectedType (TCollection elemTy) ctx
          return (applySubst s (TCollection elemTy), [], s)
        
        PDConsPat p1 p2 -> do
          -- Cons pattern: expected type should be [a] for some a
          case expectedType of
            TCollection elemType -> do
              -- Infer head pattern with element type
              (_, bindings1, s1) <- inferPrimitiveDataPattern p1 elemType ctx
              -- Infer tail pattern with collection type
              (_, bindings2, s2) <- inferPrimitiveDataPattern p2 (applySubst s1 expectedType) ctx
              let s = composeSubst s2 s1
              return (applySubst s expectedType, bindings1 ++ bindings2, s)
            
            TVar _ -> do
              -- Expected type is a type variable: constrain it to be a collection
              elemTy <- freshVar "elem"
              s <- unifyTypesWithContext expectedType (TCollection elemTy) ctx
              let collTy = applySubst s (TCollection elemTy)
                  elemTy' = applySubst s elemTy
              (_, bindings1, s1) <- inferPrimitiveDataPattern p1 elemTy' ctx
              (_, bindings2, s2) <- inferPrimitiveDataPattern p2 (applySubst s1 collTy) ctx
              let s' = composeSubst s2 (composeSubst s1 s)
              return (applySubst s' collTy, bindings1 ++ bindings2, s')
            
            _ -> do
              throwError $ TE.TypeMismatch
                (TCollection (TVar (TyVar "a")))
                expectedType
                "Cons pattern but target is not a collection type"
                ctx
        
        PDSnocPat p1 p2 -> do
          -- Snoc pattern: similar to cons but reversed
          case expectedType of
            TCollection elemType -> do
              (_, bindings1, s1) <- inferPrimitiveDataPattern p1 expectedType ctx
              (_, bindings2, s2) <- inferPrimitiveDataPattern p2 (applySubst s1 elemType) ctx
              let s = composeSubst s2 s1
              return (applySubst s expectedType, bindings1 ++ bindings2, s)
            
            TVar _ -> do
              elemTy <- freshVar "elem"
              s <- unifyTypesWithContext expectedType (TCollection elemTy) ctx
              let collTy = applySubst s (TCollection elemTy)
                  elemTy' = applySubst s elemTy
              (_, bindings1, s1) <- inferPrimitiveDataPattern p1 collTy ctx
              (_, bindings2, s2) <- inferPrimitiveDataPattern p2 (applySubst s1 elemTy') ctx
              let s' = composeSubst s2 (composeSubst s1 s)
              return (applySubst s' collTy, bindings1 ++ bindings2, s')
            
            _ -> do
              throwError $ TE.TypeMismatch
                (TCollection (TVar (TyVar "a")))
                expectedType
                "Snoc pattern but target is not a collection type"
                ctx
        
        PDInductivePat name pats -> do
          -- Inductive pattern: look up data constructor type from environment
          env <- getEnv
          case lookupEnv name env of
            Just scheme -> do
              -- Found in environment: use the declared type
              st <- get
              let (_constraints, ctorType, newCounter) = instantiate scheme (inferCounter st)
              modify $ \s -> s { inferCounter = newCounter }
              
              -- Data constructor type: arg1 -> arg2 -> ... -> resultType
              let (argTypes, resultType) = extractFunctionArgs ctorType
              
              -- Check argument count matches
              if length argTypes /= length pats
                then throwError $ TE.TypeMismatch
                       (foldr TFun resultType (replicate (length pats) (TVar (TyVar "a"))))
                       ctorType
                       ("Data constructor " ++ name ++ " expects " ++ show (length argTypes) 
                        ++ " arguments, but got " ++ show (length pats))
                       ctx
                else do
                  -- Unify result type with expected type
                  s0 <- unifyTypesWithContext resultType expectedType ctx
                  let resultType' = applySubst s0 resultType
                      argTypes' = map (applySubst s0) argTypes
                  
                  -- Recursively infer each sub-pattern
                  results <- zipWithM (\p argTy -> inferPrimitiveDataPattern p argTy ctx) pats argTypes'
                  let (_, bindingsList, substs) = unzip3 results
                      allBindings = concat bindingsList
                      s = foldr composeSubst s0 substs
                  
                  -- Return the result type, not expected type
                  return (applySubst s resultType', allBindings, s)
            
            Nothing -> do
              -- Not found in environment: use generic inference
              argTypes <- mapM (\_ -> freshVar "arg") pats
              let resultType = TInductive name argTypes
              
              s0 <- unifyTypesWithContext resultType expectedType ctx
              let resultType' = applySubst s0 resultType
              
              results <- zipWithM (\p argTy -> inferPrimitiveDataPattern p (applySubst s0 argTy) ctx) pats argTypes
              let (_, bindingsList, substs) = unzip3 results
                  allBindings = concat bindingsList
                  s = foldr composeSubst s0 substs
              
              return (applySubst s resultType', allBindings, s)
        
        -- ScalarData (MathExpr) primitive patterns
        PDDivPat patNum patDen -> do
          -- Div: MathExpr -> PolyExpr, PolyExpr
          -- However, if pattern is a pattern variable, it gets MathExpr (auto-conversion)
          let polyExprTy = TPolyExpr
              mathExprTy = TMathExpr
              numTy = if isPDPatVar patNum then mathExprTy else polyExprTy
              denTy = if isPDPatVar patDen then mathExprTy else polyExprTy
          (_, bindings1, s1) <- inferPrimitiveDataPattern patNum numTy ctx
          (_, bindings2, s2) <- inferPrimitiveDataPattern patDen (applySubst s1 denTy) ctx
          let s = composeSubst s2 s1
          return (applySubst s expectedType, bindings1 ++ bindings2, s)
        
        PDPlusPat patTerms -> do
          -- Plus: PolyExpr -> [TermExpr]
          -- If pattern variable, it gets [MathExpr]
          let termExprTy = TTermExpr
              mathExprTy = TMathExpr
              termsTy = if isPDPatVar patTerms then TCollection mathExprTy else TCollection termExprTy
          (_, bindings, s) <- inferPrimitiveDataPattern patTerms termsTy ctx
          return (applySubst s expectedType, bindings, s)
        
        PDTermPat patCoeff patMonomials -> do
          -- Term: TermExpr -> Integer, [(SymbolExpr, Integer)]
          -- If patMonomials is pattern variable, it gets [(MathExpr, Integer)]
          let symbolExprTy = TSymbolExpr
              mathExprTy = TMathExpr
              monomialsElemTy = if isPDPatVar patMonomials 
                                then TTuple [mathExprTy, TInt]
                                else TTuple [symbolExprTy, TInt]
          (_, bindings1, s1) <- inferPrimitiveDataPattern patCoeff TInt ctx
          (_, bindings2, s2) <- inferPrimitiveDataPattern patMonomials 
                                  (applySubst s1 (TCollection monomialsElemTy)) ctx
          let s = composeSubst s2 s1
          return (applySubst s expectedType, bindings1 ++ bindings2, s)
        
        PDSymbolPat patName patIndices -> do
          -- Symbol: SymbolExpr -> String, [IndexExpr]
          -- patName and patIndices types don't change for pattern variables
          let indexExprTy = TIndexExpr
          (_, bindings1, s1) <- inferPrimitiveDataPattern patName TString ctx
          (_, bindings2, s2) <- inferPrimitiveDataPattern patIndices 
                                  (applySubst s1 (TCollection indexExprTy)) ctx
          let s = composeSubst s2 s1
          return (applySubst s expectedType, bindings1 ++ bindings2, s)
        
        PDApply1Pat patFn patArg -> do
          -- Apply1: SymbolExpr -> (MathExpr -> MathExpr), MathExpr
          let mathExprTy = TMathExpr
              fnTy = TFun mathExprTy mathExprTy
          (_, bindings1, s1) <- inferPrimitiveDataPattern patFn fnTy ctx
          (_, bindings2, s2) <- inferPrimitiveDataPattern patArg (applySubst s1 mathExprTy) ctx
          let s = composeSubst s2 s1
          return (applySubst s expectedType, bindings1 ++ bindings2, s)
        
        PDApply2Pat patFn patArg1 patArg2 -> do
          let mathExprTy = TMathExpr
              fnTy = TFun mathExprTy (TFun mathExprTy mathExprTy)
          (_, bindings1, s1) <- inferPrimitiveDataPattern patFn fnTy ctx
          (_, bindings2, s2) <- inferPrimitiveDataPattern patArg1 (applySubst s1 mathExprTy) ctx
          (_, bindings3, s3) <- inferPrimitiveDataPattern patArg2 (applySubst s2 mathExprTy) ctx
          let s = composeSubst s3 (composeSubst s2 s1)
          return (applySubst s expectedType, bindings1 ++ bindings2 ++ bindings3, s)
        
        PDApply3Pat patFn patArg1 patArg2 patArg3 -> do
          let mathExprTy = TMathExpr
              fnTy = TFun mathExprTy (TFun mathExprTy (TFun mathExprTy mathExprTy))
          (_, bindings1, s1) <- inferPrimitiveDataPattern patFn fnTy ctx
          (_, bindings2, s2) <- inferPrimitiveDataPattern patArg1 (applySubst s1 mathExprTy) ctx
          (_, bindings3, s3) <- inferPrimitiveDataPattern patArg2 (applySubst s2 mathExprTy) ctx
          (_, bindings4, s4) <- inferPrimitiveDataPattern patArg3 (applySubst s3 mathExprTy) ctx
          let s = composeSubst s4 (composeSubst s3 (composeSubst s2 s1))
          return (applySubst s expectedType, bindings1 ++ bindings2 ++ bindings3 ++ bindings4, s)
        
        PDApply4Pat patFn patArg1 patArg2 patArg3 patArg4 -> do
          let mathExprTy = TMathExpr
              fnTy = TFun mathExprTy (TFun mathExprTy (TFun mathExprTy (TFun mathExprTy mathExprTy)))
          (_, bindings1, s1) <- inferPrimitiveDataPattern patFn fnTy ctx
          (_, bindings2, s2) <- inferPrimitiveDataPattern patArg1 (applySubst s1 mathExprTy) ctx
          (_, bindings3, s3) <- inferPrimitiveDataPattern patArg2 (applySubst s2 mathExprTy) ctx
          (_, bindings4, s4) <- inferPrimitiveDataPattern patArg3 (applySubst s3 mathExprTy) ctx
          (_, bindings5, s5) <- inferPrimitiveDataPattern patArg4 (applySubst s4 mathExprTy) ctx
          let s = composeSubst s5 (composeSubst s4 (composeSubst s3 (composeSubst s2 s1)))
          return (applySubst s expectedType, bindings1 ++ bindings2 ++ bindings3 ++ bindings4 ++ bindings5, s)
        
        PDQuotePat patExpr -> do
          -- Quote: SymbolExpr -> MathExpr
          let mathExprTy = TMathExpr
          (_, bindings, s) <- inferPrimitiveDataPattern patExpr mathExprTy ctx
          return (applySubst s expectedType, bindings, s)
        
        PDFunctionPat patName patArgs patKwargs -> do
          -- Function: SymbolExpr -> MathExpr, [MathExpr], [MathExpr]
          let mathExprTy = TMathExpr
          (_, bindings1, s1) <- inferPrimitiveDataPattern patName mathExprTy ctx
          (_, bindings2, s2) <- inferPrimitiveDataPattern patArgs 
                                  (applySubst s1 (TCollection mathExprTy)) ctx
          (_, bindings3, s3) <- inferPrimitiveDataPattern patKwargs 
                                  (applySubst s2 (TCollection mathExprTy)) ctx
          let s = composeSubst s3 (composeSubst s2 s1)
          return (applySubst s expectedType, bindings1 ++ bindings2 ++ bindings3, s)
        
        PDSubPat patExpr -> do
          -- Sub: IndexExpr -> MathExpr
          let mathExprTy = TMathExpr
          (_, bindings, s) <- inferPrimitiveDataPattern patExpr mathExprTy ctx
          return (applySubst s expectedType, bindings, s)
        
        PDSupPat patExpr -> do
          -- Sup: IndexExpr -> MathExpr
          let mathExprTy = TMathExpr
          (_, bindings, s) <- inferPrimitiveDataPattern patExpr mathExprTy ctx
          return (applySubst s expectedType, bindings, s)
        
        PDUserPat patExpr -> do
          -- User: IndexExpr -> MathExpr
          let mathExprTy = TMathExpr
          (_, bindings, s) <- inferPrimitiveDataPattern patExpr mathExprTy ctx
          return (applySubst s expectedType, bindings, s)
  
  -- Match expressions (pattern matching)
  IMatchExpr mode target matcher clauses -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (targetTI, s1) <- inferIExprWithContext target exprCtx
    (matcherTI, s2) <- inferIExprWithContext matcher exprCtx
    let targetType = tiExprType targetTI
        matcherType = tiExprType matcherTI
    
    -- Matcher should be TMatcher a or (TMatcher a, TMatcher b, ...) which becomes TMatcher (a, b, ...)
    let s12 = composeSubst s2 s1
        appliedMatcherType = applySubst s12 matcherType
    
    -- Normalize matcher type: if it's a tuple, ensure each element is a Matcher
    (_normalizedMatcherType, matchedInnerType, s3) <- case appliedMatcherType of
      TTuple elemTypes -> do
        -- Each tuple element should be Matcher ai
        matchedInnerTypes <- mapM (\_ -> freshVar "matched") elemTypes
        s_elems <- foldM (\accS (elemTy, innerTy) -> do
          let appliedElemTy = applySubst accS elemTy
              appliedInnerTy = applySubst accS innerTy
          s' <- unifyTypesWithContext appliedElemTy (TMatcher appliedInnerTy) exprCtx
          return $ composeSubst s' accS
          ) emptySubst (zip elemTypes matchedInnerTypes)
        -- The tuple as a whole becomes Matcher (a1, a2, ...)
        let finalInnerTypes = map (applySubst s_elems) matchedInnerTypes
            tupleInnerType = TTuple finalInnerTypes
        return (TMatcher tupleInnerType, tupleInnerType, s_elems)
      _ -> do
        -- Single matcher: TMatcher a
        matchedTy <- freshVar "matched"
        s' <- unifyTypesWithContext appliedMatcherType (TMatcher matchedTy) exprCtx
        let finalMatchedTy = applySubst s' matchedTy
        return (TMatcher finalMatchedTy, finalMatchedTy, s')
    
    let s123 = composeSubst s3 s12
    s4 <- unifyTypesWithContext (applySubst s123 targetType) (applySubst s123 matchedInnerType) exprCtx
    
    -- Infer match clauses result type
    let s1234 = composeSubst s4 s123
    case clauses of
      [] -> do
        -- No clauses: this should not happen, but handle gracefully
        resultTy <- freshVar "matchResult"
        let targetTI' = applySubstToTIExpr s1234 targetTI
            matcherTI' = applySubstToTIExpr s1234 matcherTI
        return (mkTIExpr (applySubst s1234 resultTy) (TIMatchExpr mode targetTI' matcherTI' []), s1234)
      _ -> do
        -- Infer type of each clause and unify them
        (resultTy, clauseTIs, clauseSubst) <- inferMatchClauses exprCtx (applySubst s1234 matchedInnerType) clauses s1234
        let finalS = composeSubst clauseSubst s1234
            targetTI' = applySubstToTIExpr finalS targetTI
            matcherTI' = applySubstToTIExpr finalS matcherTI
        return (mkTIExpr (applySubst finalS resultTy) (TIMatchExpr mode targetTI' matcherTI' clauseTIs), finalS)
  
  -- MatchAll expressions
  IMatchAllExpr mode target matcher clauses -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (targetTI, s1) <- inferIExprWithContext target exprCtx
    (matcherTI, s2) <- inferIExprWithContext matcher exprCtx
    let targetType = tiExprType targetTI
        matcherType = tiExprType matcherTI
    
    -- Matcher should be TMatcher a or (TMatcher a, TMatcher b, ...) which becomes TMatcher (a, b, ...)
    let s12 = composeSubst s2 s1
        appliedMatcherType = applySubst s12 matcherType
    
    -- Normalize matcher type: if it's a tuple, ensure each element is a Matcher
    (_normalizedMatcherType, matchedInnerType, s3) <- case appliedMatcherType of
      TTuple elemTypes -> do
        -- Each tuple element should be Matcher ai
        matchedInnerTypes <- mapM (\_ -> freshVar "matched") elemTypes
        s_elems <- foldM (\accS (elemTy, innerTy) -> do
          let appliedElemTy = applySubst accS elemTy
              appliedInnerTy = applySubst accS innerTy
          s' <- unifyTypesWithContext appliedElemTy (TMatcher appliedInnerTy) exprCtx
          return $ composeSubst s' accS
          ) emptySubst (zip elemTypes matchedInnerTypes)
        -- The tuple as a whole becomes Matcher (a1, a2, ...)
        let finalInnerTypes = map (applySubst s_elems) matchedInnerTypes
            tupleInnerType = TTuple finalInnerTypes
        return (TMatcher tupleInnerType, tupleInnerType, s_elems)
      _ -> do
        -- Single matcher: TMatcher a
        matchedTy <- freshVar "matched"
        s' <- unifyTypesWithContext appliedMatcherType (TMatcher matchedTy) exprCtx
        let finalMatchedTy = applySubst s' matchedTy
        return (TMatcher finalMatchedTy, finalMatchedTy, s')
    
    let s123 = composeSubst s3 s12
    s4 <- unifyTypesWithContext (applySubst s123 targetType) (applySubst s123 matchedInnerType) exprCtx
    
    -- MatchAll returns a collection of results from match clauses
    let s1234 = composeSubst s4 s123
    case clauses of
      [] -> do
        -- No clauses: return empty collection type
        resultElemTy <- freshVar "matchAllElem"
        let targetTI' = applySubstToTIExpr s1234 targetTI
            matcherTI' = applySubstToTIExpr s1234 matcherTI
        return (mkTIExpr (TCollection (applySubst s1234 resultElemTy)) (TIMatchAllExpr mode targetTI' matcherTI' []), s1234)
      _ -> do
        -- Infer type of each clause (they should all have the same type)
        (resultElemTy, clauseTIs, clauseSubst) <- inferMatchClauses exprCtx (applySubst s1234 matchedInnerType) clauses s1234
        let finalS = composeSubst clauseSubst s1234
            targetTI' = applySubstToTIExpr finalS targetTI
            matcherTI' = applySubstToTIExpr finalS matcherTI
        return (mkTIExpr (TCollection (applySubst finalS resultElemTy)) (TIMatchAllExpr mode targetTI' matcherTI' clauseTIs), finalS)
  
  -- Memoized Lambda
  IMemoizedLambdaExpr args body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argTypes <- mapM (\_ -> freshVar "memoArg") args
    let bindings = zip args argTypes  -- [(String, Type)]
        schemes = map (\(name, t) -> (name, Forall [] [] t)) bindings
    (bodyTI, s) <- withEnv schemes $ inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTI
        finalArgTypes = map (applySubst s) argTypes
        funType = foldr TFun bodyType finalArgTypes
    return (mkTIExpr funType (TIMemoizedLambdaExpr args bodyTI), s)
  
  -- Do expression
  IDoExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    -- Infer IO monad bindings: each binding should be of type IO a
    env <- getEnv
    (bindingTIs, bindingSchemes, s1) <- inferIOBindingsWithContext bindings env emptySubst exprCtx
    (bodyTI, s2) <- withEnv bindingSchemes $ inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTI
        finalS = composeSubst s2 s1
        
    -- Verify that body type is IO a
    bodyResultType <- freshVar "ioResult"
    s3 <- unifyTypesWithContext (applySubst finalS bodyType) (TIO bodyResultType) exprCtx
    let resultType = applySubst s3 (TIO bodyResultType)
        finalS' = composeSubst s3 finalS
    return (mkTIExpr resultType (TIDoExpr bindingTIs bodyTI), finalS')
  
  -- Cambda (pattern matching lambda)
  ICambdaExpr var body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argType <- freshVar "cambdaArg"
    (bodyTI, s) <- inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTI
    return (mkTIExpr (TFun argType bodyType) (TICambdaExpr var bodyTI), s)
  
  -- With symbols
  IWithSymbolsExpr syms body -> do
    -- Add symbols to type environment as MathExpr (TMathExpr = TInt)
    -- Symbols introduced by withSymbols are mathematical symbols
    let symbolBindings = [(sym, Forall [] [] TMathExpr) | sym <- syms]
    (bodyTI, s) <- withEnv symbolBindings $ inferIExprWithContext body ctx
    let bodyType = tiExprType bodyTI
    return (mkTIExpr bodyType (TIWithSymbolsExpr syms bodyTI), s)
  
  -- Quote expressions (symbolic math)
  IQuoteExpr e -> do
    (eTI, s) <- inferIExprWithContext e ctx
    return (mkTIExpr TInt (TIQuoteExpr eTI), s)
  IQuoteSymbolExpr e -> do
    (eTI, s) <- inferIExprWithContext e ctx
    return (mkTIExpr (tiExprType eTI) (TIQuoteSymbolExpr eTI), s)
  
  -- Indexed expression (tensor indexing)
  IIndexedExpr isSupported baseExpr indices -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (baseTI, s) <- inferIExprWithContext baseExpr exprCtx
    let baseType = tiExprType baseTI
    -- Check if all indices are concrete (constants) or symbolic (variables)
    let isSymbolicIndex idx = case idx of
          Sub (IVarExpr _) -> True
          Sup (IVarExpr _) -> True
          SupSub (IVarExpr _) -> True
          User (IVarExpr _) -> True
          _ -> False
        hasSymbolicIndex = any isSymbolicIndex indices
    -- For tensors with symbolic indices, keep the tensor type
    -- For concrete indices (numeric), return element type
    let resultType = case baseType of
          TTensor elemType -> 
            if hasSymbolicIndex
              then TTensor elemType  -- Symbolic index: keep tensor type
              else elemType           -- Concrete index: element access
          TCollection elemType -> elemType
          THash _keyType valType -> valType  -- Hash access returns value type
          _ -> baseType  -- Fallback: return base type
    -- TODO: Infer indices as TIExpr instead of IExpr
    return (mkTIExpr resultType (TIIndexedExpr isSupported baseTI indices), s)
  
  -- Subrefs expression (subscript references)
  ISubrefsExpr isSupported baseExpr refExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (baseTI, s1) <- inferIExprWithContext baseExpr exprCtx
    (refTI, s2) <- inferIExprWithContext refExpr exprCtx
    let baseType = tiExprType baseTI
        finalS = composeSubst s2 s1
    -- TODO: Properly handle subscript semantics
    return (mkTIExpr baseType (TISubrefsExpr isSupported baseTI refTI), finalS)
  
  -- Suprefs expression (superscript references)
  ISuprefsExpr isSupported baseExpr refExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (baseTI, s1) <- inferIExprWithContext baseExpr exprCtx
    (refTI, s2) <- inferIExprWithContext refExpr exprCtx
    let baseType = tiExprType baseTI
        finalS = composeSubst s2 s1
    -- TODO: Properly handle superscript semantics
    return (mkTIExpr baseType (TISuprefsExpr isSupported baseTI refTI), finalS)
  
  -- Userrefs expression (user-defined references)
  IUserrefsExpr isSupported baseExpr refExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (baseTI, s1) <- inferIExprWithContext baseExpr exprCtx
    (refTI, s2) <- inferIExprWithContext refExpr exprCtx
    let baseType = tiExprType baseTI
        finalS = composeSubst s2 s1
    -- TODO: Properly handle user-defined references
    return (mkTIExpr baseType (TIUserrefsExpr isSupported baseTI refTI), finalS)
  
  -- Wedge apply expression (exterior product)
  IWedgeApplyExpr func args -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (funcTI, s1) <- inferIExprWithContext func exprCtx
    let funcType = tiExprType funcTI
    -- Wedge application is similar to normal application
    inferIApplicationWithContext funcTI funcType args s1 exprCtx
  
  -- Generate tensor expression
  IGenerateTensorExpr shapeExpr genFunc -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (shapeTI, s1) <- inferIExprWithContext shapeExpr exprCtx
    (funcTI, s2) <- inferIExprWithContext genFunc exprCtx
    let funcType = tiExprType funcTI
    -- Extract element type from function result
    elemType <- case funcType of
      TFun _ resultType -> return resultType
      _ -> freshVar "tensorElem"
    let finalS = composeSubst s2 s1
        resultType = normalizeTensorType (TTensor (applySubst finalS elemType))
    return (mkTIExpr resultType (TIGenerateTensorExpr shapeTI funcTI), finalS)
  
  -- Tensor expression
  ITensorExpr shapeExpr elemsExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (shapeTI, s1) <- inferIExprWithContext shapeExpr exprCtx
    (elemsTI, s2) <- inferIExprWithContext elemsExpr exprCtx
    let elemsType = tiExprType elemsTI
    -- Extract element type
    elemType <- case elemsType of
      TCollection t -> return t
      _ -> freshVar "tensorElem"
    let finalS = composeSubst s2 s1
        resultType = normalizeTensorType (TTensor (applySubst finalS elemType))
    return (mkTIExpr resultType (TITensorExpr shapeTI elemsTI), finalS)
  
  -- Tensor contract expression
  ITensorContractExpr tensorExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (tensorTI, s) <- inferIExprWithContext tensorExpr exprCtx
    let tensorType = tiExprType tensorTI
    -- contract : Tensor a -> [Tensor a]
    -- Wraps tensor type in a collection
    return (mkTIExpr (TCollection tensorType) (TITensorContractExpr tensorTI), s)
  
  -- Tensor map expression
  ITensorMapExpr func tensorExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (funcTI, s1) <- inferIExprWithContext func exprCtx
    (tensorTI, s2) <- inferIExprWithContext tensorExpr exprCtx
    let funcType = tiExprType funcTI
        tensorType = tiExprType tensorTI
        s12 = composeSubst s2 s1
    -- Function maps elements: a -> b, tensor is Tensor a, result is Tensor b
    case tensorType of
      TTensor elemType -> do
        resultElemType <- freshVar "tmapElem"
        s3 <- unifyTypesWithContext (applySubst s12 funcType) (TFun elemType resultElemType) exprCtx
        let finalS = composeSubst s3 s12
            resultType = normalizeTensorType (TTensor (applySubst finalS resultElemType))
        return (mkTIExpr resultType (TITensorMapExpr funcTI tensorTI), finalS)
      _ -> return (mkTIExpr tensorType (TITensorMapExpr funcTI tensorTI), s12)
  
  -- Tensor map2 expression (binary map)
  ITensorMap2Expr func tensor1 tensor2 -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (funcTI, s1) <- inferIExprWithContext func exprCtx
    (tensor1TI, s2) <- inferIExprWithContext tensor1 exprCtx
    (tensor2TI, s3) <- inferIExprWithContext tensor2 exprCtx
    let funcType = tiExprType funcTI
        t1Type = tiExprType tensor1TI
        t2Type = tiExprType tensor2TI
        s123 = foldr composeSubst emptySubst [s3, s2, s1]
    -- Function: a -> b -> c, tensors are Tensor a and Tensor b, result is Tensor c
    case (t1Type, t2Type) of
      (TTensor elem1, TTensor elem2) -> do
        resultElemType <- freshVar "tmap2Elem"
        s4 <- unifyTypesWithContext (applySubst s123 funcType) 
                (TFun elem1 (TFun elem2 resultElemType)) exprCtx
        let finalS = composeSubst s4 s123
            resultType = normalizeTensorType (TTensor (applySubst finalS resultElemType))
        return (mkTIExpr resultType (TITensorMap2Expr funcTI tensor1TI tensor2TI), finalS)
      _ -> return (mkTIExpr t1Type (TITensorMap2Expr funcTI tensor1TI tensor2TI), s123)
  
  -- Transpose expression
  ITransposeExpr tensorExpr permExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (tensorTI, s) <- inferIExprWithContext tensorExpr exprCtx
    (permTI, s2) <- inferIExprWithContext permExpr exprCtx
    let tensorType = tiExprType tensorTI
        finalS = composeSubst s2 s
    -- Transpose preserves tensor type
    return (mkTIExpr (normalizeTensorType tensorType) (TITransposeExpr tensorTI permTI), finalS)
  
  -- Flip indices expression
  IFlipIndicesExpr tensorExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (tensorTI, s) <- inferIExprWithContext tensorExpr exprCtx
    let tensorType = tiExprType tensorTI
    -- Flipping indices preserves tensor type
    return (mkTIExpr (normalizeTensorType tensorType) (TIFlipIndicesExpr tensorTI), s)
  
  -- Function expression (built-in function reference)
  IFunctionExpr names -> do
    -- Built-in function: return a generic function type
    -- TODO: Look up actual function signature
    argType <- freshVar "funcArg"
    resultType <- freshVar "funcResult"
    return (mkTIExpr (TFun argType resultType) (TIFunctionExpr names), emptySubst)

-- | Infer match clauses type
-- All clauses should return the same type
-- NEW: Returns TIMatchClause list in addition to type and subst
inferMatchClauses :: TypeErrorContext -> Type -> [IMatchClause] -> Subst -> Infer (Type, [TIMatchClause], Subst)
inferMatchClauses ctx matchedType clauses initSubst = do
  case clauses of
    [] -> do
      -- No clauses (should not happen)
      ty <- freshVar "clauseResult"
      return (ty, [], initSubst)
    (firstClause:restClauses) -> do
      -- Infer first clause
      (firstTI, firstType, s1) <- inferMatchClause ctx matchedType firstClause initSubst
      
      -- Infer rest clauses and unify with first
      (finalType, clauseTIs, finalSubst) <- foldM (inferAndUnifyClause ctx matchedType) (firstType, [firstTI], s1) restClauses
      return (finalType, reverse clauseTIs, finalSubst)
  where
    inferAndUnifyClause :: TypeErrorContext -> Type -> (Type, [TIMatchClause], Subst) -> IMatchClause -> Infer (Type, [TIMatchClause], Subst)
    inferAndUnifyClause ctx' matchedTy (expectedType, accClauses, accSubst) clause = do
      (clauseTI, clauseType, s1) <- inferMatchClause ctx' (applySubst accSubst matchedTy) clause accSubst
      s2 <- unifyTypesWithContext (applySubst s1 expectedType) clauseType ctx'
      let finalS = composeSubst s2 (composeSubst s1 accSubst)
      return (applySubst finalS expectedType, clauseTI : accClauses, finalS)

-- | Infer a single match clause
-- NEW: Returns TIMatchClause in addition to type and subst
inferMatchClause :: TypeErrorContext -> Type -> IMatchClause -> Subst -> Infer (TIMatchClause, Type, Subst)
inferMatchClause ctx matchedType (pattern, bodyExpr) initSubst = do
  -- Infer pattern type and extract pattern variable bindings
  -- Use pattern constructor and pattern function type information
  (tiPattern, bindings, s_pat) <- inferIPattern pattern matchedType ctx
  let s1 = composeSubst s_pat initSubst
  
  -- Convert bindings to TypeScheme format
  let schemes = [(var, Forall [] [] ty) | (var, ty) <- bindings]
  
  -- Infer body expression type with pattern variables in scope
  (bodyTI, s2) <- withEnv schemes $ inferIExprWithContext bodyExpr ctx
  let bodyType = tiExprType bodyTI
      finalS = composeSubst s2 s1
  return ((tiPattern, bodyTI), applySubst finalS bodyType, finalS)

-- | Infer multiple patterns left-to-right, making left bindings available to right patterns
-- This enables non-linear patterns like ($p, #(p + 1))
-- Returns (list of TIPattern, accumulated bindings, substitution)
inferPatternsLeftToRight :: [IPattern] -> [Type] -> [(String, Type)] -> Subst -> TypeErrorContext 
                         -> Infer ([TIPattern], [(String, Type)], Subst)
inferPatternsLeftToRight [] [] accBindings accSubst _ctx = 
  return ([], accBindings, accSubst)
inferPatternsLeftToRight (p:ps) (t:ts) accBindings accSubst ctx = do
  -- Add accumulated bindings to environment for this pattern
  let schemes = [(var, Forall [] [] ty) | (var, ty) <- accBindings]
  
  -- Infer this pattern with left bindings in scope
  (tipat, newBindings, s) <- withEnv schemes $ inferIPattern p (applySubst accSubst t) ctx
  
  -- Compose substitutions
  let accSubst' = composeSubst s accSubst
  
  -- Apply substitution to accumulated bindings
  let accBindings' = [(v, applySubst s ty) | (v, ty) <- accBindings] ++ newBindings
  
  -- Continue with remaining patterns
  (restTipats, finalBindings, finalSubst) <- inferPatternsLeftToRight ps ts accBindings' accSubst' ctx
  return (tipat : restTipats, finalBindings, finalSubst)
inferPatternsLeftToRight _ _ accBindings accSubst _ = 
  return ([], accBindings, accSubst)  -- Mismatched lengths

-- | Infer IPattern type and extract pattern variable bindings
-- Returns (TIPattern, bindings, substitution)
-- bindings: [(variable name, type)]
inferIPattern :: IPattern -> Type -> TypeErrorContext -> Infer (TIPattern, [(String, Type)], Subst)
inferIPattern pat expectedType ctx = case pat of
  IWildCard -> do
    -- Wildcard: no bindings
    let tipat = TIPattern (Forall [] [] expectedType) TIWildCard
    return (tipat, [], emptySubst)
  
  IPatVar name -> do
    -- Pattern variable: bind to expected type
    let tipat = TIPattern (Forall [] [] expectedType) (TIPatVar name)
    return (tipat, [(name, expectedType)], emptySubst)
  
  IValuePat expr -> do
    -- Value pattern: infer expression type and unify with expected type
    (exprTI, s) <- inferIExprWithContext expr ctx
    let exprType = tiExprType exprTI
    s' <- unifyTypesWithContext (applySubst s exprType) (applySubst s expectedType) ctx
    let finalS = composeSubst s' s
        exprTI' = applySubstToTIExpr finalS exprTI
        finalType = applySubst finalS expectedType
        tipat = TIPattern (Forall [] [] finalType) (TIValuePat exprTI')
    return (tipat, [], finalS)
  
  IPredPat expr -> do
    -- Predicate pattern: infer predicate expression
    -- Expected type for predicate is: expectedType -> Bool
    let predicateType = TFun expectedType TBool
    (exprTI, s) <- inferIExprWithContext expr ctx
    -- Unify with expected predicate type to concretize type variables
    s' <- unifyTypesWithContext (applySubst s (tiExprType exprTI)) (applySubst s predicateType) ctx
    let finalS = composeSubst s' s
        exprTI' = applySubstToTIExpr finalS exprTI
        finalType = applySubst finalS expectedType
        tipat = TIPattern (Forall [] [] finalType) (TIPredPat exprTI')
    return (tipat, [], finalS)
  
  ITuplePat pats -> do
    -- Tuple pattern: decompose expected type
    case expectedType of
      TTuple types | length types == length pats -> do
        -- Types match: infer each sub-pattern left-to-right
        -- Left patterns' bindings are available for right patterns (for non-linear patterns)
        (tipats, allBindings, s) <- inferPatternsLeftToRight pats types [] emptySubst ctx
        let finalType = applySubst s expectedType
            tipat = TIPattern (Forall [] [] finalType) (TITuplePat tipats)
        return (tipat, allBindings, s)
      
      TVar _ -> do
        -- Expected type is a type variable: create tuple type
        elemTypes <- mapM (\_ -> freshVar "elem") pats
        let tupleTy = TTuple elemTypes
        s <- unifyTypesWithContext expectedType tupleTy ctx
        
        -- Recursively infer each sub-pattern left-to-right
        let elemTypes' = map (applySubst s) elemTypes
        (tipats, allBindings, s') <- inferPatternsLeftToRight pats elemTypes' [] s ctx
        let finalType = applySubst s' expectedType
            tipat = TIPattern (Forall [] [] finalType) (TITuplePat tipats)
        return (tipat, allBindings, s')
      
      _ -> do
        -- Type mismatch
        throwError $ TE.TypeMismatch
          (TTuple (replicate (length pats) (TVar (TyVar "a"))))
          expectedType
          "Tuple pattern but matched type is not a tuple"
          ctx
  
  IInductivePat name pats -> do
    -- Inductive pattern: look up pattern constructor type from pattern environment
    patternEnv <- getPatternEnv
    case lookupPatternEnv name patternEnv of
      Just scheme -> do
        -- Found in pattern environment: use the declared type
        st <- get
        let (_constraints, ctorType, newCounter) = instantiate scheme (inferCounter st)
        modify $ \s -> s { inferCounter = newCounter }
        
        -- Pattern constructor type: arg1 -> arg2 -> ... -> resultType
        let (argTypes, resultType) = extractFunctionArgs ctorType
        
        -- Check argument count matches
        if length argTypes /= length pats
          then throwError $ TE.TypeMismatch
                 (foldr TFun resultType (replicate (length pats) (TVar (TyVar "a"))))
                 ctorType
                 ("Pattern constructor " ++ name ++ " expects " ++ show (length argTypes) 
                  ++ " arguments, but got " ++ show (length pats))
                 ctx
          else do
            -- Unify result type with expected type
            s0 <- unifyTypesWithContext resultType expectedType ctx
            let argTypes' = map (applySubst s0) argTypes
            
            -- Recursively infer each sub-pattern left-to-right
            -- Left patterns' bindings are available for right patterns
            (tipats, allBindings, s) <- inferPatternsLeftToRight pats argTypes' [] s0 ctx
            let finalType = applySubst s expectedType
                tipat = TIPattern (Forall [] [] finalType) (TIInductivePat name tipats)
            return (tipat, allBindings, s)
      
      Nothing -> do
        -- Not found in pattern environment: try data constructor from value environment
        -- This handles data constructors used as patterns
        env <- getEnv
        case lookupEnv name env of
          Just scheme -> do
            st <- get
            let (_constraints, ctorType, newCounter) = instantiate scheme (inferCounter st)
            modify $ \s -> s { inferCounter = newCounter }
            
            let (argTypes, resultType) = extractFunctionArgs ctorType
            
            if length argTypes /= length pats
              then throwError $ TE.TypeMismatch
                     (foldr TFun resultType (replicate (length pats) (TVar (TyVar "a"))))
                     ctorType
                     ("Constructor " ++ name ++ " expects " ++ show (length argTypes) 
                      ++ " arguments, but got " ++ show (length pats))
                     ctx
              else do
                s0 <- unifyTypesWithContext resultType expectedType ctx
                let argTypes' = map (applySubst s0) argTypes
                
                -- Recursively infer each sub-pattern left-to-right
                (tipats, allBindings, s) <- inferPatternsLeftToRight pats argTypes' [] s0 ctx
                let finalType = applySubst s expectedType
                    tipat = TIPattern (Forall [] [] finalType) (TIInductivePat name tipats)
                return (tipat, allBindings, s)
          
          Nothing -> do
            -- Not found: generic inference
            argTypes <- mapM (\_ -> freshVar "arg") pats
            let resultType = TInductive name argTypes
            
            s0 <- unifyTypesWithContext resultType expectedType ctx
            let argTypes' = map (applySubst s0) argTypes
            
            -- Recursively infer each sub-pattern left-to-right
            (tipats, allBindings, s) <- inferPatternsLeftToRight pats argTypes' [] s0 ctx
            let finalType = applySubst s expectedType
                tipat = TIPattern (Forall [] [] finalType) (TIInductivePat name tipats)
            return (tipat, allBindings, s)
  
  IIndexedPat p indices -> do
    -- Indexed pattern: infer base pattern and index expressions
    -- For $x_i pattern, x should have type Hash keyType expectedType
    -- where expectedType is the type of the indexed result
    
    -- First, infer the index expressions to determine their types
    indexTypes <- mapM (\_ -> freshVar "idx") indices
    (indexTIs, s1) <- foldM (\(accTIs, accS) (idx, idxType) -> do
      (idxTI, idxS) <- inferIExprWithContext idx ctx
      let actualIdxType = tiExprType idxTI
      s' <- unifyTypesWithContext (applySubst idxS actualIdxType) (applySubst idxS idxType) ctx
      let finalS = composeSubst s' (composeSubst idxS accS)
      return (accTIs ++ [idxTI], finalS)) ([], emptySubst) (zip indices indexTypes)
    
    -- Construct the base type: Hash indexType expectedType
    -- For simplicity, assume single index access and use THash
    let indexType = case indexTypes of
                      [t] -> applySubst s1 t
                      _ -> TInt  -- Multiple indices: fallback to Int
        baseType = THash indexType expectedType
    
    -- Infer base pattern with Hash type
    (tipat, bindings, s2) <- inferIPattern p (applySubst s1 baseType) ctx
    
    let finalS = composeSubst s2 s1
        finalType = applySubst finalS expectedType
        tiIndexedPat = TIPattern (Forall [] [] finalType) (TIIndexedPat tipat indexTIs)
    return (tiIndexedPat, bindings, finalS)
  
  ILetPat bindings p -> do
    -- Let pattern: infer bindings and then the pattern
    -- Infer bindings first
    env <- getEnv
    (bindingTIs, bindingSchemes, s1) <- inferIBindingsWithContext bindings env emptySubst ctx
    
    -- Infer pattern with bindings in scope
    (tipat, patBindings, s2) <- withEnv bindingSchemes $ inferIPattern p (applySubst s1 expectedType) ctx
    
    let s = composeSubst s2 s1
        finalType = applySubst s expectedType
        tiLetPat = TIPattern (Forall [] [] finalType) (TILetPat bindingTIs tipat)
    -- Let bindings are not exported, only pattern bindings
    return (tiLetPat, patBindings, s)
  
  INotPat p -> do
    -- Not pattern: infer the sub-pattern but don't use its bindings
    (tipat, _, s) <- inferIPattern p expectedType ctx
    let finalType = applySubst s expectedType
        tiNotPat = TIPattern (Forall [] [] finalType) (TINotPat tipat)
    return (tiNotPat, [], s)
  
  IAndPat p1 p2 -> do
    -- And pattern: both patterns must match the same type
    -- Left bindings should be available to right pattern
    (tipat1, bindings1, s1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    (tipat2, bindings2, s2) <- withEnv schemes1 $ inferIPattern p2 (applySubst s1 expectedType) ctx
    let s = composeSubst s2 s1
        -- Apply substitution to left bindings
        bindings1' = [(v, applySubst s2 ty) | (v, ty) <- bindings1]
        finalType = applySubst s expectedType
        tiAndPat = TIPattern (Forall [] [] finalType) (TIAndPat tipat1 tipat2)
    return (tiAndPat, bindings1' ++ bindings2, s)
  
  IOrPat p1 p2 -> do
    -- Or pattern: both patterns must match the same type
    -- Left bindings should be available to right pattern for non-linear patterns
    (tipat1, bindings1, s1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    (tipat2, bindings2, s2) <- withEnv schemes1 $ inferIPattern p2 (applySubst s1 expectedType) ctx
    let s = composeSubst s2 s1
        -- Apply substitution to left bindings
        bindings1' = [(v, applySubst s2 ty) | (v, ty) <- bindings1]
        finalType = applySubst s expectedType
        tiOrPat = TIPattern (Forall [] [] finalType) (TIOrPat tipat1 tipat2)
    -- For or patterns, ideally both branches should have same variables
    -- For now, we take union of bindings
    return (tiOrPat, bindings1' ++ bindings2, s)
  
  IForallPat p1 p2 -> do
    -- Forall pattern: similar to and pattern
    -- Left bindings should be available to right pattern
    (tipat1, bindings1, s1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    (tipat2, bindings2, s2) <- withEnv schemes1 $ inferIPattern p2 (applySubst s1 expectedType) ctx
    let s = composeSubst s2 s1
        -- Apply substitution to left bindings
        bindings1' = [(v, applySubst s2 ty) | (v, ty) <- bindings1]
        finalType = applySubst s expectedType
        tiForallPat = TIPattern (Forall [] [] finalType) (TIForallPat tipat1 tipat2)
    return (tiForallPat, bindings1' ++ bindings2, s)
  
  ILoopPat var range p1 p2 -> do
    -- Loop pattern: $var is the loop variable (Integer), range contains pattern
    -- First, infer the range pattern (third element of ILoopRange)
    let ILoopRange startExpr endExpr rangePattern = range
    (tiRangePat, rangeBindings, s_range) <- inferIPattern rangePattern TInt ctx
    
    -- Infer start and end expressions
    (startTI, s_start) <- inferIExprWithContext startExpr ctx
    (endTI, s_end) <- inferIExprWithContext endExpr ctx
    let tiLoopRange = TILoopRange startTI endTI tiRangePat
    
    -- Add loop variable binding (always Integer for loop index)
    let loopVarBinding = (var, TInt)
        initialBindings = loopVarBinding : rangeBindings
        schemes0 = [(v, Forall [] [] ty) | (v, ty) <- initialBindings]
        s_combined = foldr composeSubst emptySubst [s_end, s_start, s_range]
    
    -- Infer p1 with loop variable and range bindings in scope
    (tipat1, bindings1, s1) <- withEnv schemes0 $ inferIPattern p1 (applySubst s_combined expectedType) ctx
    
    -- Infer p2 with all previous bindings in scope
    let allPrevBindings = [(v, applySubst s1 ty) | (v, ty) <- initialBindings] ++ bindings1
        schemes1 = [(v, Forall [] [] ty) | (v, ty) <- allPrevBindings]
    (tipat2, bindings2, s2) <- withEnv schemes1 $ inferIPattern p2 (applySubst s1 expectedType) ctx
    
    let s = foldr composeSubst emptySubst [s2, s1, s_combined]
        -- Apply final substitution to all bindings
        finalBindings = [(v, applySubst s ty) | (v, ty) <- loopVarBinding : rangeBindings ++ bindings1 ++ bindings2]
        finalType = applySubst s expectedType
        tiLoopPat = TIPattern (Forall [] [] finalType) (TILoopPat var tiLoopRange tipat1 tipat2)
    
    return (tiLoopPat, finalBindings, s)
  
  IContPat -> do
    -- Continuation pattern: no bindings
    let tipat = TIPattern (Forall [] [] expectedType) TIContPat
    return (tipat, [], emptySubst)
  
  IPApplyPat funcExpr argPats -> do
    -- Pattern application: infer pattern function type
    (funcTI, s1) <- inferIExprWithContext funcExpr ctx
    
    -- Pattern function should return a pattern that matches expectedType
    -- Infer argument patterns left-to-right with fresh types
    argTypes <- mapM (\_ -> freshVar "parg") argPats
    (tipats, allBindings, s2) <- inferPatternsLeftToRight argPats argTypes [] s1 ctx
    
    let finalType = applySubst s2 expectedType
        tipat = TIPattern (Forall [] [] finalType) (TIPApplyPat funcTI tipats)
    return (tipat, allBindings, s2)
  
  IVarPat name -> do
    -- Variable pattern (with ~): bind to expected type
    let tipat = TIPattern (Forall [] [] expectedType) (TIVarPat name)
    return (tipat, [(name, expectedType)], emptySubst)
  
  IInductiveOrPApplyPat name pats -> do
    -- Could be either inductive pattern or pattern application
    -- Try inductive pattern first
    (tipat, bindings, s) <- inferIPattern (IInductivePat name pats) expectedType ctx
    -- Wrap it as InductiveOrPApplyPat
    let TIPattern scheme (TIInductivePat _ tipats) = tipat
        tiInductiveOrPApplyPat = TIPattern scheme (TIInductiveOrPApplyPat name tipats)
    return (tiInductiveOrPApplyPat, bindings, s)
  
  ISeqNilPat -> do
    -- Sequence nil: no bindings
    let tipat = TIPattern (Forall [] [] expectedType) TISeqNilPat
    return (tipat, [], emptySubst)
  
  ISeqConsPat p1 p2 -> do
    -- Sequence cons: infer both patterns
    -- Left bindings should be available to right pattern
    (tipat1, bindings1, s1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    (tipat2, bindings2, s2) <- withEnv schemes1 $ inferIPattern p2 (applySubst s1 expectedType) ctx
    let s = composeSubst s2 s1
        -- Apply substitution to left bindings
        bindings1' = [(v, applySubst s2 ty) | (v, ty) <- bindings1]
        finalType = applySubst s expectedType
        tipat = TIPattern (Forall [] [] finalType) (TISeqConsPat tipat1 tipat2)
    return (tipat, bindings1' ++ bindings2, s)
  
  ILaterPatVar -> do
    -- Later pattern variable: no immediate binding
    let tipat = TIPattern (Forall [] [] expectedType) TILaterPatVar
    return (tipat, [], emptySubst)
  
  IDApplyPat p pats -> do
    -- D-apply pattern: infer base pattern and argument patterns
    -- Base pattern bindings should be available to argument patterns
    (tipat, bindings1, s1) <- inferIPattern p expectedType ctx
    
    -- Infer argument patterns left-to-right with base pattern bindings in scope
    argTypes <- mapM (\_ -> freshVar "darg") pats
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    (tipats, argBindings, s2) <- withEnv schemes1 $ inferPatternsLeftToRight pats argTypes [] s1 ctx
    
    let s = composeSubst s2 s1
        -- Apply substitution to base bindings
        bindings1' = [(v, applySubst s2 ty) | (v, ty) <- bindings1]
        finalType = applySubst s expectedType
        tiDApplyPat = TIPattern (Forall [] [] finalType) (TIDApplyPat tipat tipats)
    return (tiDApplyPat, bindings1' ++ argBindings, s)
  where
    -- Extract function argument types and result type
    -- e.g., a -> b -> c -> d  =>  ([a, b, c], d)
    extractFunctionArgs :: Type -> ([Type], Type)
    extractFunctionArgs (TFun arg rest) = 
      let (args, result) = extractFunctionArgs rest
      in (arg : args, result)
    extractFunctionArgs t = ([], t)

-- | Infer application (helper)
-- NEW: Returns TIExpr instead of (IExpr, Type, Subst)
inferIApplication :: String -> Type -> [IExpr] -> Subst -> Infer (TIExpr, Subst)
inferIApplication funcName funcType args initSubst = do
  let funcTI = mkTIExpr funcType (TIVarExpr funcName)
  inferIApplicationWithContext funcTI funcType args initSubst emptyContext

-- TensorMap insertion logic has been moved to Language.Egison.Type.TensorMapInsertion
-- This keeps type inference focused on type checking only

-- | Infer application (helper) with context
-- NEW: Returns TIExpr instead of (IExpr, Type, Subst)
-- TensorMap insertion has been moved to Phase 8 (TensorMapInsertion module)
-- This function now only performs type inference and unification
inferIApplicationWithContext :: TIExpr -> Type -> [IExpr] -> Subst -> TypeErrorContext -> Infer (TIExpr, Subst)
inferIApplicationWithContext funcTIExpr funcType args initSubst ctx = do
  -- Infer argument types
  argResults <- mapM (\arg -> inferIExprWithContext arg ctx) args
  let argTIExprs = map fst argResults
      argTypes = map (tiExprType . fst) argResults
      argSubst = foldr composeSubst initSubst (map snd argResults)
  
  -- Normal function application (no tensorMap insertion)
  resultType <- freshVar "result"
  let expectedFuncType = foldr TFun resultType argTypes
  
  case unify (applySubst argSubst funcType) expectedFuncType of
    Right s -> do
      -- Unification succeeded
      let finalS = composeSubst s argSubst
          finalType = applySubst finalS resultType
          -- Include constraints from function's type scheme
          funcScheme = tiScheme funcTIExpr
          (Forall _tvs constraints _) = funcScheme
      -- Resolve constraints based on available instances
      -- If constraint type is Tensor T and no instance exists for Tensor T,
      -- use instance for T instead (tensorMap will be inserted later)
      classEnv <- getClassEnv
      let updatedConstraints = map (resolveConstraintWithInstances classEnv finalS) constraints
          -- Create result with updated constraints
          resultScheme = Forall [] updatedConstraints finalType
          -- Apply substitution to all subexpressions to keep type information accurate
          updatedFuncTI = applySubstToTIExpr finalS funcTIExpr
          updatedArgTIs = map (applySubstToTIExpr finalS) argTIExprs
      return (TIExpr resultScheme (TIApplyExpr updatedFuncTI updatedArgTIs), finalS)
    
    Left _ -> do
      -- Unification failed
      throwError $ UnificationError (applySubst argSubst funcType) expectedFuncType ctx

-- | Infer let bindings (non-recursive)

-- | Infer let bindings (non-recursive) with context
-- NEW: Returns TIBindingExpr instead of IBindingExpr
-- Infer IO bindings for do expressions
inferIOBindingsWithContext :: [IBindingExpr] -> TypeEnv -> Subst -> TypeErrorContext -> Infer ([TIBindingExpr], [(String, TypeScheme)], Subst)
inferIOBindingsWithContext [] _env s _ctx = return ([], [], s)
inferIOBindingsWithContext ((pat, expr):bs) env s ctx = do
  -- Infer the type of the expression
  (exprTI, s1) <- inferIExprWithContext expr ctx
  let exprType = tiExprType exprTI

  -- The expression should be of type IO a
  innerType <- freshVar "ioInner"
  s2 <- unifyTypesWithContext (applySubst s1 exprType) (TIO innerType) ctx
  let s12 = composeSubst s2 s1
      actualInnerType = applySubst s12 innerType

  -- Create expected type from pattern and unify with inner type
  (patternType, s3) <- inferPatternType pat
  let s123 = composeSubst s3 s12
  s4 <- unifyTypesWithContext (applySubst s123 actualInnerType) (applySubst s123 patternType) ctx

  -- Apply all substitutions and extract bindings with inner type
  let finalS = composeSubst s4 s123
      finalInnerType = applySubst finalS actualInnerType
      bindings = extractIBindingsFromPattern pat finalInnerType
      s' = composeSubst finalS s

  _env' <- getEnv
  let extendedEnvList = bindings  -- Already a list of (String, TypeScheme)
  (restBindingTIs, restBindings, s2') <- withEnv extendedEnvList $ inferIOBindingsWithContext bs env s' ctx
  return ((pat, exprTI) : restBindingTIs, bindings ++ restBindings, s2')
  where
    -- Infer the type that a pattern expects
    inferPatternType :: IPrimitiveDataPattern -> Infer (Type, Subst)
    inferPatternType PDWildCard = do
      t <- freshVar "wild"
      return (t, emptySubst)
    inferPatternType (PDPatVar _) = do
      t <- freshVar "patvar"
      return (t, emptySubst)
    inferPatternType (PDTuplePat pats) = do
      results <- mapM inferPatternType pats
      let types = map fst results
          substs = map snd results
          s = foldr composeSubst emptySubst substs
      return (TTuple types, s)
    inferPatternType PDEmptyPat = return (TCollection (TVar (TyVar "a")), emptySubst)
    inferPatternType (PDConsPat _ _) = do
      elemType <- freshVar "elem"
      return (TCollection elemType, emptySubst)
    inferPatternType (PDSnocPat _ _) = do
      elemType <- freshVar "elem"
      return (TCollection elemType, emptySubst)
    inferPatternType (PDInductivePat name pats) = do
      results <- mapM inferPatternType pats
      let types = map fst results
          substs = map snd results
          s = foldr composeSubst emptySubst substs
      return (TInductive name types, s)
    inferPatternType (PDConstantPat c) = do
      ty <- inferConstant c
      return (ty, emptySubst)
    -- ScalarData primitive patterns
    inferPatternType (PDDivPat _ _) = return (TMathExpr, emptySubst)
    inferPatternType (PDPlusPat _) = return (TPolyExpr, emptySubst)
    inferPatternType (PDTermPat _ _) = return (TTermExpr, emptySubst)
    inferPatternType (PDSymbolPat _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply1Pat _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply2Pat _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply3Pat _ _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply4Pat _ _ _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDQuotePat _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDFunctionPat _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDSubPat _) = return (TIndexExpr, emptySubst)
    inferPatternType (PDSupPat _) = return (TIndexExpr, emptySubst)
    inferPatternType (PDUserPat _) = return (TIndexExpr, emptySubst)

inferIBindingsWithContext :: [IBindingExpr] -> TypeEnv -> Subst -> TypeErrorContext -> Infer ([TIBindingExpr], [(String, TypeScheme)], Subst)
inferIBindingsWithContext [] _env s _ctx = return ([], [], s)
inferIBindingsWithContext ((pat, expr):bs) env s ctx = do
  -- Infer the type of the expression
  (exprTI, s1) <- inferIExprWithContext expr ctx
  let exprType = tiExprType exprTI

  -- Create expected type from pattern and unify with expression type
  -- This helps resolve type variables in the expression type
  (patternType, s2) <- inferPatternType pat
  let s12 = composeSubst s2 s1
  s3 <- unifyTypesWithContext (applySubst s12 exprType) (applySubst s12 patternType) ctx

  -- Apply all substitutions and extract bindings
  let finalS = composeSubst s3 s12
      finalExprType = applySubst finalS exprType
      bindings = extractIBindingsFromPattern pat finalExprType
      s' = composeSubst finalS s

  _env' <- getEnv
  let extendedEnvList = bindings  -- Already a list of (String, TypeScheme)
  (restBindingTIs, restBindings, s2') <- withEnv extendedEnvList $ inferIBindingsWithContext bs env s' ctx
  return ((pat, exprTI) : restBindingTIs, bindings ++ restBindings, s2')
  where
    -- Infer the type that a pattern expects
    inferPatternType :: IPrimitiveDataPattern -> Infer (Type, Subst)
    inferPatternType PDWildCard = do
      t <- freshVar "wild"
      return (t, emptySubst)
    inferPatternType (PDPatVar _) = do
      t <- freshVar "patvar"
      return (t, emptySubst)
    inferPatternType (PDTuplePat pats) = do
      results <- mapM inferPatternType pats
      let types = map fst results
          substs = map snd results
          s = foldr composeSubst emptySubst substs
      return (TTuple types, s)
    inferPatternType PDEmptyPat = return (TCollection (TVar (TyVar "a")), emptySubst)
    inferPatternType (PDConsPat _ _) = do
      elemType <- freshVar "elem"
      return (TCollection elemType, emptySubst)
    inferPatternType (PDSnocPat _ _) = do
      elemType <- freshVar "elem"
      return (TCollection elemType, emptySubst)
    inferPatternType (PDInductivePat name pats) = do
      results <- mapM inferPatternType pats
      let types = map fst results
          substs = map snd results
          s = foldr composeSubst emptySubst substs
      return (TInductive name types, s)
    inferPatternType (PDConstantPat c) = do
      ty <- inferConstant c
      return (ty, emptySubst)
    -- ScalarData primitive patterns
    inferPatternType (PDDivPat _ _) = return (TMathExpr, emptySubst)
    inferPatternType (PDPlusPat _) = return (TPolyExpr, emptySubst)
    inferPatternType (PDTermPat _ _) = return (TTermExpr, emptySubst)
    inferPatternType (PDSymbolPat _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply1Pat _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply2Pat _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply3Pat _ _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply4Pat _ _ _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDQuotePat _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDFunctionPat _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDSubPat _) = return (TIndexExpr, emptySubst)
    inferPatternType (PDSupPat _) = return (TIndexExpr, emptySubst)
    inferPatternType (PDUserPat _) = return (TIndexExpr, emptySubst)

-- | Infer letrec bindings (recursive)

-- | Infer letrec bindings (recursive) with context
-- NEW: Returns TIBindingExpr instead of IBindingExpr
inferIRecBindingsWithContext :: [IBindingExpr] -> TypeEnv -> Subst -> TypeErrorContext -> Infer ([TIBindingExpr], [(String, TypeScheme)], Subst)
inferIRecBindingsWithContext bindings _env s ctx = do
  -- Create placeholders with fresh type variables
  placeholders <- mapM (\(pat, _) -> do
    ty <- freshVar "rec"
    return (pat, ty)) bindings
  
  -- Extract bindings from placeholders
  let placeholderBindings = concatMap (\(pat, ty) -> extractIBindingsFromPattern pat ty) placeholders
  
  -- Infer expressions in extended environment
  results <- withEnv placeholderBindings $ mapM (\(_, expr) -> inferIExprWithContext expr ctx) bindings
  
  let exprTIs = map fst results
      exprTypes = map (tiExprType . fst) results
      substList = map snd results
      finalS = foldr composeSubst s substList
  
  -- Re-extract bindings with inferred types
  let finalBindings = concat $ zipWith (\(pat, _) ty -> extractIBindingsFromPattern pat (applySubst finalS ty)) bindings exprTypes
      transformedBindings = zipWith (\(pat, _) exprTI -> (pat, exprTI)) bindings exprTIs
  
  return (transformedBindings, finalBindings, finalS)

-- | Extract bindings from pattern
-- This function extracts variable bindings from a primitive data pattern
-- given the type that the pattern should match against
-- Helper to check if a pattern is a pattern variable
isPatVarPat :: IPrimitiveDataPattern -> Bool
isPatVarPat (PDPatVar _) = True
isPatVarPat _ = False

extractIBindingsFromPattern :: IPrimitiveDataPattern -> Type -> [(String, TypeScheme)]
extractIBindingsFromPattern pat ty = case pat of
  PDWildCard -> []
  PDPatVar var -> [(extractNameFromVar var, Forall [] [] ty)]
  PDInductivePat _ pats -> concatMap (\p -> extractIBindingsFromPattern p ty) pats
  PDTuplePat pats -> 
    case ty of
      TTuple tys | length pats == length tys -> 
        -- Types match: bind each pattern variable to corresponding type
        concat $ zipWith extractIBindingsFromPattern pats tys
      _ -> 
        -- Type is not a resolved tuple (might be type variable or mismatch)
        -- Extract pattern variables but assign them the full tuple type for now
        -- This is imprecise but allows variables to be in scope
        -- The actual element types will be determined during later unification
        concatMap (\p -> extractIBindingsFromPattern p ty) pats
  PDEmptyPat -> []
  PDConsPat p1 p2 ->
    case ty of
      TCollection elemTy -> extractIBindingsFromPattern p1 elemTy ++ extractIBindingsFromPattern p2 ty
      _ -> []
  PDSnocPat p1 p2 ->
    case ty of
      TCollection elemTy -> extractIBindingsFromPattern p1 ty ++ extractIBindingsFromPattern p2 elemTy
      _ -> []
  -- ScalarData primitive patterns
  PDDivPat p1 p2 ->
    let polyExprTy = TPolyExpr
        mathExprTy = TMathExpr
        p1Ty = if isPatVarPat p1 then mathExprTy else polyExprTy
        p2Ty = if isPatVarPat p2 then mathExprTy else polyExprTy
    in extractIBindingsFromPattern p1 p1Ty ++ extractIBindingsFromPattern p2 p2Ty
  PDPlusPat p ->
    let termExprTy = TTermExpr
        mathExprTy = TMathExpr
        pTy = if isPatVarPat p then TCollection mathExprTy else TCollection termExprTy
    in extractIBindingsFromPattern p pTy
  PDTermPat p1 p2 ->
    let symbolExprTy = TSymbolExpr
        mathExprTy = TMathExpr
        p2Ty = if isPatVarPat p2
               then TCollection (TTuple [mathExprTy, TInt])
               else TCollection (TTuple [symbolExprTy, TInt])
    in extractIBindingsFromPattern p1 TInt ++ extractIBindingsFromPattern p2 p2Ty
  PDSymbolPat p1 p2 ->
    let indexExprTy = TIndexExpr
    in extractIBindingsFromPattern p1 TString ++ extractIBindingsFromPattern p2 (TCollection indexExprTy)
  PDApply1Pat p1 p2 ->
    let mathExprTy = TMathExpr
        fnTy = TFun mathExprTy mathExprTy
    in extractIBindingsFromPattern p1 fnTy ++ extractIBindingsFromPattern p2 mathExprTy
  PDApply2Pat p1 p2 p3 ->
    let mathExprTy = TMathExpr
        fnTy = TFun mathExprTy (TFun mathExprTy mathExprTy)
    in extractIBindingsFromPattern p1 fnTy ++ extractIBindingsFromPattern p2 mathExprTy ++ extractIBindingsFromPattern p3 mathExprTy
  PDApply3Pat p1 p2 p3 p4 ->
    let mathExprTy = TMathExpr
        fnTy = TFun mathExprTy (TFun mathExprTy (TFun mathExprTy mathExprTy))
    in extractIBindingsFromPattern p1 fnTy ++ extractIBindingsFromPattern p2 mathExprTy ++ extractIBindingsFromPattern p3 mathExprTy ++ extractIBindingsFromPattern p4 mathExprTy
  PDApply4Pat p1 p2 p3 p4 p5 ->
    let mathExprTy = TMathExpr
        fnTy = TFun mathExprTy (TFun mathExprTy (TFun mathExprTy (TFun mathExprTy mathExprTy)))
    in extractIBindingsFromPattern p1 fnTy ++ extractIBindingsFromPattern p2 mathExprTy ++ extractIBindingsFromPattern p3 mathExprTy ++ extractIBindingsFromPattern p4 mathExprTy ++ extractIBindingsFromPattern p5 mathExprTy
  PDQuotePat p ->
    let mathExprTy = TMathExpr
    in extractIBindingsFromPattern p mathExprTy
  PDFunctionPat p1 p2 p3 ->
    let mathExprTy = TMathExpr
    in extractIBindingsFromPattern p1 mathExprTy ++ extractIBindingsFromPattern p2 (TCollection mathExprTy) ++ extractIBindingsFromPattern p3 (TCollection mathExprTy)
  PDSubPat p ->
    let mathExprTy = TMathExpr
    in extractIBindingsFromPattern p mathExprTy
  PDSupPat p ->
    let mathExprTy = TMathExpr
    in extractIBindingsFromPattern p mathExprTy
  PDUserPat p ->
    let mathExprTy = TMathExpr
    in extractIBindingsFromPattern p mathExprTy
  _ -> []

-- | Infer top-level IExpr and return TITopExpr directly
inferITopExpr :: ITopExpr -> Infer (Maybe TITopExpr, Subst)
inferITopExpr topExpr = case topExpr of
  IDefine var expr -> do
    varName <- return $ extractNameFromVar var
    env <- getEnv
    -- Check if there's an explicit type signature in the environment
    -- (added by EnvBuilder from DefineWithType)
    case lookupEnv varName env of
      Just existingScheme -> do
        -- There's an explicit type signature: check that the inferred type matches
        st <- get
        let (instConstraints, expectedType, newCounter) = instantiate existingScheme (inferCounter st)
        modify $ \s -> s { inferCounter = newCounter }
        
        -- Infer the expression type
        (exprTI, subst1) <- inferIExpr expr
        let exprType = tiExprType exprTI
        
        -- Unify inferred type with expected type
        -- At top-level definitions, Tensor a can unify with a
        let exprCtx = withExpr (prettyStr expr) emptyContext
        subst2 <- unifyTypesWithTopLevel (applySubst subst1 exprType) (applySubst subst1 expectedType) exprCtx
        let finalSubst = composeSubst subst2 subst1
        
        -- Apply final substitution to exprTI to resolve all type variables
        let exprTI' = applySubstToTIExpr finalSubst exprTI
        
        -- Resolve constraints in exprTI' (Tensor t0 -> t0)
        classEnv <- getClassEnv
        let exprTI'' = resolveConstraintsInTIExpr classEnv finalSubst exprTI'
        
        -- Reconstruct type scheme from exprTI'' to match actual type variables
        -- Use instantiated constraints and apply final substitution
        let finalType = tiExprType exprTI''
            constraints' = map (applySubstConstraint finalSubst) instConstraints
            envFreeVars = freeVarsInEnv env
            typeFreeVars = freeTyVars finalType
            genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
            updatedScheme = Forall genVars constraints' finalType
        
        -- Keep the updated scheme (with actual type variables) in the environment
        return (Just (TIDefine updatedScheme var exprTI''), finalSubst)
      
      Nothing -> do
        -- No explicit type signature: infer and generalize as before
        clearConstraints  -- Start with fresh constraints for this expression
        (exprTI, subst) <- inferIExpr expr
        let exprType = tiExprType exprTI
        constraints <- getConstraints  -- Collect constraints from type inference
        
        -- Resolve constraints based on available instances
        classEnv <- getClassEnv
        let updatedConstraints = map (resolveConstraintWithInstances classEnv subst) constraints
        
        -- Generalize with updated constraints
        let envFreeVars = freeVarsInEnv env
            typeFreeVars = freeTyVars exprType
            genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
            scheme = Forall genVars updatedConstraints exprType
        
        -- Add to environment
        modify $ \s -> s { inferEnv = extendEnv varName scheme (inferEnv s) }
        
        return (Just (TIDefine scheme var exprTI), subst)
  
  ITest expr -> do
    clearConstraints  -- Start with fresh constraints
    (exprTI, subst) <- inferIExpr expr
    -- Constraints are now in state, will be retrieved by Eval.hs
    return (Just (TITest exprTI), subst)
  
  IExecute expr -> do
    clearConstraints  -- Start with fresh constraints
    (exprTI, subst) <- inferIExpr expr
    -- Constraints are now in state, will be retrieved by Eval.hs
    return (Just (TIExecute exprTI), subst)
  
  ILoadFile _path -> return (Nothing, emptySubst)
  ILoad _lib -> return (Nothing, emptySubst)

  IDefineMany bindings -> do
    -- Process each binding in the list
    env <- getEnv
    results <- mapM (inferBinding env) bindings
    let bindingsTI = map fst results
        substs = map snd results
        combinedSubst = foldr composeSubst emptySubst substs
    return (Just (TIDefineMany bindingsTI), combinedSubst)
    where
      inferBinding env (var, expr) = do
        let varName = extractNameFromVar var
        -- Check if there's an existing type signature
        case lookupEnv varName env of
          Just existingScheme -> do
            -- With type signature: check type
            st <- get
            let (_, expectedType, newCounter) = instantiate existingScheme (inferCounter st)
            modify $ \s -> s { inferCounter = newCounter }
            
            clearConstraints
            (exprTI, subst1) <- inferIExpr expr
            let exprType = tiExprType exprTI
            subst2 <- unifyTypesWithTopLevel (applySubst subst1 exprType) (applySubst subst1 expectedType) emptyContext
            let finalSubst = composeSubst subst2 subst1
                exprTI' = applySubstToTIExpr finalSubst exprTI
            return ((var, exprTI'), finalSubst)
          
          Nothing -> do
            -- Without type signature: infer and generalize
            clearConstraints
            (exprTI, subst) <- inferIExpr expr
            let exprType = tiExprType exprTI
            constraints <- getConstraints
            
            -- Resolve constraints based on available instances
            classEnv <- getClassEnv
            let updatedConstraints = map (resolveConstraintWithInstances classEnv subst) constraints
            
            -- Generalize the type
            let envFreeVars = freeVarsInEnv env
                typeFreeVars = freeTyVars exprType
                genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
                scheme = Forall genVars updatedConstraints exprType
            
            -- Add to environment for subsequent bindings
            modify $ \s -> s { inferEnv = extendEnv varName scheme (inferEnv s) }
            
            return ((var, exprTI), subst)
  
  IDeclareSymbol names mType -> do
    -- Register declared symbols with their types
    let ty = case mType of
               Just t  -> t
               Nothing -> TInt  -- Default to Integer (MathExpr)
    -- Add symbols to declared symbols map
    modify $ \s -> s { declaredSymbols = 
                        foldr (\name m -> Map.insert name ty m) 
                              (declaredSymbols s) 
                              names }
    -- Also add to type environment so they can be used in subsequent expressions
    let scheme = Forall [] [] ty
    modify $ \s -> s { inferEnv = 
                        foldr (\name e -> extendEnv name scheme e) 
                              (inferEnv s) 
                              names }
    -- Return the typed declaration
    return (Just (TIDeclareSymbol names ty), emptySubst)

-- | Infer multiple top-level IExprs
inferITopExprs :: [ITopExpr] -> Infer ([Maybe TITopExpr], Subst)
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
    Right (tiExpr, subst) -> Right (tiExprType tiExpr, subst, warnings)

-- | Run type inference on IExpr with initial environment
runInferIWithEnv :: InferConfig -> TypeEnv -> IExpr -> IO (Either TypeError (Type, Subst, TypeEnv, [TypeWarning]))
runInferIWithEnv cfg env expr = do
  let initState = (initialInferStateWithConfig cfg) { inferEnv = env }
  (result, warnings, finalState) <- runInferWithWarningsAndState (inferIExpr expr) initState
  return $ case result of
    Left err -> Left err
    Right (tiExpr, subst) -> Right (tiExprType tiExpr, subst, inferEnv finalState, warnings)
