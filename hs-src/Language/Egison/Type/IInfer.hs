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

import           Control.Monad              (foldM)
import           Control.Monad.Except       (ExceptT, runExceptT, throwError)
import           Control.Monad.State.Strict (StateT, evalStateT, runStateT, get, modify, put)
import           Data.Maybe                  (catMaybes)
import           Language.Egison.AST        (ConstantExpr (..), PrimitivePatPattern (..), PMMode (..))
import           Language.Egison.IExpr      (IExpr (..), ITopExpr (..)
                                            , IBindingExpr
                                            , IMatchClause, IPatternDef
                                            , IPattern (..), ILoopRange (..)
                                            , IPrimitiveDataPattern, PDPatternBase (..)
                                            , extractNameFromVar)
import           Language.Egison.Pretty     (prettyStr)
import           Language.Egison.Type.Env
import qualified Language.Egison.Type.Error as TE
import           Language.Egison.Type.Error (TypeError(..), TypeErrorContext(..), TypeWarning(..),
                                              emptyContext, withExpr, withContext)
import           Language.Egison.Type.Subst
import           Language.Egison.Type.Types
import           Language.Egison.Type.Unify as TU

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
unifyTypes t1 t2 = unifyTypesWithContext t1 t2 emptyContext

-- | Unify two types with context information
unifyTypesWithContext :: Type -> Type -> TypeErrorContext -> Infer Subst
unifyTypesWithContext t1 t2 ctx = case unify t1 t2 of
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

-- | Infer type for IExpr
inferIExpr :: IExpr -> Infer (Type, Subst)
inferIExpr expr = inferIExprWithContext expr emptyContext

-- | Infer type for IExpr with context information
inferIExprWithContext :: IExpr -> TypeErrorContext -> Infer (Type, Subst)
inferIExprWithContext expr ctx = case expr of
  -- Constants
  IConstantExpr c -> do
    ty <- inferConstant c
    return (ty, emptySubst)
  
  -- Variables
  IVarExpr name -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    ty <- lookupVar name
    return (ty, emptySubst)
  
  -- Tuples
  ITupleExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    results <- mapM (\e -> inferIExprWithContext e exprCtx) elems
    let elemTypes = map fst results
        s = foldr composeSubst emptySubst (map snd results)
    
    -- Check if all elements are Matcher types
    -- If so, return Matcher (Tuple ...) instead of (Matcher ..., Matcher ...)
    let appliedElemTypes = map (applySubst s) elemTypes
        matcherTypes = catMaybes (map extractMatcherType appliedElemTypes)
    
    if length matcherTypes == length appliedElemTypes && not (null appliedElemTypes)
      then do
        -- All elements are matchers: return Matcher (Tuple ...)
        let tupleType = TTuple matcherTypes
        return (TMatcher tupleType, s)
      else
        -- Not all elements are matchers: return regular tuple
        return (TTuple appliedElemTypes, s)
    where
      -- Extract the inner type from Matcher a -> Just a, otherwise Nothing
      extractMatcherType :: Type -> Maybe Type
      extractMatcherType (TMatcher t) = Just t
      extractMatcherType _ = Nothing
  
  -- Collections (Lists)
  ICollectionExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    elemType <- freshVar "elem"
    s <- foldM (inferListElem elemType exprCtx) emptySubst elems
    return (TCollection (applySubst s elemType), s)
    where
      inferListElem eType exprCtx s e = do
        (t, s') <- inferIExprWithContext e exprCtx
        s'' <- unifyTypesWithContext (applySubst s eType) t exprCtx
        return $ composeSubst s'' (composeSubst s' s)
  
  -- Cons
  IConsExpr headExpr tailExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (headType, s1) <- inferIExprWithContext headExpr exprCtx
    (tailType, s2) <- inferIExprWithContext tailExpr exprCtx
    let s12 = composeSubst s2 s1
    s3 <- unifyTypesWithContext (TCollection (applySubst s12 headType)) (applySubst s12 tailType) exprCtx
    let finalS = composeSubst s3 s12
    return (applySubst finalS tailType, finalS)
  
  -- Join (list concatenation)
  IJoinExpr leftExpr rightExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (leftType, s1) <- inferIExprWithContext leftExpr exprCtx
    (rightType, s2) <- inferIExprWithContext rightExpr exprCtx
    let s12 = composeSubst s2 s1
    s3 <- unifyTypesWithContext (applySubst s12 leftType) (applySubst s12 rightType) exprCtx
    let finalS = composeSubst s3 s12
    return (applySubst finalS leftType, finalS)
  
  -- Hash (Map)
  IHashExpr pairs -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    keyType <- freshVar "hashKey"
    valType <- freshVar "hashVal"
    s <- foldM (inferHashPair keyType valType exprCtx) emptySubst pairs
    return (THash (applySubst s keyType) (applySubst s valType), s)
    where
      inferHashPair kType vType exprCtx s' (k, v) = do
        (kt, s1) <- inferIExprWithContext k exprCtx
        (vt, s2) <- inferIExprWithContext v exprCtx
        s3 <- unifyTypesWithContext (applySubst (composeSubst s2 s1) kType) kt exprCtx
        s4 <- unifyTypesWithContext (applySubst (composeSubst s3 (composeSubst s2 s1)) vType) vt exprCtx
        return $ foldr composeSubst s' [s4, s3, s2, s1]
  
  -- Vector (Tensor)
  IVectorExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    elemType <- freshVar "vecElem"
    s <- foldM (inferListElem elemType exprCtx) emptySubst elems
    return (TTensor (applySubst s elemType), s)
    where
      inferListElem eType exprCtx s e = do
        (t, s') <- inferIExprWithContext e exprCtx
        s'' <- unifyTypesWithContext (applySubst s eType) t exprCtx
        return $ composeSubst s'' (composeSubst s' s)
  
  -- Lambda
  ILambdaExpr _mVar params body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argTypes <- mapM (\_ -> freshVar "arg") params
    let bindings = zipWith makeBinding params argTypes
    (bodyType, s) <- withEnv (map toScheme bindings) $ inferIExprWithContext body exprCtx
    let finalArgTypes = map (applySubst s) argTypes
        funType = foldr TFun bodyType finalArgTypes
    return (funType, s)
    where
      makeBinding var t = (extractNameFromVar var, t)
      toScheme (name, t) = (name, Forall [] [] t)
  
  -- Function Application
  IApplyExpr func args -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (funcType, s1) <- inferIExprWithContext func exprCtx
    inferIApplicationWithContext funcType args s1 exprCtx
  
  -- If expression
  IIfExpr cond thenExpr elseExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (condType, s1) <- inferIExprWithContext cond exprCtx
    s2 <- unifyTypesWithContext condType TBool exprCtx
    let s12 = composeSubst s2 s1
    (thenType, s3) <- inferIExprWithContext thenExpr exprCtx
    (elseType, s4) <- inferIExprWithContext elseExpr exprCtx
    s5 <- unifyTypesWithContext (applySubst s4 thenType) elseType exprCtx
    let finalS = foldr composeSubst emptySubst [s5, s4, s3, s12]
    return (applySubst finalS elseType, finalS)
  
  -- Let expression
  ILetExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    env <- getEnv
    (extendedEnv, s1) <- inferIBindingsWithContext bindings env emptySubst exprCtx
    (bodyType, s2) <- withEnv extendedEnv $ inferIExprWithContext body exprCtx
    let finalS = composeSubst s2 s1
    return (applySubst finalS bodyType, finalS)
  
  -- LetRec expression
  ILetRecExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    env <- getEnv
    (extendedEnv, s1) <- inferIRecBindingsWithContext bindings env emptySubst exprCtx
    (bodyType, s2) <- withEnv extendedEnv $ inferIExprWithContext body exprCtx
    let finalS = composeSubst s2 s1
    return (applySubst finalS bodyType, finalS)
  
  -- Sequence expression
  ISeqExpr expr1 expr2 -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (_, s1) <- inferIExprWithContext expr1 exprCtx
    (t2, s2) <- inferIExprWithContext expr2 exprCtx
    return (t2, composeSubst s2 s1)
  
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
        inferIApplication constructorType args emptySubst
      Nothing -> do
        -- Constructor not found in environment
        let exprCtx = withExpr (prettyStr expr) ctx
        permissive <- isPermissive
        if permissive
          then do
            -- In permissive mode, treat as a warning and return a fresh type variable
            addWarning $ UnboundVariableWarning name exprCtx
            resultType <- freshVar "ctor"
            return (resultType, emptySubst)
          else throwError $ UnboundVariable name exprCtx
  
  -- Matchers (return Matcher type)
  IMatcherExpr patDefs -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    -- Infer type of each pattern definition (matcher clause)
    -- Each clause has: (PrimitivePatPattern, nextMatcherExpr, [(primitiveDataPat, targetExpr)])
    results <- mapM (inferPatternDef exprCtx) patDefs
    
    -- Collect all substitutions
    let substs = concatMap snd results
        finalSubst = foldr composeSubst emptySubst substs
    
    -- All clauses should agree on the matched type
    -- For now, we use the first clause's matched type
    matchedTy <- case results of
      ((ty, _):_) -> return $ applySubst finalSubst ty
      [] -> freshVar "matched"  -- Empty matcher (edge case)
    
    return (TMatcher matchedTy, finalSubst)
    where
      -- Infer a single pattern definition (matcher clause)
      -- Returns (matched type, [substitutions])
      inferPatternDef :: TypeErrorContext -> IPatternDef -> Infer (Type, [Subst])
      inferPatternDef ctx (ppPat, nextMatcherExpr, dataClauses) = do
        -- Infer the type of next matcher expression
        -- It should be either a Matcher type or a tuple of Matcher types
        (nextMatcherType, s1) <- inferIExprWithContext nextMatcherExpr ctx
        
        -- If next matcher type contains type variables, constrain them to be Matcher types
        -- This prevents infinite type errors when using matcher parameters
        s1' <- constrainTypeVarsAsMatcher nextMatcherType s1
        let nextMatcherType' = applySubst s1' nextMatcherType
        
        -- Count the number of pattern holes in the primitive-pattern pattern
        let numPatternHoles = countPatternHoles ppPat
        
        -- Extract next matcher types (should match number of pattern holes)
        nextMatcherTypes <- extractNextMatcherTypes numPatternHoles nextMatcherType'
        
        -- Extract the matched type from the matcher clause
        -- For primitive-pattern pattern ($, $), we need to infer from next matcher
        matchedType <- inferMatchedTypeFromPP ppPat nextMatcherType'
        
        -- Extract variables from primitive-pattern pattern (e.g., #$val)
        -- These variables are bound to the matched type
        let ppPatternVars = extractPPPatternVars ppPat
            ppBindings = [(var, Forall [] [] matchedType) | var <- ppPatternVars]
        
        -- Infer the type of data clauses with pp variables in scope
        -- Each data clause: (primitiveDataPattern, targetListExpr)
        dataClauseResults <- withEnv ppBindings $ 
          mapM (inferDataClauseWithCheck ctx numPatternHoles nextMatcherTypes) dataClauses
        let s2 = foldr composeSubst emptySubst dataClauseResults
        
        return (matchedType, [s1', s2])
      
      -- Constrain type variables in next matcher expression to be Matcher types
      -- If we have a type variable t0 appearing in matcher context, unify t0 with Matcher t_new
      constrainTypeVarsAsMatcher :: Type -> Subst -> Infer Subst
      constrainTypeVarsAsMatcher ty s = case ty of
        TVar v -> do
          -- Type variable in matcher position: constrain to Matcher type
          innerTy <- freshVar "inner"
          s' <- unifyTypes (applySubst s (TVar v)) (TMatcher innerTy)
          return $ composeSubst s' s
        TTuple tys -> do
          -- Tuple of matchers: constrain each element
          foldM (\accS t -> constrainTypeVarsAsMatcher t accS) s tys
        TMatcher _ -> return s  -- Already a Matcher type, no constraint needed
        _ -> return s  -- Other types: no constraint needed
      
      -- Count pattern holes ($) in primitive-pattern pattern
      countPatternHoles :: PrimitivePatPattern -> Int
      countPatternHoles ppPat = case ppPat of
        PPWildCard -> 0
        PPPatVar -> 1
        PPValuePat _ -> 0
        PPTuplePat pps -> sum (map countPatternHoles pps)
        PPInductivePat _ pps -> sum (map countPatternHoles pps)
      
      -- Extract variables from primitive-pattern pattern
      -- For example, #$val in PPValuePat extracts "val"
      extractPPPatternVars :: PrimitivePatPattern -> [String]
      extractPPPatternVars ppPat = case ppPat of
        PPWildCard -> []
        PPPatVar -> []
        PPValuePat varName -> [varName]  -- Extract variable from value pattern
        PPTuplePat pps -> concatMap extractPPPatternVars pps
        PPInductivePat _ pps -> concatMap extractPPPatternVars pps
      
      -- Extract next matcher types from the next matcher type
      -- If n=1, returns [TMatcher a] for TMatcher a
      -- If n>1, returns [TMatcher a, TMatcher b, ...] for (TMatcher a, TMatcher b, ...) or Matcher (a, b, ...)
      extractNextMatcherTypes :: Int -> Type -> Infer [Type]
      extractNextMatcherTypes n matcherType
        | n == 0 = return []
        | n == 1 = return [matcherType]
        | otherwise = case matcherType of
            TTuple types -> 
              if length types == n
                then return types
                else throwError $ TE.TypeMismatch 
                       (TTuple (replicate n (TVar (TyVar "a"))))  -- Expected
                       matcherType  -- Actual
                       ("Expected " ++ show n ++ " matchers, but got " ++ show (length types))
                       emptyContext
            -- Special case: Matcher (a, b, ...) can be expanded to (Matcher a, Matcher b, ...)
            TMatcher (TTuple innerTypes) ->
              if length innerTypes == n
                then return (map TMatcher innerTypes)
                else throwError $ TE.TypeMismatch
                       (TMatcher (TTuple (replicate n (TVar (TyVar "a")))))  -- Expected
                       matcherType  -- Actual
                       ("Expected matcher for tuple of " ++ show n ++ " elements, but got " ++ show (length innerTypes))
                       emptyContext
            _ -> throwError $ TE.TypeMismatch
                   (TTuple (replicate n (TVar (TyVar "a"))))  -- Expected
                   matcherType  -- Actual
                   ("Expected tuple of " ++ show n ++ " matchers")
                   emptyContext
      
      -- Infer a data clause with type checking
      -- Check that the target expression returns a list of values with types matching next matchers
      inferDataClauseWithCheck :: TypeErrorContext -> Int -> [Type] -> (IPrimitiveDataPattern, IExpr) -> Infer Subst
      inferDataClauseWithCheck ctx numHoles nextMatcherTypes (pdPat, targetExpr) = do
        -- Extract expected element type from next matchers (the target type)
        targetType <- case numHoles of
          0 -> return TUnit  -- No pattern holes, should match ()
          1 -> case nextMatcherTypes of
                 [TMatcher innerType] -> return innerType
                 [ty] -> return ty  -- May be 'something' which is polymorphic
                 _ -> freshVar "target"
          _ -> case nextMatcherTypes of
                 types -> do
                   -- Extract inner types from Matcher types
                   innerTypes <- mapM extractMatcherInner types
                   return (TTuple innerTypes)
        
        -- Extract variable bindings from primitive data pattern
        -- These variables are bound to parts of the target type
        let bindings = extractPDPatternBindings pdPat targetType
        
        -- Infer the target expression with pattern variables in scope
        (exprType, s1) <- withEnv bindings $ inferIExprWithContext targetExpr ctx
        
        -- Unify with actual expression type
        -- Expected: [targetType] or [(targetType)]
        let expectedType = TCollection (applySubst s1 targetType)
        
        s2 <- unifyTypesWithContext (applySubst s1 exprType) expectedType ctx
        return $ composeSubst s2 s1
        where
          extractMatcherInner :: Type -> Infer Type
          extractMatcherInner (TMatcher t) = return t
          extractMatcherInner t = return t
      
      -- Extract variable bindings from primitive data pattern with target type
      extractPDPatternBindings :: IPrimitiveDataPattern -> Type -> [(String, TypeScheme)]
      extractPDPatternBindings pdPat targetType = case pdPat of
        PDWildCard -> []
        PDPatVar var -> [(extractNameFromVar var, Forall [] [] targetType)]
        PDInductivePat _ pats -> 
          -- For inductive patterns, we'd need to look up constructor types
          -- For now, bind each sub-pattern to the whole target type
          concatMap (\p -> extractPDPatternBindings p targetType) pats
        PDTuplePat pats -> 
          case targetType of
            TTuple types | length types == length pats ->
              concat $ zipWith extractPDPatternBindings pats types
            _ -> 
              -- Type mismatch, but let bindings be inferred as target type
              concatMap (\p -> extractPDPatternBindings p targetType) pats
        PDEmptyPat -> []
        PDConsPat p1 p2 ->
          case targetType of
            TCollection elemType ->
              extractPDPatternBindings p1 elemType ++ extractPDPatternBindings p2 targetType
            _ -> []
        PDSnocPat p1 p2 ->
          case targetType of
            TCollection elemType ->
              extractPDPatternBindings p1 targetType ++ extractPDPatternBindings p2 elemType
            _ -> []
        PDConstantPat _ -> []
      
      -- Infer matched type from primitive-pattern pattern and next matcher type
      inferMatchedTypeFromPP :: PrimitivePatPattern -> Type -> Infer Type
      inferMatchedTypeFromPP ppPat matcherType = case ppPat of
        PPWildCard -> freshVar "matched"
        PPPatVar -> freshVar "matched"
        PPValuePat _ -> freshVar "matched"
        PPTuplePat pps -> do
          -- For tuple patterns, extract types from matcher types
          case matcherType of
            TTuple matcherTypes -> do
              -- Extract inner types from each Matcher type
              innerTypes <- mapM extractMatcherInnerType matcherTypes
              return $ TTuple innerTypes
            TMatcher (TTuple ts) -> return $ TTuple ts
            _ -> freshVar "matched"
        PPInductivePat name pps -> do
          -- For inductive patterns, return the inductive type
          argTypes <- mapM (\_ -> freshVar "arg") pps
          return $ TInductive name argTypes
      
      -- Extract inner type from Matcher a -> a
      extractMatcherInnerType :: Type -> Infer Type
      extractMatcherInnerType (TMatcher t) = return t
      extractMatcherInnerType t = return t
  
  -- Match expressions (pattern matching)
  IMatchExpr _mode target matcher clauses -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (targetType, s1) <- inferIExprWithContext target exprCtx
    (matcherType, s2) <- inferIExprWithContext matcher exprCtx
    
    -- Matcher should be TMatcher a, and target should be a
    matchedTy <- freshVar "matched"
    let s12 = composeSubst s2 s1
    s3 <- unifyTypesWithContext (applySubst s12 matcherType) (TMatcher matchedTy) exprCtx
    
    let s123 = composeSubst s3 s12
    s4 <- unifyTypesWithContext (applySubst s123 targetType) (applySubst s123 matchedTy) exprCtx
    
    -- Infer match clauses result type
    let s1234 = composeSubst s4 s123
    case clauses of
      [] -> do
        -- No clauses: this should not happen, but handle gracefully
        resultTy <- freshVar "matchResult"
        return (applySubst s1234 resultTy, s1234)
      _ -> do
        -- Infer type of each clause and unify them
        (resultTy, clauseSubst) <- inferMatchClauses exprCtx (applySubst s1234 matchedTy) clauses s1234
        let finalS = composeSubst clauseSubst s1234
        return (applySubst finalS resultTy, finalS)
  
  -- MatchAll expressions
  IMatchAllExpr _mode target matcher clauses -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (targetType, s1) <- inferIExprWithContext target exprCtx
    (matcherType, s2) <- inferIExprWithContext matcher exprCtx
    
    matchedTy <- freshVar "matched"
    let s12 = composeSubst s2 s1
    s3 <- unifyTypesWithContext (applySubst s12 matcherType) (TMatcher matchedTy) exprCtx
    
    let s123 = composeSubst s3 s12
    s4 <- unifyTypesWithContext (applySubst s123 targetType) (applySubst s123 matchedTy) exprCtx
    
    -- MatchAll returns a collection of results from match clauses
    let s1234 = composeSubst s4 s123
    case clauses of
      [] -> do
        -- No clauses: return empty collection type
        resultElemTy <- freshVar "matchAllElem"
        return (TCollection (applySubst s1234 resultElemTy), s1234)
      _ -> do
        -- Infer type of each clause (they should all have the same type)
        (resultElemTy, clauseSubst) <- inferMatchClauses exprCtx (applySubst s1234 matchedTy) clauses s1234
        let finalS = composeSubst clauseSubst s1234
        return (TCollection (applySubst finalS resultElemTy), finalS)
  
  -- Memoized Lambda
  IMemoizedLambdaExpr args body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argTypes <- mapM (\_ -> freshVar "memoArg") args
    let bindings = zip args argTypes  -- [(String, Type)]
        schemes = map (\(name, t) -> (name, Forall [] [] t)) bindings
    (bodyType, s) <- withEnv schemes $ inferIExprWithContext body exprCtx
    let finalArgTypes = map (applySubst s) argTypes
        funType = foldr TFun bodyType finalArgTypes
    return (funType, s)
  
  -- Do expression
  IDoExpr _bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    -- TODO: Properly handle IO monad bindings
    (bodyType, s) <- inferIExprWithContext body exprCtx
    return (TIO bodyType, s)
  
  -- Cambda (pattern matching lambda)
  ICambdaExpr _var body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argType <- freshVar "cambdaArg"
    (bodyType, s) <- inferIExprWithContext body exprCtx
    return (TFun argType bodyType, s)
  
  -- With symbols
  IWithSymbolsExpr _syms body -> inferIExprWithContext body ctx
  
  -- Quote expressions (symbolic math)
  IQuoteExpr _ -> return (TInt, emptySubst)
  IQuoteSymbolExpr _ -> return (TInt, emptySubst)
  
  -- Other cases: return TAny for now
  _ -> return (TAny, emptySubst)

-- | Infer match clauses type
-- All clauses should return the same type
inferMatchClauses :: TypeErrorContext -> Type -> [IMatchClause] -> Subst -> Infer (Type, Subst)
inferMatchClauses ctx matchedType clauses initSubst = do
  case clauses of
    [] -> do
      -- No clauses (should not happen)
      ty <- freshVar "clauseResult"
      return (ty, initSubst)
    (firstClause:restClauses) -> do
      -- Infer first clause
      (firstType, s1) <- inferMatchClause ctx matchedType firstClause initSubst
      
      -- Infer rest clauses and unify with first
      (finalType, finalSubst) <- foldM (inferAndUnifyClause ctx matchedType) (firstType, s1) restClauses
      return (finalType, finalSubst)
  where
    inferAndUnifyClause :: TypeErrorContext -> Type -> (Type, Subst) -> IMatchClause -> Infer (Type, Subst)
    inferAndUnifyClause ctx' matchedTy (expectedType, accSubst) clause = do
      (clauseType, s1) <- inferMatchClause ctx' (applySubst accSubst matchedTy) clause accSubst
      s2 <- unifyTypesWithContext (applySubst s1 expectedType) clauseType ctx'
      let finalS = composeSubst s2 (composeSubst s1 accSubst)
      return (applySubst finalS expectedType, finalS)

-- | Infer a single match clause
inferMatchClause :: TypeErrorContext -> Type -> IMatchClause -> Subst -> Infer (Type, Subst)
inferMatchClause ctx matchedType (pattern, bodyExpr) initSubst = do
  -- Extract pattern variable bindings from the pattern
  -- All pattern variables will have the matched type for now
  -- (A more sophisticated implementation would track types through pattern decomposition)
  let patternVars = extractPatternVars pattern
      bindings = [(var, Forall [] [] matchedType) | var <- patternVars]
  
  -- Infer body expression type with pattern variables in scope
  (bodyType, s) <- withEnv bindings $ inferIExprWithContext bodyExpr ctx
  let finalS = composeSubst s initSubst
  return (applySubst finalS bodyType, finalS)

-- | Extract pattern variables from a pattern
extractPatternVars :: IPattern -> [String]
extractPatternVars pat = case pat of
  IWildCard -> []
  IPatVar name -> [name]
  IValuePat _ -> []
  IPredPat _ -> []
  IIndexedPat p _ -> extractPatternVars p
  ILetPat _ p -> extractPatternVars p
  INotPat p -> extractPatternVars p
  IAndPat p1 p2 -> extractPatternVars p1 ++ extractPatternVars p2
  IOrPat p1 p2 -> extractPatternVars p1 ++ extractPatternVars p2
  IForallPat p1 p2 -> extractPatternVars p1 ++ extractPatternVars p2
  ITuplePat ps -> concatMap extractPatternVars ps
  IInductivePat _ ps -> concatMap extractPatternVars ps
  ILoopPat _ _ p1 p2 -> extractPatternVars p1 ++ extractPatternVars p2
  IContPat -> []
  IPApplyPat _ ps -> concatMap extractPatternVars ps
  IVarPat name -> [name]
  IInductiveOrPApplyPat _ ps -> concatMap extractPatternVars ps
  ISeqNilPat -> []
  ISeqConsPat p1 p2 -> extractPatternVars p1 ++ extractPatternVars p2
  ILaterPatVar -> []
  IDApplyPat p ps -> extractPatternVars p ++ concatMap extractPatternVars ps

-- | Infer application (helper)
inferIApplication :: Type -> [IExpr] -> Subst -> Infer (Type, Subst)
inferIApplication funcType args initSubst = inferIApplicationWithContext funcType args initSubst emptyContext

-- | Infer application (helper) with context
inferIApplicationWithContext :: Type -> [IExpr] -> Subst -> TypeErrorContext -> Infer (Type, Subst)
inferIApplicationWithContext funcType args initSubst ctx = do
  -- Infer argument types
  argResults <- mapM (\arg -> inferIExprWithContext arg ctx) args
  let argTypes = map fst argResults
      argSubst = foldr composeSubst initSubst (map snd argResults)
  
  -- Create expected function type
  resultType <- freshVar "result"
  let expectedFuncType = foldr TFun resultType argTypes
  
  -- Unify
  s <- unifyTypesWithContext (applySubst argSubst funcType) expectedFuncType ctx
  let finalS = composeSubst s argSubst
  return (applySubst finalS resultType, finalS)

-- | Infer let bindings (non-recursive)
inferIBindings :: [IBindingExpr] -> TypeEnv -> Subst -> Infer ([(String, TypeScheme)], Subst)
inferIBindings bindings env s = inferIBindingsWithContext bindings env s emptyContext

-- | Infer let bindings (non-recursive) with context
inferIBindingsWithContext :: [IBindingExpr] -> TypeEnv -> Subst -> TypeErrorContext -> Infer ([(String, TypeScheme)], Subst)
inferIBindingsWithContext [] _env s _ctx = return ([], s)
inferIBindingsWithContext ((pat, expr):bs) env s ctx = do
  -- Infer the type of the expression
  (exprType, s1) <- inferIExprWithContext expr ctx
  
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
  (restBindings, s2') <- withEnv extendedEnvList $ inferIBindingsWithContext bs env s' ctx
  return (bindings ++ restBindings, s2')
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

-- | Infer letrec bindings (recursive)
inferIRecBindings :: [IBindingExpr] -> TypeEnv -> Subst -> Infer ([(String, TypeScheme)], Subst)
inferIRecBindings bindings env s = inferIRecBindingsWithContext bindings env s emptyContext

-- | Infer letrec bindings (recursive) with context
inferIRecBindingsWithContext :: [IBindingExpr] -> TypeEnv -> Subst -> TypeErrorContext -> Infer ([(String, TypeScheme)], Subst)
inferIRecBindingsWithContext bindings _env s ctx = do
  -- Create placeholders with fresh type variables
  placeholders <- mapM (\(pat, _) -> do
    ty <- freshVar "rec"
    return (pat, ty)) bindings
  
  -- Extract bindings from placeholders
  let placeholderBindings = concatMap (\(pat, ty) -> extractIBindingsFromPattern pat ty) placeholders
  
  -- Infer expressions in extended environment
  results <- withEnv placeholderBindings $ mapM (\(_, expr) -> inferIExprWithContext expr ctx) bindings
  
  let exprTypes = map fst results
      substList = map snd results
      finalS = foldr composeSubst s substList
  
  -- Re-extract bindings with inferred types
  let finalBindings = concat $ zipWith (\(pat, _) ty -> extractIBindingsFromPattern pat (applySubst finalS ty)) bindings exprTypes
  
  return (finalBindings, finalS)

-- | Extract bindings from pattern
-- This function extracts variable bindings from a primitive data pattern
-- given the type that the pattern should match against
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
        -- Type is not a matching tuple: this might be a type variable
        -- In this case, we cannot extract precise types, so return empty
        -- The type inference will handle this later
        []
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

-- | Infer top-level IExpr and return (ITopExpr, Type) instead of TypedITopExpr
-- The typed AST (TITopExpr) will be created in a separate phase.
inferITopExpr :: ITopExpr -> Infer (Maybe (ITopExpr, Type), Subst)
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
        let (_constraints, expectedType, newCounter) = instantiate existingScheme (inferCounter st)
        modify $ \s -> s { inferCounter = newCounter }
        
        -- Infer the expression type
        (exprType, subst1) <- inferIExpr expr
        
        -- Unify inferred type with expected type
        subst2 <- unifyTypes (applySubst subst1 exprType) (applySubst subst1 expectedType)
        let finalSubst = composeSubst subst2 subst1
        
        -- For display purposes, use the original type from the scheme (before instantiation)
        -- This preserves the original type variable names (a, b, c) instead of fresh ones (t0, t1, t2)
        -- Extract the type from the scheme and apply the substitution
        let (Forall _ _ originalType) = existingScheme
        let finalType = applySubst finalSubst originalType
        
        -- Keep the existing scheme (with explicit type signature) in the environment
        -- Don't override it with the generalized inferred type
        return (Just (topExpr, finalType), finalSubst)
      
      Nothing -> do
        -- No explicit type signature: infer and generalize as before
        (exprType, subst) <- inferIExpr expr
        let scheme = generalize env exprType
        -- Add to environment
        modify $ \s -> s { inferEnv = extendEnv varName scheme (inferEnv s) }
        
        let finalType = applySubst subst exprType
        return (Just (topExpr, finalType), subst)
  
  ITest expr -> do
    (exprType, subst) <- inferIExpr expr
    return (Just (topExpr, exprType), subst)
  
  IExecute expr -> do
    (exprType, subst) <- inferIExpr expr
    return (Just (topExpr, exprType), subst)
  
  ILoadFile _path -> return (Nothing, emptySubst)
  ILoad _lib -> return (Nothing, emptySubst)
  
  _ -> return (Nothing, emptySubst)

-- | Infer multiple top-level IExprs
inferITopExprs :: [ITopExpr] -> Infer ([Maybe (ITopExpr, Type)], Subst)
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
