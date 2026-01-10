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
import           Control.Monad.State.Strict (StateT, evalStateT, runStateT, get, gets, modify, put)
import           Data.Maybe                  (catMaybes)
import qualified Data.Set                    as Set
import           Language.Egison.AST        (ConstantExpr (..), PrimitivePatPattern (..), PMMode (..))
import           Language.Egison.IExpr      (IExpr (..), ITopExpr (..)
                                            , IBindingExpr
                                            , IMatchClause, IPatternDef
                                            , IPattern (..), ILoopRange (..)
                                            , IPrimitiveDataPattern, PDPatternBase (..)
                                            , extractNameFromVar, Var (..))
import           Language.Egison.Pretty     (prettyStr)
import           Language.Egison.Type.Env
import qualified Language.Egison.Type.Error as TE
import           Language.Egison.Type.Error (TypeError(..), TypeErrorContext(..), TypeWarning(..),
                                              emptyContext, withExpr, withContext)
import           Language.Egison.Type.Subst
import           Language.Egison.Type.Tensor (normalizeTensorType)
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
  { inferCounter     :: Int              -- ^ Fresh variable counter
  , inferEnv         :: TypeEnv          -- ^ Current type environment
  , inferWarnings    :: [TypeWarning]    -- ^ Collected warnings
  , inferConfig      :: InferConfig      -- ^ Configuration
  , inferClassEnv    :: ClassEnv         -- ^ Type class environment
  , inferPatternEnv  :: PatternTypeEnv   -- ^ Pattern constructor environment
  , inferConstraints :: [Constraint]     -- ^ Accumulated type class constraints
  } deriving (Show)

-- | Initial inference state
initialInferState :: InferState
initialInferState = InferState 0 emptyEnv [] defaultInferConfig emptyClassEnv emptyPatternEnv []

-- | Create initial state with config
initialInferStateWithConfig :: InferConfig -> InferState
initialInferStateWithConfig cfg = InferState 0 emptyEnv [] cfg emptyClassEnv emptyPatternEnv []

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

-- | Infer type for IExpr
-- Returns (transformed expr, type, substitution)
-- The transformed expr may have tensorMap inserted for automatic mapping
inferIExpr :: IExpr -> Infer (IExpr, Type, Subst)
inferIExpr expr = inferIExprWithContext expr emptyContext

-- | Infer type for IExpr with context information
-- Returns (transformed expr, type, substitution)
inferIExprWithContext :: IExpr -> TypeErrorContext -> Infer (IExpr, Type, Subst)
inferIExprWithContext expr ctx = case expr of
  -- Constants
  IConstantExpr c -> do
    ty <- inferConstant c
    return (expr, ty, emptySubst)
  
  -- Variables
  IVarExpr name -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    ty <- lookupVar name
    return (expr, ty, emptySubst)
  
  -- Tuples
  ITupleExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    case elems of
      [] -> do
        -- Empty tuple: unit type ()
        return (expr, TTuple [], emptySubst)
      _ -> do
        results <- mapM (\e -> inferIExprWithContext e exprCtx) elems
        let elemExprs = map (\(e, _, _) -> e) results
            elemTypes = map (\(_, t, _) -> t) results
            s = foldr composeSubst emptySubst (map (\(_, _, s') -> s') results)
        
        -- Check if all elements are Matcher types
        -- If so, return Matcher (Tuple ...) instead of (Matcher ..., Matcher ...)
        let appliedElemTypes = map (applySubst s) elemTypes
            matcherTypes = catMaybes (map extractMatcherType appliedElemTypes)
        
        if length matcherTypes == length appliedElemTypes && not (null appliedElemTypes)
          then do
            -- All elements are matchers: return Matcher (Tuple ...)
            let tupleType = TTuple matcherTypes
            return (ITupleExpr elemExprs, TMatcher tupleType, s)
          else
            -- Not all elements are matchers: return regular tuple
            return (ITupleExpr elemExprs, TTuple appliedElemTypes, s)
        where
          -- Extract the inner type from Matcher a -> Just a, otherwise Nothing
          extractMatcherType :: Type -> Maybe Type
          extractMatcherType (TMatcher t) = Just t
          extractMatcherType _ = Nothing
  
  -- Collections (Lists)
  ICollectionExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    elemType <- freshVar "elem"
    (elemExprs, s) <- foldM (inferListElem elemType exprCtx) ([], emptySubst) elems
    return (ICollectionExpr (reverse elemExprs), TCollection (applySubst s elemType), s)
    where
      inferListElem eType exprCtx (accExprs, s) e = do
        (e', t, s') <- inferIExprWithContext e exprCtx
        s'' <- unifyTypesWithContext (applySubst s eType) t exprCtx
        return (e' : accExprs, composeSubst s'' (composeSubst s' s))
  
  -- Cons
  IConsExpr headExpr tailExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (headExpr', headType, s1) <- inferIExprWithContext headExpr exprCtx
    (tailExpr', tailType, s2) <- inferIExprWithContext tailExpr exprCtx
    let s12 = composeSubst s2 s1
    s3 <- unifyTypesWithContext (TCollection (applySubst s12 headType)) (applySubst s12 tailType) exprCtx
    let finalS = composeSubst s3 s12
    return (IConsExpr headExpr' tailExpr', applySubst finalS tailType, finalS)
  
  -- Join (list concatenation)
  IJoinExpr leftExpr rightExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (leftExpr', leftType, s1) <- inferIExprWithContext leftExpr exprCtx
    (rightExpr', rightType, s2) <- inferIExprWithContext rightExpr exprCtx
    let s12 = composeSubst s2 s1
    s3 <- unifyTypesWithContext (applySubst s12 leftType) (applySubst s12 rightType) exprCtx
    let finalS = composeSubst s3 s12
    return (IJoinExpr leftExpr' rightExpr', applySubst finalS leftType, finalS)
  
  -- Hash (Map)
  IHashExpr pairs -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    keyType <- freshVar "hashKey"
    valType <- freshVar "hashVal"
    (pairs', s) <- foldM (inferHashPair keyType valType exprCtx) ([], emptySubst) pairs
    return (IHashExpr (reverse pairs'), THash (applySubst s keyType) (applySubst s valType), s)
    where
      inferHashPair kType vType exprCtx (accPairs, s') (k, v) = do
        (k', kt, s1) <- inferIExprWithContext k exprCtx
        (v', vt, s2) <- inferIExprWithContext v exprCtx
        s3 <- unifyTypesWithContext (applySubst (composeSubst s2 s1) kType) kt exprCtx
        s4 <- unifyTypesWithContext (applySubst (composeSubst s3 (composeSubst s2 s1)) vType) vt exprCtx
        return ((k', v') : accPairs, foldr composeSubst s' [s4, s3, s2, s1])
  
  -- Vector (Tensor)
  IVectorExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    elemType <- freshVar "vecElem"
    (elemExprs, s) <- foldM (inferListElem elemType exprCtx) ([], emptySubst) elems
    return (IVectorExpr (reverse elemExprs), normalizeTensorType (TTensor (applySubst s elemType)), s)
    where
      inferListElem eType exprCtx (accExprs, s) e = do
        (e', t, s') <- inferIExprWithContext e exprCtx
        s'' <- unifyTypesWithContext (applySubst s eType) t exprCtx
        return (e' : accExprs, composeSubst s'' (composeSubst s' s))
  
  -- Lambda
  ILambdaExpr mVar params body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argTypes <- mapM (\_ -> freshVar "arg") params
    let bindings = zipWith makeBinding params argTypes
    (body', bodyType, s) <- withEnv (map toScheme bindings) $ inferIExprWithContext body exprCtx
    let finalArgTypes = map (applySubst s) argTypes
        funType = foldr TFun bodyType finalArgTypes
    return (ILambdaExpr mVar params body', funType, s)
    where
      makeBinding var t = (extractNameFromVar var, t)
      toScheme (name, t) = (name, Forall [] [] t)
  
  -- Function Application
  IApplyExpr func args -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (func', funcType, s1) <- inferIExprWithContext func exprCtx
    inferIApplicationWithContext func' funcType args s1 exprCtx
  
  -- If expression
  IIfExpr cond thenExpr elseExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (cond', condType, s1) <- inferIExprWithContext cond exprCtx
    s2 <- unifyTypesWithContext condType TBool exprCtx
    let s12 = composeSubst s2 s1
    (thenExpr', thenType, s3) <- inferIExprWithContext thenExpr exprCtx
    (elseExpr', elseType, s4) <- inferIExprWithContext elseExpr exprCtx
    s5 <- unifyTypesWithContext (applySubst s4 thenType) elseType exprCtx
    let finalS = foldr composeSubst emptySubst [s5, s4, s3, s12]
    return (IIfExpr cond' thenExpr' elseExpr', applySubst finalS elseType, finalS)
  
  -- Let expression
  ILetExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    env <- getEnv
    (bindings', extendedEnv, s1) <- inferIBindingsWithContext bindings env emptySubst exprCtx
    (body', bodyType, s2) <- withEnv extendedEnv $ inferIExprWithContext body exprCtx
    let finalS = composeSubst s2 s1
    return (ILetExpr bindings' body', applySubst finalS bodyType, finalS)
  
  -- LetRec expression
  ILetRecExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    env <- getEnv
    (bindings', extendedEnv, s1) <- inferIRecBindingsWithContext bindings env emptySubst exprCtx
    (body', bodyType, s2) <- withEnv extendedEnv $ inferIExprWithContext body exprCtx
    let finalS = composeSubst s2 s1
    return (ILetRecExpr bindings' body', applySubst finalS bodyType, finalS)
  
  -- Sequence expression
  ISeqExpr expr1 expr2 -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (expr1', _, s1) <- inferIExprWithContext expr1 exprCtx
    (expr2', t2, s2) <- inferIExprWithContext expr2 exprCtx
    return (ISeqExpr expr1' expr2', t2, composeSubst s2 s1)
  
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
            return (expr, resultType, emptySubst)
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
    
    return (expr, TMatcher matchedTy, finalSubst)
    where
      -- Infer a single pattern definition (matcher clause)
      -- Returns (matched type, [substitutions])
      inferPatternDef :: TypeErrorContext -> IPatternDef -> Infer (Type, [Subst])
      inferPatternDef ctx (ppPat, nextMatcherExpr, dataClauses) = do
        -- Infer the type of next matcher expression
        -- It should be either a Matcher type or a tuple of Matcher types
        (_, nextMatcherType, s1) <- inferIExprWithContext nextMatcherExpr ctx
        
        -- If next matcher type contains type variables, constrain them to be Matcher types
        -- This prevents infinite type errors when using matcher parameters
        s1' <- constrainTypeVarsAsMatcher nextMatcherType s1
        let nextMatcherType' = applySubst s1' nextMatcherType
        
        -- Infer PrimitivePatPattern type to get matched type and pattern hole types
        (matchedType, patternHoleTypes, s_pp) <- inferPrimitivePatPattern ppPat nextMatcherType' ctx
        let s1'' = composeSubst s_pp s1'
            matchedType' = applySubst s1'' matchedType
            patternHoleTypes' = map (applySubst s1'') patternHoleTypes
        
        -- Extract variables from primitive-pattern pattern (e.g., #$val)
        -- These variables are bound to the matched type
        let ppPatternVars = extractPPPatternVars ppPat
            ppBindings = [(var, Forall [] [] matchedType') | var <- ppPatternVars]
        
        -- Verify consistency: number of pattern holes should match next matcher structure
        let numPatternHoles = length patternHoleTypes'
        nextMatcherTypes <- extractNextMatcherTypes numPatternHoles nextMatcherType'
        
        -- Check that pattern hole types are consistent with next matcher types
        s_consistency <- checkPatternHoleConsistency patternHoleTypes' nextMatcherTypes ctx
        let s1''' = composeSubst s_consistency s1''
        
        -- Infer the type of data clauses with pp variables in scope
        -- Each data clause: (primitiveDataPattern, targetListExpr)
        dataClauseResults <- withEnv ppBindings $ 
          mapM (inferDataClauseWithCheck ctx numPatternHoles nextMatcherTypes matchedType') dataClauses
        let s2 = foldr composeSubst emptySubst dataClauseResults
        
        return (matchedType', [s1''', s2])
      
      -- Infer PrimitivePatPattern type
      -- Returns (matched type, pattern hole types, substitution)
      -- Pattern hole types correspond to the types that pattern holes ($) should match
      inferPrimitivePatPattern :: PrimitivePatPattern -> Type -> TypeErrorContext -> Infer (Type, [Type], Subst)
      inferPrimitivePatPattern ppPat nextMatcherType ctx = case ppPat of
        PPWildCard -> do
          -- Wildcard pattern: no pattern holes, matched type is arbitrary
          matchedTy <- freshVar "matched"
          return (matchedTy, [], emptySubst)
        
        PPPatVar -> do
          -- Pattern variable ($): one pattern hole
          -- Matched type comes from next matcher
          matchedTy <- extractMatchedTypeFromMatcher nextMatcherType
          return (matchedTy, [nextMatcherType], emptySubst)
        
        PPValuePat _var -> do
          -- Value pattern (#$val): no pattern holes
          -- Matched type is the type of val (any type)
          matchedTy <- freshVar "matched"
          return (matchedTy, [], emptySubst)
        
        PPTuplePat ppPats -> do
          -- Tuple pattern: ($p1, $p2, ...)
          -- Extract matcher types for each element
          matcherTypes <- case nextMatcherType of
            TTuple mts -> return mts
            TMatcher (TTuple ts) -> return $ map TMatcher ts
            _ -> do
              -- Create fresh matcher types for each element
              mapM (\_ -> freshVar "matcher") ppPats
          
          -- Recursively infer each sub-pattern
          results <- zipWithM (\pp mt -> inferPrimitivePatPattern pp mt ctx) ppPats matcherTypes
          let (matchedTypes, patternHoleLists, substs) = unzip3 results
              allPatternHoles = concat patternHoleLists
              finalSubst = foldr composeSubst emptySubst substs
          
          -- Matched type is tuple of matched types
          let matchedTy = TTuple (map (applySubst finalSubst) matchedTypes)
          return (matchedTy, map (applySubst finalSubst) allPatternHoles, finalSubst)
        
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
                  -- Recursively infer each sub-pattern with corresponding argument type
                  -- For inductive patterns, the "next matcher" for each argument is derived from the arg type
                  results <- zipWithM (\pp argTy -> do
                      -- If argTy is Matcher a, use it; otherwise create Matcher argTy
                      let argMatcher = case argTy of
                            TMatcher _ -> argTy
                            _ -> TMatcher argTy
                      inferPrimitivePatPattern pp argMatcher ctx
                    ) ppPats argTypes
                  
                  let (matchedTypes, patternHoleLists, substs) = unzip3 results
                      allPatternHoles = concat patternHoleLists
                      s = foldr composeSubst emptySubst substs
                  
                  -- Verify that inferred matched types match expected argument types
                  s' <- foldM (\accS (inferredTy, expectedTy) -> do
                      s'' <- unifyTypesWithContext (applySubst accS inferredTy) (applySubst accS expectedTy) ctx
                      return $ composeSubst s'' accS
                    ) s (zip matchedTypes argTypes)
                  
                  return (applySubst s' resultType, map (applySubst s') allPatternHoles, s')
            
            Nothing -> do
              -- Not found in pattern environment: use generic inference
              -- This is for backward compatibility
              argTypes <- mapM (\_ -> freshVar "arg") ppPats
              results <- zipWithM (\pp argTy -> inferPrimitivePatPattern pp (TMatcher argTy) ctx) ppPats argTypes
              let (matchedTypes, patternHoleLists, substs) = unzip3 results
                  allPatternHoles = concat patternHoleLists
                  s = foldr composeSubst emptySubst substs
              
              -- Result type is inductive type
              let resultType = TInductive name (map (applySubst s) matchedTypes)
              return (resultType, map (applySubst s) allPatternHoles, s)
      
      -- Extract function argument types and result type
      -- e.g., a -> b -> c -> d  =>  ([a, b, c], d)
      extractFunctionArgs :: Type -> ([Type], Type)
      extractFunctionArgs (TFun arg rest) = 
        let (args, result) = extractFunctionArgs rest
        in (arg : args, result)
      extractFunctionArgs t = ([], t)
      
      -- Extract matched type from Matcher type
      extractMatchedTypeFromMatcher :: Type -> Infer Type
      extractMatchedTypeFromMatcher (TMatcher t) = return t
      extractMatchedTypeFromMatcher t = return t  -- If not Matcher, return as-is
      
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
      -- If n=1, returns [matcherType] as-is (keeping tuple structure if present)
      --   Special case: (Matcher a, Matcher b, ...) is treated as Matcher (a, b, ...)
      --   This matches the runtime behavior where single pattern hole keeps tuple intact
      -- If n>1, returns [TMatcher a, TMatcher b, ...] for (TMatcher a, TMatcher b, ...) or Matcher (a, b, ...)
      extractNextMatcherTypes :: Int -> Type -> Infer [Type]
      extractNextMatcherTypes n matcherType
        | n == 0 = return []
        | n == 1 = do
            -- For single pattern hole, keep the matcher type as-is
            -- But if it's a tuple of matchers (Matcher a, Matcher b, ...), 
            -- convert it to Matcher (a, b, ...) to match ITupleExpr behavior
            case matcherType of
              TTuple types -> do
                -- Check if all elements are Matcher types
                let matcherInners = mapM extractMatcherInner types
                case matcherInners of
                  Just inners -> 
                    -- All elements are matchers: return Matcher (a, b, ...)
                    return [TMatcher (TTuple inners)]
                  Nothing ->
                    -- Not all matchers: keep as tuple
                    return [matcherType]
              _ -> return [matcherType]
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
      
      -- Helper: Extract inner type from Matcher a -> Just a, otherwise Nothing
      extractMatcherInner :: Type -> Maybe Type
      extractMatcherInner (TMatcher t) = Just t
      extractMatcherInner _ = Nothing
      
      -- Infer a data clause with type checking
      -- Check that the target expression returns a list of values with types matching next matchers
      -- Also uses matched type for validation
      inferDataClauseWithCheck :: TypeErrorContext -> Int -> [Type] -> Type -> (IPrimitiveDataPattern, IExpr) -> Infer Subst
      inferDataClauseWithCheck ctx numHoles nextMatcherTypes matchedType (pdPat, targetExpr) = do
        -- Extract expected element type from next matchers (the target type)
        -- This is the type of elements in the list returned by the target expression
        targetType <- case numHoles of
          0 -> return (TTuple [])  -- No pattern holes: empty tuple () case
          1 -> case nextMatcherTypes of
                 [TMatcher innerType] -> return innerType
                 [ty] -> return ty  -- May be 'something' which is polymorphic
                 _ -> freshVar "target"
          _ -> case nextMatcherTypes of
                 types -> do
                   -- Extract inner types from Matcher types
                   innerTypes <- mapM extractMatcherInner types
                   return (TTuple innerTypes)
        
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
        (_, exprType, s1) <- withEnv bindings $ inferIExprWithContext targetExpr ctx
        let s_combined = composeSubst s1 s_pd'
        
        -- Unify with actual expression type
        -- Expected: [targetType]
        let expectedType = TCollection (applySubst s_combined targetType)
        
        s2 <- unifyTypesWithContext (applySubst s_combined exprType) expectedType ctx
        return $ composeSubst s2 s_combined
        where
          extractMatcherInner :: Type -> Infer Type
          extractMatcherInner (TMatcher t) = return t
          extractMatcherInner t = return t
      
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
  
  -- Match expressions (pattern matching)
  IMatchExpr mode target matcher clauses -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (target', targetType, s1) <- inferIExprWithContext target exprCtx
    (matcher', matcherType, s2) <- inferIExprWithContext matcher exprCtx
    
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
        return (IMatchExpr mode target' matcher' clauses, applySubst s1234 resultTy, s1234)
      _ -> do
        -- Infer type of each clause and unify them
        (resultTy, clauseSubst) <- inferMatchClauses exprCtx (applySubst s1234 matchedTy) clauses s1234
        let finalS = composeSubst clauseSubst s1234
        return (IMatchExpr mode target' matcher' clauses, applySubst finalS resultTy, finalS)
  
  -- MatchAll expressions
  IMatchAllExpr mode target matcher clauses -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (target', targetType, s1) <- inferIExprWithContext target exprCtx
    (matcher', matcherType, s2) <- inferIExprWithContext matcher exprCtx
    
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
        return (IMatchAllExpr mode target' matcher' clauses, TCollection (applySubst s1234 resultElemTy), s1234)
      _ -> do
        -- Infer type of each clause (they should all have the same type)
        (resultElemTy, clauseSubst) <- inferMatchClauses exprCtx (applySubst s1234 matchedTy) clauses s1234
        let finalS = composeSubst clauseSubst s1234
        return (IMatchAllExpr mode target' matcher' clauses, TCollection (applySubst finalS resultElemTy), finalS)
  
  -- Memoized Lambda
  IMemoizedLambdaExpr args body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argTypes <- mapM (\_ -> freshVar "memoArg") args
    let bindings = zip args argTypes  -- [(String, Type)]
        schemes = map (\(name, t) -> (name, Forall [] [] t)) bindings
    (body', bodyType, s) <- withEnv schemes $ inferIExprWithContext body exprCtx
    let finalArgTypes = map (applySubst s) argTypes
        funType = foldr TFun bodyType finalArgTypes
    return (IMemoizedLambdaExpr args body', funType, s)
  
  -- Do expression
  IDoExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    -- TODO: Properly handle IO monad bindings
    (body', bodyType, s) <- inferIExprWithContext body exprCtx
    return (IDoExpr bindings body', TIO bodyType, s)
  
  -- Cambda (pattern matching lambda)
  ICambdaExpr var body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argType <- freshVar "cambdaArg"
    (body', bodyType, s) <- inferIExprWithContext body exprCtx
    return (ICambdaExpr var body', TFun argType bodyType, s)
  
  -- With symbols
  IWithSymbolsExpr syms body -> do
    (body', bodyType, s) <- inferIExprWithContext body ctx
    return (IWithSymbolsExpr syms body', bodyType, s)
  
  -- Quote expressions (symbolic math)
  IQuoteExpr e -> return (IQuoteExpr e, TInt, emptySubst)
  IQuoteSymbolExpr e -> return (IQuoteSymbolExpr e, TInt, emptySubst)
  
  -- Indexed expression (tensor indexing)
  IIndexedExpr isSupported baseExpr indices -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (baseExpr', baseType, s) <- inferIExprWithContext baseExpr exprCtx
    -- For tensors, indexing returns the element type
    case baseType of
      TTensor elemType -> return (IIndexedExpr isSupported baseExpr' indices, elemType, s)
      TCollection elemType -> return (IIndexedExpr isSupported baseExpr' indices, elemType, s)
      _ -> return (IIndexedExpr isSupported baseExpr' indices, baseType, s)  -- Fallback: return base type
  
  -- Subrefs expression (subscript references)
  ISubrefsExpr isSupported baseExpr refExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (baseExpr', baseType, s1) <- inferIExprWithContext baseExpr exprCtx
    (refExpr', _, s2) <- inferIExprWithContext refExpr exprCtx
    let finalS = composeSubst s2 s1
    -- TODO: Properly handle subscript semantics
    return (ISubrefsExpr isSupported baseExpr' refExpr', baseType, finalS)
  
  -- Suprefs expression (superscript references)
  ISuprefsExpr isSupported baseExpr refExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (baseExpr', baseType, s1) <- inferIExprWithContext baseExpr exprCtx
    (refExpr', _, s2) <- inferIExprWithContext refExpr exprCtx
    let finalS = composeSubst s2 s1
    -- TODO: Properly handle superscript semantics
    return (ISuprefsExpr isSupported baseExpr' refExpr', baseType, finalS)
  
  -- Userrefs expression (user-defined references)
  IUserrefsExpr isSupported baseExpr refExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (baseExpr', baseType, s1) <- inferIExprWithContext baseExpr exprCtx
    (refExpr', _, s2) <- inferIExprWithContext refExpr exprCtx
    let finalS = composeSubst s2 s1
    -- TODO: Properly handle user-defined references
    return (IUserrefsExpr isSupported baseExpr' refExpr', baseType, finalS)
  
  -- Wedge apply expression (exterior product)
  IWedgeApplyExpr func args -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (func', funcType, s1) <- inferIExprWithContext func exprCtx
    -- Wedge application is similar to normal application
    inferIApplicationWithContext func' funcType args s1 exprCtx
  
  -- Generate tensor expression
  IGenerateTensorExpr shapeExpr genFunc -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (shapeExpr', _, s1) <- inferIExprWithContext shapeExpr exprCtx
    (genFunc', funcType, s2) <- inferIExprWithContext genFunc exprCtx
    -- Extract element type from function result
    elemType <- case funcType of
      TFun _ resultType -> return resultType
      _ -> freshVar "tensorElem"
    let finalS = composeSubst s2 s1
    return (IGenerateTensorExpr shapeExpr' genFunc', normalizeTensorType (TTensor (applySubst finalS elemType)), finalS)
  
  -- Tensor expression
  ITensorExpr shapeExpr elemsExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (shapeExpr', _, s1) <- inferIExprWithContext shapeExpr exprCtx
    (elemsExpr', elemsType, s2) <- inferIExprWithContext elemsExpr exprCtx
    -- Extract element type
    elemType <- case elemsType of
      TCollection t -> return t
      _ -> freshVar "tensorElem"
    let finalS = composeSubst s2 s1
    return (ITensorExpr shapeExpr' elemsExpr', normalizeTensorType (TTensor (applySubst finalS elemType)), finalS)
  
  -- Tensor contract expression
  ITensorContractExpr tensorExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (tensorExpr', tensorType, s) <- inferIExprWithContext tensorExpr exprCtx
    -- Contraction reduces tensor rank
    case tensorType of
      TTensor elemType -> return (ITensorContractExpr tensorExpr', elemType, s)
      _ -> return (ITensorContractExpr tensorExpr', tensorType, s)
  
  -- Tensor map expression
  ITensorMapExpr func tensorExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (func', funcType, s1) <- inferIExprWithContext func exprCtx
    (tensorExpr', tensorType, s2) <- inferIExprWithContext tensorExpr exprCtx
    let s12 = composeSubst s2 s1
    -- Function maps elements: a -> b, tensor is Tensor a, result is Tensor b
    case tensorType of
      TTensor elemType -> do
        resultElemType <- freshVar "tmapElem"
        s3 <- unifyTypesWithContext (applySubst s12 funcType) (TFun elemType resultElemType) exprCtx
        let finalS = composeSubst s3 s12
        return (ITensorMapExpr func' tensorExpr', normalizeTensorType (TTensor (applySubst finalS resultElemType)), finalS)
      _ -> return (ITensorMapExpr func' tensorExpr', tensorType, s12)
  
  -- Tensor map2 expression (binary map)
  ITensorMap2Expr func tensor1 tensor2 -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (func', funcType, s1) <- inferIExprWithContext func exprCtx
    (tensor1', t1Type, s2) <- inferIExprWithContext tensor1 exprCtx
    (tensor2', t2Type, s3) <- inferIExprWithContext tensor2 exprCtx
    let s123 = foldr composeSubst emptySubst [s3, s2, s1]
    -- Function: a -> b -> c, tensors are Tensor a and Tensor b, result is Tensor c
    case (t1Type, t2Type) of
      (TTensor elem1, TTensor elem2) -> do
        resultElemType <- freshVar "tmap2Elem"
        s4 <- unifyTypesWithContext (applySubst s123 funcType) 
                (TFun elem1 (TFun elem2 resultElemType)) exprCtx
        let finalS = composeSubst s4 s123
        return (ITensorMap2Expr func' tensor1' tensor2', normalizeTensorType (TTensor (applySubst finalS resultElemType)), finalS)
      _ -> return (ITensorMap2Expr func' tensor1' tensor2', t1Type, s123)
  
  -- Transpose expression
  ITransposeExpr tensorExpr permExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (tensorExpr', tensorType, s) <- inferIExprWithContext tensorExpr exprCtx
    -- Transpose preserves tensor type
    return (ITransposeExpr tensorExpr' permExpr, normalizeTensorType tensorType, s)
  
  -- Flip indices expression
  IFlipIndicesExpr tensorExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (tensorExpr', tensorType, s) <- inferIExprWithContext tensorExpr exprCtx
    -- Flipping indices preserves tensor type
    return (IFlipIndicesExpr tensorExpr', normalizeTensorType tensorType, s)
  
  -- Function expression (built-in function reference)
  IFunctionExpr names -> do
    -- Built-in function: return a generic function type
    -- TODO: Look up actual function signature
    argType <- freshVar "funcArg"
    resultType <- freshVar "funcResult"
    return (IFunctionExpr names, TFun argType resultType, emptySubst)

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
  -- Infer pattern type and extract pattern variable bindings
  -- Use pattern constructor and pattern function type information
  (bindings, s_pat) <- inferIPattern pattern matchedType ctx
  let s1 = composeSubst s_pat initSubst
  
  -- Convert bindings to TypeScheme format
  let schemes = [(var, Forall [] [] ty) | (var, ty) <- bindings]
  
  -- Infer body expression type with pattern variables in scope
  (_, bodyType, s2) <- withEnv schemes $ inferIExprWithContext bodyExpr ctx
  let finalS = composeSubst s2 s1
  return (applySubst finalS bodyType, finalS)

-- | Infer multiple patterns left-to-right, making left bindings available to right patterns
-- This enables non-linear patterns like ($p, #(p + 1))
inferPatternsLeftToRight :: [IPattern] -> [Type] -> [(String, Type)] -> Subst -> TypeErrorContext 
                         -> Infer ([(String, Type)], Subst)
inferPatternsLeftToRight [] [] accBindings accSubst _ctx = 
  return (accBindings, accSubst)
inferPatternsLeftToRight (p:ps) (t:ts) accBindings accSubst ctx = do
  -- Add accumulated bindings to environment for this pattern
  let schemes = [(var, Forall [] [] ty) | (var, ty) <- accBindings]
  
  -- Infer this pattern with left bindings in scope
  (newBindings, s) <- withEnv schemes $ inferIPattern p (applySubst accSubst t) ctx
  
  -- Compose substitutions
  let accSubst' = composeSubst s accSubst
  
  -- Apply substitution to accumulated bindings
  let accBindings' = [(v, applySubst s ty) | (v, ty) <- accBindings] ++ newBindings
  
  -- Continue with remaining patterns
  inferPatternsLeftToRight ps ts accBindings' accSubst' ctx
inferPatternsLeftToRight _ _ accBindings accSubst _ = 
  return (accBindings, accSubst)  -- Mismatched lengths

-- | Infer IPattern type and extract pattern variable bindings
-- Returns (bindings, substitution)
-- bindings: [(variable name, type)]
inferIPattern :: IPattern -> Type -> TypeErrorContext -> Infer ([(String, Type)], Subst)
inferIPattern pat expectedType ctx = case pat of
  IWildCard -> do
    -- Wildcard: no bindings
    return ([], emptySubst)
  
  IPatVar name -> do
    -- Pattern variable: bind to expected type
    return ([(name, expectedType)], emptySubst)
  
  IValuePat expr -> do
    -- Value pattern: infer expression type and unify with expected type
    (_, exprType, s) <- inferIExprWithContext expr ctx
    s' <- unifyTypesWithContext (applySubst s exprType) (applySubst s expectedType) ctx
    return ([], composeSubst s' s)
  
  IPredPat _expr -> do
    -- Predicate pattern: no bindings, but should check predicate type
    -- For now, just accept any expected type
    return ([], emptySubst)
  
  ITuplePat pats -> do
    -- Tuple pattern: decompose expected type
    case expectedType of
      TTuple types | length types == length pats -> do
        -- Types match: infer each sub-pattern left-to-right
        -- Left patterns' bindings are available for right patterns (for non-linear patterns)
        (allBindings, s) <- inferPatternsLeftToRight pats types [] emptySubst ctx
        return (allBindings, s)
      
      TVar _ -> do
        -- Expected type is a type variable: create tuple type
        elemTypes <- mapM (\_ -> freshVar "elem") pats
        let tupleTy = TTuple elemTypes
        s <- unifyTypesWithContext expectedType tupleTy ctx
        
        -- Recursively infer each sub-pattern left-to-right
        let elemTypes' = map (applySubst s) elemTypes
        (allBindings, s') <- inferPatternsLeftToRight pats elemTypes' [] s ctx
        return (allBindings, s')
      
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
            (allBindings, s) <- inferPatternsLeftToRight pats argTypes' [] s0 ctx
            
            return (allBindings, s)
      
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
                (allBindings, s) <- inferPatternsLeftToRight pats argTypes' [] s0 ctx
                
                return (allBindings, s)
          
          Nothing -> do
            -- Not found: generic inference
            argTypes <- mapM (\_ -> freshVar "arg") pats
            let resultType = TInductive name argTypes
            
            s0 <- unifyTypesWithContext resultType expectedType ctx
            let argTypes' = map (applySubst s0) argTypes
            
            -- Recursively infer each sub-pattern left-to-right
            (allBindings, s) <- inferPatternsLeftToRight pats argTypes' [] s0 ctx
            
            return (allBindings, s)
  
  IIndexedPat p _indices -> do
    -- Indexed pattern: just infer the base pattern
    inferIPattern p expectedType ctx
  
  ILetPat bindings p -> do
    -- Let pattern: infer bindings and then the pattern
    -- Infer bindings first
    env <- getEnv
    (_, bindingSchemes, s1) <- inferIBindingsWithContext bindings env emptySubst ctx
    
    -- Infer pattern with bindings in scope
    (patBindings, s2) <- withEnv bindingSchemes $ inferIPattern p (applySubst s1 expectedType) ctx
    
    let s = composeSubst s2 s1
    -- Let bindings are not exported, only pattern bindings
    return (patBindings, s)
  
  INotPat p -> do
    -- Not pattern: infer the sub-pattern but don't use its bindings
    (_, s) <- inferIPattern p expectedType ctx
    return ([], s)
  
  IAndPat p1 p2 -> do
    -- And pattern: both patterns must match the same type
    -- Left bindings should be available to right pattern
    (bindings1, s1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    (bindings2, s2) <- withEnv schemes1 $ inferIPattern p2 (applySubst s1 expectedType) ctx
    let s = composeSubst s2 s1
        -- Apply substitution to left bindings
        bindings1' = [(v, applySubst s2 ty) | (v, ty) <- bindings1]
    return (bindings1' ++ bindings2, s)
  
  IOrPat p1 p2 -> do
    -- Or pattern: both patterns must match the same type
    -- Left bindings should be available to right pattern for non-linear patterns
    (bindings1, s1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    (bindings2, s2) <- withEnv schemes1 $ inferIPattern p2 (applySubst s1 expectedType) ctx
    let s = composeSubst s2 s1
        -- Apply substitution to left bindings
        bindings1' = [(v, applySubst s2 ty) | (v, ty) <- bindings1]
    -- For or patterns, ideally both branches should have same variables
    -- For now, we take union of bindings
    return (bindings1' ++ bindings2, s)
  
  IForallPat p1 p2 -> do
    -- Forall pattern: similar to and pattern
    -- Left bindings should be available to right pattern
    (bindings1, s1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    (bindings2, s2) <- withEnv schemes1 $ inferIPattern p2 (applySubst s1 expectedType) ctx
    let s = composeSubst s2 s1
        -- Apply substitution to left bindings
        bindings1' = [(v, applySubst s2 ty) | (v, ty) <- bindings1]
    return (bindings1' ++ bindings2, s)
  
  ILoopPat var range p1 p2 -> do
    -- Loop pattern: $var is the loop variable (Integer), range contains pattern
    -- First, infer the range pattern (third element of ILoopRange)
    let ILoopRange _startExpr _endExpr rangePattern = range
    (rangeBindings, s_range) <- inferIPattern rangePattern TInt ctx
    
    -- Add loop variable binding (always Integer for loop index)
    let loopVarBinding = (var, TInt)
        initialBindings = loopVarBinding : rangeBindings
        schemes0 = [(v, Forall [] [] ty) | (v, ty) <- initialBindings]
    
    -- Infer p1 with loop variable and range bindings in scope
    (bindings1, s1) <- withEnv schemes0 $ inferIPattern p1 (applySubst s_range expectedType) ctx
    
    -- Infer p2 with all previous bindings in scope
    let allPrevBindings = [(v, applySubst s1 ty) | (v, ty) <- initialBindings] ++ bindings1
        schemes1 = [(v, Forall [] [] ty) | (v, ty) <- allPrevBindings]
    (bindings2, s2) <- withEnv schemes1 $ inferIPattern p2 (applySubst s1 expectedType) ctx
    
    let s = foldr composeSubst emptySubst [s2, s1, s_range]
        -- Apply final substitution to all bindings
        finalBindings = [(v, applySubst s ty) | (v, ty) <- loopVarBinding : rangeBindings ++ bindings1 ++ bindings2]
    
    return (finalBindings, s)
  
  IContPat -> do
    -- Continuation pattern: no bindings
    return ([], emptySubst)
  
  IPApplyPat funcExpr argPats -> do
    -- Pattern application: infer pattern function type
    (_, funcType, s1) <- inferIExprWithContext funcExpr ctx
    
    -- Pattern function should return a pattern that matches expectedType
    -- Infer argument patterns left-to-right with fresh types
    argTypes <- mapM (\_ -> freshVar "parg") argPats
    (allBindings, s2) <- inferPatternsLeftToRight argPats argTypes [] s1 ctx
    
    return (allBindings, s2)
  
  IVarPat name -> do
    -- Variable pattern (with ~): bind to expected type
    return ([(name, expectedType)], emptySubst)
  
  IInductiveOrPApplyPat name pats -> do
    -- Could be either inductive pattern or pattern application
    -- Try inductive pattern first
    inferIPattern (IInductivePat name pats) expectedType ctx
  
  ISeqNilPat -> do
    -- Sequence nil: no bindings
    return ([], emptySubst)
  
  ISeqConsPat p1 p2 -> do
    -- Sequence cons: infer both patterns
    -- Left bindings should be available to right pattern
    (bindings1, s1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    (bindings2, s2) <- withEnv schemes1 $ inferIPattern p2 (applySubst s1 expectedType) ctx
    let s = composeSubst s2 s1
        -- Apply substitution to left bindings
        bindings1' = [(v, applySubst s2 ty) | (v, ty) <- bindings1]
    return (bindings1' ++ bindings2, s)
  
  ILaterPatVar -> do
    -- Later pattern variable: no immediate binding
    return ([], emptySubst)
  
  IDApplyPat p pats -> do
    -- D-apply pattern: infer base pattern and argument patterns
    -- Base pattern bindings should be available to argument patterns
    (bindings1, s1) <- inferIPattern p expectedType ctx
    
    -- Infer argument patterns left-to-right with base pattern bindings in scope
    argTypes <- mapM (\_ -> freshVar "darg") pats
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    (argBindings, s2) <- withEnv schemes1 $ inferPatternsLeftToRight pats argTypes [] s1 ctx
    
    let s = composeSubst s2 s1
        -- Apply substitution to base bindings
        bindings1' = [(v, applySubst s2 ty) | (v, ty) <- bindings1]
    return (bindings1' ++ argBindings, s)
  where
    -- Extract function argument types and result type
    -- e.g., a -> b -> c -> d  =>  ([a, b, c], d)
    extractFunctionArgs :: Type -> ([Type], Type)
    extractFunctionArgs (TFun arg rest) = 
      let (args, result) = extractFunctionArgs rest
      in (arg : args, result)
    extractFunctionArgs t = ([], t)

-- | Extract pattern variables from a pattern (legacy, kept for reference)
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
-- Returns (applied expr, type, substitution)
inferIApplication :: String -> Type -> [IExpr] -> Subst -> Infer (IExpr, Type, Subst)
inferIApplication funcName funcType args initSubst = inferIApplicationWithContext (IVarExpr funcName) funcType args initSubst emptyContext

-- | Check if tensorMap should be inserted for an argument
-- This implements the type-tensor-simple.md specification
-- Extended to check type class instances to determine necessity
shouldInsertTensorMap :: ClassEnv -> [Constraint] -> Type -> Type -> Bool
shouldInsertTensorMap classEnv constraints argType paramType = case argType of
  TTensor elemType -> case paramType of
    -- Tensor matched with Tensor → no insertion needed
    TTensor _ -> False
    
    -- Tensor matched with type variable → check type class instances
    TVar tyVar -> 
      -- Find constraints on this type variable
      let relevantConstraints = filter (\(Constraint _ t) -> t == TVar tyVar) constraints
      in case relevantConstraints of
           -- No constraints → 0-rank tensor interpretation, no insertion needed
           [] -> False
           -- Has constraints → check if Tensor instance exists for ALL constraints
           cs -> not (all (hasInstanceForTensor classEnv elemType) cs)
                 -- If any constraint lacks a Tensor instance, we need tensorMap
    
    -- Tensor matched with concrete non-tensor type → insertion needed
    _ -> True
    
  -- Non-tensor argument → no insertion needed
  _ -> False

-- | Check if a type class has an instance for Tensor elemType
hasInstanceForTensor :: ClassEnv -> Type -> Constraint -> Bool
hasInstanceForTensor classEnv elemType (Constraint className _tyVar) =
  let tensorType = TTensor elemType
      instances = lookupInstances className classEnv
  in any (\inst -> matchesInstanceType tensorType (instType inst)) instances

-- | Check if a query type matches an instance type
-- This performs structural matching to determine if an instance applies
matchesInstanceType :: Type -> Type -> Bool
matchesInstanceType queryType instType = case (queryType, instType) of
  -- Exact match
  _ | queryType == instType -> True
  
  -- Tensor types
  (TTensor qt, TTensor it) -> matchesInstanceType qt it
  
  -- Collection types
  (TCollection qt, TCollection it) -> matchesInstanceType qt it
  
  -- Tuple types
  (TTuple qts, TTuple its) 
    | length qts == length its -> all (uncurry matchesInstanceType) (zip qts its)
  
  -- Inductive types
  (TInductive qn qts, TInductive in_ its)
    | qn == in_ && length qts == length its -> all (uncurry matchesInstanceType) (zip qts its)
  
  -- Type variables in instance type match any query type
  (_, TVar _) -> True
  
  _ -> False

-- | Generate a fresh variable name for tensorMap lambdas
freshVarName :: String -> Infer String
freshVarName prefix = do
  st <- get
  let n = inferCounter st
  modify $ \s -> s { inferCounter = n + 1 }
  return $ prefix ++ show n

-- | Get the parameter type at the specified index from a function type
-- Example: (a -> b -> c) at index 0 → Just a, at index 1 → Just b
getParamType :: Type -> Int -> Maybe Type
getParamType (TFun param _) 0 = Just param
getParamType (TFun _ rest) n 
  | n > 0 = getParamType rest (n - 1)
getParamType _ _ = Nothing

-- | Apply one argument to a function type
-- Example: (a -> b -> c) → (b -> c)
applyOneArgType :: Type -> Type
applyOneArgType (TFun _ rest) = rest
applyOneArgType t = t  -- No more arguments

-- | Recursively wrap function application with tensorMap where needed
-- This implements the automatic tensorMap insertion described in type-tensor-simple.md
-- Process arguments from left to right, building nested tensorMap expressions
wrapWithTensorMap :: 
    IExpr           -- Current function expression (possibly partially applied)
    -> Type         -- Current function type
    -> [IExpr]      -- Remaining argument expressions
    -> [Type]       -- Remaining argument types
    -> Subst        -- Current substitution
    -> TypeErrorContext
    -> Infer (IExpr, Type, Subst)
wrapWithTensorMap currentFunc currentType [] [] subst _ctx = do
  -- All arguments processed
  return (currentFunc, currentType, subst)

wrapWithTensorMap currentFunc currentType (arg:restArgs) (argType:restTypes) subst ctx = do
  -- Get ClassEnv and constraints from inference state
  classEnv <- gets inferClassEnv
  constraints <- gets inferConstraints
  
  -- Get the expected parameter type
  let currentType' = applySubst subst currentType
  case getParamType currentType' 0 of
    Nothing -> throwError $ UnificationError currentType' (TFun (TVar (TyVar "a")) (TVar (TyVar "b"))) ctx
    Just paramType -> do
      let argType' = applySubst subst argType
          paramType' = applySubst subst paramType
      
      if shouldInsertTensorMap classEnv constraints argType' paramType'
        then do
          -- TensorMap insertion needed
          varName <- freshVarName "tmapVar"
          let var = Var varName []
              varExpr = IVarExpr varName
              
          -- Build inner expression (recursive call)
          let innerFunc = IApplyExpr currentFunc [varExpr]
              innerType = applyOneArgType currentType'
          
          (innerExpr, finalType, s1) <- wrapWithTensorMap 
                                          innerFunc 
                                          innerType 
                                          restArgs 
                                          restTypes 
                                          subst 
                                          ctx
          
          -- Build lambda: \varName -> innerExpr
          let lambda = ILambdaExpr Nothing [var] innerExpr
              wrappedExpr = ITensorMapExpr lambda arg
          
          -- Calculate result type: Tensor resultType
          -- tensorMap : (a -> b) -> Tensor a -> Tensor b
          let resultType = normalizeTensorType (TTensor finalType)
          
          return (wrappedExpr, resultType, s1)
        
        else do
          -- No tensorMap needed, normal application
          -- Unify parameter and argument types
          s <- unifyTypesWithContext paramType' argType' ctx
          let s' = composeSubst s subst
              appliedFunc = IApplyExpr currentFunc [arg]
              appliedType = applyOneArgType currentType'
          
          -- Process remaining arguments (recursive call)
          wrapWithTensorMap appliedFunc appliedType restArgs restTypes s' ctx

wrapWithTensorMap _ currentType args argTypes _ ctx = 
  throwError $ TE.TypeMismatch currentType (foldr TFun (TVar (TyVar "result")) argTypes) "Argument count mismatch" ctx

-- | Infer application (helper) with context
-- Returns (applied expr, type, substitution)
-- This function implements automatic tensorMap insertion as described in type-tensor-simple.md
inferIApplicationWithContext :: IExpr -> Type -> [IExpr] -> Subst -> TypeErrorContext -> Infer (IExpr, Type, Subst)
inferIApplicationWithContext funcExpr funcType args initSubst ctx = do
  -- Infer argument types
  argResults <- mapM (\arg -> inferIExprWithContext arg ctx) args
  let argExprs = map (\(e, _, _) -> e) argResults
      argTypes = map (\(_, t, _) -> t) argResults
      argSubst = foldr composeSubst initSubst (map (\(_, _, s) -> s) argResults)
  
  -- Get function constraints if funcExpr is a variable
  funcConstraints <- case funcExpr of
    IVarExpr varName -> do
      env <- getEnv
      case lookupEnv varName env of
        Just (Forall _ cs _) -> do
          -- Debug: funcConstraints が取得できた
          return cs
        Nothing -> return []
    _ -> return []
  
  -- Check if tensorMap is needed based on type class instance availability
  classEnv <- gets inferClassEnv
  let needsTensorMap = or [ shouldInsertTensorMap classEnv funcConstraints argT paramT
                          | (argT, idx) <- zip argTypes [0..]
                          , Just paramT <- [getParamType funcType idx]
                          ]
  
  if needsTensorMap
    then do
      -- TensorMap needed, use wrapWithTensorMap
      wrapWithTensorMap funcExpr funcType argExprs argTypes argSubst ctx
    else do
      -- No tensorMap needed, proceed with normal application
      resultType <- freshVar "result"
      let expectedFuncType = foldr TFun resultType argTypes
      
      case unify (applySubst argSubst funcType) expectedFuncType of
        Right s -> do
          -- Unification succeeded
          let finalS = composeSubst s argSubst
          return (IApplyExpr funcExpr argExprs, applySubst finalS resultType, finalS)
        
        Left _ -> do
          -- Unification failed
          throwError $ UnificationError (applySubst argSubst funcType) expectedFuncType ctx

-- | Infer let bindings (non-recursive)
-- Returns (transformed bindings, type schemes, substitution)
inferIBindings :: [IBindingExpr] -> TypeEnv -> Subst -> Infer ([IBindingExpr], [(String, TypeScheme)], Subst)
inferIBindings bindings env s = inferIBindingsWithContext bindings env s emptyContext

-- | Infer let bindings (non-recursive) with context
-- Returns (transformed bindings, type schemes, substitution)
inferIBindingsWithContext :: [IBindingExpr] -> TypeEnv -> Subst -> TypeErrorContext -> Infer ([IBindingExpr], [(String, TypeScheme)], Subst)
inferIBindingsWithContext [] _env s _ctx = return ([], [], s)
inferIBindingsWithContext ((pat, expr):bs) env s ctx = do
  -- Infer the type of the expression
  (expr', exprType, s1) <- inferIExprWithContext expr ctx
  
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
  (restBindings', restBindings, s2') <- withEnv extendedEnvList $ inferIBindingsWithContext bs env s' ctx
  return ((pat, expr') : restBindings', bindings ++ restBindings, s2')
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
-- Returns (transformed bindings, type schemes, substitution)
inferIRecBindings :: [IBindingExpr] -> TypeEnv -> Subst -> Infer ([IBindingExpr], [(String, TypeScheme)], Subst)
inferIRecBindings bindings env s = inferIRecBindingsWithContext bindings env s emptyContext

-- | Infer letrec bindings (recursive) with context
-- Returns (transformed bindings, type schemes, substitution)
inferIRecBindingsWithContext :: [IBindingExpr] -> TypeEnv -> Subst -> TypeErrorContext -> Infer ([IBindingExpr], [(String, TypeScheme)], Subst)
inferIRecBindingsWithContext bindings _env s ctx = do
  -- Create placeholders with fresh type variables
  placeholders <- mapM (\(pat, _) -> do
    ty <- freshVar "rec"
    return (pat, ty)) bindings
  
  -- Extract bindings from placeholders
  let placeholderBindings = concatMap (\(pat, ty) -> extractIBindingsFromPattern pat ty) placeholders
  
  -- Infer expressions in extended environment
  results <- withEnv placeholderBindings $ mapM (\(_, expr) -> inferIExprWithContext expr ctx) bindings
  
  let exprs' = map (\(e, _, _) -> e) results
      exprTypes = map (\(_, t, _) -> t) results
      substList = map (\(_, _, s') -> s') results
      finalS = foldr composeSubst s substList
  
  -- Re-extract bindings with inferred types
  let finalBindings = concat $ zipWith (\(pat, _) ty -> extractIBindingsFromPattern pat (applySubst finalS ty)) bindings exprTypes
      transformedBindings = zipWith (\(pat, _) expr' -> (pat, expr')) bindings exprs'
  
  return (transformedBindings, finalBindings, finalS)

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
        (expr', exprType, subst1) <- inferIExpr expr
        
        -- Unify inferred type with expected type
        -- At top-level definitions, Tensor a can unify with a
        let exprCtx = withExpr (prettyStr expr) emptyContext
        subst2 <- unifyTypesWithTopLevel (applySubst subst1 exprType) (applySubst subst1 expectedType) exprCtx
        let finalSubst = composeSubst subst2 subst1
        
        -- For display purposes, use the original type from the scheme (before instantiation)
        -- This preserves the original type variable names (a, b, c) instead of fresh ones (t0, t1, t2)
        -- Extract the type from the scheme and apply the substitution
        let (Forall _ _ originalType) = existingScheme
        let finalType = applySubst finalSubst originalType
        
        -- Keep the existing scheme (with explicit type signature) in the environment
        -- Don't override it with the generalized inferred type
        return (Just (IDefine var expr', finalType), finalSubst)
      
      Nothing -> do
        -- No explicit type signature: infer and generalize as before
        clearConstraints  -- Start with fresh constraints for this expression
        (expr', exprType, subst) <- inferIExpr expr
        constraints <- getConstraints  -- Collect constraints from type inference
        
        -- Generalize with constraints
        let envFreeVars = freeVarsInEnv env
            typeFreeVars = freeTyVars exprType
            genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
            scheme = Forall genVars constraints exprType
        
        -- Add to environment
        modify $ \s -> s { inferEnv = extendEnv varName scheme (inferEnv s) }
        
        let finalType = applySubst subst exprType
        return (Just (IDefine var expr', finalType), subst)
  
  ITest expr -> do
    clearConstraints  -- Start with fresh constraints
    (expr', exprType, subst) <- inferIExpr expr
    -- Constraints are now in state, will be retrieved by Eval.hs
    return (Just (ITest expr', exprType), subst)
  
  IExecute expr -> do
    clearConstraints  -- Start with fresh constraints
    (expr', exprType, subst) <- inferIExpr expr
    -- Constraints are now in state, will be retrieved by Eval.hs
    return (Just (IExecute expr', exprType), subst)
  
  ILoadFile _path -> return (Nothing, emptySubst)
  ILoad _lib -> return (Nothing, emptySubst)
  
  IDefineMany bindings -> do
    -- Process each binding in the list
    env <- getEnv
    results <- mapM (inferBinding env) bindings
    let bindings' = map fst results
        substs = map snd results
        combinedSubst = foldr composeSubst emptySubst substs
    -- Return the processed bindings (type is not meaningful for IDefineMany)
    return (Just (IDefineMany bindings', TTuple []), combinedSubst)
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
            (expr', exprType, subst1) <- inferIExpr expr
            subst2 <- unifyTypesWithTopLevel (applySubst subst1 exprType) (applySubst subst1 expectedType) emptyContext
            let finalSubst = composeSubst subst2 subst1
            return ((var, expr'), finalSubst)
          
          Nothing -> do
            -- Without type signature: infer and generalize
            clearConstraints
            (expr', exprType, subst) <- inferIExpr expr
            constraints <- getConstraints
            
            -- Generalize the type
            let envFreeVars = freeVarsInEnv env
                typeFreeVars = freeTyVars exprType
                genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
                scheme = Forall genVars constraints exprType
            
            -- Add to environment for subsequent bindings
            modify $ \s -> s { inferEnv = extendEnv varName scheme (inferEnv s) }
            
            return ((var, expr'), subst)
  
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
    Right (_, ty, subst) -> Right (ty, subst, warnings)

-- | Run type inference on IExpr with initial environment
runInferIWithEnv :: InferConfig -> TypeEnv -> IExpr -> IO (Either TypeError (Type, Subst, TypeEnv, [TypeWarning]))
runInferIWithEnv cfg env expr = do
  let initState = (initialInferStateWithConfig cfg) { inferEnv = env }
  (result, warnings, finalState) <- runInferWithWarningsAndState (inferIExpr expr) initState
  return $ case result of
    Left err -> Left err
    Right (_, ty, subst) -> Right (ty, subst, inferEnv finalState, warnings)
