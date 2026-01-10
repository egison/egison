{- |
Module      : Language.Egison.Eval
Licence     : MIT

This module provides interface for evaluating Egison expressions.

Processing Flow (design/implementation.md):
  1. TopExpr (Parse result)
  2. expandLoads (File loading with caching)
  3. Environment Building Phase (Collect data constructors, type classes, instances, type signatures)
  4. Desugar (Syntactic desugaring)
  5. Type Inference Phase (Constraint generation, unification, type class constraint processing)
  6. Type Check Phase (Verify type annotations, check type class constraints)
  7. TypedTopExpr (Typed AST)
  8. TypedDesugar (Type-driven transformations: dictionary passing, tensorMap insertion)
  9. TITopExpr (Evaluatable typed IR with type info preserved)
 10. Evaluation (Pattern matching execution, expression evaluation, IO actions)
-}

module Language.Egison.Eval
  (
  -- * Eval Egison expressions
    evalExpr
  , evalTopExpr
  , evalTopExprStr
  , evalTopExprs
  , evalTopExprs'
  , evalTopExprsNoPrint
  , runExpr
  , runTopExpr
  , runTopExprStr
  , runTopExprs
  -- * Load Egison files
  , loadEgisonLibrary
  , loadEgisonFile
  -- * Load expansion
  , expandLoads
  ) where

import           Control.Monad              (foldM, forM_, when)
import           Data.List                  (intercalate)
import           Control.Monad.Except       (throwError, catchError)
import           Control.Monad.Reader       (ask, asks)
import           Control.Monad.State
import           System.IO                  (hPutStrLn, stderr)

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Core
import           Language.Egison.Data
import           Language.Egison.Desugar (desugarExpr, desugarTopExpr, desugarTopExprs)
import           Language.Egison.EnvBuilder (buildEnvironments, EnvBuildResult(..))
import           Language.Egison.EvalState  (MonadEval (..), ConstructorEnv, PatternConstructorEnv)
import           Language.Egison.IExpr (TIExpr(..), TIExprNode(..), TITopExpr(..), ITopExpr(..), IExpr(..), Var, extractNameFromVar, stringToVar, stripTypeTopExpr)
import           Language.Egison.MathOutput (prettyMath)
import           Language.Egison.Parser
import qualified Language.Egison.Type.Types as Types
import           Language.Egison.Type.IInfer (inferITopExpr, runInferWithWarningsAndState, InferState(..), initialInferStateWithConfig, permissiveInferConfig, defaultInferConfig)
import           Language.Egison.Type.Env (TypeEnv, ClassEnv, PatternTypeEnv, extendEnvMany, envToList, classEnvToList, lookupInstances, patternEnvToList)
import qualified Language.Egison.Type.Env as Env
import qualified Data.Set as Set
import           Language.Egison.Type.TypeClassExpand (expandTypeClassMethodsT, addDictionaryParametersT)
import           Language.Egison.Type.Error (formatTypeError, formatTypeWarning)
import           Language.Egison.Type.Check (builtinEnv)
import           Language.Egison.Type.Pretty (prettyTypeScheme, prettyType)
import           Language.Egison.Pretty (prettyStr)
import           Language.Egison.EvalState (ConstructorInfo(..))
import qualified Data.HashMap.Strict as HashMap


-- | Evaluate an Egison expression.
evalExpr :: Env -> Expr -> EvalM EgisonValue
evalExpr env expr = desugarExpr expr >>= evalExprDeep env

--------------------------------------------------------------------------------
-- Phase 1: expandLoads - File Loading with Caching
--------------------------------------------------------------------------------
-- Recursively expand all Load/LoadFile statements into a flat list of TopExprs.
-- This phase handles file reading and prevents duplicate loading through caching.
-- After this phase, all source code is loaded and ready for environment building.

-- | Expand all Load/LoadFile statements recursively into a flat list of TopExprs.
-- Files are loaded recursively and deduplicated (same file loaded multiple times
-- will only appear once in the final list).
expandLoads :: [TopExpr] -> EvalM [TopExpr]
expandLoads [] = return []
expandLoads (expr:rest) = case expr of
  Load lib -> do
    libExprs <- loadLibraryFile lib
    expanded <- expandLoads libExprs
    restExpanded <- expandLoads rest
    return $ expanded ++ restExpanded
  LoadFile file -> do
    fileExprs <- loadFile file
    expanded <- expandLoads fileExprs
    restExpanded <- expandLoads rest
    return $ expanded ++ restExpanded
  _ -> do
    restExpanded <- expandLoads rest
    return $ expr : restExpanded

--------------------------------------------------------------------------------
-- Main Pipeline Entry Point
--------------------------------------------------------------------------------

-- | Evaluate an Egison top expression.
-- Implements the complete processing flow:
--   expandLoads → Environment Building → Desugar → Type Inference/Check → 
--   TypedDesugar → Evaluation
evalTopExpr :: Env -> TopExpr -> EvalM (Maybe EgisonValue, Env)
evalTopExpr env topExpr = do
  -- Phase 1: Expand all Load/LoadFile recursively
  expanded <- expandLoads [topExpr]
  -- Phase 2-10: Process all expanded expressions through remaining pipeline
  evalExpandedTopExprsTyped env expanded

-- | Evaluate expanded top expressions using typed pipeline
-- TODO: Implement type environment accumulation for proper type checking
evalExpandedTopExprsTyped :: Env -> [TopExpr] -> EvalM (Maybe EgisonValue, Env)
evalExpandedTopExprsTyped env exprs = evalExpandedTopExprsTyped' env exprs False True

--------------------------------------------------------------------------------
-- Phase 2-10: Environment Building → Desugar → Type Inference/Check → 
--             TypedDesugar → Evaluation
--------------------------------------------------------------------------------

-- | Evaluate expanded top expressions using the typed pipeline with optional printing.
-- This function implements phases 2-10 of the processing flow.

-- | Helper: Convert IExpr to TIExpr (simplified, without full type inference)
-- This is a temporary solution that wraps IExpr nodes in TIExpr with a given type scheme
-- TODO: Replace this with proper TIExpr construction from IInfer results
iexprToTIExprSimple :: Types.TypeScheme -> IExpr -> TIExpr
iexprToTIExprSimple scheme expr = case expr of
  -- For now, we extract the type from scheme and create a minimal TIExprNode
  -- This is not ideal but allows compilation to proceed
  _ -> 
    let ty = case scheme of
              Types.Forall _ _ t -> t
    in TIExpr scheme (iexprToTIExprNodeSimple ty expr)
  where
    -- Convert IExpr to TIExprNode recursively (simplified)
    iexprToTIExprNodeSimple :: Types.Type -> IExpr -> TIExprNode
    iexprToTIExprNodeSimple ty e = case e of
      IConstantExpr c -> TIConstantExpr c
      IVarExpr name -> TIVarExpr name
      
      -- Collections
      ITupleExpr exprs -> TITupleExpr (map (iexprToTIExprSimple (Types.Forall [] [] ty)) exprs)
      ICollectionExpr exprs -> TICollectionExpr (map (iexprToTIExprSimple (Types.Forall [] [] ty)) exprs)
      IConsExpr h t -> TIConsExpr
        (iexprToTIExprSimple (Types.Forall [] [] ty) h)
        (iexprToTIExprSimple (Types.Forall [] [] ty) t)
      IJoinExpr l r -> TIJoinExpr
        (iexprToTIExprSimple (Types.Forall [] [] ty) l)
        (iexprToTIExprSimple (Types.Forall [] [] ty) r)
      IHashExpr pairs -> TIHashExpr
        (map (\(k, v) -> (iexprToTIExprSimple (Types.Forall [] [] ty) k,
                          iexprToTIExprSimple (Types.Forall [] [] ty) v)) pairs)
      IVectorExpr exprs -> TIVectorExpr (map (iexprToTIExprSimple (Types.Forall [] [] ty)) exprs)
      
      -- Functions
      ILambdaExpr mvar vars body -> TILambdaExpr mvar vars (iexprToTIExprSimple (Types.Forall [] [] ty) body)
      IMemoizedLambdaExpr vars body -> TIMemoizedLambdaExpr vars (iexprToTIExprSimple (Types.Forall [] [] ty) body)
      ICambdaExpr var body -> TICambdaExpr var (iexprToTIExprSimple (Types.Forall [] [] ty) body)
      IApplyExpr func args -> TIApplyExpr 
        (iexprToTIExprSimple (Types.Forall [] [] ty) func)
        (map (iexprToTIExprSimple (Types.Forall [] [] ty)) args)
      
      -- Control flow
      IIfExpr cond thenExpr elseExpr -> TIIfExpr
        (iexprToTIExprSimple (Types.Forall [] [] ty) cond)
        (iexprToTIExprSimple (Types.Forall [] [] ty) thenExpr)
        (iexprToTIExprSimple (Types.Forall [] [] ty) elseExpr)
      ISeqExpr e1 e2 -> TISeqExpr
        (iexprToTIExprSimple (Types.Forall [] [] ty) e1)
        (iexprToTIExprSimple (Types.Forall [] [] ty) e2)
      
      -- Let bindings
      ILetExpr bindings body -> TILetExpr 
        (map (\(v, e') -> (v, iexprToTIExprSimple (Types.Forall [] [] ty) e')) bindings)
        (iexprToTIExprSimple (Types.Forall [] [] ty) body)
      ILetRecExpr bindings body -> TILetRecExpr
        (map (\(v, e') -> (v, iexprToTIExprSimple (Types.Forall [] [] ty) e')) bindings)
        (iexprToTIExprSimple (Types.Forall [] [] ty) body)
      
      -- Pattern matching
      IMatchExpr mode target matcher clauses -> TIMatchExpr mode
        (iexprToTIExprSimple (Types.Forall [] [] ty) target)
        (iexprToTIExprSimple (Types.Forall [] [] ty) matcher)
        (map (\(pat, body) -> (pat, iexprToTIExprSimple (Types.Forall [] [] ty) body)) clauses)
      IMatchAllExpr mode target matcher clauses -> TIMatchAllExpr mode
        (iexprToTIExprSimple (Types.Forall [] [] ty) target)
        (iexprToTIExprSimple (Types.Forall [] [] ty) matcher)
        (map (\(pat, body) -> (pat, iexprToTIExprSimple (Types.Forall [] [] ty) body)) clauses)
      IMatcherExpr patDefs -> TIMatcherExpr
        (map (\(ppPat, nextMatcher, dataClauses) ->
          let nextMatcherTI = iexprToTIExprSimple (Types.Forall [] [] ty) nextMatcher
              dataClausesTI = map (\(pdPat, expr) -> 
                (pdPat, iexprToTIExprSimple (Types.Forall [] [] ty) expr)) dataClauses
          in (ppPat, nextMatcherTI, dataClausesTI)) patDefs)
      
      -- Inductive data
      IInductiveDataExpr name exprs -> TIInductiveDataExpr name
        (map (iexprToTIExprSimple (Types.Forall [] [] ty)) exprs)
      
      -- Tensors
      IGenerateTensorExpr shape func -> TIGenerateTensorExpr
        (iexprToTIExprSimple (Types.Forall [] [] ty) shape)
        (iexprToTIExprSimple (Types.Forall [] [] ty) func)
      ITensorExpr shape elems -> TITensorExpr
        (iexprToTIExprSimple (Types.Forall [] [] ty) shape)
        (iexprToTIExprSimple (Types.Forall [] [] ty) elems)
      ITensorContractExpr tensor -> TITensorContractExpr
        (iexprToTIExprSimple (Types.Forall [] [] ty) tensor)
      ITensorMapExpr func tensor -> TITensorMapExpr
        (iexprToTIExprSimple (Types.Forall [] [] ty) func)
        (iexprToTIExprSimple (Types.Forall [] [] ty) tensor)
      ITensorMap2Expr func t1 t2 -> TITensorMap2Expr
        (iexprToTIExprSimple (Types.Forall [] [] ty) func)
        (iexprToTIExprSimple (Types.Forall [] [] ty) t1)
        (iexprToTIExprSimple (Types.Forall [] [] ty) t2)
      ITransposeExpr tensor perm -> TITransposeExpr
        (iexprToTIExprSimple (Types.Forall [] [] ty) tensor)
        (iexprToTIExprSimple (Types.Forall [] [] ty) perm)
      IFlipIndicesExpr tensor -> TIFlipIndicesExpr
        (iexprToTIExprSimple (Types.Forall [] [] ty) tensor)
      
      -- Other
      IWithSymbolsExpr syms body -> TIWithSymbolsExpr syms
        (iexprToTIExprSimple (Types.Forall [] [] ty) body)
      IDoExpr bindings body -> TIDoExpr
        (map (\(v, e') -> (v, iexprToTIExprSimple (Types.Forall [] [] ty) e')) bindings)
        (iexprToTIExprSimple (Types.Forall [] [] ty) body)
      IQuoteExpr inner -> TIQuoteExpr (iexprToTIExprSimple (Types.Forall [] [] ty) inner)
      IQuoteSymbolExpr inner -> TIQuoteSymbolExpr (iexprToTIExprSimple (Types.Forall [] [] ty) inner)
      IWedgeApplyExpr func args -> TIWedgeApplyExpr
        (iexprToTIExprSimple (Types.Forall [] [] ty) func)
        (map (iexprToTIExprSimple (Types.Forall [] [] ty)) args)
      IFunctionExpr names -> TIFunctionExpr names
      
      -- Indexed expressions (keep indices as IExpr for now)
      IIndexedExpr b base indices -> TIIndexedExpr b
        (iexprToTIExprSimple (Types.Forall [] [] ty) base)
        indices
      ISubrefsExpr b base ref -> TISubrefsExpr b
        (iexprToTIExprSimple (Types.Forall [] [] ty) base)
        (iexprToTIExprSimple (Types.Forall [] [] ty) ref)
      ISuprefsExpr b base ref -> TISuprefsExpr b
        (iexprToTIExprSimple (Types.Forall [] [] ty) base)
        (iexprToTIExprSimple (Types.Forall [] [] ty) ref)
      IUserrefsExpr b base ref -> TIUserrefsExpr b
        (iexprToTIExprSimple (Types.Forall [] [] ty) base)
        (iexprToTIExprSimple (Types.Forall [] [] ty) ref)

-- NOTE: iTopExprToTITopExprFromScheme and iexprToTIExprSimple are no longer needed
-- since inferITopExpr now returns TITopExpr directly

-- | Expand type class methods in a top-level expression
expandTypeClassInTopExpr :: TITopExpr -> EvalM TITopExpr
expandTypeClassInTopExpr tiTopExpr = case tiTopExpr of
  TIDefine scheme var tiExpr -> do
    -- First expand methods in the expression
    tiExpr' <- expandTypeClassMethodsT tiExpr
    -- Then add dictionary parameters if there are constraints
    tiExpr'' <- addDictionaryParametersT scheme tiExpr'
    return $ TIDefine scheme var tiExpr''
  TITest tiExpr -> do
    tiExpr' <- expandTypeClassMethodsT tiExpr
    return $ TITest tiExpr'
  TIExecute tiExpr -> do
    tiExpr' <- expandTypeClassMethodsT tiExpr
    return $ TIExecute tiExpr'
  TILoadFile path -> return $ TILoadFile path
  TILoad lib -> return $ TILoad lib
  TIDefineMany bindings -> do
    bindings' <- mapM (\(var, tiExpr) -> do
      tiExpr' <- expandTypeClassMethodsT tiExpr
      return (var, tiExpr')) bindings
    return $ TIDefineMany bindings'

evalExpandedTopExprsTyped' :: Env -> [TopExpr] -> Bool -> Bool -> EvalM (Maybe EgisonValue, Env)
evalExpandedTopExprsTyped' env exprs printValues shouldDumpTyped = do
  opts <- ask
  
  --------------------------------------------------------------------------------
  -- Phase 2: Environment Building Phase (完全に独立したフェーズ)
  --------------------------------------------------------------------------------
  -- Collect ALL environment information BEFORE type inference begins:
  --   1. Data constructor definitions (from InductiveDecl)
  --   2. Type class definitions (from ClassDeclExpr)
  --   3. Instance definitions (from InstanceDeclExpr)
  --   4. Type signatures (from DefineWithType)
  envResult <- buildEnvironments exprs
  
  -- Merge builtinEnv (primitive functions) with user-defined types
  -- builtinEnv is the base, user definitions extend it (can override primitives)
  let initialTypeEnv = extendEnvMany (envToList (ebrTypeEnv envResult)) builtinEnv
  
  -- Update EvalState with collected environments (including builtinEnv)
  setTypeEnv initialTypeEnv
  setClassEnv (ebrClassEnv envResult)
  
  -- Register constructors to EvalState
  forM_ (HashMap.toList (ebrConstructorEnv envResult)) $ \(ctorName, ctorInfo) ->
    registerConstructor ctorName ctorInfo
  
  -- Dump environment if requested
  when (optDumpEnv opts) $ do
    dumpEnvironment initialTypeEnv (ebrClassEnv envResult) (ebrConstructorEnv envResult) 
                    (ebrPatternConstructorEnv envResult) (ebrPatternTypeEnv envResult)
  
  -- Dump desugared AST if requested
  when (optDumpDesugared opts) $ do
    desugaredExprs <- desugarTopExprs exprs
    dumpDesugared (map Just desugaredExprs)
  
  -- Get the environments for type inference
  -- Permissive mode allows falling back to untyped evaluation on type errors
  let permissive = not (optTypeCheckStrict opts)
  
  -- Process each expression sequentially through phases 3-10, accumulating environments
  -- Also collect typed ASTs if dump-typed is enabled
  ((lastVal, finalEnv), typedExprs) <- foldM (\((lastVal, currentEnv), typedExprs) expr -> do
    -- Get current type and class environments from EvalState
    currentTypeEnv <- getTypeEnv
    currentClassEnv <- getClassEnv
    
    -- Phase 3-4: Desugar (TopExpr → ITopExpr)
    mITopExpr <- desugarTopExpr expr
    
    case mITopExpr of
      Nothing -> return ((lastVal, currentEnv), typedExprs)  -- No desugared output
      Just iTopExpr -> do
        -- Phase 5-6: Type Inference (ITopExpr → TypedITopExpr)
        let inferConfig = if permissive then permissiveInferConfig else defaultInferConfig
        let initState = (initialInferStateWithConfig inferConfig) { 
              inferEnv = currentTypeEnv,
              inferClassEnv = currentClassEnv,
              inferPatternEnv = ebrPatternConstructorEnv envResult
            }
        (result, warnings, finalState) <- liftIO $ 
          runInferWithWarningsAndState (inferITopExpr iTopExpr) initState
        
        let updatedTypeEnv = inferEnv finalState
        let updatedClassEnv = inferClassEnv finalState
    
        -- Print type warnings if any
        when (not (null warnings)) $ do
          liftIO $ mapM_ (hPutStrLn stderr . formatTypeWarning) warnings
        
        -- Update type and class environments in EvalState
        setTypeEnv updatedTypeEnv
        setClassEnv updatedClassEnv
        
        case result of
          Left err -> do
            liftIO $ hPutStrLn stderr $ "Type error:\n" ++ formatTypeError err
            -- Fallback: Use untyped evaluation if type checking fails (permissive mode)
            topExpr' <- desugarTopExpr expr
            case topExpr' of
              Nothing -> return ((lastVal, currentEnv), typedExprs)
              Just topExpr'' -> do
                (mVal, env') <- evalTopExpr' currentEnv topExpr''
                when printValues $ case mVal of
                  Nothing -> return ()
                  Just val -> valueToStr val >>= liftIO . putStrLn
                return ((mVal, env'), typedExprs)
          
          Right (Nothing, _subst) -> 
            -- No code generated (e.g., load statements that are already processed)
            return ((lastVal, currentEnv), typedExprs)
          
          Right (Just tiTopExpr, _subst) -> do
            -- Phase 7: inferITopExpr now returns TITopExpr directly
            -- No need for separate conversion
            
            -- Phase 8: Type Class Expansion (TypedDesugar)
            -- Expand type class method calls to dictionary-based dispatch
            tiTopExprExpanded <- expandTypeClassInTopExpr tiTopExpr
            
            -- Extract ITopExpr for evaluation
            let iTopExprExpanded = stripTypeTopExpr tiTopExprExpanded
            
            -- Collect typed AST for dumping if requested (use expanded version)
            let typedExprs' = if optDumpTyped opts then typedExprs ++ [Just tiTopExprExpanded] else typedExprs
            
            -- Type scheme is already in the environment (added by inferITopExpr), no need to add again
            
            -- Phase 9-10: Evaluation using IExpr (type info stripped)
            -- Catch errors during evaluation to preserve typedExprs for dumping
            evalResult <- catchError
              (Right <$> evalTopExpr' currentEnv iTopExprExpanded)
              (\err -> do
                liftIO $ hPutStrLn stderr $ "Evaluation error (continuing to preserve typed AST): " ++ show err
                return $ Left err)
            
            case evalResult of
              Left _ -> 
                -- Error occurred, but preserve typedExprs for dumping
                return ((lastVal, currentEnv), typedExprs')
              Right (mVal, env') -> do
                -- Print value if requested
                when printValues $ case mVal of
                  Nothing -> return ()
                  Just val -> valueToStr val >>= liftIO . putStrLn
                return ((mVal, env'), typedExprs')
    ) ((Nothing, env), []) exprs
  
  -- Dump typed AST if requested and shouldDumpTyped is True
  when (optDumpTyped opts && shouldDumpTyped) $ do
    dumpTyped typedExprs
  
  return (lastVal, finalEnv)

--------------------------------------------------------------------------------
-- Phase 2 Helper: Environment Building (moved to EnvBuilder module)
--------------------------------------------------------------------------------
-- | Evaluate an Egison top expression.
evalTopExprStr :: Env -> TopExpr -> EvalM (Maybe String, Env)
evalTopExprStr env topExpr = do
  (val, env') <- evalTopExpr env topExpr
  case val of
    Nothing  -> return (Nothing, env')
    Just val -> do str <- valueToStr val
                   return (Just str, env')

valueToStr :: EgisonValue -> EvalM String
valueToStr val = do
  mathExpr <- asks optMathExpr
  case mathExpr of
    Nothing   -> return (show val)
    Just lang -> return (prettyMath lang val)

-- | Evaluate Egison top expressions.
-- Pipeline: ExpandLoads → TypeCheck → TypedDesugar → Eval
evalTopExprs :: Env -> [TopExpr] -> EvalM Env
evalTopExprs env exprs = evalTopExprs' env exprs True True

-- | Evaluate Egison top expressions with control over printing and dumping.
evalTopExprs' :: Env -> [TopExpr] -> Bool -> Bool -> EvalM Env
evalTopExprs' env exprs printValues shouldDumpTyped = do
  -- Expand all Load/LoadFile recursively
  expanded <- expandLoads exprs
  -- Evaluate using typed pipeline with printing
  (_, env') <- evalExpandedTopExprsTyped' env expanded printValues shouldDumpTyped
  return env'

-- | Evaluate Egison top expressions without printing.
-- Pipeline: ExpandLoads → TypeCheck → TypedDesugar → Eval
evalTopExprsNoPrint :: Env -> [TopExpr] -> EvalM Env
evalTopExprsNoPrint env exprs = evalTopExprs' env exprs False True

-- | Evaluate an Egison expression. Input is a Haskell string.
runExpr :: Env -> String -> EvalM EgisonValue
runExpr env input =
  readExpr input >>= evalExpr env

-- | Evaluate an Egison top expression. Input is a Haskell string.
runTopExpr :: Env -> String -> EvalM (Maybe EgisonValue, Env)
runTopExpr env input =
  readTopExpr input >>= evalTopExpr env

-- | Evaluate an Egison top expression. Input is a Haskell string.
runTopExprStr :: Env -> String -> EvalM (Maybe String, Env)
runTopExprStr env input =
  readTopExpr input >>= evalTopExprStr env

-- | Evaluate Egison top expressions. Input is a Haskell string.
runTopExprs :: Env -> String -> EvalM Env
runTopExprs env input =
  readTopExprs input >>= evalTopExprs env

-- | Load an Egison file.
loadEgisonFile :: Env -> FilePath -> EvalM Env
loadEgisonFile env path = do
  (_, env') <- evalTopExpr env (LoadFile path)
  return env'

-- | Load an Egison library.
loadEgisonLibrary :: Env -> FilePath -> EvalM Env
loadEgisonLibrary env path = do
  (_, env') <- evalTopExpr env (Load path)
  return env'


--
-- Helper functions
--

collectDefs :: EgisonOpts -> [ITopExpr] -> EvalM ([(Var, IExpr)], [ITopExpr])
collectDefs opts exprs = collectDefs' opts exprs [] []
  where
    collectDefs' :: EgisonOpts -> [ITopExpr] -> [(Var, IExpr)] -> [ITopExpr] -> EvalM ([(Var, IExpr)], [ITopExpr])
    collectDefs' opts (expr:exprs) bindings rest =
      case expr of
        IDefine name expr -> collectDefs' opts exprs ((name, expr) : bindings) rest
        IDefineMany defs  -> collectDefs' opts exprs (defs ++ bindings) rest
        ITest{}     -> collectDefs' opts exprs bindings (expr : rest)
        IExecute{}  -> collectDefs' opts exprs bindings (expr : rest)
        ILoadFile _ | optNoIO opts -> throwError (Default "No IO support")
        ILoadFile file -> do
          exprs' <- loadFile file >>= desugarTopExprs
          collectDefs' opts (exprs' ++ exprs) bindings rest
        ILoad _ | optNoIO opts -> throwError (Default "No IO support")
        ILoad file -> do
          exprs' <- loadLibraryFile file >>= desugarTopExprs
          collectDefs' opts (exprs' ++ exprs) bindings rest
    collectDefs' _ [] bindings rest = return (bindings, reverse rest)

evalTopExpr' :: Env -> ITopExpr -> EvalM (Maybe EgisonValue, Env)
evalTopExpr' env (IDefine name expr) = do
  env' <- recursiveBind env [(name, expr)]
  return (Nothing, env')
evalTopExpr' env (IDefineMany defs) = do
  env' <- recursiveBind env defs
  return (Nothing, env')
evalTopExpr' env (ITest expr) = do
  pushFuncName (stringToVar "<stdin>")
  val <- evalExprDeep env expr
  popFuncName
  return (Just val, env)
evalTopExpr' env (IExecute expr) = do
  pushFuncName (stringToVar "<stdin>")
  io <- evalExprShallow env expr
  case io of
    Value (IOFunc m) -> m >> popFuncName >> return (Nothing, env)
    _                -> throwErrorWithTrace (TypeMismatch "io" io)
evalTopExpr' env (ILoad file) = do
  opts <- ask
  when (optNoIO opts) $ throwError (Default "No IO support")
  exprs <- loadLibraryFile file >>= desugarTopExprs
  (bindings, _) <- collectDefs opts exprs
  env' <- recursiveBind env bindings
  return (Nothing, env')
evalTopExpr' env (ILoadFile file) = do
  opts <- ask
  when (optNoIO opts) $ throwError (Default "No IO support")
  exprs <- loadFile file >>= desugarTopExprs
  (bindings, _) <- collectDefs opts exprs
  env' <- recursiveBind env bindings
  return (Nothing, env')

--------------------------------------------------------------------------------
-- Environment Dumping
--------------------------------------------------------------------------------

-- | Dump environment information after Phase 2 (Environment Building)
dumpEnvironment :: TypeEnv -> ClassEnv -> ConstructorEnv -> PatternConstructorEnv -> PatternTypeEnv -> EvalM ()
dumpEnvironment typeEnv classEnv ctorEnv patternCtorEnv patternEnv = do
  liftIO $ do
    putStrLn "=== Environment Information (Phase 2: Environment Building) ==="
    putStrLn ""
    
    -- 1. Type Signatures
    putStrLn "--- Type Signatures ---"
    let typeBindings = envToList typeEnv
    if null typeBindings
      then putStrLn "  (none)"
      else forM_ typeBindings $ \(name, scheme) ->
        putStrLn $ "  " ++ name ++ " : " ++ prettyTypeScheme scheme
    putStrLn ""
    
    -- 2. Type Classes
    putStrLn "--- Type Classes ---"
    let classBindings = classEnvToList classEnv
    if null classBindings
      then putStrLn "  (none)"
      else forM_ classBindings $ \(className, classInfo) -> do
        let paramName = case Types.classParam classInfo of
              Types.TyVar name -> name
        putStrLn $ "  class " ++ className ++ " " ++ paramName ++ " where"
        forM_ (Types.classMethods classInfo) $ \(methName, methType) ->
          putStrLn $ "    " ++ methName ++ " : " ++ prettyType methType
    putStrLn ""
    
    -- 3. Instances
    putStrLn "--- Type Class Instances ---"
    let allInstances = concatMap (\(clsName, _) -> 
          map (\inst -> (clsName, inst)) (lookupInstances clsName classEnv)) classBindings
    if null allInstances
      then putStrLn "  (none)"
      else forM_ allInstances $ \(className, instInfo) -> do
        let contextStr = if null (Types.instContext instInfo)
              then ""
              else let showConstraint (Types.Constraint cls ty) = cls ++ " " ++ prettyType ty
                   in intercalate ", " (map showConstraint (Types.instContext instInfo)) ++ " => "
        putStrLn $ "  instance " ++ contextStr ++ className ++ " " ++ prettyType (Types.instType instInfo)
    putStrLn ""
    
    -- 4. Data Constructors
    putStrLn "--- Data Constructors ---"
    let ctorBindings = HashMap.toList ctorEnv
    if null ctorBindings
      then putStrLn "  (none)"
      else forM_ ctorBindings $ \(ctorName, ctorInfo) -> do
        let typeParams = ctorTypeParams ctorInfo
        let retType = if null typeParams
              then ctorTypeName ctorInfo
              else ctorTypeName ctorInfo ++ " " ++ unwords typeParams
        let ctorType = if null (ctorArgTypes ctorInfo)
              then retType
              else intercalate " -> " (map prettyType (ctorArgTypes ctorInfo) ++ [retType])
        putStrLn $ "  " ++ ctorName ++ " : " ++ ctorType
    putStrLn ""
    
    -- 5. Pattern Constructors
    putStrLn "--- Pattern Constructors ---"
    let patternCtorBindings = patternEnvToList patternCtorEnv
    if null patternCtorBindings
      then putStrLn "  (none)"
      else forM_ patternCtorBindings $ \(ctorName, scheme) ->
        putStrLn $ "  " ++ ctorName ++ " : " ++ prettyTypeScheme scheme
    putStrLn ""
    
    -- 6. Pattern Functions
    putStrLn "--- Pattern Functions ---"
    let patternBindings = patternEnvToList patternEnv
    if null patternBindings
      then putStrLn "  (none)"
      else forM_ patternBindings $ \(name, scheme) ->
        putStrLn $ "  " ++ name ++ " : " ++ prettyTypeScheme scheme
    putStrLn ""
    
    putStrLn "=== End of Environment Information ==="

-- | Dump desugared AST after Phase 3 (Desugaring)
dumpDesugared :: [Maybe ITopExpr] -> EvalM ()
dumpDesugared desugaredExprs = do
  liftIO $ do
    putStrLn "=== Desugared AST (Phase 3: Desugaring) ==="
    putStrLn ""
    if null desugaredExprs
      then putStrLn "  (none)"
      else forM_ (zip [1 :: Int ..] desugaredExprs) $ \(i :: Int, mExpr) ->
        case mExpr of
          Nothing -> putStrLn $ "  [" ++ show i ++ "] (skipped)"
          Just expr -> putStrLn $ "  [" ++ show i ++ "] " ++ prettyStr expr
    putStrLn ""
    putStrLn "=== End of Desugared AST ==="

-- | Dump typed AST after Phase 6 (Type Inference & Check)
dumpTyped :: [Maybe TITopExpr] -> EvalM ()
dumpTyped typedExprs = do
  liftIO $ do
    putStrLn "=== Typed AST (Phase 5-6: Type Inference) ==="
    putStrLn ""
    if null typedExprs
      then putStrLn "  (none)"
      else forM_ (zip [1 :: Int ..] typedExprs) $ \(i :: Int, mExpr) ->
        case mExpr of
          Nothing -> putStrLn $ "  [" ++ show i ++ "] (skipped)"
          Just expr -> do
            putStrLn $ "  [" ++ show i ++ "] " ++ prettyStr expr
    putStrLn ""
    putStrLn "=== End of Typed AST ==="

