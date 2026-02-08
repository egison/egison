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
  8. TypedDesugar (Type-driven transformations: type class expansion, tensorMap insertion)
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
import           Data.List                  (intercalate, partition)
import           Control.Monad.Except       (throwError, catchError)
import           Control.Monad.Reader       (ask, asks)
import           Control.Monad.State
import           System.IO                  (hPutStrLn, stderr)

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Core
import           Language.Egison.Data
import           Language.Egison.Data.Utils     (newEvaluatedObjectRef)
import           Language.Egison.Desugar (desugarExpr, desugarTopExpr, desugarTopExprs)
import           Language.Egison.EnvBuilder (buildEnvironments, EnvBuildResult(..))
import           Language.Egison.EvalState  (MonadEval (..), ConstructorEnv, PatternConstructorEnv)
import           Language.Egison.IExpr (TITopExpr(..), ITopExpr(..), IExpr(..), Var(..), stringToVar, stripTypeTopExpr)
import           Language.Egison.MathOutput (prettyMath)
import           Language.Egison.Parser
import qualified Language.Egison.Type.Types as Types
import           Language.Egison.Type.IInfer (inferITopExpr, runInferWithWarningsAndState, InferState(..), initialInferStateWithConfig, permissiveInferConfig, defaultInferConfig)
import           Language.Egison.Type.Env (TypeEnv, ClassEnv, PatternTypeEnv, extendEnvMany, envToList, classEnvToList, lookupInstances, patternEnvToList, mergeClassEnv, extendPatternEnv)
import           Language.Egison.Type.TypeClassExpand ()
import           Language.Egison.Type.TypedDesugar (desugarTypedTopExprT_TensorMapOnly, desugarTypedTopExprT_TypeClassOnly)
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
  
  -- Get existing environments (may contain previously loaded libraries)
  currentTypeEnv <- getTypeEnv
  currentClassEnv <- getClassEnv
  currentPatternEnv <- getPatternEnv

  -- Build environments from current expressions
  envResult <- buildEnvironments exprs

  -- Merge existing environments with newly built environments
  -- New definitions extend existing ones (can override)
  let newTypeEnv = ebrTypeEnv envResult
      -- If currentTypeEnv is empty, use builtinEnv as base
      baseTypeEnv = if null (envToList currentTypeEnv) then builtinEnv else currentTypeEnv
      mergedTypeEnv = extendEnvMany (envToList newTypeEnv) baseTypeEnv
      mergedClassEnv = mergeClassEnv currentClassEnv (ebrClassEnv envResult)
      -- Merge pattern environments (new definitions can override)
      -- Pattern constructors from ebrPatternConstructorEnv and pattern functions from ebrPatternTypeEnv
      patternConstructorEnv = ebrPatternConstructorEnv envResult
      newPatternFuncEnv = ebrPatternTypeEnv envResult
  
  -- Get current pattern function environment
  currentPatternFuncEnv <- getPatternFuncEnv
  
  let -- Merge both into a single pattern environment
      mergedPatternEnv = foldr (\(name, scheme) env -> extendPatternEnv name scheme env) 
                               (foldr (\(name, scheme) env -> extendPatternEnv name scheme env)
                                      currentPatternEnv
                                      (patternEnvToList patternConstructorEnv))
                               (patternEnvToList newPatternFuncEnv)
      -- Also update pattern function environment separately
      mergedPatternFuncEnv = foldr (\(name, scheme) env -> extendPatternEnv name scheme env)
                                   currentPatternFuncEnv
                                   (patternEnvToList newPatternFuncEnv)

  -- Update EvalState with merged environments
  setTypeEnv mergedTypeEnv
  setClassEnv mergedClassEnv
  setPatternEnv mergedPatternEnv
  setPatternFuncEnv mergedPatternFuncEnv
  
  -- Register constructors to EvalState
  forM_ (HashMap.toList (ebrConstructorEnv envResult)) $ \(ctorName, ctorInfo) ->
    registerConstructor ctorName ctorInfo
  
  -- Dump environment if requested
  when (optDumpEnv opts) $ do
    dumpEnvironment mergedTypeEnv mergedClassEnv (ebrConstructorEnv envResult) 
                    (ebrPatternConstructorEnv envResult) (ebrPatternTypeEnv envResult)
  
  -- Dump desugared AST if requested
  when (optDumpDesugared opts) $ do
    desugaredExprs <- desugarTopExprs exprs
    dumpDesugared (map Just desugaredExprs)
  
  -- Get the environments for type inference
  -- Permissive mode allows falling back to untyped evaluation on type errors
  let permissive = not (optTypeCheckStrict opts)
  
  -- Process each expression sequentially through phases 3-8 (type inference and desugaring)
  -- Collect all definitions to bind them together later (Phase 9)
  -- Non-definition expressions (ITest, IExecute) will be evaluated in Phase 10
  -- Also collect typed ASTs if dump-typed, dump-ti, or dump-tc is enabled
  ((allBindings, nonDefExprs), typedExprs, tiExprs, tcExprs) <- foldM (\((bindings, nonDefs), typedExprs, tiExprs, tcExprs) expr -> do
    -- Get current type and class environments from EvalState
    currentTypeEnv <- getTypeEnv
    currentClassEnv <- getClassEnv
    
    -- Phase 3-4: Desugar (TopExpr → ITopExpr)
    mITopExpr <- desugarTopExpr expr
    
    case mITopExpr of
      Nothing -> return ((bindings, nonDefs), typedExprs, tiExprs, tcExprs)  -- No desugared output
      Just iTopExpr -> do
        -- Phase 5-6: Type Inference (ITopExpr → TypedITopExpr)
        let inferConfig = if permissive then permissiveInferConfig else defaultInferConfig
        -- Get the current pattern environment from EvalState
        currentPatternEnv' <- getPatternEnv
        currentPatternFuncEnv' <- getPatternFuncEnv
        -- Add pattern function types to inferEnv so they can be referenced as variables
        let patternFuncBindings = [(stringToVar name, scheme) | (name, scheme) <- patternEnvToList currentPatternFuncEnv']
            enrichedTypeEnv = extendEnvMany patternFuncBindings currentTypeEnv
            initState = (initialInferStateWithConfig inferConfig) {
              inferEnv = enrichedTypeEnv,
              inferClassEnv = currentClassEnv,
              inferPatternEnv = currentPatternEnv',
              inferPatternFuncEnv = currentPatternFuncEnv'
            }
        (result, warnings, finalState) <- liftIO $ 
          runInferWithWarningsAndState (inferITopExpr iTopExpr) initState
        
        let updatedTypeEnv = inferEnv finalState
        let updatedClassEnv = inferClassEnv finalState
        let updatedPatternEnv = inferPatternEnv finalState
        let updatedPatternFuncEnv = inferPatternFuncEnv finalState
    
        -- Print type warnings if any
        when (not (null warnings)) $ do
          liftIO $ mapM_ (hPutStrLn stderr . formatTypeWarning) warnings
        
        -- Update type, class, and pattern environments in EvalState
        setTypeEnv updatedTypeEnv
        setClassEnv updatedClassEnv
        setPatternEnv updatedPatternEnv
        setPatternFuncEnv updatedPatternFuncEnv
        
        case result of
          Left err -> do
            liftIO $ hPutStrLn stderr $ "Type error:\n" ++ formatTypeError err
            -- Fallback: Use untyped evaluation if type checking fails (permissive mode)
            -- Type errors are handled immediately, not collected
            topExpr' <- desugarTopExpr expr
            case topExpr' of
              Nothing -> return ((bindings, nonDefs), typedExprs, tiExprs, tcExprs)
              Just topExpr'' -> do
                -- Evaluate type-error expressions immediately (not collected)
                -- This is a fallback for permissive mode
                case topExpr'' of
                  IDefine name expr ->
                    return ((bindings ++ [(name, expr)], nonDefs), typedExprs, tiExprs, tcExprs)
                  IDefineMany defs ->
                    return ((bindings ++ defs, nonDefs), typedExprs, tiExprs, tcExprs)
                  _ ->
                    -- Non-definition: collect for later evaluation
                    return ((bindings, nonDefs ++ [(topExpr'', printValues)]), typedExprs, tiExprs, tcExprs)

          Right (Nothing, _subst) ->
            -- No code generated (e.g., load statements that are already processed)
            return ((bindings, nonDefs), typedExprs, tiExprs, tcExprs)
          
          Right (Just tiTopExpr, _subst) -> do
            -- Phase 7: inferITopExpr now returns TITopExpr directly
            -- No need for separate conversion

            -- Collect typed AST for --dump-typed (Phase 6: after type inference, before TypedDesugar)
            let typedExprs' = if optDumpTyped opts then typedExprs ++ [Just tiTopExpr] else typedExprs

            -- Phase 8a: TensorMap Insertion
            -- Insert tensorMap where needed (scalar vs tensor argument type conversion)
            mTiTopExprAfterTensorMap <- desugarTypedTopExprT_TensorMapOnly tiTopExpr

            case mTiTopExprAfterTensorMap of
              Nothing ->
                -- Load/LoadFile statements - no evaluation needed
                return ((bindings, nonDefs), typedExprs', tiExprs, tcExprs)

              Just tiTopExprAfterTensorMap -> do
                -- Collect TensorMap-inserted AST for --dump-ti (after TensorMap insertion)
                let tiExprs' = if optDumpTi opts then tiExprs ++ [Just tiTopExprAfterTensorMap] else tiExprs

                -- Phase 8b: Type Class Expansion
                -- Expand type class method calls to dictionary-based dispatch
                mTcTopExprAfterTypeClass <- desugarTypedTopExprT_TypeClassOnly tiTopExprAfterTensorMap

                case mTcTopExprAfterTypeClass of
                  Nothing ->
                    -- Load/LoadFile statements - no evaluation needed
                    return ((bindings, nonDefs), typedExprs', tiExprs', tcExprs)

                  Just tcTopExprAfterTypeClass -> do
                    -- Collect TypeClass-expanded AST for --dump-tc (after TypeClass expansion)
                    let tcExprs' = if optDumpTc opts then tcExprs ++ [Just tcTopExprAfterTypeClass] else tcExprs

                    -- Extract ITopExpr for evaluation
                    let iTopExprExpanded = stripTypeTopExpr tcTopExprAfterTypeClass

                    -- Type scheme is already in the environment (added by inferITopExpr), no need to add again

                    -- Phase 9-10: Collect definitions and non-definitions
                    -- Definitions will be bound together using recursiveBind to support mutual recursion
                    -- Non-definitions will be evaluated sequentially after all definitions are bound
                    case iTopExprExpanded of
                      IDefine name expr ->
                        -- Collect definition for later binding
                        return ((bindings ++ [(name, expr)], nonDefs), typedExprs', tiExprs', tcExprs')
                      IDefineMany defs ->
                        -- Collect multiple definitions for later binding
                        return ((bindings ++ defs, nonDefs), typedExprs', tiExprs', tcExprs')
                      IPatternFunctionDecl _ _ _ _ _ ->
                        -- Pattern functions are handled specially: they need to be evaluated immediately
                        -- so they are available in the environment when subsequent definitions reference them
                        return ((bindings, nonDefs ++ [(iTopExprExpanded, printValues)]), typedExprs', tiExprs', tcExprs')
                      _ ->
                        -- Non-definition expressions (ITest, IExecute)
                        -- Collect for evaluation after all definitions are bound
                        return ((bindings, nonDefs ++ [(iTopExprExpanded, printValues)]), typedExprs', tiExprs', tcExprs')
    ) (([], []), [], [], []) exprs

  -- Dump typed AST BEFORE evaluation (so dumps are available even if evaluation fails)
  -- This is important for debugging - we want to see the typed AST even when there are runtime errors
  when (optDumpTyped opts && shouldDumpTyped) $ do
    dumpTyped typedExprs

  when (optDumpTi opts && shouldDumpTyped) $ do
    dumpTi tiExprs

  when (optDumpTc opts && shouldDumpTyped) $ do
    dumpTc tcExprs

  -- Phase 9a: Evaluate pattern function declarations first
  -- Pattern functions need to be in the environment before other definitions are bound
  -- so that definitions can reference them in pattern matching
  let isPatternFuncDecl (IPatternFunctionDecl{}, _) = True
      isPatternFuncDecl _ = False
      (patternFuncs, otherNonDefs) = partition isPatternFuncDecl nonDefExprs
  
  envWithPatternFuncs <- foldM (\currentEnv (iExpr, _) -> do
      (_, env'') <- evalTopExpr' currentEnv iExpr
      return env''
    ) env patternFuncs

  -- Phase 9b: Bind all definitions together using recursiveBind
  -- This ensures mutual recursion works correctly (e.g., length can reference foldl even if defined earlier)
  env' <- recursiveBind envWithPatternFuncs allBindings

  -- Phase 10: Evaluate non-definition expressions in order (excluding pattern functions, already evaluated)
  (lastVal, finalEnv) <- foldM (\(lastVal, currentEnv) (iExpr, shouldPrint) -> do
      evalResult <- catchError
        (Right <$> evalTopExpr' currentEnv iExpr)
        (\err -> do
          liftIO $ hPutStrLn stderr $ "Evaluation error: " ++ show err
          return $ Left err)

      case evalResult of
        Left _ -> return (lastVal, currentEnv)
        Right (mVal, env'') -> do
          when shouldPrint $ case mVal of
            Nothing -> return ()
            Just val -> valueToStr val >>= liftIO . putStrLn
          return (mVal, env'')
    ) (Nothing, env') otherNonDefs

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
evalTopExpr' env (IDeclareSymbol _names _mType) = do
  -- Symbol declarations are only used during type inference
  -- At runtime, they don't produce any value or modify the environment
  return (Nothing, env)
evalTopExpr' env (IPatternFunctionDecl name _tyVars params _retType body) = do
  -- Pattern function declarations create a PatternFunc value and bind it to the function name
  -- This allows the pattern function to be used in pattern matching
  let paramNames = map fst params
      patternFunc = PatternFunc env paramNames body
      var = stringToVar name
  ref <- newEvaluatedObjectRef (Value patternFunc)
  let env' = extendEnv env [(var, ref)]
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
      else forM_ typeBindings $ \(Var varName indices, scheme) ->
        let displayName = if null indices 
                          then varName
                          else varName ++ concatMap (const "_") indices
        in putStrLn $ "  " ++ displayName ++ " : " ++ prettyTypeScheme scheme
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

dumpTi :: [Maybe TITopExpr] -> EvalM ()
dumpTi tiExprs = do
  liftIO $ do
    putStrLn "=== Typed AST after TensorMap Insertion (Phase 8a) ==="
    putStrLn ""
    if null tiExprs
      then putStrLn "  (none)"
      else forM_ (zip [1 :: Int ..] tiExprs) $ \(i :: Int, mExpr) ->
        case mExpr of
          Nothing -> putStrLn $ "  [" ++ show i ++ "] (skipped)"
          Just expr -> do
            putStrLn $ "  [" ++ show i ++ "] " ++ prettyStr expr
    putStrLn ""
    putStrLn "=== End of TensorMap Insertion AST ==="

dumpTc :: [Maybe TITopExpr] -> EvalM ()
dumpTc tcExprs = do
  liftIO $ do
    putStrLn "=== Typed AST after Type Class Expansion (Phase 8b) ==="
    putStrLn ""
    if null tcExprs
      then putStrLn "  (none)"
      else forM_ (zip [1 :: Int ..] tcExprs) $ \(i :: Int, mExpr) ->
        case mExpr of
          Nothing -> putStrLn $ "  [" ++ show i ++ "] (skipped)"
          Just expr -> do
            putStrLn $ "  [" ++ show i ++ "] " ++ prettyStr expr
    putStrLn ""
    putStrLn "=== End of Type Class Expansion AST ==="

