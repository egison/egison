{- |
Module      : Language.Egison.Eval
Licence     : MIT

This module provides interface for evaluating Egison expressions.

Processing Flow (design/implementation.md):
  1. TopExpr (Parse result)
  2. expandLoads (File loading with caching)
  3. Environment Building Phase (Collect data constructors, type classes, instances, type signatures)
  4. Desugar (Syntactic desugaring)
  5-6. Type Inference Phase (Constraint generation, unification, TIExpr generation)
  7. TypedDesugar (Type-driven transformations: tensorMap insertion, type class expansion)
  8. Definition Binding (Recursive binding of all definitions)
  9. Evaluation (Pattern matching execution, expression evaluation, IO actions)
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
import           Language.Egison.Type.Infer (inferITopExpr, runInferWithWarningsAndState, InferState(..), initialInferStateWithConfig, permissiveInferConfig, defaultInferConfig)
import           Language.Egison.Type.Env (TypeEnv, ClassEnv, PatternTypeEnv, extendEnvMany, envToList, classEnvToList, lookupInstances, patternEnvToList, mergeClassEnv, extendPatternEnv)
import           Language.Egison.Type.TypeClassExpand ()
import           Language.Egison.Type.TypedDesugar (desugarTypedTopExprT_TensorMapOnly, desugarTypedTopExprT_TypeClassOnly)
import           Language.Egison.Type.Error (TypeError, formatTypeError, formatTypeWarning)
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

-- | Expand all Load/LoadFile statements recursively into a flat list of TopExprs.
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
evalTopExpr :: Env -> TopExpr -> EvalM (Maybe EgisonValue, Env)
evalTopExpr env topExpr = do
  expanded <- expandLoads [topExpr]
  evalExpandedTopExprsTyped env expanded

-- | Evaluate expanded top expressions using typed pipeline
evalExpandedTopExprsTyped :: Env -> [TopExpr] -> EvalM (Maybe EgisonValue, Env)
evalExpandedTopExprsTyped env exprs = evalExpandedTopExprsTyped' env exprs False True

--------------------------------------------------------------------------------
-- Pipeline Accumulator
--------------------------------------------------------------------------------

-- | Accumulator used during per-expression fold in phases 3-8.
-- Separates value bindings, pattern function bindings, non-definition expressions,
-- and optional dump lists for --dump-typed / --dump-ti / --dump-tc flags.
data PipelineAccum = PipelineAccum
  { accumBindings       :: [(Var, IExpr)]
  , accumPatFuncBindings :: [(String, IExpr)]
  , accumNonDefExprs    :: [(ITopExpr, Bool)]
  , accumTypedExprs     :: [Maybe TITopExpr]
  , accumTiExprs        :: [Maybe TITopExpr]
  , accumTcExprs        :: [Maybe TITopExpr]
  }

emptyAccum :: PipelineAccum
emptyAccum = PipelineAccum [] [] [] [] [] []

-- | Classify an ITopExpr into one of the accumulator bins.
classifyITopExpr :: ITopExpr -> Bool -> PipelineAccum -> PipelineAccum
classifyITopExpr iExpr printValues acc = case iExpr of
  IDefine name expr ->
    acc { accumBindings = accumBindings acc ++ [(name, expr)] }
  IDefineMany defs ->
    acc { accumBindings = accumBindings acc ++ defs }
  IPatternFunctionDecl name _tyVars params _retType body ->
    let paramNames = map fst params
        patternFuncExpr = IPatternFuncExpr paramNames body
    in acc { accumPatFuncBindings = accumPatFuncBindings acc ++ [(name, patternFuncExpr)] }
  _ ->
    acc { accumNonDefExprs = accumNonDefExprs acc ++ [(iExpr, printValues)] }

--------------------------------------------------------------------------------
-- Phase 2-10: Environment Building → Desugar → Type Inference/Check →
--             TypedDesugar → Evaluation
--------------------------------------------------------------------------------

-- | Evaluate expanded top expressions using the typed pipeline with optional printing.
evalExpandedTopExprsTyped' :: Env -> [TopExpr] -> Bool -> Bool -> EvalM (Maybe EgisonValue, Env)
evalExpandedTopExprsTyped' env exprs printValues shouldDumpTyped = do
  opts <- ask

  -- Phase 2: Environment Building
  buildAndMergeEnvironments exprs opts

  let permissive = not (optTypeCheckStrict opts)

  -- Phases 3-8: Desugar, type-infer, typed-desugar each expression
  accum <- foldM (processOneExpr opts permissive printValues) emptyAccum exprs

  -- Dump typed ASTs before evaluation
  when (optDumpTyped opts && shouldDumpTyped) $
    dumpPhaseExprs "Typed AST (Phase 5-6: Type Inference)" "End of Typed AST" (accumTypedExprs accum)
  when (optDumpTi opts && shouldDumpTyped) $
    dumpPhaseExprs "Typed AST after TensorMap Insertion (Phase 7a)" "End of TensorMap Insertion AST" (accumTiExprs accum)
  when (optDumpTc opts && shouldDumpTyped) $
    dumpPhaseExprs "Typed AST after Type Class Expansion (Phase 7b)" "End of Type Class Expansion AST" (accumTcExprs accum)

  -- Phase 8: Bind all definitions together (supports mutual recursion)
  envWithPatFuncs <- recursiveBindAll env (accumBindings accum) (accumPatFuncBindings accum)

  -- Phase 9: Evaluate non-definition expressions in order
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
    ) (Nothing, envWithPatFuncs) (accumNonDefExprs accum)

  return (lastVal, finalEnv)

--------------------------------------------------------------------------------
-- Phase 2: Environment Building & Merging
--------------------------------------------------------------------------------

buildAndMergeEnvironments :: [TopExpr] -> EgisonOpts -> EvalM ()
buildAndMergeEnvironments exprs opts = do
  currentTypeEnv <- getTypeEnv
  currentClassEnv <- getClassEnv
  currentPatternEnv <- getPatternEnv
  currentPatternFuncEnv <- getPatternFuncEnv

  envResult <- buildEnvironments exprs

  let newTypeEnv = ebrTypeEnv envResult
      baseTypeEnv = if null (envToList currentTypeEnv) then builtinEnv else currentTypeEnv
      mergedTypeEnv = extendEnvMany (envToList newTypeEnv) baseTypeEnv
      mergedClassEnv = mergeClassEnv currentClassEnv (ebrClassEnv envResult)
      patternConstructorEnv = ebrPatternConstructorEnv envResult
      newPatternFuncEnv = ebrPatternTypeEnv envResult
      mergedPatternEnv = foldr (\(name, scheme) e -> extendPatternEnv name scheme e)
                               (foldr (\(name, scheme) e -> extendPatternEnv name scheme e)
                                      currentPatternEnv
                                      (patternEnvToList patternConstructorEnv))
                               (patternEnvToList newPatternFuncEnv)
      mergedPatternFuncEnv = foldr (\(name, scheme) e -> extendPatternEnv name scheme e)
                                   currentPatternFuncEnv
                                   (patternEnvToList newPatternFuncEnv)

  setTypeEnv mergedTypeEnv
  setClassEnv mergedClassEnv
  setPatternEnv mergedPatternEnv
  setPatternFuncEnv mergedPatternFuncEnv

  forM_ (HashMap.toList (ebrConstructorEnv envResult)) $ \(ctorName, ctorInfo) ->
    registerConstructor ctorName ctorInfo

  when (optDumpEnv opts) $
    dumpEnvironment mergedTypeEnv mergedClassEnv (ebrConstructorEnv envResult)
                    (ebrPatternConstructorEnv envResult) (ebrPatternTypeEnv envResult)

  when (optDumpDesugared opts) $ do
    desugaredExprs <- desugarTopExprs exprs
    dumpDesugared (map Just desugaredExprs)

--------------------------------------------------------------------------------
-- Per-Expression Pipeline (Phases 3-8)
--------------------------------------------------------------------------------

processOneExpr :: EgisonOpts -> Bool -> Bool -> PipelineAccum -> TopExpr -> EvalM PipelineAccum
processOneExpr opts permissive printValues acc expr = do
  currentTypeEnv <- getTypeEnv
  currentClassEnv <- getClassEnv

  mITopExpr <- desugarTopExpr expr

  case mITopExpr of
    Nothing -> return acc
    Just iTopExpr -> do
      -- Phase 5-6: Type Inference
      let inferConfig = if permissive then permissiveInferConfig else defaultInferConfig
      currentPatternEnv' <- getPatternEnv
      currentPatternFuncEnv' <- getPatternFuncEnv
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

      when (not (null warnings)) $
        liftIO $ mapM_ (hPutStrLn stderr . formatTypeWarning) warnings

      setTypeEnv (inferEnv finalState)
      setClassEnv (inferClassEnv finalState)
      setPatternEnv (inferPatternEnv finalState)
      setPatternFuncEnv (inferPatternFuncEnv finalState)

      case result of
        Left err -> handleTypeError err acc expr printValues

        Right (Nothing, _subst) ->
          return acc

        Right (Just tiTopExpr, _subst) ->
          runTypedDesugaring opts acc tiTopExpr printValues

-- | Handle type error: fall back to untyped evaluation in permissive mode.
handleTypeError :: TypeError -> PipelineAccum -> TopExpr -> Bool -> EvalM PipelineAccum
handleTypeError err acc expr printValues = do
  liftIO $ hPutStrLn stderr $ "Type error:\n" ++ formatTypeError err
  topExpr' <- desugarTopExpr expr
  case topExpr' of
    Nothing      -> return acc
    Just iExpr   -> return $ classifyITopExpr iExpr printValues acc

-- | Run TensorMap insertion and TypeClass expansion (Phase 7a-7b),
-- then classify the resulting ITopExpr.
runTypedDesugaring :: EgisonOpts -> PipelineAccum -> TITopExpr -> Bool -> EvalM PipelineAccum
runTypedDesugaring opts acc tiTopExpr printValues = do
  let acc1 = if optDumpTyped opts
             then acc { accumTypedExprs = accumTypedExprs acc ++ [Just tiTopExpr] }
             else acc

  -- Phase 7a: TensorMap Insertion
  mAfterTensor <- desugarTypedTopExprT_TensorMapOnly tiTopExpr
  case mAfterTensor of
    Nothing -> return acc1
    Just afterTensor -> do
      let acc2 = if optDumpTi opts
                 then acc1 { accumTiExprs = accumTiExprs acc1 ++ [Just afterTensor] }
                 else acc1

      -- Phase 7b: Type Class Expansion
      mAfterTC <- desugarTypedTopExprT_TypeClassOnly afterTensor
      case mAfterTC of
        Nothing -> return acc2
        Just afterTC -> do
          let acc3 = if optDumpTc opts
                     then acc2 { accumTcExprs = accumTcExprs acc2 ++ [Just afterTC] }
                     else acc2
              iTopExprExpanded = stripTypeTopExpr afterTC
          return $ classifyITopExpr iTopExprExpanded printValues acc3

--------------------------------------------------------------------------------
-- Remaining public API
--------------------------------------------------------------------------------

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
evalTopExprs :: Env -> [TopExpr] -> EvalM Env
evalTopExprs env exprs = evalTopExprs' env exprs True True

-- | Evaluate Egison top expressions with control over printing and dumping.
evalTopExprs' :: Env -> [TopExpr] -> Bool -> Bool -> EvalM Env
evalTopExprs' env exprs printValues shouldDumpTyped = do
  expanded <- expandLoads exprs
  (_, env') <- evalExpandedTopExprsTyped' env expanded printValues shouldDumpTyped
  return env'

-- | Evaluate Egison top expressions without printing.
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

collectDefs :: EgisonOpts -> [ITopExpr] -> EvalM ([(Var, IExpr)], [(String, IExpr)], [ITopExpr])
collectDefs opts exprs = collectDefs' opts exprs [] [] []
  where
    collectDefs' :: EgisonOpts -> [ITopExpr] -> [(Var, IExpr)] -> [(String, IExpr)] -> [ITopExpr] -> EvalM ([(Var, IExpr)], [(String, IExpr)], [ITopExpr])
    collectDefs' opts (expr:exprs) bindings patFuncBindings rest =
      case expr of
        IDefine name expr -> collectDefs' opts exprs ((name, expr) : bindings) patFuncBindings rest
        IDefineMany defs  -> collectDefs' opts exprs (defs ++ bindings) patFuncBindings rest
        IPatternFunctionDecl name _tyVars params _retType body ->
          let paramNames = map fst params
              patternFuncExpr = IPatternFuncExpr paramNames body
          in collectDefs' opts exprs bindings ((name, patternFuncExpr) : patFuncBindings) rest
        ITest{}     -> collectDefs' opts exprs bindings patFuncBindings (expr : rest)
        IExecute{}  -> collectDefs' opts exprs bindings patFuncBindings (expr : rest)
        ILoadFile _ | optNoIO opts -> throwError (Default "No IO support")
        ILoadFile file -> do
          exprs' <- loadFile file >>= desugarTopExprs
          collectDefs' opts (exprs' ++ exprs) bindings patFuncBindings rest
        ILoad _ | optNoIO opts -> throwError (Default "No IO support")
        ILoad file -> do
          exprs' <- loadLibraryFile file >>= desugarTopExprs
          collectDefs' opts (exprs' ++ exprs) bindings patFuncBindings rest
        _ -> collectDefs' opts exprs bindings patFuncBindings rest
    collectDefs' _ [] bindings patFuncBindings rest = return (bindings, patFuncBindings, reverse rest)

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
  (bindings, patFuncBindings, _) <- collectDefs opts exprs
  env' <- recursiveBindAll env bindings patFuncBindings
  return (Nothing, env')
evalTopExpr' env (ILoadFile file) = do
  opts <- ask
  when (optNoIO opts) $ throwError (Default "No IO support")
  exprs <- loadFile file >>= desugarTopExprs
  (bindings, patFuncBindings, _) <- collectDefs opts exprs
  env' <- recursiveBindAll env bindings patFuncBindings
  return (Nothing, env')
evalTopExpr' env (IDeclareSymbol _names _mType) = do
  return (Nothing, env)
evalTopExpr' _env (IPatternFunctionDecl name _ _ _ _) = do
  throwError $ Default $ "Pattern function " ++ name ++ " should have been converted to IPatternFuncExpr"

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

-- | Generic dump for typed AST phases (--dump-typed, --dump-ti, --dump-tc).
dumpPhaseExprs :: String -> String -> [Maybe TITopExpr] -> EvalM ()
dumpPhaseExprs header footer exprs = liftIO $ do
  putStrLn $ "=== " ++ header ++ " ==="
  putStrLn ""
  if null exprs
    then putStrLn "  (none)"
    else forM_ (zip [1 :: Int ..] exprs) $ \(i :: Int, mExpr) ->
      case mExpr of
        Nothing -> putStrLn $ "  [" ++ show i ++ "] (skipped)"
        Just expr -> putStrLn $ "  [" ++ show i ++ "] " ++ prettyStr expr
  putStrLn ""
  putStrLn $ "=== " ++ footer ++ " ==="
