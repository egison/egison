{- |
Module      : Language.Egison.Eval
Licence     : MIT

This module provides interface for evaluating Egison expressions.

Pipeline: ExpandLoads → TypeCheck → TypedDesugar → Eval
-}

module Language.Egison.Eval
  (
  -- * Eval Egison expressions
    evalExpr
  , evalTopExpr
  , evalTopExprStr
  , evalTopExprs
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

import           Control.Monad              (foldM, forM, forM_, when)
import           Control.Monad.Except       (throwError)
import           Control.Monad.Reader       (ask, asks)
import           Control.Monad.State
import           Data.IORef                 (newIORef)
import           System.IO                  (hPutStrLn, stderr)

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Core
import           Language.Egison.Data
import           Language.Egison.Desugar (desugarExpr, desugarTopExpr, desugarTopExprs)
import           Language.Egison.EvalState  (MonadEval (..), ConstructorInfo(..))
import           Language.Egison.IExpr
import           Language.Egison.MathOutput (prettyMath)
import           Language.Egison.Parser
import           Language.Egison.Type.Types (Type(..), TyVar(..), TensorShape(..))
import           Language.Egison.Type.TypeInfer (runTypedInferTopExprWithEnv)
import           Language.Egison.EvalState (getTypeEnv, setTypeEnv, getClassEnv, setClassEnv, extendTypeEnv)
import           Language.Egison.Type.Env (generalize)
import           Language.Egison.Type.TypedDesugar (desugarTypedTopExprT)
import           Language.Egison.Type.TypedAST (TypedTopExpr(..), texprType)
import           Language.Egison.Type.Error (formatTypeWarning)


-- | Evaluate an Egison expression.
evalExpr :: Env -> Expr -> EvalM EgisonValue
evalExpr env expr = desugarExpr expr >>= evalExprDeep env

--
-- New Pipeline: ExpandLoads → TypeCheck → TypedDesugar → Eval
--
-- 1. expandLoads: Recursively expand Load/LoadFile into flat [TopExpr]
-- 2. For each TopExpr: TypeCheck → TypedDesugar → Eval
--

-- | Expand all Load/LoadFile statements recursively into a flat list of TopExprs
-- This is done BEFORE type checking so that all definitions are available
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

-- | Evaluate an Egison top expression.
-- Pipeline: ExpandLoads → TypeCheck → TypedDesugar → Eval
evalTopExpr :: Env -> TopExpr -> EvalM (Maybe EgisonValue, Env)
evalTopExpr env topExpr = do
  -- Expand all Load/LoadFile recursively
  expanded <- expandLoads [topExpr]
  -- Then evaluate all expanded expressions
  evalExpandedTopExprsTyped env expanded

-- | Evaluate expanded top expressions using typed pipeline
-- TODO: Implement type environment accumulation for proper type checking
evalExpandedTopExprsTyped :: Env -> [TopExpr] -> EvalM (Maybe EgisonValue, Env)
evalExpandedTopExprsTyped env exprs = evalExpandedTopExprsTyped' env exprs False

-- | Evaluate expanded top expressions using typed pipeline with optional printing
evalExpandedTopExprsTyped' :: Env -> [TopExpr] -> Bool -> EvalM (Maybe EgisonValue, Env)
evalExpandedTopExprsTyped' env exprs printValues = do
  opts <- ask
  
  -- Register all inductive constructors first
  forM_ exprs registerInductiveDecl
  
  -- Get initial type and class environments from EvalState
  initialTypeEnv <- getTypeEnv
  initialClassEnv <- getClassEnv
  
  -- Try to type check and desugar each expression, accumulating type environment
  let permissive = not (optTypeCheckStrict opts)
  
  -- Process each expression sequentially, accumulating type environment
  (lastVal, finalEnv) <- foldM (\(lastVal, currentEnv) expr -> do
    -- Get current type and class environments from EvalState
    currentTypeEnv <- getTypeEnv
    currentClassEnv <- getClassEnv
    
    -- Type check with current environments
    (result, warnings, updatedTypeEnv, updatedClassEnv) <- liftIO $ 
      runTypedInferTopExprWithEnv permissive expr currentTypeEnv currentClassEnv
    
    -- Print warnings
    when (not (null warnings)) $ do
      liftIO $ mapM_ (hPutStrLn stderr . formatTypeWarning) warnings
    
    -- Update type and class environments in EvalState
    setTypeEnv updatedTypeEnv
    setClassEnv updatedClassEnv
    
    case result of
      Left err -> do
        liftIO $ hPutStrLn stderr $ "Type error in expression: " ++ show err
        -- Fall back to untyped evaluation for this expression
        topExpr' <- desugarTopExpr expr
        case topExpr' of
          Nothing -> return (lastVal, currentEnv)
          Just topExpr'' -> do
            (mVal, env') <- evalTopExpr' currentEnv topExpr''
            when printValues $ case mVal of
              Nothing -> return ()
              Just val -> valueToStr val >>= liftIO . putStrLn
            return (mVal, env')
      Right Nothing -> 
        -- No code generated (e.g., InductiveDecl), continue
        return (lastVal, currentEnv)
      Right (Just typedTopExpr) -> do
        -- Add definition types to type environment before desugaring
        case typedTopExpr of
          TDefine varName typedExpr -> do
            typeEnv <- getTypeEnv
            let ty = texprType typedExpr
                typeScheme = generalize typeEnv ty
            extendTypeEnv varName typeScheme
          TDefineWithType varName _params retType _typedExpr -> do
            typeEnv <- getTypeEnv
            let ty = retType  -- Use explicit return type
                typeScheme = generalize typeEnv ty
            extendTypeEnv varName typeScheme
          _ -> return ()  -- Other expressions don't define variables
        
        -- TypedDesugar to TITopExpr
        mTiTopExpr <- desugarTypedTopExprT typedTopExpr
        case mTiTopExpr of
          Nothing -> return (lastVal, currentEnv)
          Just tiTopExpr -> do
            -- Evaluate TITopExpr
            (mVal, env') <- evalTITopExpr currentEnv tiTopExpr
            -- Print value if requested
            when printValues $ case mVal of
              Nothing -> return ()
              Just val -> valueToStr val >>= liftIO . putStrLn
            return (mVal, env')
    ) (Nothing, env) exprs
  
  return (lastVal, finalEnv)

-- | Evaluate a typed internal top expression
evalTITopExpr :: Env -> TITopExpr -> EvalM (Maybe EgisonValue, Env)
evalTITopExpr env tiTopExpr = case tiTopExpr of
  TIDefine ty var tiexpr -> do
    -- Add type to type environment
    typeEnv <- getTypeEnv
    let typeScheme = generalize typeEnv ty
    extendTypeEnv (extractNameFromVar var) typeScheme
    
    -- Evaluate using typed evaluation
    twhnf <- evalTIExprShallow env tiexpr
    ref <- liftIO $ newIORef $ WHNF (twhnfData twhnf)
    let env' = extendEnv env [(var, ref)]  -- Direct binding
    -- Recursive binding
    _ <- recursiveBind env' [(var, tiExpr tiexpr)]
    return (Nothing, env')
  
  TIDefineMany bindings -> do
    -- Add types to type environment
    typeEnv <- getTypeEnv
    forM_ bindings $ \(var, tiexpr) -> do
      let ty = tiType tiexpr
          typeScheme = generalize typeEnv ty
      extendTypeEnv (extractNameFromVar var) typeScheme
    
    refs <- forM bindings $ \(var, tiexpr) -> do
      twhnf <- evalTIExprShallow env tiexpr
      ref <- liftIO $ newIORef $ WHNF (twhnfData twhnf)
      return (var, ref)
    let env' = extendEnv env refs  -- Direct binding
    forM_ bindings $ \(var, tiexpr) -> recursiveBind env' [(var, tiExpr tiexpr)]
    return (Nothing, env')
  
  TITest tiexpr -> do
    (_ty, val) <- evalTIExprDeep env tiexpr
    return (Just val, env)
  
  TIExecute tiexpr -> do
    twhnf <- evalTIExprShallow env tiexpr
    -- Execute IO action
    _ <- evalWHNF (twhnfData twhnf)
    return (Nothing, env)
  
  -- These should not appear after expandLoads, but handle them anyway
  TILoadFile file -> do
    exprs <- loadFile file
    env' <- evalTopExprs env exprs
    return (Nothing, env')
  
  TILoad file -> do
    exprs <- loadLibraryFile file
    env' <- evalTopExprs env exprs
    return (Nothing, env')

-- | Register constructors from an InductiveDecl into the EvalState
registerInductiveDecl :: TopExpr -> EvalM ()
registerInductiveDecl (InductiveDecl typeName typeParams ctors) = do
  forM_ ctors $ \(InductiveConstructor ctorName argTypeExprs) -> do
    let argTypes = map typeExprToType argTypeExprs
        info = ConstructorInfo
          { ctorTypeName = typeName
          , ctorArgTypes = argTypes
          , ctorTypeParams = typeParams
          }
    registerConstructor ctorName info
registerInductiveDecl _ = return ()

-- | Convert TypeExpr to Type
typeExprToType :: TypeExpr -> Type
typeExprToType TEInt = TInt
typeExprToType TEMathExpr = TInt  -- MathExpr = Integer in Egison
typeExprToType TEFloat = TFloat
typeExprToType TEBool = TBool
typeExprToType TEChar = TChar
typeExprToType TEString = TString
typeExprToType (TEVar name) = TVar (TyVar name)
typeExprToType (TEList t) = TList (typeExprToType t)
typeExprToType (TETuple ts) = TTuple (map typeExprToType ts)
typeExprToType (TEFun t1 t2) = TFun (typeExprToType t1) (typeExprToType t2)
typeExprToType (TEApp t1 ts) = 
  case typeExprToType t1 of
    TInductive name existingTs -> TInductive name (existingTs ++ map typeExprToType ts)
    _ -> TAny  -- Fallback
typeExprToType (TEMatcher t) = TMatcher (typeExprToType t)
typeExprToType (TEPattern t) = TPattern (typeExprToType t)
typeExprToType (TEIO t) = TIO (typeExprToType t)
typeExprToType (TETensor elemT _ _) = TTensor (typeExprToType elemT) ShapeUnknown []
typeExprToType (TEConstrained _ t) = typeExprToType t  -- Ignore constraints for now

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
evalTopExprs env exprs = do
  -- Expand all Load/LoadFile recursively
  expanded <- expandLoads exprs
  -- Evaluate using typed pipeline with printing
  (_, env') <- evalExpandedTopExprsTyped' env expanded True
  return env'

-- | Evaluate Egison top expressions without printing.
-- Pipeline: ExpandLoads → TypeCheck → TypedDesugar → Eval
evalTopExprsNoPrint :: Env -> [TopExpr] -> EvalM Env
evalTopExprsNoPrint env exprs = do
  -- Expand all Load/LoadFile recursively
  expanded <- expandLoads exprs
  -- Evaluate using typed pipeline
  (_, env') <- evalExpandedTopExprsTyped env expanded
  return env'

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

