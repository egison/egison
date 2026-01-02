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

import           Control.Monad              (forM, forM_, when)
import           Control.Monad.Except       (throwError)
import           Control.Monad.Reader       (ask, asks)
import           Control.Monad.State
import           Data.IORef                 (newIORef)
import           System.IO                  (hPutStrLn, stderr)

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Core
import           Language.Egison.Data
import           Language.Egison.Desugar
import           Language.Egison.EvalState  (MonadEval (..), ConstructorInfo(..))
import           Language.Egison.IExpr
import           Language.Egison.MathOutput (prettyMath)
import           Language.Egison.Parser
import           Language.Egison.Type.Types (Type(..), TyVar(..), TensorShape(..))


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
  -- Expand loads first
  expanded <- expandLoads [topExpr]
  -- Then evaluate all expanded expressions
  evalExpandedTopExprs env expanded

-- | Evaluate already-expanded top expressions (no Load/LoadFile)
evalExpandedTopExprs :: Env -> [TopExpr] -> EvalM (Maybe EgisonValue, Env)
evalExpandedTopExprs env [] = return (Nothing, env)
evalExpandedTopExprs env (expr:rest) = do
  (mVal, env') <- evalSingleTopExpr env expr
  if null rest
    then return (mVal, env')
    else evalExpandedTopExprs env' rest

-- | Evaluate a single top expression (must not be Load/LoadFile)
-- Pipeline: Desugar → Eval
-- TODO: Implement type environment accumulation for proper type checking
evalSingleTopExpr :: Env -> TopExpr -> EvalM (Maybe EgisonValue, Env)
evalSingleTopExpr env topExpr = do
  -- Step 0: Handle InductiveDecl by registering constructors
  registerInductiveDecl topExpr
  
  -- For now, use desugar-only pipeline until type environment accumulation is implemented
  -- TODO: Re-enable type checking with accumulated type environment
  topExpr' <- desugarTopExpr topExpr
  case topExpr' of
    Nothing      -> return (Nothing, env)
    Just topExpr'' -> evalTopExpr' env topExpr''

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
-- Pipeline: ExpandLoads → Desugar → Collect defs → RecursiveBind → Eval rest
evalTopExprs :: Env -> [TopExpr] -> EvalM Env
evalTopExprs env exprs = do
  opts <- ask
  
  -- Step 1: Expand all Load/LoadFile recursively
  expanded <- expandLoads exprs
  
  -- Step 2: Register inductive constructors
  forM_ expanded registerInductiveDecl
  
  -- Step 3: Desugar all expressions
  desugaredExprs <- desugarTopExprs expanded
  
  -- Step 4: Collect definitions and add them all at once (for mutual recursion)
  (bindings, rest) <- collectDefs opts desugaredExprs
  env' <- recursiveBind env bindings
  
  -- Step 5: Evaluate remaining expressions (tests, executes)
  forM_ rest $ \expr -> do
    (val, _) <- evalTopExpr' env' expr
    case val of
      Nothing  -> return ()
      Just val' -> valueToStr val' >>= liftIO . putStrLn
  return env'

-- | Evaluate Egison top expressions without printing.
evalTopExprsNoPrint :: Env -> [TopExpr] -> EvalM Env
evalTopExprsNoPrint env exprs = do
  opts <- ask
  
  -- Step 1: Expand all Load/LoadFile recursively
  expanded <- expandLoads exprs
  
  -- Step 2: Register inductive constructors
  forM_ expanded registerInductiveDecl
  
  -- Step 3: Desugar all expressions
  desugaredExprs <- desugarTopExprs expanded
  
  -- Step 4: Collect definitions and add them all at once
  (bindings, rest) <- collectDefs opts desugaredExprs
  env' <- recursiveBind env bindings
  
  -- Step 5: Evaluate remaining expressions without printing
  forM_ rest $ evalTopExpr' env'
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

