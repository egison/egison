module Language.Egison.Eval
  -- * Eval Egison expressions
  ( evalExpr
  , evalTopExpr
  , evalTopExprs
  , runExpr
  , runTopExpr
  , runTopExprs
  -- * Load Egison files
  , loadEgisonLibrary
  , loadEgisonFile
  -- * Helpers
  , collectDefs
  ) where

import           Control.Monad.Except        (throwError)
import           Control.Monad.Reader        (ask, asks)
import           Control.Monad.State

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Core
import           Language.Egison.Data
import           Language.Egison.EvalState   (MonadEval(..))
import           Language.Egison.IExpr
import           Language.Egison.MathOutput  (prettyMath)
import           Language.Egison.Parser


-- | Evaluate an Egison expression.
evalExpr :: Env -> IExpr -> EvalM EgisonValue
evalExpr = evalExprDeep

-- | Evaluate an Egison top expression.
evalTopExpr :: Env -> ITopExpr -> EvalM (Maybe String, Env)
evalTopExpr env topExpr = do
  mathExpr <- asks optMathExpr
  (mVal, env') <- evalTopExpr' env topExpr
  case mVal of
    Nothing  -> return (Nothing, env')
    Just val ->
      case mathExpr of
        Nothing   -> return (Just (show val), env')
        Just lang -> return (Just (prettyMath lang val), env')

-- | Evaluate Egison top expressions.
evalTopExprs :: Env -> [ITopExpr] -> EvalM Env
evalTopExprs env exprs = do
  opts <- ask
  (bindings, rest) <- collectDefs opts exprs
  env <- recursiveBind env bindings
  forM_ rest $ \expr -> do
    (str, _) <- evalTopExpr env expr
    case str of
      Nothing  -> return ()
      Just str -> liftIO $ putStrLn str
  return env

-- | Evaluate an Egison expression. Input is a Haskell string.
runExpr :: Env -> String -> EvalM EgisonValue
runExpr env input =
  readExpr input >>= evalExprDeep env

-- | Evaluate an Egison top expression. Input is a Haskell string.
runTopExpr :: Env -> String -> EvalM (Maybe String, Env)
runTopExpr env input =
  readTopExpr input >>= evalTopExpr env

-- | Evaluate Egison top expressions. Input is a Haskell string.
runTopExprs :: Env -> String -> EvalM Env
runTopExprs env input =
  readTopExprs input >>= evalTopExprs env

-- | Load an Egison file.
loadEgisonFile :: Env -> FilePath -> EvalM Env
loadEgisonFile env path = do
  (_, env') <- evalTopExpr env (ILoadFile path)
  return env'

-- | Load an Egison library.
loadEgisonLibrary :: Env -> FilePath -> EvalM Env
loadEgisonLibrary env path = do
  (_, env') <- evalTopExpr env (ILoad path)
  return env'


--
-- Helper functions
--

collectDefs :: EgisonOpts -> [ITopExpr] -> EvalM ([IBindingExpr], [ITopExpr])
collectDefs opts exprs = collectDefs' opts exprs [] []
  where
    collectDefs' :: EgisonOpts -> [ITopExpr] -> [IBindingExpr] -> [ITopExpr] -> EvalM ([IBindingExpr], [ITopExpr])
    collectDefs' opts (expr:exprs) bindings rest =
      case expr of
        IDefine name expr -> collectDefs' opts exprs ((PDPatVar name, expr) : bindings) rest
        ITest{}     -> collectDefs' opts exprs bindings (expr : rest)
        IExecute{}  -> collectDefs' opts exprs bindings (expr : rest)
        ILoadFile _ | optNoIO opts -> throwError (Default "No IO support")
        ILoadFile file -> do
          exprs' <- loadFile file
          collectDefs' opts (exprs' ++ exprs) bindings rest
        ILoad _ | optNoIO opts -> throwError (Default "No IO support")
        ILoad file -> do
          exprs' <- loadLibraryFile file
          collectDefs' opts (exprs' ++ exprs) bindings rest
    collectDefs' _ [] bindings rest = return (bindings, reverse rest)

evalTopExpr' :: Env -> ITopExpr -> EvalM (Maybe EgisonValue, Env)
evalTopExpr' env (IDefine name expr) = do
  env' <- recursiveBind env [(PDPatVar name, expr)]
  return (Nothing, env')
evalTopExpr' env (ITest expr) = do
  pushFuncName "<stdin>"
  val <- evalExprDeep env expr
  popFuncName
  return (Just val, env)
evalTopExpr' env (IExecute expr) = do
  pushFuncName "<stdin>"
  io <- evalExprShallow env expr
  case io of
    Value (IOFunc m) -> m >> popFuncName >> return (Nothing, env)
    _                -> throwError =<< TypeMismatch "io" io <$> getFuncNameStack
evalTopExpr' env (ILoad file) = do
  opts <- ask
  when (optNoIO opts) $ throwError (Default "No IO support")
  exprs <- loadLibraryFile file
  (bindings, _) <- collectDefs opts exprs
  env' <- recursiveBind env bindings
  return (Nothing, env')
evalTopExpr' env (ILoadFile file) = do
  opts <- ask
  when (optNoIO opts) $ throwError (Default "No IO support")
  exprs <- loadFile file
  (bindings, _) <- collectDefs opts exprs
  env' <- recursiveBind env bindings
  return (Nothing, env')
