module Language.Egison.Eval
  -- * Eval Egison expressions
  ( evalExpr
  , evalTopExpr
  , evalTopExprStr
  , evalTopExprs
  , evalTopExprsNoPrint
  , runExpr
  , runTopExpr
  , runTopExprs
  -- * Load Egison files
  , loadEgisonLibrary
  , loadEgisonFile
  ) where

import           Control.Monad.Except        (throwError)
import           Control.Monad.Reader        (ask, asks)
import           Control.Monad.State

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Core
import           Language.Egison.Data
import           Language.Egison.Desugar
import           Language.Egison.EvalState   (MonadEval(..))
import           Language.Egison.IExpr
import           Language.Egison.MathOutput  (prettyMath)
import           Language.Egison.Parser


-- | Evaluate an Egison expression.
evalExpr :: Env -> Expr -> EvalM EgisonValue
evalExpr env expr = desugarExpr expr >>= evalExprDeep env

-- | Evaluate an Egison top expression.
evalTopExpr :: Env -> TopExpr -> EvalM (Maybe EgisonValue, Env)
evalTopExpr env topExpr = do
  topExpr <- desugarTopExpr topExpr
  case topExpr of
    Nothing -> return (Nothing, env)
    Just topExpr -> evalTopExpr' env topExpr

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
evalTopExprs :: Env -> [TopExpr] -> EvalM Env
evalTopExprs env exprs = do
  exprs <- desugarTopExprs exprs
  opts <- ask
  (bindings, rest) <- collectDefs opts exprs
  env <- recursiveBind env bindings
  forM_ rest $ \expr -> do
    (val, _) <- evalTopExpr' env expr
    case val of
      Nothing  -> return ()
      Just val -> valueToStr val >>= liftIO . putStrLn
  return env

-- | Evaluate Egison top expressions.
evalTopExprsNoPrint :: Env -> [TopExpr] -> EvalM Env
evalTopExprsNoPrint env exprs = do
  exprs <- desugarTopExprs exprs
  opts <- ask
  (bindings, rest) <- collectDefs opts exprs
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr' env
  return env

-- | Evaluate an Egison expression. Input is a Haskell string.
runExpr :: Env -> String -> EvalM EgisonValue
runExpr env input =
  readExpr input >>= evalExpr env

-- | Evaluate an Egison top expression. Input is a Haskell string.
runTopExpr :: Env -> String -> EvalM (Maybe EgisonValue, Env)
runTopExpr env input =
  readTopExpr input >>= evalTopExpr env

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
          exprs' <- loadFile file >>= desugarTopExprs
          collectDefs' opts (exprs' ++ exprs) bindings rest
        ILoad _ | optNoIO opts -> throwError (Default "No IO support")
        ILoad file -> do
          exprs' <- loadLibraryFile file >>= desugarTopExprs
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
