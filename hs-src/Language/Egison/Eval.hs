module Language.Egison.Eval
  -- * Eval Egison expressions
  ( evalExpr
  , evalTopExpr
  , evalTopExprs
  , evalTopExprsNoPrint
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
import           Language.Egison.MathOutput  (prettyMath)
import           Language.Egison.Parser


-- | Evaluate an Egison expression.
evalExpr :: Env -> Expr -> EvalM EgisonValue
evalExpr = evalExprDeep

-- | Evaluate an Egison top expression.
evalTopExpr :: Env -> TopExpr -> EvalM (Maybe String, Env)
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
evalTopExprs :: Env -> [TopExpr] -> EvalM Env
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

-- | Evaluate Egison top expressions.
evalTopExprsNoPrint :: Env -> [TopExpr] -> EvalM Env
evalTopExprsNoPrint env exprs = do
  opts <- ask
  (bindings, rest) <- collectDefs opts exprs
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr env
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

collectDefs :: EgisonOpts -> [TopExpr] -> EvalM ([BindingExpr], [TopExpr])
collectDefs opts exprs = collectDefs' opts exprs [] []
  where
    collectDefs' :: EgisonOpts -> [TopExpr] -> [BindingExpr] -> [TopExpr] -> EvalM ([BindingExpr], [TopExpr])
    collectDefs' opts (expr:exprs) bindings rest =
      case expr of
        Define name expr -> collectDefs' opts exprs ((PDPatVar name, expr) : bindings) rest
        DefineWithIndices{} -> throwError =<< EgisonBug "should not reach here (desugared)" <$> getFuncNameStack
        Test{}     -> collectDefs' opts exprs bindings (expr : rest)
        Execute{}  -> collectDefs' opts exprs bindings (expr : rest)
        LoadFile _ | optNoIO opts -> throwError (Default "No IO support")
        LoadFile file -> do
          exprs' <- loadFile file
          collectDefs' opts (exprs' ++ exprs) bindings rest
        Load _ | optNoIO opts -> throwError (Default "No IO support")
        Load file -> do
          exprs' <- loadLibraryFile file
          collectDefs' opts (exprs' ++ exprs) bindings rest
        InfixDecl{} -> collectDefs' opts exprs bindings rest
    collectDefs' _ [] bindings rest = return (bindings, reverse rest)

evalTopExpr' :: Env -> TopExpr -> EvalM (Maybe EgisonValue, Env)
evalTopExpr' env (Define name expr) = do
  env' <- recursiveBind env [(PDPatVar name, expr)]
  return (Nothing, env')
evalTopExpr' _ DefineWithIndices{} = throwError =<< EgisonBug "should not reach here (desugared)" <$> getFuncNameStack
evalTopExpr' env (Test expr) = do
  pushFuncName "<stdin>"
  val <- evalExprDeep env expr
  popFuncName
  return (Just val, env)
evalTopExpr' env (Execute expr) = do
  pushFuncName "<stdin>"
  io <- evalExprShallow env expr
  case io of
    Value (IOFunc m) -> m >> popFuncName >> return (Nothing, env)
    _                -> throwError =<< TypeMismatch "io" io <$> getFuncNameStack
evalTopExpr' env (Load file) = do
  opts <- ask
  when (optNoIO opts) $ throwError (Default "No IO support")
  exprs <- loadLibraryFile file
  (bindings, _) <- collectDefs opts exprs
  env' <- recursiveBind env bindings
  return (Nothing, env')
evalTopExpr' env (LoadFile file) = do
  opts <- ask
  when (optNoIO opts) $ throwError (Default "No IO support")
  exprs <- loadFile file
  (bindings, _) <- collectDefs opts exprs
  env' <- recursiveBind env bindings
  return (Nothing, env')
evalTopExpr' env InfixDecl{} = return (Nothing, env)
