{- |
Module      : Language.Egison.Eval
Licence     : MIT

This module provides interface for evaluating Egison expressions.
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
  ) where

import           Control.Monad              (forM_, when)
import           Control.Monad.Except       (throwError)
import           Control.Monad.Reader       (ask, asks)
import           Control.Monad.State
import           System.IO                  (hPutStrLn, stderr)

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Core
import           Language.Egison.Data
import           Language.Egison.Desugar
import           Language.Egison.EvalState  (MonadEval (..))
import           Language.Egison.IExpr
import           Language.Egison.MathOutput (prettyMath)
import           Language.Egison.Parser
import           Language.Egison.Type.Check (TypeCheckConfig (..), TypeCheckError (..),
                                              defaultConfig, strictConfig, permissiveConfig,
                                              typeCheckWithWarnings, typeCheckWithLoader,
                                              FileLoader)
import           Language.Egison.Type.Error (formatTypeError, formatTypeWarning, TypeWarning)
import           Language.Egison.Type.TypeClassExpand (expandTopExpr)
import           Language.Egison.Type.Env (emptyEnv)
import qualified Language.Egison.Parser.NonS as NonS
import           Language.Egison.RState     (evalRuntimeT)
import           System.Directory           (doesFileExist, getHomeDirectory)
import           Paths_egison               (getDataFileName)
import           Language.Egison.CmdOptions (defaultOption)


-- | Evaluate an Egison expression.
evalExpr :: Env -> Expr -> EvalM EgisonValue
evalExpr env expr = desugarExpr expr >>= evalExprDeep env

-- | Evaluate an Egison top expression.
-- Pipeline: Parse → TypeCheck → TypeClassExpand → Desugar → Eval
evalTopExpr :: Env -> TopExpr -> EvalM (Maybe EgisonValue, Env)
evalTopExpr env topExpr = do
  opts <- ask
  
  -- Step 1: Run type checking if enabled
  typeEnv <- if optTypeCheck opts || optTypeCheckStrict opts
    then do
      let config = if optTypeCheckStrict opts then strictConfig else permissiveConfig
      (result, warnings) <- liftIO $ typeCheckWithLoader config makeFileLoader [topExpr]
      -- Print warnings to stderr (so they don't interfere with normal output)
      when (not (null warnings)) $ do
        liftIO $ mapM_ (hPutStrLn stderr . formatTypeWarning) warnings
      -- Handle errors
      case result of
        Left errs -> do
          liftIO $ hPutStrLn stderr "Type errors found:"
          liftIO $ mapM_ (hPutStrLn stderr . ("  " ++) . formatTypeError . tceError) errs
          throwError $ Default "Type checking failed"
        Right env' -> return env'
    else return emptyEnv
  
  -- Step 2: Expand type class methods using type information
  let expandedTopExpr = expandTopExpr typeEnv topExpr
  
  -- Step 3: Desugar using existing Desugar.hs
  topExpr' <- desugarTopExpr expandedTopExpr
  
  -- Step 4: Evaluate
  case topExpr' of
    Nothing      -> return (Nothing, env)
    Just topExpr'' -> evalTopExpr' env topExpr''
  where
    tceError (TypeCheckError err _) = err

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
-- Pipeline: Parse → TypeCheck → TypeClassExpand → Desugar → Eval
evalTopExprs :: Env -> [TopExpr] -> EvalM Env
evalTopExprs env exprs = do
  opts <- ask
  
  -- Step 1: Run type checking if enabled
  typeEnv <- if optTypeCheck opts || optTypeCheckStrict opts
    then do
      let config = if optTypeCheckStrict opts then strictConfig else permissiveConfig
      (result, warnings) <- liftIO $ typeCheckWithLoader config makeFileLoader exprs
      -- Print warnings to stderr (so they don't interfere with normal output)
      when (not (null warnings)) $ do
        liftIO $ mapM_ (hPutStrLn stderr . formatTypeWarning) warnings
      -- Handle errors
      case result of
        Left errs -> do
          liftIO $ hPutStrLn stderr "Type errors found:"
          liftIO $ mapM_ (hPutStrLn stderr . ("  " ++) . formatTypeError . tceError) errs
          throwError $ Default "Type checking failed"
        Right env' -> return env'
    else return emptyEnv
  
  -- Step 2: Expand type class methods using type information
  let expandedExprs = map (expandTopExpr typeEnv) exprs
  
  -- Step 3: Desugar using existing Desugar.hs
  desugaredExprs <- desugarTopExprs expandedExprs
  
  -- Step 4: Evaluate
  (bindings, rest) <- collectDefs opts desugaredExprs
  env' <- recursiveBind env bindings
  forM_ rest $ \expr -> do
    (val, _) <- evalTopExpr' env' expr
    case val of
      Nothing  -> return ()
      Just val -> valueToStr val >>= liftIO . putStrLn
  return env'
  where
    tceError (TypeCheckError err _) = err

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

-- | Create a file loader for type checking
-- This loader parses Egison files and returns their TopExprs
makeFileLoader :: FileLoader
makeFileLoader path = do
  -- Resolve the file path (check ~/.egison/ first, then data directory)
  homeDir <- getHomeDirectory
  let homePath = homeDir ++ "/.egison/" ++ path
  homeExists <- doesFileExist homePath
  actualPath <- if homeExists
    then return homePath
    else do
      dataPath <- getDataFileName path
      dataExists <- doesFileExist dataPath
      return $ if dataExists then dataPath else path

  -- Check if file exists
  exists <- doesFileExist actualPath
  if not exists
    then return $ Left $ "File not found: " ++ path
    else do
      -- Read and parse the file
      content <- readUTF8File actualPath
      let cleanContent = removeShebang content
      -- Parse using the NonS parser with evalRuntimeT
      evalRuntimeT defaultOption (NonS.parseTopExprs cleanContent)
