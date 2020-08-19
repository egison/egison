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
  ) where

import           Control.Monad.Reader        (ask, asks)
import           Control.Monad.State

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Core
import           Language.Egison.Data
import           Language.Egison.MathOutput  (prettyMath)
import           Language.Egison.Parser


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

-- |eval an Egison expression
evalExpr :: Env -> Expr -> EvalM EgisonValue
evalExpr = evalExprDeep

-- |eval an Egison expression. Input is a Haskell string.
runExpr :: Env -> String -> EvalM EgisonValue
runExpr env input =
  readExpr input >>= evalExprDeep env

-- |eval an Egison top expression. Input is a Haskell string.
runTopExpr :: Env -> String -> EvalM (Maybe String, Env)
runTopExpr env input =
  readTopExpr input >>= evalTopExpr env

-- |eval Egison top expressions. Input is a Haskell string.
runTopExprs :: Env -> String -> EvalM Env
runTopExprs env input =
  readTopExprs input >>= evalTopExprs env

-- |load an Egison file
loadEgisonFile :: Env -> FilePath -> EvalM Env
loadEgisonFile env path = do
  (_, env') <- evalTopExpr env (LoadFile path)
  return env'

-- |load an Egison library
loadEgisonLibrary :: Env -> FilePath -> EvalM Env
loadEgisonLibrary env path = do
  (_, env') <- evalTopExpr env (Load path)
  return env'
