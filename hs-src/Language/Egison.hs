{-# LANGUAGE TupleSections #-}

{- |
Module      : Language.Egison
Licence     : MIT

This is the top module of Egison.
-}

module Language.Egison
       ( module Language.Egison.AST
       , module Language.Egison.Data
       , module Language.Egison.Primitives
       -- * Eval Egison expressions
       , evalTopExprs
       , evalTopExpr
       , evalEgisonExpr
       , evalEgisonTopExpr
       , evalEgisonTopExprs
       , runEgisonExpr
       , runEgisonTopExpr
       , runEgisonTopExpr'
       , runEgisonTopExprs
       -- * Load Egison files
       , loadEgisonLibrary
       , loadEgisonFile
       -- * Environment
       , initialEnv
       -- * Information
       , version
      ) where

import           Data.Version
import qualified Paths_egison                as P

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Core
import           Language.Egison.Data
import           Language.Egison.MathOutput  (changeOutputInLang)
import           Language.Egison.Parser
import           Language.Egison.Primitives

import           Control.Monad.State

-- |Version number
version :: Version
version = P.version

evalTopExprs :: EgisonOpts -> Env -> [EgisonTopExpr] -> EvalM Env
evalTopExprs opts env exprs = do
  (bindings, rest) <- collectDefs opts exprs
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr opts env
  return env

evalTopExpr :: EgisonOpts -> Env -> EgisonTopExpr -> EvalM Env
evalTopExpr opts env topExpr = do
  ret <- evalTopExpr' opts (StateT $ \defines -> (, defines) <$> recursiveBind env defines) topExpr
  case fst ret of
    Nothing     -> return ()
    Just output -> liftIO $
            case optMathExpr opts of
              Nothing   -> putStrLn output
              Just lang -> putStrLn $ changeOutputInLang lang output
  evalStateT (snd ret) []

-- |eval an Egison expression
evalEgisonExpr :: Env -> EgisonExpr -> IO (Either EgisonError EgisonValue)
evalEgisonExpr env expr = fromEvalM $ evalExprDeep env expr

-- |eval an Egison top expression
evalEgisonTopExpr :: EgisonOpts -> Env -> EgisonTopExpr -> IO (Either EgisonError Env)
evalEgisonTopExpr opts env exprs = fromEvalM $ evalTopExpr opts env exprs

-- |eval Egison top expressions
evalEgisonTopExprs :: EgisonOpts -> Env -> [EgisonTopExpr] -> IO (Either EgisonError Env)
evalEgisonTopExprs opts env exprs = fromEvalM $ evalTopExprs opts env exprs

-- |eval an Egison expression. Input is a Haskell string.
runEgisonExpr :: EgisonOpts -> Env -> String -> IO (Either EgisonError EgisonValue)
runEgisonExpr opts env input =
  fromEvalM $ readExpr (optSExpr opts) input >>= evalExprDeep env

-- |eval an Egison top expression. Input is a Haskell string.
runEgisonTopExpr :: EgisonOpts -> Env -> String -> IO (Either EgisonError Env)
runEgisonTopExpr opts env input =
  fromEvalM $ readTopExpr (optSExpr opts) input >>= evalTopExpr opts env

-- |eval an Egison top expression. Input is a Haskell string.
runEgisonTopExpr' :: EgisonOpts -> StateT [(Var, EgisonExpr)] EvalM Env -> String -> IO (Either EgisonError (Maybe String, StateT [(Var, EgisonExpr)] EvalM Env))
runEgisonTopExpr' opts st input =
  fromEvalM $ readTopExpr (optSExpr opts) input >>= evalTopExpr' opts st

-- |eval Egison top expressions. Input is a Haskell string.
runEgisonTopExprs :: EgisonOpts -> Env -> String -> IO (Either EgisonError Env)
runEgisonTopExprs opts env input =
  fromEvalM $ readTopExprs (optSExpr opts) input >>= evalTopExprs opts env

-- |load an Egison file
loadEgisonFile :: EgisonOpts -> Env -> FilePath -> IO (Either EgisonError Env)
loadEgisonFile opts env path = evalEgisonTopExpr opts env (LoadFile path)

-- |load an Egison library
loadEgisonLibrary :: EgisonOpts -> Env -> FilePath -> IO (Either EgisonError Env)
loadEgisonLibrary opts env path = evalEgisonTopExpr opts env (Load path)

-- |Environment that contains core libraries
initialEnv :: EgisonOpts -> IO Env
initialEnv opts = do
  env <- if optNoIO opts then primitiveEnvNoIO
                         else primitiveEnv
  ret <- evalEgisonTopExprs defaultOption env $ map Load coreLibraries
  case ret of
    Left err -> do
      print . show $ err
      return env
    Right env' -> return env'

coreLibraries :: [String]
coreLibraries =
  [ "lib/math/expression.egi"
  , "lib/math/normalize.egi"
  , "lib/math/common/arithmetic.egi"
  , "lib/math/common/constants.egi"
  , "lib/math/common/functions.egi"
  , "lib/math/algebra/root.egi"
  , "lib/math/algebra/equations.egi"
  , "lib/math/algebra/inverse.egi"
  , "lib/math/analysis/derivative.egi"
  , "lib/math/analysis/integral.egi"
  , "lib/math/algebra/vector.egi"
  , "lib/math/algebra/matrix.egi"
  , "lib/math/algebra/tensor.egi"
  , "lib/math/geometry/differential-form.egi"
  , "lib/core/assoc.egi"
  , "lib/core/base.egi"
  , "lib/core/collection.egi"
  , "lib/core/io.egi"
  , "lib/core/maybe.egi"
  , "lib/core/number.egi"
  , "lib/core/order.egi"
  , "lib/core/random.egi"
  , "lib/core/string.egi"
  ]
