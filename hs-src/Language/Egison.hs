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

import           Control.Monad.Reader        (ask, asks, local)

import           Data.Version
import qualified Paths_egison                as P

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Core
import           Language.Egison.Data
import           Language.Egison.MathOutput  (changeOutputInLang)
import           Language.Egison.Parser
import           Language.Egison.Primitives
import           Language.Egison.RState

import           Control.Monad.State

-- |Version number
version :: Version
version = P.version

evalTopExprs :: Env -> [EgisonTopExpr] -> EvalM Env
evalTopExprs env exprs = do
  opts <- ask
  (bindings, rest) <- collectDefs opts exprs
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr env
  return env

evalTopExpr :: Env -> EgisonTopExpr -> EvalM Env
evalTopExpr env topExpr = do
  mathExpr <- asks optMathExpr
  (mOutput, env') <- evalTopExpr' env topExpr
  case mOutput of
    Nothing     -> return ()
    Just output -> liftIO $
            case mathExpr of
              Nothing   -> putStrLn output
              Just lang -> putStrLn $ changeOutputInLang lang output
  return env'

-- |eval an Egison expression
evalEgisonExpr :: Env -> EgisonExpr -> RuntimeM (Either EgisonError EgisonValue)
evalEgisonExpr env expr = fromEvalT $ evalExprDeep env expr

-- |eval an Egison top expression
evalEgisonTopExpr :: Env -> EgisonTopExpr -> RuntimeM (Either EgisonError Env)
evalEgisonTopExpr env exprs = fromEvalT $ evalTopExpr env exprs

-- |eval Egison top expressions
evalEgisonTopExprs :: Env -> [EgisonTopExpr] -> RuntimeM (Either EgisonError Env)
evalEgisonTopExprs env exprs = fromEvalT $ evalTopExprs env exprs

-- |eval an Egison expression. Input is a Haskell string.
runEgisonExpr :: Env -> String -> RuntimeM (Either EgisonError EgisonValue)
runEgisonExpr env input = do
  isSExpr <- asks optSExpr
  fromEvalT $ readExpr isSExpr input >>= evalExprDeep env

-- |eval an Egison top expression. Input is a Haskell string.
runEgisonTopExpr :: Env -> String -> RuntimeM (Either EgisonError Env)
runEgisonTopExpr env input = do
  isSExpr <- asks optSExpr
  fromEvalT $ readTopExpr isSExpr input >>= evalTopExpr env

-- |eval an Egison top expression. Input is a Haskell string.
runEgisonTopExpr' :: Env -> String -> RuntimeM (Either EgisonError (Maybe String, Env))
runEgisonTopExpr' env input = do
  isSExpr <- asks optSExpr
  fromEvalT $ readTopExpr isSExpr input >>= evalTopExpr' env

-- |eval Egison top expressions. Input is a Haskell string.
runEgisonTopExprs :: Env -> String -> RuntimeM (Either EgisonError Env)
runEgisonTopExprs env input = do
  isSExpr <- asks optSExpr
  fromEvalT $ readTopExprs isSExpr input >>= evalTopExprs env

-- |load an Egison file
loadEgisonFile :: Env -> FilePath -> RuntimeM (Either EgisonError Env)
loadEgisonFile env path = evalEgisonTopExpr env (LoadFile path)

-- |load an Egison library
loadEgisonLibrary :: Env -> FilePath -> RuntimeM (Either EgisonError Env)
loadEgisonLibrary env path = evalEgisonTopExpr env (Load path)

-- |Environment that contains core libraries
initialEnv :: RuntimeM Env
initialEnv = do
  isNoIO <- asks optNoIO
  env <- liftIO $ if isNoIO then primitiveEnvNoIO else primitiveEnv
  ret <- local (const defaultOption)
               (evalEgisonTopExprs env $ map Load coreLibraries)
  case ret of
    Left err -> do
      liftIO $ print (show err)
      return env
    Right env' -> return env'

coreLibraries :: [String]
coreLibraries =
  -- Libs that defines user-defined infixes comes first
  [ "lib/core/base.egi"              -- Defines (&&) (||)
  , "lib/math/common/arithmetic.egi" -- Defines (+) (-) (*) (/) (+') (-') (*') (/')
  , "lib/math/algebra/tensor.egi"    -- Defines (.) (.')

  , "lib/core/assoc.egi"
  , "lib/core/collection.egi"
  , "lib/core/io.egi"
  , "lib/core/maybe.egi"
  , "lib/core/number.egi"
  , "lib/core/order.egi"
  , "lib/core/random.egi"
  , "lib/core/string.egi"
  , "lib/math/expression.egi"
  , "lib/math/normalize.egi"
  , "lib/math/common/constants.egi"
  , "lib/math/common/functions.egi"
  , "lib/math/algebra/root.egi"
  , "lib/math/algebra/equations.egi"
  , "lib/math/algebra/inverse.egi"
  , "lib/math/analysis/derivative.egi"
  , "lib/math/analysis/integral.egi"
  , "lib/math/algebra/vector.egi"
  , "lib/math/algebra/matrix.egi"
  , "lib/math/geometry/differential-form.egi"
  ]
