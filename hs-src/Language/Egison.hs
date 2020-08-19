{- |
Module      : Language.Egison
Licence     : MIT

This is the top module of Egison.
-}

module Language.Egison
       ( module Language.Egison.AST
       , module Language.Egison.Data
       , module Language.Egison.Primitives
       -- * Modules needed to execute Egison
       , module Language.Egison.CmdOptions
       , module Language.Egison.RState
       -- * Eval Egison expressions
       , evalTopExprs
       , evalTopExpr
       , evalEgisonExpr
       , evalEgisonTopExpr
       , evalEgisonTopExprs
       , runEgisonExpr
       , runEgisonTopExpr
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
import           Language.Egison.MathOutput  (prettyMath)
import           Language.Egison.Parser
import           Language.Egison.Primitives
import           Language.Egison.RState

import           Control.Monad.State

-- |Version number
version :: Version
version = P.version

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
evalEgisonExpr :: Env -> Expr -> EvalM EgisonValue
evalEgisonExpr = evalExprDeep

-- |eval an Egison top expression
evalEgisonTopExpr :: Env -> TopExpr -> EvalM (Maybe String, Env)
evalEgisonTopExpr = evalTopExpr

-- |eval Egison top expressions
evalEgisonTopExprs :: Env -> [TopExpr] -> EvalM Env
evalEgisonTopExprs = evalTopExprs

-- |eval an Egison expression. Input is a Haskell string.
runEgisonExpr :: Env -> String -> EvalM EgisonValue
runEgisonExpr env input =
  readExpr input >>= evalExprDeep env

-- |eval an Egison top expression. Input is a Haskell string.
runEgisonTopExpr :: Env -> String -> EvalM (Maybe String, Env)
runEgisonTopExpr env input =
  readTopExpr input >>= evalTopExpr env

-- |eval Egison top expressions. Input is a Haskell string.
runEgisonTopExprs :: Env -> String -> EvalM Env
runEgisonTopExprs env input =
  readTopExprs input >>= evalTopExprs env

-- |load an Egison file
loadEgisonFile :: Env -> FilePath -> EvalM Env
loadEgisonFile env path = do
  (_, env') <- evalEgisonTopExpr env (LoadFile path)
  return env'

-- |load an Egison library
loadEgisonLibrary :: Env -> FilePath -> EvalM Env
loadEgisonLibrary env path = do
  (_, env') <- evalEgisonTopExpr env (Load path)
  return env'

-- |Environment that contains core libraries
initialEnv :: RuntimeM Env
initialEnv = do
  isNoIO <- asks optNoIO
  useMathNormalize <- asks optMathNormalize
  env <- liftIO $ if isNoIO then primitiveEnvNoIO else primitiveEnv
  let normalizeLib = if useMathNormalize then "lib/math/normalize.egi" else "lib/math/no-normalize.egi"
  ret <- local (const defaultOption)
               (fromEvalT (evalEgisonTopExprs env $ map Load (coreLibraries ++ [normalizeLib])))
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
  , "lib/math/expression.egi"        -- Defines (+) (*) (/) (^) for patterns

  , "lib/core/assoc.egi"
  , "lib/core/collection.egi"
  , "lib/core/io.egi"
  , "lib/core/maybe.egi"
  , "lib/core/number.egi"
  , "lib/core/order.egi"
  , "lib/core/random.egi"
  , "lib/core/string.egi"
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
