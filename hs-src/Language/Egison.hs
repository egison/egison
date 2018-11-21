{- |
Module      : Language.Egison
Copyright   : Satoshi Egi
Licence     : MIT

This is the top module of Egison.
-}

module Language.Egison
       ( module Language.Egison.Types
       , module Language.Egison.Primitives
       -- * Eval Egison expressions
       , evalEgisonExpr
       , evalEgisonTopExpr
       , evalEgisonTopExprs
       , evalEgisonTopExprsTestOnly
       , runEgisonExpr
       , runEgisonTopExpr
       , runEgisonTopExpr'
       , runEgisonTopExprs
       , runEgisonTopExprsNoIO
       -- * Load Egison files
       , loadEgisonLibrary
       , loadEgisonFile
       -- * Environment
       , initialEnv
       , initialEnvNoIO
       -- * Information
       , version
       ) where

import Data.Version
import qualified Paths_egison as P

import Language.Egison.Types
import Language.Egison.Parser as Parser
import Language.Egison.ParserNonS as ParserNonS
import Language.Egison.Primitives
import Language.Egison.Core

-- |Version number
version :: Version
version = P.version

-- |eval an Egison expression
evalEgisonExpr :: Env -> EgisonExpr -> IO (Either EgisonError EgisonValue)
evalEgisonExpr env expr = fromEgisonM $ evalExprDeep env expr

-- |eval an Egison top expression
evalEgisonTopExpr :: Env -> EgisonTopExpr -> IO (Either EgisonError Env)
evalEgisonTopExpr env exprs = fromEgisonM $ evalTopExpr env exprs

-- |eval Egison top expressions
evalEgisonTopExprs :: Env -> [EgisonTopExpr] -> IO (Either EgisonError Env)
evalEgisonTopExprs env exprs = fromEgisonM $ evalTopExprs env exprs

-- |eval Egison top expressions and execute test expressions
evalEgisonTopExprsTestOnly :: Env -> [EgisonTopExpr] -> IO (Either EgisonError Env)
evalEgisonTopExprsTestOnly env exprs = fromEgisonM $ evalTopExprsTestOnly env exprs

-- |eval an Egison expression. Input is a Haskell string.
runEgisonExpr :: Bool -> Env -> String -> IO (Either EgisonError EgisonValue)
runEgisonExpr True env input = fromEgisonM $ Parser.readExpr input >>= evalExprDeep env
runEgisonExpr False env input = fromEgisonM $ ParserNonS.readExpr input >>= evalExprDeep env

-- |eval an Egison top expression. Input is a Haskell string.
runEgisonTopExpr :: Bool -> Env -> String -> IO (Either EgisonError Env)
runEgisonTopExpr True env input = fromEgisonM $ Parser.readTopExpr input >>= evalTopExpr env
runEgisonTopExpr False env input = fromEgisonM $ ParserNonS.readTopExpr input >>= evalTopExpr env

-- |eval an Egison top expression. Input is a Haskell string.
runEgisonTopExpr' :: Bool -> Env -> [(Var, EgisonExpr)] -> String -> IO (Either EgisonError (Maybe String, [(Var, EgisonExpr)]))
runEgisonTopExpr' True env defines input = fromEgisonM $ Parser.readTopExpr input >>= evalTopExpr' env defines
runEgisonTopExpr' False env defines input = fromEgisonM $ ParserNonS.readTopExpr input >>= evalTopExpr' env defines

-- |eval Egison top expressions. Input is a Haskell string.
runEgisonTopExprs :: Bool -> Env -> String -> IO (Either EgisonError Env)
runEgisonTopExprs True env input = fromEgisonM $ Parser.readTopExprs input >>= evalTopExprs env
runEgisonTopExprs False env input = fromEgisonM $ ParserNonS.readTopExprs input >>= evalTopExprs env

-- |eval Egison top expressions without IO. Input is a Haskell string.
runEgisonTopExprsNoIO :: Bool -> Env -> String -> IO (Either EgisonError Env)
runEgisonTopExprsNoIO True env input = fromEgisonM $ Parser.readTopExprs input >>= evalTopExprsNoIO env
runEgisonTopExprsNoIO False env input = fromEgisonM $ ParserNonS.readTopExprs input >>= evalTopExprsNoIO env

-- |load an Egison file
loadEgisonFile :: Bool -> Env -> FilePath -> IO (Either EgisonError Env)
loadEgisonFile isSExpr env path = evalEgisonTopExpr env (LoadFile isSExpr path)

-- |load an Egison library
loadEgisonLibrary :: Bool -> Env -> FilePath -> IO (Either EgisonError Env)
loadEgisonLibrary isSExpr env path = evalEgisonTopExpr env (Load isSExpr path)

-- |Environment that contains core libraries
initialEnv :: IO Env
initialEnv = do
  env <- primitiveEnv
  ret <- evalEgisonTopExprs env $ map (Load True) coreLibraries
  case ret of
    Left err -> do
      print . show $ err
      return env
    Right env' -> return env'

-- |Environment that contains core libraries without IO primitives
initialEnvNoIO :: IO Env
initialEnvNoIO = do
  env <- primitiveEnvNoIO
  ret <- evalEgisonTopExprs env $ map (Load True) coreLibraries
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
  , "lib/core/base.egi"
  , "lib/core/collection.egi"
  , "lib/core/assoc.egi"
  , "lib/core/order.egi"
  , "lib/core/number.egi"
  , "lib/core/io.egi"
  , "lib/core/random.egi"
  , "lib/core/string.egi"
  ]
