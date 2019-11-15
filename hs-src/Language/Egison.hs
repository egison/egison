{-# LANGUAGE TupleSections #-}

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

import           Language.Egison.Core
import           Language.Egison.MathOutput  (changeOutputInLang)
import           Language.Egison.Parser      as Parser
import           Language.Egison.ParserNonS  as ParserNonS
import           Language.Egison.Primitives
import           Language.Egison.Types

import           Control.Monad.State

-- |Version number
version :: Version
version = P.version

evalTopExprs :: EgisonOpts -> Env -> [EgisonTopExpr] -> EgisonM Env
evalTopExprs opts env exprs = do
  (bindings, rest) <- collectDefs opts exprs [] []
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr opts env
  return env

evalTopExpr :: EgisonOpts -> Env -> EgisonTopExpr -> EgisonM Env
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
evalEgisonExpr env expr = fromEgisonM $ evalExprDeep env expr

-- |eval an Egison top expression
evalEgisonTopExpr :: EgisonOpts -> Env -> EgisonTopExpr -> IO (Either EgisonError Env)
evalEgisonTopExpr opts env exprs = fromEgisonM $ evalTopExpr opts env exprs

-- |eval Egison top expressions
evalEgisonTopExprs :: EgisonOpts -> Env -> [EgisonTopExpr] -> IO (Either EgisonError Env)
evalEgisonTopExprs opts env exprs = fromEgisonM $ evalTopExprs opts env exprs

-- |eval an Egison expression. Input is a Haskell string.
runEgisonExpr :: EgisonOpts -> Env -> String -> IO (Either EgisonError EgisonValue)
runEgisonExpr opts env input
  | optSExpr opts = fromEgisonM $ Parser.readExpr input >>= evalExprDeep env
  | otherwise     = fromEgisonM $ ParserNonS.readExpr input >>= evalExprDeep env

-- |eval an Egison top expression. Input is a Haskell string.
runEgisonTopExpr :: EgisonOpts -> Env -> String -> IO (Either EgisonError Env)
runEgisonTopExpr opts env input
  | optSExpr opts = fromEgisonM $ Parser.readTopExpr input >>= evalTopExpr opts env
  | otherwise     = fromEgisonM $ ParserNonS.readTopExpr input >>= evalTopExpr opts env

-- |eval an Egison top expression. Input is a Haskell string.
runEgisonTopExpr' :: EgisonOpts -> StateT [(Var, EgisonExpr)] EgisonM Env -> String -> IO (Either EgisonError (Maybe String, StateT [(Var, EgisonExpr)] EgisonM Env))
runEgisonTopExpr' opts st input
  | optSExpr opts = fromEgisonM $ Parser.readTopExpr input >>= evalTopExpr' opts st
  | otherwise     = fromEgisonM $ ParserNonS.readTopExpr input >>= evalTopExpr' opts st

-- |eval Egison top expressions. Input is a Haskell string.
runEgisonTopExprs :: EgisonOpts -> Env -> String -> IO (Either EgisonError Env)
runEgisonTopExprs opts env input
  | optSExpr opts = fromEgisonM $ Parser.readTopExprs input >>= evalTopExprs opts env
  | otherwise     = fromEgisonM $ ParserNonS.readTopExprs input >>= evalTopExprs opts env

-- |load an Egison file
loadEgisonFile :: EgisonOpts -> Env -> FilePath -> IO (Either EgisonError Env)
loadEgisonFile opts env path = evalEgisonTopExpr opts env (LoadFile path)

-- |load an Egison library
loadEgisonLibrary :: EgisonOpts -> Env -> FilePath -> IO (Either EgisonError Env)
loadEgisonLibrary opts env path = evalEgisonTopExpr opts env (Load path)

-- |Environment that contains core libraries
initialEnv :: EgisonOpts -> IO Env
initialEnv opts = do
  env <- if optNoIO opts then if optSExpr opts then primitiveEnvNoIO
                                               else primitiveEnvNoIO'
                         else if optSExpr opts then primitiveEnv
                                               else primitiveEnv'
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
  , "lib/core/base.egi"
  , "lib/core/collection.egi"
  , "lib/core/assoc.egi"
  , "lib/core/order.egi"
  , "lib/core/number.egi"
  , "lib/core/io.egi"
  , "lib/core/random.egi"
  , "lib/core/string.egi"
  , "lib/core/maybe.egi"
  ]
