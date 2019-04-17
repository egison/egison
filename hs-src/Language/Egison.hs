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
       , EgisonOpts (..)
      ) where

import           Data.Version
import qualified Paths_egison               as P

import           Language.Egison.Core
import           Language.Egison.Parser     as Parser
import           Language.Egison.ParserNonS as ParserNonS
import           Language.Egison.Primitives
import           Language.Egison.Types

import           Control.Lens               ((^.))
import           Control.Monad.State

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
runEgisonExpr :: EgisonOpts -> Env -> String -> IO (Either EgisonError EgisonValue)
runEgisonExpr opts env input =
  if optSExpr opts then fromEgisonM $ Parser.readExpr input >>= evalExprDeep env
                   else fromEgisonM $ ParserNonS.readExpr input >>= evalExprDeep env

-- |eval an Egison top expression. Input is a Haskell string.
runEgisonTopExpr :: EgisonOpts -> Env -> String -> IO (Either EgisonError Env)
runEgisonTopExpr opts env input =
  if optSExpr opts then fromEgisonM $ Parser.readTopExpr input >>= evalTopExpr env
                   else fromEgisonM $ ParserNonS.readTopExpr input >>= evalTopExpr env

-- |eval an Egison top expression. Input is a Haskell string.
runEgisonTopExpr' :: EgisonOpts -> StateT [(Var, EgisonExpr)] EgisonM Env -> String -> IO (Either EgisonError (Maybe String, StateT [(Var, EgisonExpr)] EgisonM Env))
runEgisonTopExpr' opts st input =
  if optSExpr opts then fromEgisonM $ Parser.readTopExpr input >>= evalTopExpr' st
                   else fromEgisonM $ ParserNonS.readTopExpr input >>= evalTopExpr' st

-- |eval Egison top expressions. Input is a Haskell string.
runEgisonTopExprs :: EgisonOpts -> Env -> String -> IO (Either EgisonError Env)
runEgisonTopExprs opts env input =
  if optSExpr opts then fromEgisonM $ Parser.readTopExprs input >>= evalTopExprs env
                   else fromEgisonM $ ParserNonS.readTopExprs input >>= evalTopExprs env

-- |eval Egison top expressions without IO. Input is a Haskell string.
runEgisonTopExprsNoIO :: EgisonOpts -> Env -> String -> IO (Either EgisonError Env)
runEgisonTopExprsNoIO opts env input =
  if optSExpr opts then fromEgisonM $ Parser.readTopExprs input >>= evalTopExprsNoIO env
                   else fromEgisonM $ ParserNonS.readTopExprs input >>= evalTopExprsNoIO env

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
  , "lib/core/maybe.egi"
  ]

--
-- options
--

data EgisonOpts = EgisonOpts {
    optExecFile         :: Maybe (String, [String]),
    optShowVersion      :: Bool,
    optEvalString       :: Maybe String,
    optExecuteString    :: Maybe String,
    optFieldInfo        :: [(String, String)],
    optLoadLibs         :: [String],
    optLoadFiles        :: [String],
    optSubstituteString :: Maybe String,
    optMapTsvInput      :: Maybe String,
    optFilterTsvInput   :: Maybe String,
    optTsvOutput        :: Bool,
    optNoIO             :: Bool,
    optShowBanner       :: Bool,
    optTestOnly         :: Bool,
    optPrompt           :: String,
    optMathExpr         :: Maybe String,
    optSExpr            :: Bool
    }

