{- |
Module      : Language.Egison
Copyright   : Satoshi Egi
Licence     : MIT

This is the top module of Egison.
-}

module Language.Egison
       ( module Language.Egison.Types
       , module Language.Egison.Parser
       , module Language.Egison.Primitives
       -- * Eval Egison expressions
       , evalEgisonExpr
       , evalEgisonTopExpr
       , evalEgisonTopExprs
       , runEgisonExpr
       , runEgisonTopExpr
       , runEgisonTopExprs
       -- * Load Egison files
       , loadCoreLibraries
       , loadEgisonLibrary
       , loadEgisonFile
       -- * Information
       , version
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error

import Data.IORef
import Data.Version
import qualified Paths_egison as P

import Language.Egison.Types
import Language.Egison.Parser
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

-- |eval an Egison expression. Input is a Haskell string.
runEgisonExpr :: Env -> String -> IO (Either EgisonError EgisonValue)
runEgisonExpr env input = fromEgisonM $ readExpr input >>= evalExprDeep env

-- |eval an Egison top expression. Input is a Haskell string.
runEgisonTopExpr :: Env -> String -> IO (Either EgisonError Env)
runEgisonTopExpr env input = fromEgisonM $ readTopExpr input >>= evalTopExpr env

-- |eval Egison top expressions. Input is a Haskell string.
runEgisonTopExprs :: Env -> String -> IO (Either EgisonError Env)
runEgisonTopExprs env input = fromEgisonM $ readTopExprs input >>= evalTopExprs env

loadCoreLibraries :: Env -> IO Env
loadCoreLibraries env = do
  ret <- evalEgisonTopExprs env $ map Load libraries
  case ret of
    Left err -> do
      print . show $ err
      return env
    Right env' -> return env'
  where
    libraries :: [String]
    libraries = [ "lib/core/base.egi"
                , "lib/core/collection.egi"
                , "lib/core/order.egi"
                , "lib/core/number.egi"
                , "lib/core/natural-number.egi"
                , "lib/core/string.egi"
                , "lib/core/database.egi"
                , "lib/core/io.egi"
                 ]

loadEgisonFile :: Env -> FilePath -> IO (Either EgisonError Env)
loadEgisonFile env path = evalEgisonTopExpr env (LoadFile path)

loadEgisonLibrary :: Env -> FilePath -> IO (Either EgisonError Env)
loadEgisonLibrary env path = evalEgisonTopExpr env (Load path)

