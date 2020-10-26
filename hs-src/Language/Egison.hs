{- |
Module      : Language.Egison
Licence     : MIT

This is the top module of Egison.
-}

module Language.Egison
       ( module Language.Egison.AST
       , module Language.Egison.Data
       , module Language.Egison.Eval
       , module Language.Egison.Parser
       , module Language.Egison.Primitives
       -- * Modules needed to execute Egison
       , module Language.Egison.CmdOptions
       , module Language.Egison.RState
       -- * Environment
       , initialEnv
       -- * Information
       , version
      ) where

import           Control.Monad.Reader       (asks, local)
import           Control.Monad.State

import           Data.Version
import qualified Paths_egison               as P

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Data
import           Language.Egison.Eval
import           Language.Egison.Parser
import           Language.Egison.Primitives
import           Language.Egison.RState

-- |Version number
version :: Version
version = P.version

-- |Environment that contains core libraries
initialEnv :: RuntimeM Env
initialEnv = do
  isNoIO <- asks optNoIO
  useMathNormalize <- asks optMathNormalize
  env <- liftIO $ if isNoIO then primitiveEnvNoIO else primitiveEnv
  let normalizeLib = if useMathNormalize then "lib/math/normalize.egi" else "lib/math/no-normalize.egi"
  ret <- local (const defaultOption)
               (fromEvalT (evalTopExprs env $ map Load (coreLibraries ++ [normalizeLib])))
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
  , "lib/core/sort.egi"
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
