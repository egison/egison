{- |
Module      : Language.Egison
Licence     : MIT

This is the top module of Egison.
-}

module Language.Egison
       ( module Language.Egison.AST
       , module Language.Egison.Data
       , module Language.Egison.Eval
       , module Language.Egison.EvalState
       , module Language.Egison.Parser
       , module Language.Egison.Primitives
       -- * Modules needed to execute Egison
       , module Language.Egison.CmdOptions
       , module Language.Egison.RState
       -- * Environment
       , initialEnv
       , coreLibraries
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
import           Language.Egison.EvalState
import           Language.Egison.Parser
import           Language.Egison.Primitives
import           Language.Egison.RState

-- |Version number
version :: Version
version = P.version

-- |Create initial environment with only primitive functions
-- Core libraries will be loaded separately to maintain consistent Env chain
-- Returns EvalM Env to preserve EvalState (type environment, class environment)
initialEnv :: EvalM Env
initialEnv = do
  isNoIO <- lift $ lift $ asks optNoIO
  env <- liftIO $ if isNoIO then primitiveEnvNoIO else primitiveEnv
  return env

coreLibraries :: [String]
coreLibraries =
  -- Libs that defines user-defined infixes comes first
  [ "lib/core/base.egi"              -- Defines (&&) (||)
  , "lib/core/order.egi"
--  , "lib/math/common/arithmetic.egi" -- Defines (+) (-) (*) (/) (+') (-') (*') (/')
--  , "lib/math/algebra/tensor.egi"    -- Defines (.) (.')
  , "lib/core/collection.egi"        -- Defines (++) for patterns
--  , "lib/math/expression.egi"        -- Defines (+) (*) (/) (^) for patterns

--  , "lib/core/assoc.egi"
--  , "lib/core/io.egi"
  , "lib/core/maybe.egi"
--  , "lib/core/number.egi"
--  , "lib/core/random.egi"
--  , "lib/core/string.egi"
--  , "lib/core/sort.egi"
--  , "lib/math/common/constants.egi"
--  , "lib/math/common/functions.egi"
--  , "lib/math/algebra/root.egi"
--  , "lib/math/algebra/equations.egi"
--  , "lib/math/algebra/inverse.egi"
--  , "lib/math/analysis/derivative.egi"
--  , "lib/math/analysis/integral.egi"
--  , "lib/math/algebra/vector.egi"
--  , "lib/math/algebra/matrix.egi"
--  , "lib/math/geometry/differential-form.egi"
  ]
