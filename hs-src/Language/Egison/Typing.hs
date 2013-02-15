module Language.Egison.Typing where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Language.Egison.Types

-- I will deprecate this synonym
type EgisonType = EgisonTypeExpr

type TypeVar = String
type ConsName = String
data TypeCons = TCons ConsName -- Name
                      Int      -- Arity

type ClassName = String

type TypeContext = [(ClassName, TypeVar)]

type TypeVarEnv = Set TypeVar
type TypeConsEnv = Map TypeCons Int
type 