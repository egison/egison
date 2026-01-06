{- |
Module      : Language.Egison.Type
Licence     : MIT

This module re-exports the type system modules for Egison.

= Usage

To enable type checking in your Egison code, use type annotations:

@
def take (n : Integer) (xs : [a]) : [a] :=
  if n = 0
    then []
    else match xs as list something with
      | $x :: $xs -> x :: take (n - 1) xs
      | [] -> []
@

= Tensor Types

Tensor types include shape and index information:

@
def g_i_j : Tensor Integer [2, 2]_#_# := ...

g_i_j . g~i~j : Integer  -- Tensor Integer [] = Integer
@

= Type System Features

* Hindley-Milner type inference with let-polymorphism
* Tensor types with index tracking (contravariant ~i, covariant _i)
* Automatic contraction when matching indices
* Scalar function lifting to tensors
* Matcher types
-}

module Language.Egison.Type
  ( -- * Core Types
    module Language.Egison.Type.Types
    -- * Type Inference
  , module Language.Egison.Type.IInfer
    -- * Type Checking
  , module Language.Egison.Type.Check
    -- * Type Errors
  , module Language.Egison.Type.Error
    -- * Tensor Index Types
  , module Language.Egison.Type.Index
    -- * Tensor Type Rules
  , module Language.Egison.Type.Tensor
  ) where

import           Language.Egison.Type.Check
import           Language.Egison.Type.Error
import           Language.Egison.Type.Index
import           Language.Egison.Type.IInfer
import           Language.Egison.Type.Tensor
import           Language.Egison.Type.Types

