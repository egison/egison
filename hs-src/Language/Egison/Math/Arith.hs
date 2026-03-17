{- |
Module      : Language.Egison.Math.Arith
Licence     : MIT

This module defines some basic arithmetic operations for Egison's computer
algebra system. The operations use CASValue internally for computation.
-}

module Language.Egison.Math.Arith
  ( -- * ScalarData operations (backward compatible)
    mathPlus
  , mathMinus
  , mathMult
  , mathDiv
  , mathPower
  , mathNumerator
  , mathDenominator
  -- * CASValue operations (new)
  , casArithPlus
  , casArithMinus
  , casArithMult
  , casArithDiv
  , casArithPower
  ) where

import           Language.Egison.Math.Expr
import           Language.Egison.Math.CAS

--------------------------------------------------------------------------------
-- ScalarData Operations (backward compatible, using CAS internally)
--------------------------------------------------------------------------------

-- | Add two ScalarData values
mathPlus :: ScalarData -> ScalarData -> ScalarData
mathPlus s1 s2 = casValueToScalarData $
  casPlus (scalarDataToCASValue s1) (scalarDataToCASValue s2)

-- | Subtract two ScalarData values
mathMinus :: ScalarData -> ScalarData -> ScalarData
mathMinus s1 s2 = casValueToScalarData $
  casMinus (scalarDataToCASValue s1) (scalarDataToCASValue s2)

-- | Multiply two ScalarData values
mathMult :: ScalarData -> ScalarData -> ScalarData
mathMult s1 s2 = casValueToScalarData $
  casMult (scalarDataToCASValue s1) (scalarDataToCASValue s2)

-- | Divide two ScalarData values
mathDiv :: ScalarData -> ScalarData -> ScalarData
mathDiv s1 s2 = casValueToScalarData $
  casDivide (scalarDataToCASValue s1) (scalarDataToCASValue s2)

-- | Raise a ScalarData value to an integer power
mathPower :: ScalarData -> Integer -> ScalarData
mathPower s n = casValueToScalarData $
  casPower (scalarDataToCASValue s) n

-- | Get the numerator of a ScalarData value
mathNumerator :: ScalarData -> ScalarData
mathNumerator (Div m _) = Div m (Plus [Term 1 []])

-- | Get the denominator of a ScalarData value
mathDenominator :: ScalarData -> ScalarData
mathDenominator (Div _ n) = Div n (Plus [Term 1 []])

--------------------------------------------------------------------------------
-- CASValue Operations (direct, no conversion)
--------------------------------------------------------------------------------

-- | Add two CASValues (direct operation)
casArithPlus :: CASValue -> CASValue -> CASValue
casArithPlus = casPlus

-- | Subtract two CASValues (direct operation)
casArithMinus :: CASValue -> CASValue -> CASValue
casArithMinus = casMinus

-- | Multiply two CASValues (direct operation)
casArithMult :: CASValue -> CASValue -> CASValue
casArithMult = casMult

-- | Divide two CASValues (direct operation)
casArithDiv :: CASValue -> CASValue -> CASValue
casArithDiv = casDivide

-- | Raise a CASValue to an integer power (direct operation)
casArithPower :: CASValue -> Integer -> CASValue
casArithPower = casPower
