{- |
Module      : Language.Egison.Types
Licence     : MIT

This module contains functions for dynamic type systems.
-}

module Language.Egison.Types
  ( isSymbol
  , isBool'
  , isInteger'
  , isRational'
  , isScalar'
  , isFloat'
  , isComplex'
  , isTensor'
  , isTensorWithIndex'
  , isChar'
  , isString'
  , isCollection'
  , isHash'
  ) where

import           Language.Egison.Data
import           Language.Egison.Math

--
-- Typing
--

isBool' :: PrimitiveFunc
isBool' (Bool _) = return $ Bool True
isBool' _        = return $ Bool False

isInteger :: EgisonValue -> Bool
isInteger (ScalarData (Div (Plus []) (Plus [Term 1 []])))          = True
isInteger (ScalarData (Div (Plus [Term _ []]) (Plus [Term 1 []]))) = True
isInteger _                                                        = False

isInteger' :: PrimitiveFunc
isInteger' val = return $ Bool $ isInteger val

isRational :: EgisonValue -> Bool
isRational (ScalarData (Div (Plus []) (Plus [Term _ []])))          = True
isRational (ScalarData (Div (Plus [Term _ []]) (Plus [Term _ []]))) = True
isRational _                                                        = False

isRational' :: PrimitiveFunc
isRational' val = return $ Bool $ isRational val

isSymbol :: EgisonValue -> Bool
isSymbol (ScalarData (Div (Plus [Term 1 [(Symbol{}, 1)]]) (Plus [Term 1 []]))) = True
isSymbol _ = False

isScalar' :: PrimitiveFunc
isScalar' (ScalarData _) = return $ Bool True
isScalar' _              = return $ Bool False

isTensor' :: PrimitiveFunc
isTensor' (TensorData _) = return $ Bool True
isTensor' _              = return $ Bool False

isTensorWithIndex :: EgisonValue -> Bool
isTensorWithIndex (TensorData (Tensor _ _ (_:_))) = True
isTensorWithIndex _                               = False

isTensorWithIndex' :: PrimitiveFunc
isTensorWithIndex' val = return $ Bool $ isTensorWithIndex val

isFloat' :: PrimitiveFunc
isFloat' (Float _) = return $ Bool True
isFloat' _         = return $ Bool False

isComplex' :: PrimitiveFunc
isComplex' (Float _) = return $ Bool True
isComplex' _         = return $ Bool False

isChar' :: PrimitiveFunc
isChar' (Char _) = return $ Bool True
isChar' _        = return $ Bool False

isString' :: PrimitiveFunc
isString' (String _) = return $ Bool True
isString' _          = return $ Bool False

isCollection' :: PrimitiveFunc
isCollection' (Collection _) = return $ Bool True
isCollection' _              = return $ Bool False

isHash' :: PrimitiveFunc
isHash' (IntHash _)  = return $ Bool True
isHash' (CharHash _) = return $ Bool True
isHash' (StrHash _)  = return $ Bool True
isHash' _            = return $ Bool False
