{- |
Module      : Language.Egison.Types
Licence     : MIT

This module contains functions for dynamic type systems.
-}

module Language.Egison.Types
  ( isInteger
  , isRational
  , isSymbol
  , isScalar
  , isTensor
  , isTensorWithIndex
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
import           Language.Egison.MathExpr

--
-- Typing
--

isBool' :: PrimitiveFunc
isBool' (Value (Bool _)) = return $ Value $ Bool True
isBool' (Value _)        = return $ Value $ Bool False

isInteger :: EgisonValue -> Bool
isInteger (ScalarData (Div (Plus []) (Plus [Term 1 []])))          = True
isInteger (ScalarData (Div (Plus [Term _ []]) (Plus [Term 1 []]))) = True
isInteger _                                                        = False

isInteger' :: PrimitiveFunc
isInteger' (Value val) = return $ Value $ Bool $ isInteger val

isRational :: EgisonValue -> Bool
isRational (ScalarData (Div (Plus []) (Plus [Term _ []])))          = True
isRational (ScalarData (Div (Plus [Term _ []]) (Plus [Term _ []]))) = True
isRational _                                                        = False

isRational' :: PrimitiveFunc
isRational' (Value val) = return $ Value $ Bool $ isRational val

isSymbol :: EgisonValue -> Bool
isSymbol (ScalarData (Div (Plus [Term 1 [(Symbol{}, 1)]]) (Plus [Term 1 []]))) = True
isSymbol _ = False

isScalar :: EgisonValue -> Bool
isScalar (ScalarData _) = True
isScalar _              = False

isScalar' :: PrimitiveFunc
isScalar' (Value (ScalarData _)) = return $ Value $ Bool True
isScalar' _                      = return $ Value $ Bool False

isTensor :: EgisonValue -> Bool
isTensor (TensorData _) = True
isTensor _              = False

isTensor' :: PrimitiveFunc
isTensor' (Value (TensorData _))     = return $ Value $ Bool True
isTensor' (Intermediate (ITensor _)) = return $ Value $ Bool True
isTensor' _                          = return $ Value $ Bool False

isTensorWithIndex :: EgisonValue -> Bool
isTensorWithIndex (TensorData (Tensor _ _ (_:_))) = True
isTensorWithIndex _                               = False

isTensorWithIndex' :: PrimitiveFunc
isTensorWithIndex' (Value val) = return $ Value $ Bool $ isTensorWithIndex val
isTensorWithIndex' _           = return $ Value $ Bool False

isFloat' :: PrimitiveFunc
isFloat' (Value (Float _)) = return $ Value $ Bool True
isFloat' _                 = return $ Value $ Bool False

isComplex' :: PrimitiveFunc
isComplex' (Value (Float _)) = return $ Value $ Bool True
isComplex' _                 = return $ Value $ Bool False

isChar' :: PrimitiveFunc
isChar' (Value (Char _)) = return $ Value $ Bool True
isChar' _                = return $ Value $ Bool False

isString' :: PrimitiveFunc
isString' (Value (String _)) = return $ Value $ Bool True
isString' _                  = return $ Value $ Bool False

isCollection' :: PrimitiveFunc
isCollection' (Value (Collection _))         = return $ Value $ Bool True
isCollection' (Intermediate (ICollection _)) = return $ Value $ Bool True
isCollection' _                              = return $ Value $ Bool False

isHash' :: PrimitiveFunc
isHash' (Value (IntHash _))          = return $ Value $ Bool True
isHash' (Value (CharHash _))         = return $ Value $ Bool True
isHash' (Value (StrHash _))          = return $ Value $ Bool True
isHash' (Intermediate (IIntHash _))  = return $ Value $ Bool True
isHash' (Intermediate (ICharHash _)) = return $ Value $ Bool True
isHash' (Intermediate (IStrHash _))  = return $ Value $ Bool True
isHash' _                            = return $ Value $ Bool False
