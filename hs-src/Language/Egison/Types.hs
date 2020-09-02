{- |
Module      : Language.Egison.Types
Licence     : MIT

This module contains functions for dynamic type systems.
-}

module Language.Egison.Types
  ( isBool
  , isInteger
  , isRational
  , isScalar
  , isFloat
  , isTensor
  , isChar
  , isString
  , isCollection
  , isHash
  ) where

import           Language.Egison.Data
import           Language.Egison.Math

--
-- Typing
--

isBool :: PrimitiveFunc
isBool (Bool _) = return $ Bool True
isBool _        = return $ Bool False

isInteger :: PrimitiveFunc
isInteger (ScalarData (Div (Plus []) (Plus [Term 1 []])))          = return $ Bool True
isInteger (ScalarData (Div (Plus [Term _ []]) (Plus [Term 1 []]))) = return $ Bool True
isInteger _                                                        = return $ Bool False

isRational :: PrimitiveFunc
isRational (ScalarData (Div (Plus []) (Plus [Term _ []])))          = return $ Bool True
isRational (ScalarData (Div (Plus [Term _ []]) (Plus [Term _ []]))) = return $ Bool True
isRational _                                                        = return $ Bool False

isScalar :: PrimitiveFunc
isScalar (ScalarData _) = return $ Bool True
isScalar _              = return $ Bool False

isTensor :: PrimitiveFunc
isTensor (TensorData _) = return $ Bool True
isTensor _              = return $ Bool False

isFloat :: PrimitiveFunc
isFloat (Float _) = return $ Bool True
isFloat _         = return $ Bool False

isChar :: PrimitiveFunc
isChar (Char _) = return $ Bool True
isChar _        = return $ Bool False

isString :: PrimitiveFunc
isString (String _) = return $ Bool True
isString _          = return $ Bool False

isCollection :: PrimitiveFunc
isCollection (Collection _) = return $ Bool True
isCollection _              = return $ Bool False

isHash :: PrimitiveFunc
isHash (IntHash _)  = return $ Bool True
isHash (CharHash _) = return $ Bool True
isHash (StrHash _)  = return $ Bool True
isHash _            = return $ Bool False
