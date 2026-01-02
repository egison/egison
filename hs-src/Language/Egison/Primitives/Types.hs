{- |
Module      : Language.Egison.Primitives.Types
Licence     : MIT

This module implements primitive functions that dynamically checks the types of
objects.
-}

module Language.Egison.Primitives.Types
  ( primitiveTypeFunctions
  ) where

import           Data.Char                        (chr, ord)
import           Data.Ratio                       ((%))
import qualified Data.Text                        as T
import qualified Data.HashMap.Strict              as HashMap

import           Language.Egison.Data
import           Language.Egison.Data.Utils       (evalRef)
import           Language.Egison.EvalState        (MonadEval(..))
import           Language.Egison.IExpr            (Var(..))
import           Language.Egison.Math
import           Language.Egison.Primitives.Utils
import           Language.Egison.Type.Types       (Type(..))

primitiveTypeFunctions :: [(String, EgisonValue)]
primitiveTypeFunctions =
  map (\(name, fn) -> (name, PrimitiveFunc (fn name))) strictPrimitives ++
    map (\(name, fn) -> (name, LazyPrimitiveFunc (fn name))) lazyPrimitives

strictPrimitives :: [(String, String -> PrimitiveFunc)]
strictPrimitives =
  [ ("itof", integerToFloat)
  , ("rtof", rationalToFloat)
  , ("ctoi", charToInteger)
  , ("itoc", integerToChar)
  ]

lazyPrimitives :: [(String, String -> LazyPrimitiveFunc)]
lazyPrimitives =
  [ ("isBool",       lazyOneArg isBool)
  , ("isInteger",    lazyOneArg isInteger)
  , ("isRational",   lazyOneArg isRational)
  , ("isScalar",     lazyOneArg isScalar)
  , ("isFloat",      lazyOneArg isFloat)
  , ("isChar",       lazyOneArg isChar)
  , ("isString",     lazyOneArg isString)
  , ("isCollection", lazyOneArg isCollection)
  , ("isHash",       lazyOneArg isHash)
  , ("isTensor",     lazyOneArg isTensor)
  , ("typeName",     lazyOneArg typeName)
  , ("makeClassMethod", lazyThreeArg makeClassMethod)
  ]

--
-- Typing
--

isBool :: WHNFData -> EvalM WHNFData
isBool (Value (Bool _)) = return . Value $ Bool True
isBool _                = return . Value $ Bool False

isInteger :: WHNFData -> EvalM WHNFData
isInteger (Value (ScalarData (Div (Plus []) (Plus [Term 1 []]))))          = return . Value $ Bool True
isInteger (Value (ScalarData (Div (Plus [Term _ []]) (Plus [Term 1 []])))) = return . Value $ Bool True
isInteger _                                                                = return . Value $ Bool False

isRational :: WHNFData -> EvalM WHNFData
isRational (Value (ScalarData (Div (Plus []) (Plus [Term _ []]))))          = return . Value $ Bool True
isRational (Value (ScalarData (Div (Plus [Term _ []]) (Plus [Term _ []])))) = return . Value $ Bool True
isRational _                                                                = return . Value $ Bool False

isScalar :: WHNFData -> EvalM WHNFData
isScalar (Value (ScalarData _)) = return . Value $ Bool True
isScalar _                      = return . Value $ Bool False

isTensor :: WHNFData -> EvalM WHNFData
isTensor (Value (TensorData _)) = return . Value $ Bool True
isTensor (ITensor _)            = return . Value $ Bool True
isTensor _                      = return . Value $ Bool False

isFloat :: WHNFData -> EvalM WHNFData
isFloat (Value (Float _)) = return . Value $ Bool True
isFloat _                 = return . Value $ Bool False

isChar :: WHNFData -> EvalM WHNFData
isChar (Value (Char _)) = return . Value $ Bool True
isChar _                = return . Value $ Bool False

isString :: WHNFData -> EvalM WHNFData
isString (Value (String _)) = return . Value $ Bool True
isString _                  = return . Value $ Bool False

isCollection :: WHNFData -> EvalM WHNFData
isCollection (Value (Collection _)) = return . Value $ Bool True
isCollection (ICollection _)        = return . Value $ Bool True
isCollection _                      = return . Value $ Bool False

isHash :: WHNFData -> EvalM WHNFData
isHash (Value (IntHash _))  = return . Value $ Bool True
isHash (Value (CharHash _)) = return . Value $ Bool True
isHash (Value (StrHash _))  = return . Value $ Bool True
isHash (IIntHash _)         = return . Value $ Bool True
isHash (ICharHash _)        = return . Value $ Bool True
isHash (IStrHash _)         = return . Value $ Bool True
isHash _                    = return . Value $ Bool False

-- | Get the type name of a value as a string
-- This is used for type class instance resolution
typeName :: WHNFData -> EvalM WHNFData
typeName (Value (Bool _))         = return . Value $ String (T.pack "Bool")
typeName (Value (ScalarData (Div (Plus []) (Plus [Term 1 []]))))          = return . Value $ String (T.pack "Integer")
typeName (Value (ScalarData (Div (Plus [Term _ []]) (Plus [Term 1 []])))) = return . Value $ String (T.pack "Integer")
typeName (Value (ScalarData _))   = return . Value $ String (T.pack "MathExpr")
typeName (Value (Float _))        = return . Value $ String (T.pack "Float")
typeName (Value (Char _))         = return . Value $ String (T.pack "Char")
typeName (Value (String _))       = return . Value $ String (T.pack "String")
typeName (Value (Collection _))   = return . Value $ String (T.pack "List")
typeName (ICollection _)          = return . Value $ String (T.pack "List")
typeName (Value (Tuple _))        = return . Value $ String (T.pack "Tuple")
typeName (ITuple _)               = return . Value $ String (T.pack "Tuple")
typeName (Value (IntHash _))      = return . Value $ String (T.pack "Hash")
typeName (Value (CharHash _))     = return . Value $ String (T.pack "Hash")
typeName (Value (StrHash _))      = return . Value $ String (T.pack "Hash")
typeName (IIntHash _)             = return . Value $ String (T.pack "Hash")
typeName (ICharHash _)            = return . Value $ String (T.pack "Hash")
typeName (IStrHash _)             = return . Value $ String (T.pack "Hash")
typeName (Value (TensorData _))   = return . Value $ String (T.pack "Tensor")
typeName (ITensor _)              = return . Value $ String (T.pack "Tensor")
typeName (Value (InductiveData name _)) = return . Value $ String (T.pack name)
typeName (IInductiveData name _)  = return . Value $ String (T.pack name)
typeName _                        = return . Value $ String (T.pack "Unknown")

--
-- Transform
--
integerToFloat :: String -> PrimitiveFunc
integerToFloat = rationalToFloat

rationalToFloat :: String -> PrimitiveFunc
rationalToFloat = oneArg $ \val ->
  case val of
    ScalarData (Div (Plus []) _)                           -> return $ Float 0
    ScalarData (Div (Plus [Term x []]) (Plus [Term y []])) -> return $ Float (fromRational (x % y))
    _                                                      -> throwErrorWithTrace (TypeMismatch "integer or rational number" (Value val))

charToInteger :: String -> PrimitiveFunc
charToInteger = unaryOp ctoi
  where
    ctoi :: Char -> Integer
    ctoi = fromIntegral . ord

integerToChar :: String -> PrimitiveFunc
integerToChar = unaryOp itoc
  where
    itoc :: Integer -> Char
    itoc = chr . fromIntegral

--
-- Type Class Support
--

-- | Create a ClassMethodRef from class name and method name
-- The third argument (dict) registers instances into the instance environment
-- Usage: makeClassMethod "Eq" "eq" {| ("Integer", eqIntegerEq), ("Float", eqFloatEq) |}
makeClassMethod :: WHNFData -> WHNFData -> WHNFData -> EvalM WHNFData
makeClassMethod clsArg methArg dictArg = do
  clsName <- extractString clsArg
  methName <- extractString methArg
  -- Register instances from the dictionary
  registerInstancesFromDict clsName methName dictArg
  -- Return a ClassMethodRef that will look up instances at call time
  return $ Value $ ClassMethodRef clsName methName
  where
    extractString :: WHNFData -> EvalM String
    extractString (Value (String s)) = return $ T.unpack s
    extractString whnf = throwErrorWithTrace (TypeMismatch "string" whnf)
    
    -- Register instances from a string hash
    registerInstancesFromDict :: String -> String -> WHNFData -> EvalM ()
    registerInstancesFromDict cls meth (Value (StrHash hash)) = do
      mapM_ (\(typeName, impl) -> do
        let ty = stringToType (T.unpack typeName)
            implName = getFuncName impl
        registerInstance cls meth ty implName
        ) (HashMap.toList hash)
    registerInstancesFromDict cls meth (IStrHash refs) = do
      mapM_ (\(typeName, ref) -> do
        impl <- evalRef ref >>= evalWHNF'
        let ty = stringToType (T.unpack typeName)
            implName = getFuncName impl
        registerInstance cls meth ty implName
        ) (HashMap.toList refs)
    registerInstancesFromDict _ _ whnf = throwErrorWithTrace (TypeMismatch "string hash" whnf)
    
    -- Convert type name string to Type
    stringToType :: String -> Type
    stringToType "Integer" = TInt
    stringToType "Float" = TFloat
    stringToType "Bool" = TBool
    stringToType "Char" = TChar
    stringToType "String" = TString
    stringToType name = TInductive name []
    
    -- Get function name from a value (assumes it's stored as a Func)
    getFuncName :: EgisonValue -> String
    getFuncName (Func (Just (Var name _)) _ _ _) = name
    getFuncName (Func Nothing _ _ _) = "<anonymous>"
    getFuncName (PrimitiveFunc _) = "<primitive>"
    getFuncName _ = "<unknown>"
    
    evalWHNF' :: WHNFData -> EvalM EgisonValue
    evalWHNF' (Value v) = return v
    evalWHNF' _ = throwErrorWithTrace (TypeMismatch "value" (Value Undefined))
