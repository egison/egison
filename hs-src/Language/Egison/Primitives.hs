{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}

{- |
Module      : Language.Egison.Primitives
Licence     : MIT

This module provides primitive functions in Egison.
-}

module Language.Egison.Primitives
  (
    primitiveEnv
  , primitiveEnvNoIO
  ) where

import           Control.Monad.Except

import           Data.Foldable             (toList)
import           Data.IORef
import           Data.Ratio
import           Text.Regex.TDFA           ((=~~))

import           System.IO
import           System.Process            (readProcess)
import           System.Random             (getStdRandom, randomR)

import qualified Data.Sequence             as Sq
import qualified Data.Vector               as V

import           Data.Char                 (chr, ord)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T

 {--  -- for 'egison-sqlite'
import qualified Database.SQLite3 as SQLite
 --}  -- for 'egison-sqlite'

import           Language.Egison.AST
import           Language.Egison.Core
import           Language.Egison.Data
import           Language.Egison.Data.Utils
import           Language.Egison.EvalState (MonadEval(..))
import           Language.Egison.Parser
import           Language.Egison.Pretty
import           Language.Egison.Math
import           Language.Egison.Types
import           Language.Egison.Tensor

primitiveEnv :: IO Env
primitiveEnv = do
  let ops = map (\(name, fn) -> (name, PrimitiveFunc name fn)) (primitives ++ ioPrimitives)
  bindings <- forM (constants ++ ops) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

primitiveEnvNoIO :: IO Env
primitiveEnvNoIO = do
  let ops = map (\(name, fn) -> (name, PrimitiveFunc name fn)) primitives
  bindings <- forM (constants ++ ops) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

{-# INLINE noArg #-}
noArg :: String -> EvalM EgisonValue -> PrimitiveFunc
noArg name f args = do
    args' <- tupleToList <$> evalWHNF args
    case args' of
      [] -> Value <$> f
      _  -> throwError =<< ArgumentsNumPrimitive name 0 (length args') <$> getFuncNameStack

{-# INLINE oneArg #-}
oneArg :: (EgisonValue -> EvalM EgisonValue) -> PrimitiveFunc
oneArg f arg = do
  arg' <- evalWHNF arg
  case arg' of
    (TensorData (Tensor ns ds js)) -> do
      ds' <- V.mapM f ds
      Value <$> fromTensor (Tensor ns ds' js)
    _ -> Value <$> f arg'

{-# INLINE oneArg' #-}
oneArg' :: (EgisonValue -> EvalM EgisonValue) -> PrimitiveFunc
oneArg' f arg = do
  arg' <- evalWHNF arg
  Value <$> f arg'

{-# INLINE twoArgs #-}
twoArgs :: String -> (EgisonValue -> EgisonValue -> EvalM EgisonValue) -> PrimitiveFunc
twoArgs name f args = do
  args' <- tupleToList <$> evalWHNF args
  case args' of
    [TensorData t1@Tensor{}, TensorData t2@Tensor{}] -> Value <$> (tProduct f t1 t2 >>= fromTensor)
    [TensorData(Tensor ns ds js), val] -> do
      ds' <- V.mapM (`f` val) ds
      Value <$> fromTensor (Tensor ns ds' js)
    [val, TensorData (Tensor ns ds js)] -> do
      ds' <- V.mapM (f val) ds
      Value <$> fromTensor (Tensor ns ds' js)
    [val, val'] -> Value <$> f val val'
    _ -> throwError =<< ArgumentsNumPrimitive name 2 (length args') <$> getFuncNameStack

{-# INLINE twoArgs' #-}
twoArgs' :: String -> (EgisonValue -> EgisonValue -> EvalM EgisonValue) -> PrimitiveFunc
twoArgs' name f args = do
  args' <- tupleToList <$> evalWHNF args
  case args' of
    [val, val'] -> Value <$> f val val'
    _           -> throwError =<< ArgumentsNumPrimitive name 2 (length args') <$> getFuncNameStack

{-# INLINE threeArgs' #-}
threeArgs' :: String -> (EgisonValue -> EgisonValue -> EgisonValue -> EvalM EgisonValue) -> PrimitiveFunc
threeArgs' name f args = do
  args' <- tupleToList <$> evalWHNF args
  case args' of
    [val, val', val''] -> Value <$> f val val' val''
    _                  -> throwError =<< ArgumentsNumPrimitive name 3 (length args') <$> getFuncNameStack

--
-- Constants
--

constants :: [(String, EgisonValue)]
constants = [
              ("f.pi", Float 3.141592653589793)
             ,("f.e" , Float 2.718281828459045)
              ]

--
-- Primitives
--

primitives :: [(String, PrimitiveFunc)]
primitives = [ ("b.+", plus)
             , ("b.-", minus)
             , ("b.*", multiply)
             , ("b./", divide)
             , ("f.+", floatBinaryOp "f.+" (+))
             , ("f.-", floatBinaryOp "f.-" (-))
             , ("f.*", floatBinaryOp "f.*" (*))
             , ("f./", floatBinaryOp "f./" (/))
             , ("numerator", numerator')
             , ("denominator", denominator')
             , ("fromMathExpr", fromScalarData)
             , ("toMathExpr'", toScalarData)
             , ("symbolNormalize", symbolNormalize)

             , ("modulo",   integerBinaryOp "modulo" mod)
             , ("quotient", integerBinaryOp "quotient" quot)
             , ("%",        integerBinaryOp "remainder" rem)
             , ("b.abs", rationalUnaryOp abs)
             , ("b.neg", rationalUnaryOp negate)

             , ("=",  eq)
             , ("<",  scalarCompare "(<)"  (<))
             , ("<=", scalarCompare "(<=)" (<=))
             , (">",  scalarCompare "(>)"  (>))
             , (">=", scalarCompare "(>=)" (>=))

             , ("round",    floatToIntegerOp round)
             , ("floor",    floatToIntegerOp floor)
             , ("ceiling",  floatToIntegerOp ceiling)
             , ("truncate", truncate')

             , ("b.sqrt",  floatUnaryOp sqrt)
             , ("b.sqrt'", floatUnaryOp sqrt)
             , ("b.exp",   floatUnaryOp exp)
             , ("b.log",   floatUnaryOp log)
             , ("b.sin",   floatUnaryOp sin)
             , ("b.cos",   floatUnaryOp cos)
             , ("b.tan",   floatUnaryOp tan)
             , ("b.asin",  floatUnaryOp asin)
             , ("b.acos",  floatUnaryOp acos)
             , ("b.atan",  floatUnaryOp atan)
             , ("b.sinh",  floatUnaryOp sinh)
             , ("b.cosh",  floatUnaryOp cosh)
             , ("b.tanh",  floatUnaryOp tanh)
             , ("b.asinh", floatUnaryOp asinh)
             , ("b.acosh", floatUnaryOp acosh)
             , ("b.atanh", floatUnaryOp atanh)

             , ("tensorShape", tensorShape')
             , ("tensorToList", tensorToList')
             , ("dfOrder", dfOrder')

             , ("itof", integerToFloat)
             , ("rtof", rationalToFloat)
             , ("ctoi", charToInteger)
             , ("itoc", integerToChar)

             , ("pack", pack)
             , ("unpack", unpack)
             , ("unconsString", unconsString)
             , ("lengthString", lengthString)
             , ("appendString", appendString)
             , ("splitString", splitString)
             , ("regex", regexString)
             , ("regexCg", regexStringCaptureGroup)

             , ("addPrime", addPrime)
             , ("addSubscript", addSubscript)
             , ("addSuperscript", addSuperscript)

             , ("readProcess", readProcess')

             , ("read", read')
             , ("readTsv", readTSV)
             , ("show", show')
             , ("showTsv", showTSV')

             , ("isBool", isBool')
             , ("isInteger", isInteger')
             , ("isRational", isRational')
             , ("isScalar", isScalar')
             , ("isFloat", isFloat')
             , ("isChar", isChar')
             , ("isString", isString')
             , ("isCollection", isCollection')
             , ("isHash", isHash')
             , ("isTensor", isTensor')

             , ("assert", assert)
             , ("assertEqual", assertEqual)
             ]

unaryOp :: (EgisonData a, EgisonData b) => (a -> b) -> PrimitiveFunc
unaryOp op = oneArg $ \val -> do
  v <- fromEgison val
  return $ toEgison (op v)

binaryOp :: String -> (EgisonData a, EgisonData b) => (a -> a -> b) -> PrimitiveFunc
binaryOp name op = twoArgs name $ \val val' -> do
  i <- fromEgison val
  i' <- fromEgison val'
  return $ toEgison (op i i')

rationalUnaryOp :: (Rational -> Rational) -> PrimitiveFunc
rationalUnaryOp = unaryOp

integerBinaryOp :: String -> (Integer -> Integer -> Integer) -> PrimitiveFunc
integerBinaryOp = binaryOp

floatUnaryOp :: (Double -> Double) -> PrimitiveFunc
floatUnaryOp = unaryOp

floatBinaryOp :: String -> (Double -> Double -> Double) -> PrimitiveFunc
floatBinaryOp = binaryOp

--
-- Arith
--

scalarBinaryOp :: String -> (ScalarData -> ScalarData -> ScalarData) -> PrimitiveFunc
scalarBinaryOp name mOp = twoArgs name scalarBinaryOp'
 where
  scalarBinaryOp' (ScalarData m1) (ScalarData m2) = (return . ScalarData) (mOp m1 m2)
  scalarBinaryOp' (ScalarData _)  val             = throwError =<< TypeMismatch "number" (Value val) <$> getFuncNameStack
  scalarBinaryOp' val             _               = throwError =<< TypeMismatch "number" (Value val) <$> getFuncNameStack

plus :: PrimitiveFunc
plus = scalarBinaryOp "b.+" mathPlus

minus :: PrimitiveFunc
minus = scalarBinaryOp "b.-" (\m1 m2 -> mathPlus m1 (mathNegate m2))

multiply :: PrimitiveFunc
multiply = scalarBinaryOp "b.*" mathMult

divide :: PrimitiveFunc
divide = scalarBinaryOp "b./" mathDiv

numerator' :: PrimitiveFunc
numerator' =  oneArg numerator''
 where
  numerator'' (ScalarData m) = return $ ScalarData (mathNumerator m)
  numerator'' val = throwError =<< TypeMismatch "rational" (Value val) <$> getFuncNameStack

denominator' :: PrimitiveFunc
denominator' =  oneArg denominator''
 where
  denominator'' (ScalarData m) = return $ ScalarData (mathDenominator m)
  denominator'' val = throwError =<< TypeMismatch "rational" (Value val) <$> getFuncNameStack

fromScalarData :: PrimitiveFunc
fromScalarData = oneArg fromScalarData'
 where
  fromScalarData' (ScalarData m) = return $ mathExprToEgison m
  fromScalarData' val = throwError =<< TypeMismatch "number" (Value val) <$> getFuncNameStack

toScalarData :: PrimitiveFunc
toScalarData = oneArg $ \val ->
  ScalarData . mathNormalize' <$> egisonToScalarData val

symbolNormalize :: PrimitiveFunc
symbolNormalize = oneArg $ \val ->
  case val of
    ScalarData s -> return $ ScalarData (rewriteSymbol s)
    _ -> throwError =<< TypeMismatch "math expression" (Value val) <$> getFuncNameStack


--
-- Pred
--
eq :: PrimitiveFunc
eq = twoArgs' "equal" $ \val val' ->
  return $ Bool $ val == val'

scalarCompare :: String -> (forall a. Ord a => a -> a -> Bool) -> PrimitiveFunc
scalarCompare name cmp = twoArgs' name $ \val1 val2 ->
  case (val1, val2) of
    (ScalarData _, ScalarData _) -> do
      r1 <- fromEgison val1 :: EvalM Rational
      r2 <- fromEgison val2 :: EvalM Rational
      return $ Bool (cmp r1 r2)
    (Float f1, Float f2) -> return $ Bool (cmp f1 f2)
    (ScalarData _, _) -> throwError =<< TypeMismatch "number" (Value val2) <$> getFuncNameStack
    (Float _,      _) -> throwError =<< TypeMismatch "float"  (Value val2) <$> getFuncNameStack
    _                 -> throwError =<< TypeMismatch "number" (Value val1) <$> getFuncNameStack

truncate' :: PrimitiveFunc
truncate' = oneArg $ \val -> numberUnaryOp' val
 where
  numberUnaryOp' (ScalarData (Div (Plus []) _)) = return $ toEgison (0 :: Integer)
  numberUnaryOp' (ScalarData (Div (Plus [Term x []]) (Plus [Term y []]))) = return $ toEgison (quot x y)
  numberUnaryOp' (Float x)             = return $ toEgison (truncate x :: Integer)
  numberUnaryOp' val                   = throwError =<< TypeMismatch "rational or float" (Value val) <$> getFuncNameStack

--
-- Tensor
--

tensorShape' :: PrimitiveFunc
tensorShape' = oneArg' tensorShape''
 where
  tensorShape'' (TensorData (Tensor ns _ _)) = return . Collection . Sq.fromList $ map toEgison ns
  tensorShape'' _ = return . Collection $ Sq.fromList []

tensorToList' :: PrimitiveFunc
tensorToList' = oneArg' tensorToList''
 where
  tensorToList'' (TensorData (Tensor _ xs _)) = return . Collection . Sq.fromList $ V.toList xs
  tensorToList'' x = return . Collection $ Sq.fromList [x]

dfOrder' :: PrimitiveFunc
dfOrder' = oneArg' dfOrder''
 where
  dfOrder'' (TensorData (Tensor ns _ is)) = return (toEgison (fromIntegral (length ns - length is) :: Integer))
  dfOrder'' _ = return (toEgison (0 :: Integer))

--
-- Transform
--
integerToFloat :: PrimitiveFunc
integerToFloat = rationalToFloat

rationalToFloat :: PrimitiveFunc
rationalToFloat = oneArg $ \val ->
  case val of
    (ScalarData (Div (Plus []) _)) -> return $ Float 0
    (ScalarData (Div (Plus [Term x []]) (Plus [Term y []]))) -> return $ Float (fromRational (x % y))
    _ -> throwError =<< TypeMismatch "integer or rational number" (Value val) <$> getFuncNameStack

charToInteger :: PrimitiveFunc
charToInteger = unaryOp ctoi
  where
    ctoi :: Char -> Integer
    ctoi = fromIntegral . ord

integerToChar :: PrimitiveFunc
integerToChar = unaryOp itoc
  where
    itoc :: Integer -> Char
    itoc = chr . fromIntegral

floatToIntegerOp :: (Double -> Integer) -> PrimitiveFunc
floatToIntegerOp = unaryOp

--
-- String
--
pack :: PrimitiveFunc
pack = oneArg $ \val -> do
  str <- packStringValue val
  return $ String str
  where
    packStringValue :: EgisonValue -> EvalM Text
    packStringValue (Collection seq) = do
      let ls = toList seq
      str <- mapM fromEgison ls
      return $ T.pack str
    packStringValue (Tuple [val]) = packStringValue val
    packStringValue val = throwError =<< TypeMismatch "collection" (Value val) <$> getFuncNameStack

unpack :: PrimitiveFunc
unpack = unaryOp T.unpack

unconsString :: PrimitiveFunc
unconsString = oneArg $ \val -> do
  str <- fromEgison val
  case T.uncons str of
    Just (c, rest) -> return $ Tuple [Char c, String rest]
    Nothing -> throwError $ Default "Tried to unsnoc empty string"

lengthString :: PrimitiveFunc
lengthString = unaryOp (toInteger . T.length)

appendString :: PrimitiveFunc
appendString = binaryOp "appendString" T.append

splitString :: PrimitiveFunc
splitString = twoArgs "splitString" $ \pat src -> do
  patStr <- fromEgison pat
  srcStr <- fromEgison src
  return . Collection . Sq.fromList $ map String $ T.splitOn patStr srcStr

regexString :: PrimitiveFunc
regexString = twoArgs "regexString" $ \pat src -> do
  patStr <- fromEgison pat
  srcStr <- fromEgison src
  case (T.unpack srcStr =~~ T.unpack patStr) :: (Maybe (String, String, String)) of
    Nothing -> return . Collection . Sq.fromList $ []
    Just (a,b,c) -> return . Collection . Sq.fromList $ [Tuple [String $ T.pack a, String $ T.pack b, String $ T.pack c]]

regexStringCaptureGroup :: PrimitiveFunc
regexStringCaptureGroup = twoArgs "regexCg" $ \pat src -> do
  patStr <- fromEgison pat
  srcStr <- fromEgison src
  case (T.unpack srcStr =~~ T.unpack patStr) :: (Maybe [[String]]) of
    Nothing -> return . Collection . Sq.fromList $ []
    Just ((x:xs):_) -> do let (a, c) = T.breakOn (T.pack x) srcStr
                          return . Collection . Sq.fromList $ [Tuple [String a, Collection (Sq.fromList (map (String . T.pack) xs)), String (T.drop (length x) c)]]

--regexStringMatch :: PrimitiveFunc
--regexStringMatch = twoArgs $ \pat src -> do
--  case (pat, src) of
--    (String patStr, String srcStr) -> return . Bool $ (((T.unpack srcStr) =~ (T.unpack patStr)) :: Bool)
--    (String _, _) -> throwError =<< TypeMismatch "string" (Value src) <$> getFuncNameStack
--    (_, _) -> throwError =<< TypeMismatch "string" (Value pat) <$> getFuncNameStack

addPrime :: PrimitiveFunc
addPrime = oneArg $ \sym ->
  case sym of
    ScalarData (SingleSymbol (Symbol id name is)) ->
      return (ScalarData (SingleSymbol (Symbol id (name ++ "'") is)))
    _ -> throwError =<< TypeMismatch "symbol" (Value sym) <$> getFuncNameStack

addSubscript :: PrimitiveFunc
addSubscript = twoArgs "addSubscript" $ \fn sub ->
  case (fn, sub) of
    (ScalarData (SingleSymbol (Symbol id name is)), ScalarData s@(SingleSymbol (Symbol _ _ []))) ->
      return (ScalarData (SingleSymbol (Symbol id name (is ++ [Subscript s]))))
    (ScalarData (SingleSymbol (Symbol id name is)), ScalarData s@(SingleTerm _ [])) ->
      return (ScalarData (SingleSymbol (Symbol id name (is ++ [Subscript s]))))
    _ -> throwError =<< TypeMismatch "symbol or integer" (Value fn) <$> getFuncNameStack

addSuperscript :: PrimitiveFunc
addSuperscript = twoArgs "addSuperscript" $ \fn sub ->
  case (fn, sub) of
    (ScalarData (SingleSymbol (Symbol id name is)), ScalarData s@(SingleSymbol (Symbol _ _ []))) ->
      return (ScalarData (SingleSymbol (Symbol id name (is ++ [Superscript s]))))
    (ScalarData (SingleSymbol (Symbol id name is)), ScalarData s@(SingleTerm _ [])) ->
      return (ScalarData (SingleSymbol (Symbol id name (is ++ [Superscript s]))))
    _ -> throwError =<< TypeMismatch "symbol" (Value fn) <$> getFuncNameStack

readProcess' :: PrimitiveFunc
readProcess' = threeArgs' "readProcess" $ \cmd args input ->
  case (cmd, args, input) of
    (String cmdStr, Collection argStrs, String inputStr) -> do
      let cmd' = T.unpack cmdStr
      let args' = map (\case String argStr -> T.unpack argStr) (toList argStrs)
      let input' = T.unpack inputStr
      outputStr <- liftIO $ readProcess cmd' args' input'
      return (String (T.pack outputStr))
    (_, _, _) -> throwError =<< TypeMismatch "(string, collection, string)" (Value (Tuple [cmd, args, input])) <$> getFuncNameStack

read' :: PrimitiveFunc
read'= oneArg' $ \val -> do
  str <- fromEgison val
  ast <- readExpr (T.unpack str)
  evalExprDeep nullEnv ast

readTSV :: PrimitiveFunc
readTSV = oneArg' $ \val -> do
  str   <- fromEgison val
  exprs <- mapM (readExpr . T.unpack) (T.split (== '\t') str)
  rets  <- mapM (evalExprDeep nullEnv) exprs
  case rets of
    [ret] -> return ret
    _     -> return (Tuple rets)

show' :: PrimitiveFunc
show'= oneArg' $ \val -> return $ toEgison $ T.pack $ show val

showTSV' :: PrimitiveFunc
showTSV'= oneArg' $ \val -> return $ toEgison $ T.pack $ showTSV val

--
-- Test
--

assert ::  PrimitiveFunc
assert = twoArgs' "assert" $ \label test -> do
  test <- fromEgison test
  if test
    then return $ Bool True
    else throwError =<< Assertion (show label) <$> getFuncNameStack

assertEqual :: PrimitiveFunc
assertEqual = threeArgs' "assertEqual" $ \label actual expected ->
  if actual == expected
     then return $ Bool True
     else throwError =<< Assertion
       (show label ++ "\n expected: " ++ show expected ++ "\n but found: " ++ show actual) <$> getFuncNameStack

--
-- IO Primitives
--

ioPrimitives :: [(String, PrimitiveFunc)]
ioPrimitives = [ ("return", return')
               , ("openInputFile", makePort ReadMode)
               , ("openOutputFile", makePort WriteMode)
               , ("closeInputPort", closePort)
               , ("closeOutputPort", closePort)
               , ("readChar", readChar)
               , ("readLine", readLine)
               , ("writeChar", writeChar)
               , ("write", writeString)

               , ("readCharFromPort", readCharFromPort)
               , ("readLineFromPort", readLineFromPort)
               , ("writeCharToPort", writeCharToPort)
               , ("writeToPort", writeStringToPort)

               , ("isEof", isEOFStdin)
               , ("flush", flushStdout)
               , ("isEofPort", isEOFPort)
               , ("flushPort", flushPort)
               , ("readFile", readFile')

               , ("rand", randRange)
               , ("f.rand", randRangeDouble)

               , ("newIORef", newIORef')
               , ("writeIORef", writeIORef')
               , ("readIORef", readIORef')
               ]

makeIO :: EvalM EgisonValue -> EgisonValue
makeIO m = IOFunc $ fmap (Value . Tuple . (World :) . (:[])) m

makeIO' :: EvalM () -> EgisonValue
makeIO' m = IOFunc $ m >> return (Value $ Tuple [World, Tuple []])

return' :: PrimitiveFunc
return' = oneArg' $ \val -> return $ makeIO $ return val

makePort :: IOMode -> PrimitiveFunc
makePort mode = oneArg' $ \val -> do
  filename <- fromEgison val
  port <- liftIO $ openFile (T.unpack filename) mode
  return $ makeIO $ return (Port port)

closePort :: PrimitiveFunc
closePort = oneArg' $ \val -> do
  port <- fromEgison val
  return $ makeIO' $ liftIO $ hClose port

writeChar :: PrimitiveFunc
writeChar = oneArg' $ \val -> do
  c <- fromEgison val
  return $ makeIO' $ liftIO $ putChar c

writeCharToPort :: PrimitiveFunc
writeCharToPort = twoArgs' "writeCharToPort" $ \val val' -> do
  port <- fromEgison val
  c <- fromEgison val'
  return $ makeIO' $ liftIO $ hPutChar port c

writeString :: PrimitiveFunc
writeString = oneArg' $ \val -> do
  s <- fromEgison val
  return $ makeIO' $ liftIO $ T.putStr s

writeStringToPort :: PrimitiveFunc
writeStringToPort = twoArgs' "writeStringToPort" $ \val val' -> do
  port <- fromEgison val
  s <- fromEgison val'
  return $ makeIO' $ liftIO $ T.hPutStr port s

flushStdout :: PrimitiveFunc
flushStdout = noArg "flush" $ return $ makeIO' $ liftIO $ hFlush stdout

flushPort :: PrimitiveFunc
flushPort = oneArg' $ \val -> do
  port <- fromEgison val
  return $ makeIO' $ liftIO $ hFlush port

readChar :: PrimitiveFunc
readChar = noArg "readChar" $ return $ makeIO $ liftIO $ fmap Char getChar

readCharFromPort :: PrimitiveFunc
readCharFromPort = oneArg' $ \val -> do
  port <- fromEgison val
  c <- liftIO $ hGetChar port
  return $ makeIO $ return (Char c)

readLine :: PrimitiveFunc
readLine = noArg "readLine" $ return $ makeIO $ liftIO $ fmap toEgison T.getLine

readLineFromPort :: PrimitiveFunc
readLineFromPort = oneArg' $ \val -> do
  port <- fromEgison val
  s <- liftIO $ T.hGetLine port
  return $ makeIO $ return $ toEgison s

readFile' :: PrimitiveFunc
readFile' =  oneArg' $ \val -> do
  filename <- fromEgison val
  s <- liftIO $ T.readFile $ T.unpack filename
  return $ makeIO $ return $ toEgison s

isEOFStdin :: PrimitiveFunc
isEOFStdin = noArg "isEOF" $ return $ makeIO $ liftIO $ fmap Bool isEOF

isEOFPort :: PrimitiveFunc
isEOFPort = oneArg' $ \val -> do
  port <- fromEgison val
  b <- liftIO $ hIsEOF port
  return $ makeIO $ return (Bool b)

randRange :: PrimitiveFunc
randRange = twoArgs' "rand" $ \val val' -> do
  i <- fromEgison val :: EvalM Integer
  i' <- fromEgison val' :: EvalM Integer
  n <- liftIO $ getStdRandom $ randomR (i, i')
  return $ makeIO $ return $ toEgison n

randRangeDouble :: PrimitiveFunc
randRangeDouble = twoArgs' "f.rand" $ \val val' -> do
  i <- fromEgison val :: EvalM Double
  i' <- fromEgison val' :: EvalM Double
  n <- liftIO $ getStdRandom $ randomR (i, i')
  return $ makeIO $ return $ toEgison n

newIORef' :: PrimitiveFunc
newIORef' = noArg "newIORef" $ do
  ref <- liftIO $ newIORef Undefined
  return $ makeIO $ return (RefBox ref)

writeIORef' :: PrimitiveFunc
writeIORef' = twoArgs "writeIORef" $ \ref val -> do
  ref' <- fromEgison ref
  return $ makeIO' $ liftIO $ writeIORef ref' val

readIORef' :: PrimitiveFunc
readIORef' = oneArg $ \ref -> do
  ref' <- fromEgison ref
  val <- liftIO $ readIORef ref'
  return $ makeIO $ return val


 {-- -- for 'egison-sqlite'
sqlite :: PrimitiveFunc
sqlite  = twoArgs' $ \val val' -> do
  dbName <- fromEgison val
  qStr <- fromEgison val'
  ret <- liftIO $ query' (T.pack dbName) $ T.pack qStr
  return $ makeIO $ return $ Collection $ Sq.fromList $ map (\r -> Tuple (map toEgison r)) ret
 where
  query' :: T.Text -> T.Text -> IO [[String]]
  query' dbName q = do
    db <- SQLite.open dbName
    rowsRef <- newIORef []
    SQLite.execWithCallback db q (\_ _ mcs -> do
                                    row <- forM mcs (\mcol -> case mcol of
                                                              Just col ->  return $ T.unpack col
                                                              Nothing -> return "null")
                                    rows <- readIORef rowsRef
                                    writeIORef rowsRef (row:rows))
    SQLite.close db
    ret <- readIORef rowsRef
    return $ reverse ret
 --} -- for 'egison-sqlite'
