{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

{- |
Module      : Language.Egison.Primitives
Licence     : MIT

This module provides primitive functions in Egison.
-}

module Language.Egison.Primitives
  ( primitiveEnv
  , primitiveEnvNoIO
  ) where

import           Control.Monad.Except

import           Data.Foldable             (toList)
import           Data.IORef
import           Text.Regex.TDFA           ((=~~))

import           System.Process            (readProcess)

import qualified Data.Sequence             as Sq
import qualified Data.Vector               as V

import           Data.Text                 (Text)
import qualified Data.Text                 as T

 {--  -- for 'egison-sqlite'
import qualified Database.SQLite3 as SQLite
 --}  -- for 'egison-sqlite'

import           Language.Egison.AST
import           Language.Egison.Data
import           Language.Egison.Eval
import           Language.Egison.EvalState (MonadEval(..))
import           Language.Egison.IExpr     (stringToVar)
import           Language.Egison.Parser
import           Language.Egison.Pretty
import           Language.Egison.Primitives.Arith
import           Language.Egison.Primitives.IO
import           Language.Egison.Primitives.Types
import           Language.Egison.Primitives.Utils
import           Language.Egison.Math

primitiveEnv :: IO Env
primitiveEnv = do
  let ops = map (\(name, fn) -> (name, PrimitiveFunc (fn name))) (primitives ++ ioPrimitives)
  bindings <- forM (constants ++ ops) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

primitiveEnvNoIO :: IO Env
primitiveEnvNoIO = do
  let ops = map (\(name, fn) -> (name, PrimitiveFunc (fn name))) primitives
  bindings <- forM (constants ++ ops) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

--
-- Constants
--

constants :: [(String, EgisonValue)]
constants = [ ("f.pi", Float 3.141592653589793)
            , ("f.e" , Float 2.718281828459045)
            ]

--
-- Primitives
--

primitives :: [(String, String -> PrimitiveFunc)]
primitives =
  [ ("tensorShape", tensorShape')
  , ("tensorToList", tensorToList')
  , ("dfOrder", dfOrder')

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

  , ("assert",      assert)
  , ("assertEqual", assertEqual)
  ]
  ++ primitiveTypeFunctions
  ++ primitiveArithFunctions

--
-- Tensor
--

tensorShape' :: String -> PrimitiveFunc
tensorShape' = oneArg' tensorShape''
 where
  tensorShape'' (TensorData (Tensor ns _ _)) = return . Collection . Sq.fromList $ map toEgison ns
  tensorShape'' _ = return . Collection $ Sq.fromList []

tensorToList' :: String -> PrimitiveFunc
tensorToList' = oneArg' tensorToList''
 where
  tensorToList'' (TensorData (Tensor _ xs _)) = return . Collection . Sq.fromList $ V.toList xs
  tensorToList'' x = return . Collection $ Sq.fromList [x]

dfOrder' :: String -> PrimitiveFunc
dfOrder' = oneArg' dfOrder''
 where
  dfOrder'' (TensorData (Tensor ns _ is)) = return (toEgison (fromIntegral (length ns - length is) :: Integer))
  dfOrder'' _ = return (toEgison (0 :: Integer))

--
-- String
--
pack :: String -> PrimitiveFunc
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

unpack :: String -> PrimitiveFunc
unpack = unaryOp T.unpack

unconsString :: String -> PrimitiveFunc
unconsString = oneArg $ \val -> do
  str <- fromEgison val
  case T.uncons str of
    Just (c, rest) -> return $ Tuple [Char c, String rest]
    Nothing -> throwError $ Default "Tried to unsnoc empty string"

lengthString :: String -> PrimitiveFunc
lengthString = unaryOp (toInteger . T.length)

appendString :: String -> PrimitiveFunc
appendString = binaryOp T.append

splitString :: String -> PrimitiveFunc
splitString = twoArgs $ \pat src -> do
  patStr <- fromEgison pat
  srcStr <- fromEgison src
  return . Collection . Sq.fromList $ map String $ T.splitOn patStr srcStr

regexString :: String -> PrimitiveFunc
regexString = twoArgs $ \pat src -> do
  patStr <- fromEgison pat
  srcStr <- fromEgison src
  case (T.unpack srcStr =~~ T.unpack patStr) :: (Maybe (String, String, String)) of
    Nothing -> return . Collection . Sq.fromList $ []
    Just (a,b,c) -> return . Collection . Sq.fromList $ [Tuple [String (T.pack a), String (T.pack b), String (T.pack c)]]

regexStringCaptureGroup :: String -> PrimitiveFunc
regexStringCaptureGroup = twoArgs $ \pat src -> do
  patStr <- fromEgison pat
  srcStr <- fromEgison src
  case (T.unpack srcStr =~~ T.unpack patStr) :: (Maybe [[String]]) of
    Nothing -> return . Collection . Sq.fromList $ []
    Just ((x:xs):_) -> do let (a, c) = T.breakOn (T.pack x) srcStr
                          return . Collection . Sq.fromList $ [Tuple [String a, Collection (Sq.fromList (map (String . T.pack) xs)), String (T.drop (length x) c)]]

addPrime :: String -> PrimitiveFunc
addPrime = oneArg $ \sym ->
  case sym of
    ScalarData (SingleSymbol (Symbol id name is)) ->
      return (ScalarData (SingleSymbol (Symbol id (name ++ "'") is)))
    _ -> throwError =<< TypeMismatch "symbol" (Value sym) <$> getFuncNameStack

addSubscript :: String -> PrimitiveFunc
addSubscript = twoArgs $ \fn sub ->
  case (fn, sub) of
    (ScalarData (SingleSymbol (Symbol id name is)), ScalarData s@(SingleSymbol (Symbol _ _ []))) ->
      return (ScalarData (SingleSymbol (Symbol id name (is ++ [Subscript s]))))
    (ScalarData (SingleSymbol (Symbol id name is)), ScalarData s@(SingleTerm _ [])) ->
      return (ScalarData (SingleSymbol (Symbol id name (is ++ [Subscript s]))))
    _ -> throwError =<< TypeMismatch "symbol or integer" (Value fn) <$> getFuncNameStack

addSuperscript :: String -> PrimitiveFunc
addSuperscript = twoArgs $ \fn sub ->
  case (fn, sub) of
    (ScalarData (SingleSymbol (Symbol id name is)), ScalarData s@(SingleSymbol (Symbol _ _ []))) ->
      return (ScalarData (SingleSymbol (Symbol id name (is ++ [Superscript s]))))
    (ScalarData (SingleSymbol (Symbol id name is)), ScalarData s@(SingleTerm _ [])) ->
      return (ScalarData (SingleSymbol (Symbol id name (is ++ [Superscript s]))))
    _ -> throwError =<< TypeMismatch "symbol" (Value fn) <$> getFuncNameStack

readProcess' :: String -> PrimitiveFunc
readProcess' = threeArgs' $ \cmd args input ->
  case (cmd, args, input) of
    (String cmdStr, Collection argStrs, String inputStr) -> do
      let cmd' = T.unpack cmdStr
      let args' = map (\case String argStr -> T.unpack argStr) (toList argStrs)
      let input' = T.unpack inputStr
      outputStr <- liftIO $ readProcess cmd' args' input'
      return (String (T.pack outputStr))
    (_, _, _) -> throwError =<< TypeMismatch "(string, collection, string)" (Value (Tuple [cmd, args, input])) <$> getFuncNameStack

read' :: String -> PrimitiveFunc
read'= oneArg' $ \val -> do
  str <- fromEgison val
  ast <- readExpr (T.unpack str)
  evalExpr nullEnv ast

readTSV :: String -> PrimitiveFunc
readTSV = oneArg' $ \val -> do
  str   <- fromEgison val
  exprs <- mapM (readExpr . T.unpack) (T.split (== '\t') str)
  rets  <- mapM (evalExpr nullEnv) exprs
  case rets of
    [ret] -> return ret
    _     -> return (Tuple rets)

show' :: String -> PrimitiveFunc
show'= oneArg' $ \val -> return $ toEgison $ T.pack $ show val

showTSV' :: String -> PrimitiveFunc
showTSV'= oneArg' $ \val -> return $ toEgison $ T.pack $ showTSV val

--
-- Test
--

assert ::  String -> PrimitiveFunc
assert = twoArgs' $ \label test -> do
  test <- fromEgison test
  if test
    then return $ Bool True
    else throwError =<< Assertion (show label) <$> getFuncNameStack

assertEqual :: String -> PrimitiveFunc
assertEqual = threeArgs' $ \label actual expected ->
  if actual == expected
     then return $ Bool True
     else throwError =<< Assertion
       (show label ++ "\n expected: " ++ show expected ++ "\n but found: " ++ show actual) <$> getFuncNameStack

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
