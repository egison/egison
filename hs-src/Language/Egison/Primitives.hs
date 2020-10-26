{-# LANGUAGE FlexibleContexts #-}

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

import           Data.IORef

import qualified Data.Sequence             as Sq
import qualified Data.Vector               as V

 {--  -- for 'egison-sqlite'
import qualified Database.SQLite3 as SQLite
 --}  -- for 'egison-sqlite'

import           Language.Egison.Data
import           Language.Egison.Data.Collection  (makeICollection)
import           Language.Egison.IExpr            (stringToVar, Index(..))
import           Language.Egison.Primitives.Arith
import           Language.Egison.Primitives.IO
import           Language.Egison.Primitives.String
import           Language.Egison.Primitives.Types
import           Language.Egison.Primitives.Utils
import           Language.Egison.Math

primitiveEnv :: IO Env
primitiveEnv = do
  bindings <- forM (constants ++ primitives ++ ioPrimitives) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

primitiveEnvNoIO :: IO Env
primitiveEnvNoIO = do
  bindings <- forM (constants ++ primitives) $ \(name, op) -> do
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

primitives :: [(String, EgisonValue)]
primitives =
  map (\(name, fn) -> (name, PrimitiveFunc (fn name))) strictPrimitives
  ++ map (\(name, fn) -> (name, LazyPrimitiveFunc (fn name))) lazyPrimitives
  ++ primitiveArithFunctions
  ++ primitiveStringFunctions
  ++ primitiveTypeFunctions
    where
      strictPrimitives =
        [ ("addSubscript", addSubscript)
        , ("addSuperscript", addSuperscript)

        , ("assert",      assert)
        , ("assertEqual", assertEqual)
        ]
      lazyPrimitives =
        [ ("tensorShape", tensorShape')
        , ("tensorToList", tensorToList')
        , ("dfOrder", dfOrder')
        ]

--
-- Miscellaneous primitive functions
--

tensorShape' :: String -> LazyPrimitiveFunc
tensorShape' = lazyOneArg tensorShape''
 where
  tensorShape'' (Value (TensorData (Tensor ns _ _))) =
    return . Value . Collection . Sq.fromList $ map toEgison ns
  tensorShape'' (ITensor (Tensor ns _ _)) =
    return . Value . Collection . Sq.fromList $ map toEgison ns
  tensorShape'' _ = return . Value . Collection $ Sq.fromList []

tensorToList' :: String -> LazyPrimitiveFunc
tensorToList' = lazyOneArg tensorToList''
 where
  tensorToList'' (Value (TensorData (Tensor _ xs _))) =
    return . Value . Collection . Sq.fromList $ V.toList xs
  tensorToList'' (ITensor (Tensor _ xs _)) = do
    inners <- liftIO . newIORef $ Sq.fromList (map IElement (V.toList xs))
    return (ICollection inners)
  tensorToList'' x = makeICollection [x]

dfOrder' :: String -> LazyPrimitiveFunc
dfOrder' = lazyOneArg dfOrder''
 where
  dfOrder'' (Value (TensorData (Tensor ns _ is))) =
    return $ Value (toEgison (fromIntegral (length ns - length is) :: Integer))
  dfOrder'' (ITensor (Tensor ns _ is)) =
    return $ Value (toEgison (fromIntegral (length ns - length is) :: Integer))
  dfOrder'' _ = return $ Value (toEgison (0 :: Integer))

addSubscript :: String -> PrimitiveFunc
addSubscript = twoArgs $ \fn sub ->
  case (fn, sub) of
    (ScalarData (SingleSymbol (Symbol id name is)), ScalarData s@(SingleSymbol (Symbol _ _ []))) ->
      return (ScalarData (SingleSymbol (Symbol id name (is ++ [Sub s]))))
    (ScalarData (SingleSymbol (Symbol id name is)), ScalarData s@(SingleTerm _ [])) ->
      return (ScalarData (SingleSymbol (Symbol id name (is ++ [Sub s]))))
    _ -> throwErrorWithTrace (TypeMismatch "symbol or integer" (Value fn))

addSuperscript :: String -> PrimitiveFunc
addSuperscript = twoArgs $ \fn sub ->
  case (fn, sub) of
    (ScalarData (SingleSymbol (Symbol id name is)), ScalarData s@(SingleSymbol (Symbol _ _ []))) ->
      return (ScalarData (SingleSymbol (Symbol id name (is ++ [Sup s]))))
    (ScalarData (SingleSymbol (Symbol id name is)), ScalarData s@(SingleTerm _ [])) ->
      return (ScalarData (SingleSymbol (Symbol id name (is ++ [Sup s]))))
    _ -> throwErrorWithTrace (TypeMismatch "symbol" (Value fn))

assert ::  String -> PrimitiveFunc
assert = twoArgs' $ \label test -> do
  test <- fromEgison test
  if test
    then return $ Bool True
    else throwErrorWithTrace (Assertion (show label))

assertEqual :: String -> PrimitiveFunc
assertEqual = threeArgs' $ \label actual expected ->
  if actual == expected
     then return $ Bool True
     else throwErrorWithTrace (Assertion
            (show label ++ "\n expected: " ++ show expected ++ "\n but found: " ++ show actual))

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
