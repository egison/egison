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

import           Control.Monad                     (forM)
import           Control.Monad.IO.Class            (liftIO)

import           Data.IORef
import           Data.List                         (lookup)
import           Data.Foldable                     (toList)

import qualified Data.Sequence                     as Sq
import qualified Data.Vector                       as V

 {--  -- for 'egison-sqlite'
import qualified Database.SQLite3 as SQLite
 --}  -- for 'egison-sqlite'

import           Language.Egison.Data
import           Language.Egison.Data.Collection   (makeICollection)
import           Language.Egison.IExpr             (Index (..), stringToVar)
import           Language.Egison.Math
import           Language.Egison.Primitives.Arith
import           Language.Egison.Primitives.IO
import           Language.Egison.Primitives.String
import           Language.Egison.Primitives.Types
import           Language.Egison.Primitives.Utils

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
        
        , ("sortWithSign", sortWithSign)
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
     then return actual
     else throwErrorWithTrace (Assertion
            (show label ++ "\n expected: " ++ show expected ++ "\n but found: " ++ show actual))

-- | Sort a list of lists of integers and return the sign of the permutation
-- Each sublist is treated as a unit and sorted lexicographically
-- Used for antisymmetric tensor indices
sortWithSign :: String -> PrimitiveFunc
sortWithSign = oneArg' $ \val -> do
  case val of
    Collection xss -> do
      -- Extract list of lists
      let xss' = toList xss
      xs <- mapM extractIntList xss'
      -- Sort lists lexicographically and calculate permutation sign
      let (sign, sortedLists) = sortWithPermSign xs
      let flatList = concat sortedLists
      return $ Tuple [toEgison sign, Collection (Sq.fromList (map toEgison flatList))]
    _ -> throwErrorWithTrace (TypeMismatch "collection of collections" (Value val))
 where
  -- Extract integers from a collection
  extractIntList :: EgisonValue -> EvalM [Integer]
  extractIntList (Collection xs) = mapM extractInt (toList xs)
  extractIntList x = (:[]) <$> extractInt x
  
  extractInt :: EgisonValue -> EvalM Integer
  extractInt (ScalarData s) = fromEgison (ScalarData s)
  extractInt val = throwErrorWithTrace (TypeMismatch "integer" (Value val))
  
  -- Sort lists lexicographically and calculate permutation sign using bubble sort
  sortWithPermSign :: [[Integer]] -> (Integer, [[Integer]])
  sortWithPermSign [] = (1, [])
  sortWithPermSign [x] = (1, [x])
  sortWithPermSign [x, y] =
    if x > y then (-1, [y, x]) else (1, [x, y])
  sortWithPermSign xs =
    let sorted = bubbleSort xs
        swaps = countInversions xs sorted
        sign = if even swaps then 1 else -1
    in (sign, sorted)
  
  -- Bubble sort for lists (lexicographic comparison)
  bubbleSort :: [[Integer]] -> [[Integer]]
  bubbleSort [] = []
  bubbleSort xs =
    let (xs', changed) = bubblePass xs
    in if changed then bubbleSort xs' else xs'
  
  bubblePass :: [[Integer]] -> ([[Integer]], Bool)
  bubblePass [] = ([], False)
  bubblePass [x] = ([x], False)
  bubblePass (x:y:rest) =
    if x > y
      then let (rest', _) = bubblePass (x:rest)
           in (y:rest', True)
      else let (rest', changed) = bubblePass (y:rest)
           in (x:rest', changed)
  
  -- Count inversions between original and sorted list
  countInversions :: (Eq a) => [a] -> [a] -> Int
  countInversions orig sorted =
    let indices = map (\x -> findIndex x sorted) orig
        findIndex x xs = case lookup x (zip xs [0..]) of
          Just i -> i
          Nothing -> 0
    in countInv indices
  
  countInv :: [Int] -> Int
  countInv [] = 0
  countInv (x:xs) = length (filter (< x) xs) + countInv xs

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
