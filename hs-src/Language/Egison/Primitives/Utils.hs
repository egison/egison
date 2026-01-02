{- |
Module      : Language.Egison.Primitives.Utils
Licence     : MIT
-}

module Language.Egison.Primitives.Utils
  ( noArg
  , oneArg
  , oneArg'
  , twoArgs
  , twoArgs'
  , threeArgs'
  , lazyOneArg
  , lazyThreeArg
  , unaryOp
  , binaryOp
  ) where

import qualified Data.Vector            as V

import           Language.Egison.Data
import           Language.Egison.Tensor

{-# INLINE noArg #-}
noArg :: EvalM EgisonValue -> String -> PrimitiveFunc
noArg f name args =
  case args of
    [] -> f
    [Tuple []] -> f
    _ ->
      throwErrorWithTrace (ArgumentsNumPrimitive name 1 (length args))

{-# INLINE oneArg #-}
oneArg :: (EgisonValue -> EvalM EgisonValue) -> String -> PrimitiveFunc
oneArg f name args =
  case args of
    [TensorData (Tensor ns ds js)] -> do
      ds' <- V.mapM f ds
      return $ TensorData (Tensor ns ds' js)
    [arg] -> f arg
    _ ->
      throwErrorWithTrace (ArgumentsNumPrimitive name 1 (length args))

{-# INLINE oneArg' #-}
oneArg' :: (EgisonValue -> EvalM EgisonValue) -> String -> PrimitiveFunc
oneArg' f name args =
  case args of
    [arg] -> f arg
    _     ->
      throwErrorWithTrace (ArgumentsNumPrimitive name 1 (length args))

{-# INLINE twoArgs #-}
twoArgs :: (EgisonValue -> EgisonValue -> EvalM EgisonValue) -> String -> PrimitiveFunc
twoArgs f name args =
  case args of
    [TensorData t1@Tensor{}, TensorData t2@Tensor{}] ->
      tProduct f t1 t2 >>= fromTensor
    [TensorData(Tensor ns ds js), val] -> do
      ds' <- V.mapM (`f` val) ds
      return $ TensorData (Tensor ns ds' js)
    [val, TensorData (Tensor ns ds js)] -> do
      ds' <- V.mapM (f val) ds
      return $ TensorData (Tensor ns ds' js)
    [val, val'] -> f val val'
    [val] -> return . PrimitiveFunc $ oneArg (f val) name
    _ -> throwErrorWithTrace (ArgumentsNumPrimitive name 2 (length args))

{-# INLINE twoArgs' #-}
twoArgs' :: (EgisonValue -> EgisonValue -> EvalM EgisonValue) -> String -> PrimitiveFunc
twoArgs' f name args =
  case args of
    [val, val'] -> f val val'
    [val]       -> return . PrimitiveFunc $ oneArg' (f val) name
    _           -> throwErrorWithTrace (ArgumentsNumPrimitive name 2 (length args))

{-# INLINE threeArgs' #-}
threeArgs' :: (EgisonValue -> EgisonValue -> EgisonValue -> EvalM EgisonValue) -> String -> PrimitiveFunc
threeArgs' f name args =
  case args of
    [val, val', val''] -> f val val' val''
    [val, val']        -> return . PrimitiveFunc $ oneArg' (f val val') name
    [val]              -> return . PrimitiveFunc $ twoArgs' (f val) name
    _                  -> throwErrorWithTrace (ArgumentsNumPrimitive name 3 (length args))

lazyOneArg :: (WHNFData -> EvalM WHNFData) -> String -> LazyPrimitiveFunc
lazyOneArg f name args =
  case args of
    [arg] -> f arg
    _     -> throwErrorWithTrace (ArgumentsNumPrimitive name 1 (length args))

lazyThreeArg :: (WHNFData -> WHNFData -> WHNFData -> EvalM WHNFData) -> String -> LazyPrimitiveFunc
lazyThreeArg f name args =
  case args of
    [arg1, arg2, arg3] -> f arg1 arg2 arg3
    _     -> throwErrorWithTrace (ArgumentsNumPrimitive name 3 (length args))

unaryOp :: (EgisonData a, EgisonData b) => (a -> b) -> String -> PrimitiveFunc
unaryOp op = oneArg $ \val -> do
  v <- fromEgison val
  return $ toEgison (op v)

binaryOp :: (EgisonData a, EgisonData b) => (a -> a -> b) -> String -> PrimitiveFunc
binaryOp op = twoArgs $ \val val' -> do
  i <- fromEgison val
  i' <- fromEgison val'
  return $ toEgison (op i i')
