{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ViewPatterns          #-}

{- |
Module      : Language.Egison.Tensor
Licence     : MIT

This module contains functions for tensors.
-}

module Language.Egison.Tensor
    ( TensorComponent (..)
    -- * Tensor
    , tref
    , enumTensorIndices
    , changeIndex
    , tTranspose
    , tTranspose'
    , tFlipIndices
    , appendDF
    , removeDF
    , tMap
    , tMap2
    , tProduct
    , tContract
    , tContract'
    , tConcat'
    ) where

import           Prelude                   hiding (foldr, mappend, mconcat)

import           Control.Monad.Except      hiding (join)
import qualified Data.Vector               as V
import           Data.List                 (delete, intersect, partition, (\\))

import           Control.Egison
import qualified Control.Egison            as M

import           Language.Egison.Data
import           Language.Egison.EvalState (getFuncNameStack)
import           Language.Egison.IExpr     (Index(..), extractSupOrSubIndex)
import           Language.Egison.Math
import           Language.Egison.RState


data IndexM m = IndexM m
instance M.Matcher m a => M.Matcher (IndexM m) (Index a)

sub :: M.Matcher m a => M.Pattern (PP a) (IndexM m) (Index a) a
sub _ _ (Sub a) = pure a
sub _ _ _       = mzero
subM :: M.Matcher m a => IndexM m -> Index a -> m
subM (IndexM m) _ = m

sup :: M.Matcher m a => M.Pattern (PP a) (IndexM m) (Index a) a
sup _ _ (Sup a) = pure a
sup _ _ _       = mzero
supM :: M.Matcher m a => IndexM m -> Index a -> m
supM (IndexM m) _ = m

supsub :: M.Matcher m a => M.Pattern (PP a) (IndexM m) (Index a) a
supsub _ _ (SupSub a) = pure a
supsub _ _ _          = mzero
supsubM :: M.Matcher m a => IndexM m -> Index a -> m
supsubM (IndexM m) _ = m

--
-- Tensors
--

class TensorComponent a where
  fromTensor :: Tensor a -> EvalM a
  toTensor :: a -> EvalM (Tensor a)

instance TensorComponent EgisonValue where
  fromTensor t@Tensor{} = return $ TensorData t
  fromTensor (Scalar x) = return x
  toTensor (TensorData t) = return t
  toTensor x              = return $ Scalar x

instance TensorComponent WHNFData where
  fromTensor t@Tensor{} = return (ITensor t)
  fromTensor (Scalar x) = return x
  toTensor (ITensor t) = return t
  toTensor x           = return (Scalar x)

tensorElems :: TensorComponent a => a -> EvalM (V.Vector a)
tensorElems t = do
  Tensor _ xs _ <- toTensor t
  return xs

tShape :: Tensor a -> Shape
tShape (Tensor ns _ _) = ns
tShape (Scalar _)      = []

tToVector :: Tensor a -> V.Vector a
tToVector (Tensor _ xs _) = xs
tToVector (Scalar x)      = V.fromList [x]

tIndex :: Tensor a -> [Index EgisonValue]
tIndex (Tensor _ _ js) = js
tIndex (Scalar _)      = []

tIntRef' :: Integer -> Tensor a -> EvalM (Tensor a)
tIntRef' i (Tensor [n] xs _) =
  if 0 < i && i <= n
     then return . Scalar $ xs V.! fromIntegral (i - 1)
     else throwError =<< TensorIndexOutOfBounds i n <$> getFuncNameStack
tIntRef' i (Tensor (n:ns) xs js) =
  if 0 < i && i <= n
   then let w = fromIntegral (product ns)
            ys = V.take w (V.drop (w * fromIntegral (i - 1)) xs)
         in return $ Tensor ns ys (cdr js)
   else throwError =<< TensorIndexOutOfBounds i n <$> getFuncNameStack
tIntRef' _ _ = throwError $ Default "More indices than the order of the tensor"

tIntRef :: [Integer] -> Tensor a -> EvalM (Tensor a)
tIntRef [] (Tensor [] xs _)
  | V.length xs == 1 = return $ Scalar (xs V.! 0)
  | otherwise = throwError =<< EgisonBug "sevaral elements in scalar tensor" <$> getFuncNameStack
tIntRef [] t = return t
tIntRef (m:ms) t = tIntRef' m t >>= tIntRef ms

tIntRef1 :: [Integer] -> Tensor a -> EvalM a
tIntRef1 [] (Scalar x) = return x
tIntRef1 [] (Tensor [] xs _) | V.length xs == 1 = return (xs V.! 0)
tIntRef1 [] _ = throwError =<< EgisonBug "sevaral elements in scalar tensor" <$> getFuncNameStack
tIntRef1 (m:ms) t = tIntRef' m t >>= tIntRef1 ms

pattern SupOrSubIndex :: a -> Index a
pattern SupOrSubIndex i <- (extractSupOrSubIndex -> Just i)

tref :: [Index EgisonValue] -> Tensor a -> EvalM (Tensor a)
tref [] (Tensor [] xs _)
  | V.length xs == 1 = return $ Scalar (xs V.! 0)
  | otherwise = throwError =<< EgisonBug "sevaral elements in scalar tensor" <$> getFuncNameStack
tref [] t = return t
tref (s@(SupOrSubIndex (ScalarData (SingleSymbol _))):ms) (Tensor (_:ns) xs js) = do
  let yss = split (product ns) xs
  ts <- mapM (\ys -> tref ms (Tensor ns ys (cdr js))) yss
  tConcat s ts
tref (SupOrSubIndex (ScalarData (SingleTerm m [])):ms) t = tIntRef' m t >>= tref ms
tref (SupOrSubIndex (ScalarData ZeroExpr):_) _ = throwError $ Default "tensor index out of bounds: 0"
tref (s@(SupOrSubIndex (Tuple [mVal, nVal])):ms) t@(Tensor is _ _) = do
  m <- fromEgison mVal
  n <- fromEgison nVal
  if m > n
    then
      return (Tensor (replicate (length is) 0) V.empty [])
    else do
      ts <- mapM (\i -> tIntRef' i t >>= tref ms) [m..n]
      symId <- fresh
      let index = symbolScalarData "" (":::" ++ symId)
      case s of
        Sub{}    -> tConcat (Sub index) ts
        Sup{}    -> tConcat (Sup index) ts
        SupSub{} -> tConcat (SupSub index) ts
tref (_:_) _ = throwError $ Default "Tensor index must be an integer or a single symbol."

-- Enumarates all indices (1-indexed) from shape
-- ex.
-- >>> enumTensorIndices [2,2,2]
-- [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]]
enumTensorIndices :: Shape -> [[Integer]]
enumTensorIndices [] = [[]]
enumTensorIndices (n:ns) = concatMap (\i -> map (i:) (enumTensorIndices ns)) [1..n]

changeIndex :: Index String -> EgisonValue -> Index String
changeIndex (Sup s) m = Sup (s ++ show m)
changeIndex (Sub s) m = Sub (s ++ show m)

-- transIndex [a, b, c] [c, a, b] [2, 3, 4] = [4, 2, 3]
transIndex :: [Index EgisonValue] -> [Index EgisonValue] -> Shape -> EvalM Shape
transIndex is js ns = do
  mapM (\j -> case lookup j (zip is ns) of
                Just n  -> return n
                Nothing -> throwError $ Default "cannot transpose becuase of the inconsitent symbolic tensor indices")
       js

tTranspose :: [Index EgisonValue] -> Tensor a -> EvalM (Tensor a)
tTranspose is t@(Tensor _ _ js) | length is > length js =
  return t
tTranspose is t@(Tensor ns _ js) = do
  let js' = take (length is) js
  let ds = complementWithDF ns is
  ns' <- transIndex (js' ++ ds) (is ++ ds) ns
  xs' <- V.fromList <$> mapM (transIndex (is ++ ds) (js' ++ ds)) (enumTensorIndices ns') >>= mapM (`tIntRef1` t)
  return $ Tensor ns' xs' is

tTranspose' :: [EgisonValue] -> Tensor a -> EvalM (Tensor a)
tTranspose' is t@(Tensor _ _ js) =
  case mapM (\i -> f i js) is of
    Nothing -> return t
    Just is' -> tTranspose is' t
 where
  f :: EgisonValue -> [Index EgisonValue] -> Maybe (Index EgisonValue)
  f i js =
    match dfs js (List (IndexM Eql))
      [ [mc| _ ++ ($j & (sub #i | sup #i | supsub #i)) : _ -> Just j |]
      , [mc| _ -> Nothing |]
      ]

tFlipIndices :: Tensor a -> EvalM (Tensor a)
tFlipIndices (Tensor ns xs js) = return $ Tensor ns xs (map reverseIndex js)

appendDF :: Integer -> WHNFData -> WHNFData
appendDF id (ITensor (Tensor s xs is)) =
  let k = fromIntegral (length s - length is)
   in ITensor (Tensor s xs (is ++ map (DF id) [1..k]))
appendDF id (Value (TensorData (Tensor s xs is))) =
  let k = fromIntegral (length s - length is)
   in Value (TensorData (Tensor s xs (is ++ map (DF id) [1..k])))
appendDF _ whnf = whnf

removeDF :: WHNFData -> EvalM WHNFData
removeDF (ITensor (Tensor s xs is)) = do
  let (ds, js) = partition isDF is
  Tensor s ys _ <- tTranspose (js ++ ds) (Tensor s xs is)
  return (ITensor (Tensor s ys js))
 where
  isDF (DF _ _) = True
  isDF _        = False
removeDF (Value (TensorData (Tensor s xs is))) = do
  let (ds, js) = partition isDF is
  Tensor s ys _ <- tTranspose (js ++ ds) (Tensor s xs is)
  return (Value (TensorData (Tensor s ys js)))
 where
  isDF (DF _ _) = True
  isDF _        = False
removeDF whnf = return whnf

tMap :: TensorComponent b => (a -> EvalM b) -> Tensor a -> EvalM (Tensor b)
tMap f (Tensor ns xs js') = do
  let js = js' ++ complementWithDF ns js'
  xs' <- V.mapM f xs
  t <- toTensor (V.head xs')
  case t of
    Tensor ns1 _ js1' -> do
      let js1 = js1' ++ complementWithDF ns1 js1'
      xs'' <- V.concat <$> mapM tensorElems (V.toList xs')
      tContract' $ Tensor (ns ++ ns1) xs'' (js ++ js1)
    _ -> return $ Tensor ns xs' js
tMap f (Scalar x) = Scalar <$> f x

tMap2 :: TensorComponent c => (a -> b -> EvalM c) -> Tensor a -> Tensor b -> EvalM (Tensor c)
tMap2 f (Tensor ns1 xs1 js1') (Tensor ns2 xs2 js2') = do
  let js1 = js1' ++ complementWithDF ns1 js1'
  let js2 = js2' ++ complementWithDF ns2 js2'
  let cjs = js1 `intersect` js2
  t1' <- tTranspose (cjs ++ (js1 \\ cjs)) (Tensor ns1 xs1 js1)
  t2' <- tTranspose (cjs ++ (js2 \\ cjs)) (Tensor ns2 xs2 js2)
  let cns = take (length cjs) (tShape t1')
  rts1 <- mapM (`tIntRef` t1') (enumTensorIndices cns)
  rts2 <- mapM (`tIntRef` t2') (enumTensorIndices cns)
  rts' <- zipWithM (tProduct f) rts1 rts2
  let ret = Tensor (cns ++ tShape (head rts')) (V.concat (map tToVector rts')) (cjs ++ tIndex (head rts'))
  tTranspose (uniq (tDiagIndex (js1 ++ js2))) ret
 where
  uniq :: [Index EgisonValue] -> [Index EgisonValue]
  uniq []     = []
  uniq (x:xs) = x:uniq (delete x xs)
tMap2 f t@Tensor{} (Scalar x) = tMap (`f` x) t
tMap2 f (Scalar x) t@Tensor{} = tMap (f x) t
tMap2 f (Scalar x1) (Scalar x2) = Scalar <$> f x1 x2

tDiag :: Tensor a -> EvalM (Tensor a)
tDiag t@(Tensor _ _ js) =
  case filter (\j -> any (p j) js) js of
    [] -> return t
    xs -> do
      let ys = js \\ (xs ++ map reverseIndex xs)
      t2 <- tTranspose (xs ++ map reverseIndex xs ++ ys) t
      let (ns1, tmp) = splitAt (length xs) (tShape t2)
      let ns2 = drop (length xs) tmp
      ts <- mapM (\is -> tIntRef (is ++ is) t2) (enumTensorIndices ns1)
      return $ Tensor (ns1 ++ ns2) (V.concat (map tToVector ts)) (map toSupSub xs ++ ys)
 where
  p :: Index EgisonValue -> Index EgisonValue -> Bool
  p (Sup i) (Sub j) = i == j
  p _ _             = False
tDiag t = return t

tDiagIndex :: [Index EgisonValue] -> [Index EgisonValue]
tDiagIndex js =
  match dfs js (List (IndexM Eql))
    [ [mc| $hjs ++ sup $i : $mjs ++ sub #i : $tjs ->
             tDiagIndex (SupSub i : hjs ++ mjs ++ tjs) |]
    , [mc| $hjs ++ sub $i : $mjs ++ sup #i : $tjs ->
             tDiagIndex (SupSub i : hjs ++ mjs ++ tjs) |]
    , [mc| _ -> js |]
    ]

tProduct :: (a -> b -> EvalM c) -> Tensor a -> Tensor b -> EvalM (Tensor c)
tProduct f (Tensor ns1 xs1 js1') (Tensor ns2 xs2 js2') = do
  let js1 = js1' ++ complementWithDF ns1 js1'
  let js2 = js2' ++ complementWithDF ns2 js2'
  let (cjs1, cjs2, tjs1, tjs2) = h js1 js2
  let t1 = Tensor ns1 xs1 js1
  let t2 = Tensor ns2 xs2 js2
  case cjs1 of
    [] -> do
      xs' <- mapM (\is -> do let is1 = take (length ns1) is
                             let is2 = take (length ns2) (drop (length ns1) is)
                             x1 <- tIntRef1 is1 t1
                             x2 <- tIntRef1 is2 t2
                             f x1 x2)
                  (enumTensorIndices (ns1 ++ ns2))
      tContract' (Tensor (ns1 ++ ns2) (V.fromList xs') (js1 ++ js2))
    _ -> do
      t1' <- tTranspose (cjs1 ++ tjs1) t1
      t2' <- tTranspose (cjs2 ++ tjs2) t2
      let (cns1, _) = splitAt (length cjs1) (tShape t1')
      rts' <- mapM (\is -> do rt1 <- tIntRef is t1'
                              rt2 <- tIntRef is t2'
                              tProduct f rt1 rt2)
                   (enumTensorIndices cns1)
      let ret = Tensor (cns1 ++ tShape (head rts')) (V.concat (map tToVector rts')) (map toSupSub cjs1 ++ tIndex (head rts'))
      tTranspose (uniq (map toSupSub cjs1 ++ tjs1 ++ tjs2)) ret
 where
  h :: [Index EgisonValue] -> [Index EgisonValue] -> ([Index EgisonValue], [Index EgisonValue], [Index EgisonValue], [Index EgisonValue])
  h js1 js2 = let cjs = filter (\j -> any (p j) js2) js1 in
                (cjs, map reverseIndex cjs, js1 \\ cjs, js2 \\ map reverseIndex cjs)
  p :: Index EgisonValue -> Index EgisonValue -> Bool
  p (Sup i) (Sub j) = i == j
  p (Sub i) (Sup j) = i == j
  p _ _             = False
  uniq :: [Index EgisonValue] -> [Index EgisonValue]
  uniq []     = []
  uniq (x:xs) = x:uniq (delete x xs)
tProduct f (Scalar x) (Tensor ns xs js) = do
  xs' <- V.mapM (f x) xs
  return $ Tensor ns xs' js
tProduct f (Tensor ns xs js) (Scalar x) = do
  xs' <- V.mapM (`f` x) xs
  return $ Tensor ns xs' js
tProduct f (Scalar x1) (Scalar x2) = Scalar <$> f x1 x2

tContract :: Tensor a -> EvalM [Tensor a]
tContract t = do
  t' <- tDiag t
  case t' of
    Tensor (n:_) _ (SupSub _ : _) -> do
      ts <- mapM (`tIntRef'` t') [1..n]
      tss <- mapM tContract ts
      return $ concat tss
    _ -> return [t']

tContract' :: Tensor a -> EvalM (Tensor a)
tContract' t@(Tensor ns _ js) =
  match dfs js (List M.Something)
    [ [mc| $hjs ++ $a : $mjs ++ ?(p a) : $tjs -> do
             let m = fromIntegral (length hjs)
             xs' <- mapM (\i -> tref (hjs ++ (Sub (ScalarData (SingleTerm i [])) : mjs)
                                          ++ (Sub (ScalarData (SingleTerm i [])) : tjs)) t)
                         [1..(ns !! m)]
             tConcat a xs' >>= tTranspose (hjs ++ a : mjs ++ tjs) >>= tContract' |]
    , [mc| _ -> return t |]
    ]
 where
  p :: Index EgisonValue -> Index EgisonValue -> Bool
  p (Sup i)    (Sup j)    = i == j
  p (Sub i)    (Sub j)    = i == j
  p (DF i1 j1) (DF i2 j2) = (i1 == i2) && (j1 == j2)
  p _ _                   = False
tContract' val = return val

tConcat :: Index EgisonValue -> [Tensor a] -> EvalM (Tensor a)
tConcat s (Tensor ns@(0:_) _ js:_) = return $ Tensor (0:ns) V.empty (s:js)
tConcat s ts@(Tensor ns _ js:_) = return $ Tensor (fromIntegral (length ts):ns) (V.concat (map tToVector ts)) (s:js)
tConcat s ts = do
  ts' <- mapM getScalar ts
  return $ Tensor [fromIntegral (length ts)] (V.fromList ts') [s]

tConcat' :: [Tensor a] -> EvalM (Tensor a)
tConcat' (Tensor ns@(0:_) _ _ : _) = return $ Tensor (0:ns) V.empty []
tConcat' ts@(Tensor ns _ _ : _) = return $ Tensor (fromIntegral (length ts):ns) (V.concat (map tToVector ts)) []
tConcat' ts = do
  ts' <- mapM getScalar ts
  return $ Tensor [fromIntegral (length ts)] (V.fromList ts') []

-- utility functions for tensors

cdr :: [a] -> [a]
cdr []     = []
cdr (_:ts) = ts

split :: Integer -> V.Vector a -> [V.Vector a]
split w xs
  | V.null xs = []
  | otherwise = let (hs, ts) = V.splitAt (fromIntegral w) xs in
                    hs:split w ts

getScalar :: Tensor a -> EvalM a
getScalar (Scalar x) = return x
getScalar _          = throwError $ Default "Inconsitent Tensor order"

reverseIndex :: Index a -> Index a
reverseIndex (Sup i) = Sub i
reverseIndex (Sub i) = Sup i
reverseIndex x       = x

toSupSub :: Index a -> Index a
toSupSub (Sup i) = SupSub i
toSupSub (Sub i) = SupSub i

complementWithDF :: Shape -> [Index a] -> [Index a]
complementWithDF ns js' = map (DF 0) [1..k]
  where k = fromIntegral $ length ns - length js'
