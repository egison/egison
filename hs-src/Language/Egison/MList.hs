{- |
Module      : Language.Egison.MList
Licence     : MIT

This module provides definition and utility functions for monadic list.
-}

module Language.Egison.MList
  ( MList (..)
  , fromList
  , fromSeq
  , fromMList
  , msingleton
  , mfoldr
  , mappend
  , mconcat
  , mmap
  , mfor
  , mAny
  ) where

import           Prelude       hiding (mappend, mconcat,)
import           Data.Sequence (Seq)

data MList m a = MNil | MCons a (m (MList m a))

instance Show a => Show (MList m a) where
  show MNil        = "MNil"
  show (MCons x _) = "(MCons " ++ show x ++ " ...)"

fromList :: Monad m => [a] -> MList m a
fromList = foldr f MNil
 where f x xs = MCons x $ return xs

fromSeq :: Monad m => Seq a -> MList m a
fromSeq = foldr f MNil
 where f x xs = MCons x $ return xs

fromMList :: Monad m => MList m a -> m [a]
fromMList = mfoldr f $ return []
  where f x xs = (x:) <$> xs

msingleton :: Monad m => a -> MList m a
msingleton = flip MCons $ return MNil

mfoldr :: Monad m => (a -> m b -> m b) -> m b -> MList m a -> m b
mfoldr _ init MNil         = init
mfoldr f init (MCons x xs) = f x (xs >>= mfoldr f init)

mappend :: Monad m => MList m a -> m (MList m a) -> m (MList m a)
mappend xs ys = mfoldr ((return .) . MCons) ys xs

mconcat :: Monad m => MList m (MList m a) -> m (MList m a)
mconcat = mfoldr mappend $ return MNil

mmap :: Monad m => (a -> m b) -> MList m a -> m (MList m b)
mmap f = mfoldr g $ return MNil
  where g x xs = flip MCons xs <$> f x

mfor :: Monad m => MList m a -> (a -> m b) -> m (MList m b)
mfor = flip mmap

mAny :: Monad m => (a -> m Bool) -> MList m a -> m Bool
mAny _ MNil = return False
mAny p (MCons x xs) = do
  b <- p x
  if b
   then return True
   else do xs' <- xs
           mAny p xs'
