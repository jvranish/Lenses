{-# LANGUAGE RankNTypes #-}

{- |


-}

module Data.Lenses ( runOn, runOnT
                   , evalFrom, evalFromT
                   , execIn, execInT
                   , fetch, update, alter
                   , getAndModify, modifyAndGet
                   , fromGetSet, with
                   , runSTLense, injectState
                   , to, from
                   ) where

import Data.Traversable
import Data.STRef

import Control.Monad.ST
import Control.Monad.State hiding (sequence, mapM)
import Control.Monad.Identity hiding (sequence, mapM)

import Prelude hiding (sequence, mapM)


fromGetSet :: (MonadState r m) => (r -> a) -> (a -> r -> r) -> StateT a m b -> m b
fromGetSet getter setter m = do
  s <- get
  (a, newFieldValue) <- runStateT m $ getter s
  put $ setter newFieldValue s
  return a

runOn :: StateT b Identity a -> b -> (a, b)
runOn l s = runIdentity $ runOnT l s

runOnT :: StateT b m a -> b -> m (a, b)
runOnT l s = runStateT l s

evalFrom :: StateT b Identity a -> b -> a
evalFrom l s = runIdentity $ evalFromT l s

evalFromT :: (Monad m) => StateT b m a -> b -> m a
evalFromT l s = liftM fst $ runStateT l s

execIn :: StateT a Identity a1 -> a -> a
execIn l s = runIdentity $ execInT l s

execInT :: (Monad m) => StateT b m a -> b -> m b
execInT l s = liftM snd $ runStateT l s

-- this could have the type :: (MonadState a m) => (m a -> StateT r Identity b) -> r -> b
--    without causeing problems, but it might be confusing.
fetch :: (MonadState a m) => (m a -> StateT r Identity a) -> r -> a
fetch lense s = lense get `evalFrom` s

update :: (MonadState a m) => (m () -> StateT r Identity b) -> a -> r -> r
update lense newValue s = lense (put newValue) `execIn` s

alter :: (MonadState a m) => (m () -> StateT r Identity b) -> (a -> a) -> r -> r
alter lense f s = lense (modify f) `execIn` s

getAndModify :: (MonadState s m) => (s -> s) -> m s
getAndModify f = do
  a <- get
  modify f
  return a
  
modifyAndGet :: (MonadState s m) => (s -> s) -> m s 
modifyAndGet f = modify f >> get


with :: a -> a
with = id

runSTLense :: (Traversable f, Traversable t) => (forall s. f (State a b -> s) -> t s) -> f a -> (t b, f a)
runSTLense f x = runST $ do
    stLenses <- mapM newSTRef x
    values <- sequence $ f $ fmap injectState stLenses
    updates <- mapM readSTRef stLenses
    return (values, updates)

injectState :: STRef s a -> State a b -> ST s b
injectState ref m = do
  (a, s) <- liftM (runState m) $ readSTRef ref
  writeSTRef ref s
  return a

to :: (Functor f) => a -> (c -> f (a -> b)) -> c -> f b
to m f = fmap ($ m) . f

from :: (Traversable t) => (forall s. t (State a b -> s) -> t s) -> t a -> t a
from m x = snd $ runSTLense m x


