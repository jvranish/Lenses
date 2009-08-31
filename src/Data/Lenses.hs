{-# LANGUAGE RankNTypes #-}

{- |
This modules provides a convienient way to access and update the elements of a structure.
It is very similar to 'Data.Accessors', but simpler, a bit more generic and has fewer dependencies.
I particularly like how cleanly it handles nested structures in state monads.
'runSTLense' is also a particularly useful function.

I whipped out this documentation in a hurry, so if you spot any errors, or have a better way to explain something, /please/ let me know.
-}

module Data.Lenses (
                   -- * Basic functions to create lenses and use them
                     fromGetSet, fetch, update, alter
                   -- * More advanced functions that allow chaining fetching//updating actio
                   , runOn, runOnT
                   , evalFrom, evalFromT
                   , execIn, execInT
                   -- * Structure lenses
                   , runSTLense
                   , to, from
                   -- * Generic state monad helper functions
                   , getAndModify, modifyAndGet
                   ) where

import Data.Traversable
import Data.STRef

import Control.Monad.ST
import Control.Monad.State hiding (sequence, mapM)
import Control.Monad.Identity hiding (sequence, mapM)

import Prelude hiding (sequence, mapM)

-- * Basic functions to create lenses and use them
{- |
This function takes a "getter" and "setter" function and returns a function that takes a StateT Monad action and returns the monadic result of the action.

Usually you only need to use this if you don't want to use Template Haskell to derive your Lenses for you.
With a structure Point:

> data Point = Point {
>                 x_ :: Float,
>                 y_ :: Float
>                 }
>    deriving (Show)

This (from "Data.Lenses.Template"):

$( deriveLenses ''Point )

is equivalent to this:

> x :: (MonadState Point m) => StateT Float m b -> m b
> x = fromGetSet x_ (\a s -> s { x_ = a })
> y :: (MonadState Point m) => StateT Float m b -> m b
> y = fromGetSet y_ (\a s -> s { y_ = a })

-}
fromGetSet :: (MonadState r m) => (r -> a) -> (a -> r -> r) -> StateT a m b -> m b
fromGetSet getter setter m = do
  s <- get
  (a, newFieldValue) <- runStateT m $ getter s
  put $ setter newFieldValue s
  return a

{- |
fetches a field from a structure using a lense:

> somePoint = Point 5 3
> a = somePoint `fetch` x
> b = somePoint `fetch` y

> -- a == 5
> -- b == 3

-}
-- this could have the type :: (MonadState a m) => (m a -> StateT r Identity b) -> r -> b
--    without causeing problems, but it might be confusing.
fetch :: r -> (State a a -> State r a) -> a
fetch s lense = lense get `evalState` s

{- |
updates a field in a structure using a lense:

> somePoint = Point 5 3
> newPoint = (somePoint `update` y) 15

> -- newPoint == Point 5 15

-}
update :: r -> (State a () -> State r b) -> a -> r
update s lense newValue = lense (put newValue) `execState` s

{- |
alters a field in a structure using a lense and a function:

> somePoint = Point 5 3
> newPoint = (somePoint `alter` y) (+1)

> -- newPoint == Point 5 4

-}
alter :: (State a () -> State r b) -> (a -> a) -> r -> r
alter lense f s = lense (modify f) `execState` s

-- * More advanced functions that allow chaining fetching//updating actions

{- |
Runs a state monad action on a structure and returns the value returned from the action and the updated structure.

> data Triangle = Triangle {
>                 pa_ :: Point,
>                 pb_ :: Point,
>                 pc_ :: Point
>                 }
>    deriving (Show)

$( deriveLenses ''Point )

> somePoint = Point 5 3
> a = x (modifyAndGet (+1)) `runOn` somePoint
> -- a == (6, Point 6 3)

But is more useful for chaining actions

> someTriangle = Triangle (Point 5 3) (Point 0 1) (Point 10 6)
> a = pc . x (modifyAndGet (+1)) `runOn` someTriangle
> -- a == (11, Triangle (Point 5 3) (Point 0 1) (Point 11 6))

-}
runOn :: StateT b Identity a -> b -> (a, b)
runOn l s = runIdentity $ runOnT l s

{- |
Monad transformer version of 'runOn'.

-}
runOnT :: StateT b m a -> b -> m (a, b)
runOnT l s = runStateT l s

{- |
Runs a state monad action on a structure and returns the value returned from the action.

Use it to fetch values from fields.

> somePoint = Point 5 3
> a = x get `evalFrom` somePoint
> -- a == 5

But is more useful for chaining fetches (and other actions)

> someTriangle = Triangle (Point 5 3) (Point 0 1) (Point 10 6)
> a = pb . x get `evalFrom` someTriangle
> -- a == 0

-}
evalFrom :: StateT b Identity a -> b -> a
evalFrom l s = fst $ runOn l s

{- |
Monad transformer version of 'evalFrom'.

-}
evalFromT :: (Monad m) => StateT b m a -> b -> m a
evalFromT l s = evalStateT l s

{- |

Runs a state monad action on a structure and returns the updated structure

Use it to update fields:

> somePoint = Point 5 3
> a = x (put 1) `execIn` somePoint
> -- a == Point 1 3

But is more useful for chaining updates (and other actions)

> someTriangle = Triangle (Point 5 3) (Point 0 1) (Point 10 6)
> a = pb . x (put 7) `evalFrom` someTriangle
> -- a == Triangle (Point 5 3) (Point 7 1) (Point 10 6)

-}
execIn :: StateT a Identity a1 -> a -> a
execIn l s = snd $ runOn l s

{- |
Monad transformer version of 'execIn'.

-}
execInT :: (Monad m) => StateT b m a -> b -> m b
execInT l s = execStateT l s

-- * Structure lenses
{- |
This function has the magical ability to convert a function that fetches elements from a structure, to a function that lets you modify the elements in the structure.
The catch is that the structure must be a member of 'Traversable'.

So say you have a function that gets the diagonal of a list of lists:

> diagonal :: [[a]] -> [a]

we can make a function that increments the diagonal like so:

> addOne :: State Int ()
> addOne = modify (+1)

> incrementDiagonal :: [[a]] -> [[a]]
> incrementDiagonal xss = snd $ runSTLense (fmap ($ addOne) . diagonal) xss

Of course there are some helper combinators to make this cleaner:

> incrementDiagonal xss = (addOne `to` diagonal) `from` xss


'runSTLense' takes a function, a traversable structure, and returns a pair of (collected values, updated structure)
For clarification:

> specialFunction ::  (Traversable f, Traversable t) => f (State a b -> s) -> t s
> (collectedValues, updatedStructure) = runSTLense specialFunction originalStructure
> collectedAlmostValues = specialFunction processedOriginalStructure

processedOriginalStructure has the same shape as originalStructure but every element has been replaced with a transformer function (State a b -> s).
specialFunction needs to return the result of the application of the functions in processedOriginalStructure to a state monad.
The state monad by definition will return a result and potentially update state. Getting state will get the value of the element in originalStructure.
Updating state will update the value of the element in updatedStructure. The returned values are gathered in collectedValues.

-}

runSTLense :: (Traversable f, Traversable t) => (forall s. f (State a b -> s) -> t s) -> f a -> (t b, f a)
runSTLense f x = runST $ do
    stLenses <- mapM newSTRef x
    values <- sequence $ f $ fmap injectState stLenses
    updates <- mapM readSTRef stLenses
    return (values, updates)
  where
    injectState :: STRef s a -> State a b -> ST s b
    injectState ref m = do
      s <- readSTRef ref
      let (a, s') = runState m s
      writeSTRef ref s'
      return a

{- |
A helper combinator used for applying a monad to element collected by a fetching function.
For example:

> everyOther [] = []
> everyOther (x:[]) = [x]
> everyOther (x:y:xs) = x : everyOther xs

> addOne :: State Int ()
> addOne = modify (+1)

> test = (addOne `to` everyOther) `from` [1, 2, 9, 6, 7, 8, 4]
> -- test == [2, 2, 10, 6, 8, 8, 5]

which is the same as:

> test = snd $ runSTLense (addOne `to` everyOther) [1, 2, 9, 6, 7, 8, 4]

which is the same as:

> test = snd $ runSTLense (fmap ($ addOne) . everyOther) [1, 2, 9, 6, 7, 8, 4]

-}
to :: (Functor f) => a -> (c -> f (a -> b)) -> c -> f b
to m f = fmap ($ m) . f

{- |
Applies 'runSTLense' to a function and a structure and returns the snd of the result.
See 'to' for examples of use.

-}
from :: (Traversable t) => (forall s. t (State a b -> s) -> t s) -> t a -> t a
from f x = snd $ runSTLense f x

-- * Generic helper functions

{- |
Modifies the state in a state monad and returns the original value.

'getAndModify' and 'modifyAndGet' should really be in 'Control.Monad.State.Class'

-}
getAndModify :: (MonadState s m) => (s -> s) -> m s
getAndModify f = do
  a <- get
  modify f
  return a

{- |
Modifies the state in a state monad and returns the new value.

-}
modifyAndGet :: (MonadState s m) => (s -> s) -> m s
modifyAndGet f = modify f >> get

