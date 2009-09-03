{-# LANGUAGE RankNTypes #-}

{- |
This modules provides a convienient way to access and update the elements of a structure.
It is very similar to "Data.Accessors", but a bit more generic and has fewer dependencies.
I particularly like how cleanly it handles nested structures in state monads.
'runSTLense' is also a very useful function.


A brief tutorial to get started:

To create a lense, you can use fromGetSet (although usually you would just derive them using templat haskell and 'deriveLenses' from "Data.Lenses.Template"):

> lense = fromGetSet getField setField

The lense has type:

> lense :: (MonadState r m) => StateT a m b -> m b

Where r is the type of the record, and a is the type of the field, (b can be any type you choose, more on that latter). Though it may help to think of it as:

> lense :: State a b -> State r b

Which is not entirely accurate, but emphasises how the lense works.
You can think of it as \"pass in an action that operates on the field, and you get an action that operates on the record\".
So say we pass in get (with a more specific type for clarity):

> get :: State a a
>
> lense get :: State r a

We get out a state monad that we can run on our record to fetch our field.

> fieldValue = lense get `evalState` record

This module has a special function 'fetch' that does this:

> fieldValue = record `fetch` lense

You can also pass in 'put' to get back an action that updates the field.

> put :: a -> State a ()
>
> lense (put someValue) :: State r ()

Now we have a state monad that we can run on our record to update our field.

> updatedRecord = lense (put someValue) `execState` record

This module has a special function 'update' that does this:

> updatedRecord = (record `update` lense) someValue

To aid in clarity and to deal with the actual types of the lenses this module provides 'execIn', 'evalFrom', and 'runOn' to be used in place of 'execState', 'evalState', and 'runState'. Also note that 'execIn', 'evalFrom', and 'runOn' have their parameters fliped from their state counterparts. There is nothing magical about these functions, they are just a little more handy than their state counterparts.

The lenses are especially convienient if you have nested structures. Lense composition is just function composition.

> data Point = Point {
>                 x_ :: Float,
>                 y_ :: Float
>                 }
>    deriving (Show)

$( deriveLenses ''Point )

> data Triangle = Triangle {
>                 pa_ :: Point,
>                 pb_ :: Point,
>                 pc_ :: Point
>                 }
>    deriving (Show)

$( deriveLenses ''Triangle )

> a_y :: (MonadState Triangle m) => StateT Float (StateT Point m) b -> m b
> a_y = pa . y

a_y is now a lense that can operate on the y coordinate of point \"a\" inside a triangle.
We can use a_y to fetch the coordinate or update it, on whatever triangle we choose.

> someTriangle = Triangle (Point 5 3) (Point 0 1) (Point 10 6)
>
> ayValue = someTriangle `fetch` a_y
> -- ayValue == 3
>
> updatedTriangle = (someTriangle `update` a_y) 7
> -- updatedTriangle == Triangle (Point 5 7) (Point 0 1) (Point 10 6)

Or we could apply our lense to an action and pass it into 'execIn'

> (someTriangle `update` a_y) 7 == execIn someTriangle (a_y (put 7))



We can also chain actions together:

> a_x :: (MonadState Triangle m) => StateT Float (StateT Point m) b -> m b
> a_x = pa . x
> c_y :: (MonadState Triangle m) => StateT Float (StateT Point m) b -> m b
> c_y = pc . y
>
> updatedTriangle = execIn someTriangle $ a_y (put 7) >> a_x (put 1) >> c_y (put 9)
> -- updatedTriangle == Triangle (Point 1 7) (Point 0 1) (Point 10 9)

What if we wanted to put the value of c_y into a_x? Can do!

>  updatedTriangle = execIn someTriangle $ do
>    cy <- c_y get
>    a_x $ put cy
>  -- updatedTriangle == Triangle (Point 6 3) (Point 0 1) (Point 10 6)

Or if the order really bugs you, you can use the '$%' operator (taken from "Data.Accessors.Basic", it really should be in a standard lib)

>  updatedTriangle = execIn someTriangle $ do
>    cy <- get $% c_y
>    put cy $% a_x
>  -- updatedTriangle == Triangle (Point 6 3) (Point 0 1) (Point 10 6)

Or you can use the '$=' operator:

>  updatedTriangle = execIn someTriangle $ do
>    cy <- c_y get
>    a_x $= cy

Or more concisely:

>  updatedTriangle = execIn someTriangle $ (c_y get >>= a_x . put)

Or say we want to put the value of c_y into a_x, but want to throw an error if c_y is zero. We can do that as well!

>  updatdeTriangle :: Either String Triangle
>  updatdeTriangle = execInT someTriangle $ do
>    cy <- c_y get
>    when (cy == 0) $ throwError "Something bad happend"
>    a_x $ put cy
>  -- updatedTriangle == Right $ Triangle (Point 6 3) (Point 0 1) (Point 10 6)
>  -- if cy had equaled 0 then we would have gotten this:
>  -- updatedTriangle == Left "Something bad happend"

Note that 'execInT' = 'flip' 'execStateT'.

Yay for monad transformers!

This module has one last feature that allows you to convert a function that fetches data from a structure to a function that modifies it! For an example see the documentation for 'runSTLense'.

One final note: Due to the generality of the lenses you might end up accidentally running into the monomorphism restriction.
So if get a type error like:

>    Couldn't match expected type `SomeMonad SomeStructureType'
>           against inferred type `Control.Monad.Identity.Identity SomeStructureType'

and nothing appears to be wrong with your code, try turning the restriction off with -XNoMonomorphismRestriction and see if it goes away.
If it does then you probably need to add some explicit type signatures somewhere.

I whipped out this documentation in a hurry, so if you spot any errors, or think I should explain something better, /please/ let me know.
Also since this module is new I'm open to radical modifications if you have a good suggestion, so suggest away! :)

-}

module Data.Lenses (
                   -- * Basic functions to create lenses and use them
                     fromGetSet, fetch, update, alter
                   -- * Lense evaluators
                   , runOn, runOnT
                   , evalFrom, evalFromT
                   , execIn, execInT
                   -- * Structure lenses
                   , runSTLense
                   , to, from
                   -- * Generic helper functions
                   , getAndModify, modifyAndGet, ($=), ($%)
                   ) where

import Data.Traversable
import Data.STRef

import Control.Monad.ST
import Control.Monad.State hiding (sequence, mapM)
import Control.Monad.Identity hiding (sequence, mapM)

import Prelude hiding (sequence, mapM)

{- |
This function takes a "getter" and "setter" function and returns our lense.

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
fetch :: (MonadState a m) => r -> (m a -> StateT r Identity a) -> a
fetch s lense = evalFrom s $ lense get

{- |
updates a field in a structure using a lense:

> somePoint = Point 5 3
> newPoint = (somePoint `update` y) 15

> -- newPoint == Point 5 15

-}
update :: (MonadState a m) => r -> (m () -> StateT r Identity b) -> a -> r
update s lense newValue = execIn s $ lense (put newValue)

{- |
alters a field in a structure using a lense and a function:

> somePoint = Point 5 3
> newPoint = (somePoint `alter` y) (+1)

> -- newPoint == Point 5 4

-}
alter :: (MonadState a m) => (m () -> StateT r Identity b) -> (a -> a) -> r -> r
alter lense f s = execIn s $ lense (modify f)

{- |
Runs a state monad action on a structure and returns the value returned from the action and the updated structure.

> somePoint = Point 5 3
> a = runOn somePoint $ x (modifyAndGet (+1))
> -- a == (6, Point 6 3)

-}
runOn :: b -> StateT b Identity a -> (a, b)
runOn s l = runIdentity $ runOnT s l

{- |
Monad transformer version of 'runOn'. Note that 'runOnT' = 'runStateT'.

-}
runOnT :: b -> StateT b m a -> m (a, b)
runOnT = flip runStateT

{- |
Runs a state monad action on a structure and returns the value returned from the action.

Use it to fetch values from fields.

> someTriangle = Triangle (Point 5 3) (Point 0 1) (Point 10 6)
> a = evalFrom someTriangle $ pb . x get
> -- a == 0

note that:

> evalFrom someTriangle (pb . x get) == someTriangle `fetch` (pb . x)

The advantage over 'fetch' is that it allows you to specify a different final action besides 'get' like so:

> evalFrom someTriangle $ pb . x (modifyAndGet (+1))


-}
evalFrom :: b -> StateT b Identity a -> a
evalFrom s l = fst $ runOn s l

{- |
Monad transformer version of 'evalFrom'. Note that 'evalFromT' = 'flip' 'evalStateT'.

-}
evalFromT :: (Monad m) => b -> StateT b m a -> m a
evalFromT = flip evalStateT

{- |

Runs a state monad action on a structure and returns the updated structure

Use it to update fields:

> somePoint = Point 5 3
> a = execIn somePoint $ x (put 1)
> -- a == Point 1 3

note that:

> execIn somePoint (x (put 1)) == (somePoint `update` x) 1

The advantage over 'update' is that it allows you to specify a different final action besides 'put' like so:

> a = execIn somePoint $ x (modifyAndGet (+1))
> -- a = Point 6 3

-}
execIn :: a -> StateT a Identity b -> a
execIn s l = snd $ runOn s l

{- |
Monad transformer version of 'execIn'. Note that 'execIn' = 'flip' 'execStateT'.

-}
execInT :: (Monad m) => b -> StateT b m a -> m b
execInT = flip execStateT

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

> everyOther :: [a] -> [a]
> everyOther [] = []
> everyOther (x:[]) = [x]
> everyOther (x:y:xs) = x : everyOther xs

> addOne :: State Int ()
> addOne = modify (+1)

> test :: [Int]
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
Applies 'runSTLense' to a function and a structure and returns the 'snd' of the result.
See 'to' for example of use.

-}
from :: (Traversable t) => (forall s. t (State a b -> s) -> t s) -> t a -> t a
from f x = snd $ runSTLense f x


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

{- |
An operator for assigning a value to the value referenced by a lense.
(see the example near the end of the tutorial at the start of this module)

-}
infixl 0 $=
($=) :: (MonadState s m) => (m () -> b) -> s -> b
lense $= x = lense $ put x

{- |
Flipped version of '($)'.

-}
infixl 0 $%
($%) :: a -> (a -> b) -> b
($%) = flip ($)


