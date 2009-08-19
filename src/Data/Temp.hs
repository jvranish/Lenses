{-# LANGUAGE TemplateHaskell, 
             FlexibleContexts #-}

module Data.Temp (x, test, test2) where

import Data.Lenses
import Data.Lenses.Template
import Control.Monad.Error()
import Control.Monad.State


data Test = Test {
                x1_ :: Int,
                y1_ :: Float
                }  
   deriving (Show)
$( deriveLenses ''Test )


x :: Test
x = Test 5 6

test :: Either String Test
test =  (x1 (put 10) >> y1 (put 3)) `updateInT` x

test2 :: Test
test2 =  (x1 (put 10) >> y1 (put 3)) `updateIn` x



{-
{-

Wouldn't it be nice if we could use the same functions for getting data to also update it?
Well you can!


How it works:

The runSTLense function converts a structure of type (f a) to a structure of type (f  (State a b -> ST s b))
and then passes this structure to a passed in function, and returns both the updated structure


I use the state monad for two reasons.

It already has get/modify/put functions, (so I don't have to make them and add new things to the namespace)

It was specifically designed to handle the concept of  "modify some state, and return a value"

The function passed in needs to take a structure of type f  (State a b -> s)  and return a structure of type t s
essentially you apply the function in the structure to a State monad, the "state" of the state monad will be the value at
that point in the structure, and any value returned from the monad will be collected


In this case I the state tat


the only catch is that the getter can't look at the values of the items in the structure (just return them)
One coule make a version that uses stableNames that would allow that though... but there are other issues to consider...
I'll have to think about it... :)

-}

everyOther [] = []
everyOther (x:[]) = [x]
everyOther (x:y:xs) = x : everyOther xs

addOne :: State Int ()
addOne = modify (+1)

--test = snd $ runLense (addOne `to` everyOther) [1, 2, 9, 6, 7, 8, 4]
test2 = (addOne `to` everyOther) `from` [1, 2, 9, 6, 7, 8, 4]


fetchFrom l s = fst $ runState l s
updateIn l s = snd $ runState l s
runLense = runState
test2 s = verticies' get `fetchFrom` s
test3 s x = verticies' (put x) `updateIn` s
test1 s f = verticies' (getAndModify f) `runLense` s

test4 s = fetch verticies' s
test5 s x = update verticies' x s


{-# LANGUAGE TemplateHaskell, 
             FlexibleContexts #-}


test = do
  index <- with verticies' $ addItem undefined
  return index

--asdf = 1245
--h = "asdf" ++ v
--r = "asdf" ++ (show $(dyn v) )
-- [|  $(liftM VarE (newName v)) |]
--varName s = [| s |]

-- y1 :: Test -> Float
--name = "f"
--type T r a b = State a b -> State r b

y = runQ [d| 
              f :: (MonadState r m) => StateT a m b -> m b
              f = undefined
      |] >>= print


data Test = Test {
                x1_ :: Int,
                y1_ :: Float
                }  
   deriving (Show)
$( deriveLenses ''Test )
--fetch :: (MonadState s m) => (m s -> State s1 a) -> s1 -> a
x = Test 5 6
test :: Either String Test
test =  (x1 (put 10) >> y1 (put 3)) `updateInT` x

test2 =  (x1 (put 10) >> y1 (put 3)) `updateIn` x


[SigD f (ForallT b:[a_0,b_1,r_2] [] (AppT (AppT ArrowT (AppT (AppT (ConT Control.Monad.State.Lazy.State) (VarT a_0)) (VarT b_1))) (AppT (AppT (ConT Control.Monad.State.Lazy.State) (VarT r_2)) (VarT b_1)))),ValD (VarP f) (NormalB (VarE GHC.Err.undefined)) []]
b = mkName "b"
SigD f (ForallT b:params [] (AppT (AppT ArrowT (AppT (AppT (ConT Control.Monad.State.Lazy.State) appliedT) (VarT b))) (AppT (AppT (ConT Control.Monad.State.Lazy.State) ftype) (VarT b))))
ValD (VarP f) (NormalB (VarE GHC.Err.undefined)) []
SigD accName (ForallT params [] (AppT (AppT (ConT ''Accessor.T) appliedT) ftype))
SigD accName (ForallT (b:params) [] (AppT (AppT ArrowT (AppT (AppT (ConT ''State) ftype) (VarT b))) (AppT (AppT (ConT ''State) appliedT) (VarT b))))
SigD accName (ForallT (b:m:params) [AppT (AppT (ConT ''MonadState) appliedT) (VarT m)] (AppT (AppT ArrowT (AppT (AppT (AppT (ConT ''StateT) ftype) (VarT m)) (VarT b))) (AppT (VarT m) (VarT b))))

T r a
State a b -> State r b  
verticies' m = do
  (MeshState v e f) <- get
  let (a, v') = runState m v
  put $ MeshState v' e f
  return a
  
[d|  
verticies' :: State (Indexable VertIdx (HEVert a)) b -> State (MeshState a) b  
$(dyn lenseName) m = do
  s <- get
  let (a, s') = runState m $ $(dyn fieldName) s
  put $ s { $(dyn fieldName) = s' }
  return a
  
  |]

-}

