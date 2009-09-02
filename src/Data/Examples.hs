{-# LANGUAGE  TemplateHaskell,
              FlexibleContexts #-}

import Data.Lenses
import Data.Lenses.Template
import Control.Monad.State
import Control.Monad.Error

data Point = Point {
                x_ :: Float,
                y_ :: Float
                }
   deriving (Show)

$( deriveLenses ''Point )

data Triangle = Triangle {
                pa_ :: Point,
                pb_ :: Point,
                pc_ :: Point
                }
   deriving (Show)

$( deriveLenses ''Triangle )


a_y :: (MonadState Triangle m) => StateT Float (StateT Point m) b -> m b
a_y = pa . y

someTriangle :: Triangle
someTriangle = Triangle (Point 5 3) (Point 0 1) (Point 10 6)

ayValue :: Float
ayValue = someTriangle `fetch` a_y
ayValue1 :: Float
ayValue1 = someTriangle `fetch` (pa . y)
-- ayValue == 3

updatedTriangle :: Triangle
updatedTriangle = (someTriangle `update` a_y) 7
-- updatedTriangle == Triangle (Point 5 7) (Point 0 1) (Point 10 6)

-- (someTriangle `update` a_y) 7 == a_y (put 7) `execIn` someTriangle

a_x :: (MonadState Triangle m) => StateT Float (StateT Point m) b -> m b
a_x = pa . x
c_y :: (MonadState Triangle m) => StateT Float (StateT Point m) b -> m b
c_y = pc . y

updatedTriangle1 :: Triangle
updatedTriangle1 = execIn someTriangle $ a_y (put 7) >> a_x (put 1) >> c_y (put 9)
-- updatedTriangle == Triangle (Point 1 7) (Point 0 1) (Point 10 9)

updatedTriangle2 :: Triangle
updatedTriangle2 = execIn someTriangle $ do
   cy <- c_y get
   a_x $ put cy
-- updatedTriangle == Triangle (Point 6 3) (Point 0 1) (Point 10 6)

updatedTriangle3 :: Triangle
updatedTriangle3 = execIn someTriangle $ do
  cy <- get $% c_y
  put cy $% a_x
-- updatedTriangle == Triangle (Point 6 3) (Point 0 1) (Point 10 6)

updatedTriangle4 :: Triangle
updatedTriangle4 = execIn someTriangle $ c_y get >>= a_x . put

updatedTriangle5 :: Triangle
updatedTriangle5 = execIn someTriangle $ do
  cy <- c_y get
  a_x $= cy


updatdeTriangle6 :: Either String Triangle
updatdeTriangle6 = execInT someTriangle $ do
  cy <- c_y get
  when (cy == 0) $ throwError "Something bad happend"
  a_x $ put cy
-- updatedTriangle == Right $ Triangle (Point 6 3) (Point 0 1) (Point 10 6)
-- if cy had equaled 0 then we would have gotten this:
-- updatedTriangle == Left "Something bad happend"

