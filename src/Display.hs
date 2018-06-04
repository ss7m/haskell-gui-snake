module Display (drawState) where

import Graphics.Gloss hiding (Point)
import Data.Maybe

import State
import Direction
import Grid

-- Create a rectangle given screen information and a point
makeRect :: Color -> Point -> IO Picture
makeRect c (x, y) = do
  let x' = fromIntegral x
  let y' = fromIntegral y
  w <- blockWidth
  h <- blockHeight
  sw <- screenWidth
  sh <- screenHeight
  let transW = -sw / 2 + (x' + 1) * w
  let transH =  sh / 2 - (y' + 1) * h

  return $ color c $ translate transW transH $ rectangleSolid w h

-- cons a maybe to a list
(?:) :: Maybe a -> [a] -> [a]
(?:) (Just x) = (x :)
(?:) Nothing  = id

-- Draw the current state
drawState :: State -> IO Picture
drawState state = do
  food  <- mapM (makeRect red)   $ getFood state
  snake <- mapM (makeRect white) $ getSnake state
  return $ pictures $ food ?: snake
