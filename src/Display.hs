module Display (drawState) where

import Graphics.Gloss hiding (Point)

import State
import Direction
import Grid

-- Create a rectangle given screen information and a point
makeRect :: Float -> Float -> Float -> Float -> Point -> Picture
makeRect w h sw sh (x, y) = translate transW transH rect
  where
    x' = fromIntegral x
    y' = fromIntegral $ negate y
    transW = -sw / 2 + (x' + 1) * w
    transH =  sh / 2 - (y' + 1) * h
    rect = rectangleSolid (w - 10) (h - 10)

-- cons a maybe to a list
consMaybe :: Maybe a -> [a] -> [a]
consMaybe (Just x) = (x :)
consMaybe Nothing  = id

-- Draw the current state
drawState :: State -> IO Picture
drawState state = do
  w <- blockWidth
  h <- blockHeight
  sw <- fromIntegral <$> screenWidth
  sh <- fromIntegral <$> screenHeight
  let food = color red . makeRect w h sw sh <$> getFood state
  let snake = color white . makeRect w h sw sh <$> getSnake state
  return $ pictures $ consMaybe food snake
