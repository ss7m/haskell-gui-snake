module Display (drawState) where

import Graphics.Gloss hiding (Point)

import State
import Direction
import Grid

-- Create a rectangle given screen information and a point
makeRect :: Float -> Float -> Float -> Float -> Point -> Picture
makeRect w h sw sh (x, y) = col $ translate transW transH rect
  where
    x' = fromIntegral x
    y' = fromIntegral $ negate y
    transW = -sw / 2 + (x' + 1) * w
    transH =  sh / 2 - (y' + 1) * h
    rect = rectangleSolid w h
    
    sqrt' = sqrt . fromIntegral
    r = divInts x gridWidth
    g = divInts y gridHeight
    b = 1 - (sqrt' (x^2 + y^2) / sqrt' (gridWidth^2 + gridHeight^2))

    col = color $ makeColor r g b 1

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
  let food = recolor white . makeRect w h sw sh <$> getFood state
  let snake = makeRect w h sw sh <$> getSnake state
  return $ pictures $ consMaybe food snake

recolor :: Color -> Picture -> Picture
recolor col (Color _ x) = Color col x
