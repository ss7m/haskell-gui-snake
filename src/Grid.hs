module Grid (gridWidth, gridHeight, screenWidth, screenHeight, blockWidth, blockHeight) where

import Graphics.Gloss.Interface.Environment

-- Width of the game grid
gridWidth :: Int
gridWidth = 40

-- Height of the game grid
gridHeight :: Int
gridHeight = 22

-- Number of pixels in the screen
screenWidth :: IO Int
screenWidth = fst <$> getScreenSize

-- Number of pixels in the screen
screenHeight :: IO Int
screenHeight = snd <$> getScreenSize

-- Divide two Ints into a Float
divInts :: (Integral a, Fractional b) => a -> a -> b
divInts x y = fromIntegral x / fromIntegral y

-- Number of pixels in the block of a grid
blockWidth :: IO Float
blockWidth = do
  w <- screenWidth
  return $ divInts w (gridWidth + 1)

-- Number of pixels in the block of a grid
blockHeight :: IO Float
blockHeight = do
  h <- screenHeight
  return $ divInts h (gridHeight + 1)
