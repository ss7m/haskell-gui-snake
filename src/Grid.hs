module Grid where

import Graphics.Gloss.Interface.Environment
import Control.Monad

-- Width of the game grid
gridWidth :: Num a => a
gridWidth = 40

-- Height of the game grid
gridHeight :: Num a => a
gridHeight = 22

-- Number of pixels in the screen
screenWidth :: Num a => IO a
screenWidth = fromIntegral . fst <$> getScreenSize

-- Number of pixels in the screen
screenHeight :: Num a => IO a
screenHeight = fromIntegral . snd <$> getScreenSize

-- Number of pixels in the block of a grid
blockWidth :: IO Float
blockWidth = (/ (gridWidth+1)) <$> screenWidth

-- Number of pixels in the block of a grid
blockHeight :: IO Float
blockHeight = (/ (gridHeight+1)) <$> screenHeight
