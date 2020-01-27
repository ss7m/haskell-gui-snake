module Display
    ( drawState
    ) where

import Data.Maybe
import Graphics.Gloss hiding (Point)

import Direction
import Grid
import State

-- Create a rectangle given screen information and a point
makeRect :: Float -> Float -> Float -> Float -> Color -> Point -> IO Picture
makeRect bw bh sw sh c (x, y) = do
    let x' = fromIntegral x
    let y' = fromIntegral y
    let transW = -sw / 2 + (x' + 1) * bw
    let transH = sh / 2 - (y' + 1) * bh
    return $ color c $ translate transW transH $ rectangleSolid bw bh

-- cons a maybe to a list
(?:) :: Maybe a -> [a] -> [a]
(?:) = maybe id (:)

-- Draw the current state
drawState :: State -> IO Picture
drawState state = do
    bw <- blockWidth
    bh <- blockHeight
    sw <- screenWidth
    sh <- screenHeight
    food <- mapM (makeRect bw bw sw sh red) $ getFood state
    snake <- mapM (makeRect bw bw sw sh white) $ getSnake state
    return $ pictures $ food ?: snake
