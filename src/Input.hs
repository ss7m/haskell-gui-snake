module Input (handleInput) where

import Graphics.Gloss.Interface.IO.Game hiding (Point)
import System.Exit

import Direction
import State

-- Handle input
handleInput :: Event -> State -> IO State
handleInput (EventKey (SpecialKey KeyEsc) Down _ _) = const exitSuccess
handleInput (EventKey k Down _ _) = return . trySetDirection (keyToDir k)
handleInput _ = return

-- Return a direction given a key
keyToDir :: Key -> Maybe Direction
keyToDir (Char 'w') = Just UP
keyToDir (Char 'a') = Just LEFT
keyToDir (Char 's') = Just DOWN
keyToDir (Char 'd') = Just RIGHT
keyToDir _ = Nothing

-- Attempt to set the snake in a state to a direction
trySetDirection :: Maybe Direction -> State -> State
trySetDirection Nothing state = state
trySetDirection (Just dir) state = 
  case getSnake state of
    [x] -> setDirection dir state
    (x:y:_) -> if push dir x == y then state else setDirection dir state
