module Bindings (keyboardMouse) where

import Graphics.UI.GLUT
import Data.IORef
import Snake2
import Direction

-- attempts to set the direction of the snake
set :: Snake -> Direction -> Direction
set (Snake [_]) dir = dir
set (Snake (x:y:_)) dir
  | push dir x == y = opp dir
  | otherwise = dir

-- if wasd or arrow key, returns corresponding direction
keyToDir :: Key -> Maybe Direction
keyToDir (Char 'w')            = Just UP
keyToDir (Char 'a')            = Just LEFT
keyToDir (Char 's')            = Just DOWN
keyToDir (Char 'd')            = Just RIGHT
keyToDir (SpecialKey KeyUp)    = Just UP
keyToDir (SpecialKey KeyLeft)  = Just LEFT
keyToDir (SpecialKey KeyRight) = Just RIGHT
keyToDir (SpecialKey KeyDown)  = Just DOWN
keyToDir _                     = Nothing

-- reacts to keyboard and mouse presses
keyboardMouse :: IORef Snake -> IORef Direction -> KeyboardMouseCallback
keyboardMouse snake dir key Down _ _ = case keyToDir key of
  (Just d)  -> do
    s <- get snake
    dir $= set s d
  (Nothing) -> return ()
keyboardMouse _ _ _ _ _ _ = return ()
