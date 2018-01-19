module Bindings (keyboardMouse) where

import Graphics.UI.GLUT
import Data.IORef
import Snake
import Direction

set :: Snake -> Direction -> Direction
set (Snake _ []) dir = dir
set (Snake _ (d:_)) dir
  | d == dir  = opp dir
  | otherwise = dir

charToDir :: Key -> Maybe Direction
charToDir (Char 'w')            = Just UP
charToDir (Char 'a')            = Just LEFT
charToDir (Char 's')            = Just DOWN
charToDir (Char 'd')            = Just RIGHT
charToDir (SpecialKey KeyUp)    = Just UP
charToDir (SpecialKey KeyLeft)  = Just LEFT
charToDir (SpecialKey KeyRight) = Just RIGHT
charToDir (SpecialKey KeyDown)  = Just DOWN
charToDir _                     = Nothing

keyboardMouse :: IORef Snake -> IORef Direction -> KeyboardMouseCallback
keyboardMouse snake dir key Down _ _ = case charToDir key of
  (Just d)  -> do
    s <- get snake
    dir $= set s d
  (Nothing) -> return ()
keyboardMouse _ _ _ _ _ _ = return ()
