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

keyboardMouse :: IORef Snake -> IORef Direction -> KeyboardMouseCallback
keyboardMouse snake dir key Down _ _ = case key of
  (Char 'w') -> do
    s <- get snake
    dir $= set s UP
  (Char 'a') -> do
    s <- get snake
    dir $= set s LEFT
  (Char 's') -> do
    s <- get snake
    dir $= set s DOWN
  (Char 'd') -> do
    s <- get snake
    dir $= set s RIGHT
  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()
