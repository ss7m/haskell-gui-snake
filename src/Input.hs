{-# LANGUAGE PatternSynonyms #-}

module Input
    ( handleInput
    ) where

import Data.Maybe
import Graphics.Gloss.Interface.IO.Game hiding (Point)
import System.Exit

import Direction
import State

pattern KeyHeld k <- EventKey k Down x y

pattern SKeyHeld k <- KeyHeld (SpecialKey k)

-- Handle input
handleInput :: Event -> State -> IO State
handleInput (SKeyHeld KeyEsc) _ = exitSuccess
handleInput (SKeyHeld KeySpace) state =
    return $ setPaused (not $ getPaused state) state
handleInput (KeyHeld k) state
    | getPaused state = return state
    | otherwise = return $ maybe id trySetDirection (keyToDir k) state
handleInput _ state = return state

-- Return a direction given a key
keyToDir :: Key -> Maybe Direction
keyToDir (Char 'w') = Just UP
keyToDir (Char 'a') = Just LEFT
keyToDir (Char 's') = Just DOWN
keyToDir (Char 'd') = Just RIGHT
keyToDir (Char 'k') = Just UP
keyToDir (Char 'h') = Just LEFT
keyToDir (Char 'j') = Just DOWN
keyToDir (Char 'l') = Just RIGHT
keyToDir (SpecialKey KeyUp) = Just UP
keyToDir (SpecialKey KeyLeft) = Just LEFT
keyToDir (SpecialKey KeyDown) = Just DOWN
keyToDir (SpecialKey KeyRight) = Just RIGHT
keyToDir _ = Nothing

-- Attempt to set the snake in a state to a direction
trySetDirection :: Direction -> State -> State
trySetDirection dir state =
    case getSnake state of
        [x] -> setDirection dir state
        (x:y:_) ->
            if push dir x == y
                then state
                else setDirection dir state
