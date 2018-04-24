module State (State(..), initState) where

import Snake
import Direction

data State = State {getSnake :: Snake, getDirection :: Direction, getFood :: Maybe Point, getEatCounter :: Int}

-- Initial State for the game
initState :: State
initState = State [(0, 0)] DOWN Nothing 0
