module State (State(..), initState, setSnake, setDirection, setFood, setCounter) where

import Snake
import Direction
import Grid

data State = State {getSnake :: Snake, getDirection :: Direction, getFood :: Maybe Point, getCounter :: Int}

-- Initial State for the game
initState :: State
initState = State [(0, 0)] DOWN Nothing 0

setSnake :: Snake -> State -> State
setSnake snake (State _ dir food counter) = State snake dir food counter

setDirection :: Direction -> State -> State
setDirection dir (State snake _ food counter) = State snake dir food counter

setFood :: Maybe Point -> State -> State
setFood food (State snake dir _ counter) = State snake dir food counter

setCounter :: Int -> State -> State
setCounter counter (State snake dir food _) = State snake dir food counter
