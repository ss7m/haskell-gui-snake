module State
    ( State(..)
    , initState
    , setSnake
    , setDirection
    , setFood
    , setCounter
    , setPaused
    ) where

import Direction
import Grid
import Snake

data State =
    State
        { getSnake :: Snake
        , getDirection :: Direction
        , getFood :: Maybe Point
        , getCounter :: Int
        , getPaused :: Bool
        }

-- Initial State for the game
initState :: State
initState = State [(0, 0)] DOWN Nothing 0 False

setSnake :: Snake -> State -> State
setSnake snake state = state {getSnake = snake}

setDirection :: Direction -> State -> State
setDirection dir state = state {getDirection = dir}

setFood :: Maybe Point -> State -> State
setFood food state = state {getFood = food}

setCounter :: Int -> State -> State
setCounter counter state = state {getCounter = counter}

setPaused :: Bool -> State -> State
setPaused paused state = state {getPaused = paused}
