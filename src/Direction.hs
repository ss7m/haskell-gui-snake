module Direction (Direction(..), Point, push, opp) where

import Data.Bifunctor

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq)

type Point = (Int, Int)

-- pushes the points in a direction
push :: Direction -> Point -> Point
push UP    = second pred
push DOWN  = second succ
push LEFT  = first  pred
push RIGHT = first  succ

-- returns the opposite of a direction
opp :: Direction -> Direction
opp UP    = DOWN
opp DOWN  = UP
opp LEFT  = RIGHT
opp RIGHT = LEFT
