module Direction (Direction(..), Point, push, opp) where

data Direction = UP | DOWN | LEFT | RIGHT deriving(Eq)

type Point = (Int, Int)

push :: Direction -> Point -> Point
push UP = \(x,y)->(x,y+1)
push DOWN = \(x,y)->(x,y-1)
push LEFT = \(x,y)->(x-1,y)
push RIGHT = \(x,y)->(x+1,y)

opp :: Direction -> Direction
opp UP = DOWN
opp DOWN = UP
opp LEFT = RIGHT
opp RIGHT = LEFT
