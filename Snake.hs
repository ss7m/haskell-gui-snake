module Snake (Snake(..), valid, move, eating, intersecting, eat, toPoints, score) where

import Direction
import Data.List

data Snake = Snake Point [Direction]

-- moves the tail of the snake
moveTail :: Direction -> [Direction] -> [Direction]
moveTail _ [] = []
moveTail dir xs = (opp dir) : (init xs)

-- moves the whole snake
move :: Snake -> Direction -> Snake
move (Snake loc xs) dir = Snake (push dir loc) (moveTail dir xs)

-- moves the snake and adds to the tail
eat :: Snake -> Direction -> Snake
eat (Snake loc xs) dir = Snake (push dir loc) ((opp dir):xs)

-- checks if snake is inside the screen
inside :: Int -> Int -> Point -> Bool
inside xMax yMax (x,y) = abs x < xMax && abs y < yMax

-- check if snake is inside the screen and not on top of itself
valid :: Int -> Int -> Snake -> Bool
valid xMax yMax snake = points == nub points && all (inside xMax yMax) points
  where points = toPoints snake

-- turns snake into the list of coordinates
toPoints :: Snake -> [Point]
toPoints (Snake loc ds) = scanl (flip push) loc ds

-- checks if snake is eating a piece of food
eating :: Snake -> Point -> Bool
eating (Snake loc _)  food = loc == food

-- checks if snake intersects a point
intersecting :: Snake -> Point -> Bool
intersecting snake point = point `elem` toPoints snake

-- returns the length of the snake
score :: Snake -> Int
score (Snake _ dirs) = 1 + length dirs
