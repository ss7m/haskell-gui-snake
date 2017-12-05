module Snake (Snake(..), valid, move, eating, intersecting, eat, toPoints, score) where

import Direction
import Data.List

data Snake = Snake Point [Direction]

moveTail :: Direction -> [Direction] -> [Direction]
moveTail _ [] = []
moveTail dir xs = (opp dir) : (init xs)

move :: Snake -> Direction -> Snake
move (Snake loc xs) dir = Snake (push dir loc) (moveTail dir xs)

eat :: Snake -> Direction -> Snake
eat (Snake loc xs) dir = Snake (push dir loc) ((opp dir):xs)

inside :: Int -> Int -> Point -> Bool
inside xMax yMax (x,y) = abs x < xMax && abs y < yMax

valid :: Int -> Int -> Snake -> Bool
valid xMax yMax snake = points == nub points && all (inside xMax yMax) points
  where points = toPoints snake

toPoints :: Snake -> [Point]
toPoints (Snake loc ds) = scanl (flip push) loc ds

intersecting :: Snake -> Point -> Bool
intersecting snake = \x -> elem x $ toPoints snake

eating :: Snake -> Point -> Bool
eating (Snake loc _) = (==) loc

score :: Snake -> Int
score (Snake _ dirs) = 1 + length dirs
