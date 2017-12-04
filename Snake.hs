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

inside :: Point -> Bool
inside (x,y) = abs x < 20 && abs y < 20

valid :: Snake -> Bool
valid snake = points == nub points && all inside points
  where points = toPoints snake

toPoints :: Snake -> [Point]
toPoints (Snake loc ds) = scanl (flip push) loc ds

intersecting :: Snake -> Point -> Bool
intersecting snake = \x -> elem x $ toPoints snake

eating :: Snake -> Point -> Bool
eating (Snake loc _) = (==) loc

score :: Snake -> Int
score (Snake _ dirs) = 1 + length dirs
