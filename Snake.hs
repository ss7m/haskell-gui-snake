module Snake (Snake(..), valid, move, eating, intersecting, eat, toPoints, score) where

import Direction

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
valid (Snake loc dirs) = inside loc && validRec [loc] dirs

validRec ::[Point] -> [Direction] -> Bool
validRec _ [] = True
validRec locs@(l:_) (d:ds) = noOverlap && validRec (newLoc:locs) ds
  where newLoc = push d l
        noOverlap = not $ newLoc `elem` locs

toPoints :: Snake -> [Point]
toPoints (Snake loc []) = [loc]
toPoints (Snake loc (d:ds)) = loc : toPoints (Snake (push d loc) ds)

intersecting :: Snake -> Point -> Bool
intersecting snake = \x -> elem x $ toPoints snake

eating :: Snake -> Point -> Bool
eating (Snake loc _) = (==) loc

score :: Snake -> Int
score (Snake _ dirs) = 1 + length dirs
