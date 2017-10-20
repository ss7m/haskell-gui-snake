module SnakeDef (Direction(..), opp, Snake(..), toGraphics, valid, move, GLpoint) where

import Graphics.UI.GLUT

data Direction = UP | DOWN | LEFT | RIGHT deriving(Eq)

type GLpoint = (GLfloat, GLfloat)

data Snake = Snake GLpoint [Direction]

gWidth = 0.05 :: GLfloat
offset = (gWidth/2) - 0.005 :: GLfloat

push :: Direction -> GLpoint -> GLpoint
push UP = (\(x,y)->(x,y+gWidth))
push DOWN = (\(x,y)->(x,y-gWidth))
push LEFT = (\(x,y)->(x-gWidth,y))
push RIGHT = (\(x,y)->(x+gWidth,y))

opp :: Direction -> Direction
opp UP = DOWN
opp DOWN = UP
opp LEFT = RIGHT
opp RIGHT = LEFT

moveTail :: Direction -> [Direction] -> [Direction]
moveTail _ [] = []
moveTail dir xs = (opp dir) : (init xs)

move :: Snake -> Direction -> Snake
move (Snake loc xs) dir = Snake (push dir loc) (moveTail dir xs)

inside :: GLpoint -> Bool
inside (x,y) = (ax < 1) && (ay < 1)
  where ax = abs x
        ay = abs y

valid :: Snake -> Bool
valid (Snake loc dirs) = inside loc && validRec [loc] dirs

validRec ::[GLpoint] -> [Direction] -> Bool
validRec _ [] = True
validRec locs@(l:_) (d:ds) = noOverlap && validRec (newLoc:locs) ds
  where newLoc = push d l
        noOverlap = not $ newLoc `elem` locs

toPoints :: Snake -> [GLpoint]
toPoints (Snake loc []) = [loc]
toPoints (Snake loc (d:ds)) = loc : toPoints (Snake (push d loc) ds)

toQuad :: GLpoint -> [GLpoint]
toQuad (x,y) = [(x-offset,y-offset),(x+offset,y-offset),(x+offset,y+offset),(x-offset,y+offset)]

unpack :: [[a]] -> [a]
unpack [] = []
unpack (x:xs) = x ++ unpack xs

toGraphics :: Snake -> [GLpoint]
toGraphics = unpack . ((map toQuad) . toPoints) 
