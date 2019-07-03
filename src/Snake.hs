module Snake (Snake, valid, move, eating, eat) where

import Direction
import Data.List

type Snake = [Point]

-- moves the whole snake
move :: Snake -> Direction -> Snake
move snake dir = push dir (head snake) : init snake

-- moves the snake and adds to the tail
eat :: Snake -> Direction -> Snake
eat snake dir = push dir (head snake) : snake

-- checks if a point is inside the screen
inside :: Int -> Int -> Point -> Bool
inside xMax yMax (x,y) = x >= 0 && y >= 0 && x < xMax && y < yMax

-- checks if the head of the snake overlaps with the rest of the snake
noOverlap :: Snake -> Bool
noOverlap (x:xs) = not $ elem x xs

-- check if snake is inside the screen and not on top of itself
valid :: Int -> Int -> Snake -> Bool
valid xMax yMax snake = noOverlap snake && inside xMax yMax (head snake)

-- checks if snake is eating a piece of food
eating :: Snake -> Point -> Bool
eating snake food = (head snake) == food
