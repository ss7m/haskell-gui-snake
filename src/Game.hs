module Game where

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Game hiding (Point)
import System.Exit
import System.Random
import Control.Monad

import Snake
import Direction
import Grid
import State
import Display
import Input

-- Randomly generate a location for food if food is Nothing
genFood :: Maybe Point -> Snake -> IO Point
genFood (Just x) snake = return $ x
genFood Nothing  snake = do
  x <- randomRIO (0, gridWidth - 1)
  y <- randomRIO (0, gridHeight - 1)
  if (x, y) `elem` snake then
    genFood Nothing snake
  else
    return (x, y)

doMove :: State -> State
doMove (State s d f 0) = State (move s d) d f 0
doMove (State s d f c) = State (eat  s d) d f $ c-1

eatFood :: State -> IO State
eatFood (State s d f c) = do
  f' <- genFood f s
  if s `eating` f' then
    return $ State s d Nothing $ c+5
  else
    return $ State s d (Just f') c

-- Perform one step of the game
step :: Float -> State -> IO State
step _ state = do
  when (not $ valid gridWidth gridHeight $ getSnake state) (exitGame state)
  eatFood $ doMove state 

--exit the game and print out the player's score
exitGame :: State -> IO ()
exitGame state = do
  putStrLn $ "Your score is " ++ show (length (getSnake state))
  putStrLn "Thanks for playing!"
  exitSuccess

-- The Game!
game :: IO ()
game = playIO FullScreen black 13 initState drawState handleInput step
