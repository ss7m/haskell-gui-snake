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
  if (x, -y) `elem` snake then
    genFood Nothing snake
  else
    return (x, -y)

-- Moves or eat the snake, depending on the current counter
-- Changes counter accordingly
moveOrEat :: Snake -> Int -> Direction -> (Snake, Int)
moveOrEat snake 0 dir = (move snake dir, 0)
moveOrEat snake counter dir = (eat snake dir, counter - 1)

-- Perform one step of the game
step :: Float -> State -> IO State
step _ state = do
  let snake = getSnake state
  let dir = getDirection state
  let food = getFood state
  let counter = getCounter state

  newFood <- genFood food snake
  let (newSnake, newCounter) = moveOrEat snake counter dir

  if not (valid gridWidth gridHeight snake) then do
    exitProcedure $ State newSnake dir (Just newFood) newCounter
  else if snake `eating` newFood then
    return $ State newSnake dir Nothing (newCounter + 5)
  else 
    return $ State newSnake dir (Just newFood) newCounter

exitProcedure :: State -> IO a
exitProcedure state = do
  putStrLn $ "Your score is " ++ show (length (getSnake state))
  putStrLn "Thanks for playing!"
  exitSuccess

-- The Game!
game :: IO ()
game = playIO FullScreen black 13 initState drawState handleInput step
