module Game where

import Control.Monad
import Data.Maybe (isNothing)
import Direction
import Display

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Game hiding (Point)
import Grid
import Input

import Snake
import State
import System.Exit
import System.Random

-- Randomly generate a location for food if food is Nothing
genFood :: State -> IO State
genFood state =
    case getFood state of
        Just _ -> return state
        Nothing -> do
            x <- randomRIO (0, gridWidth - 1)
            y <- randomRIO (0, gridHeight - 1)
            if (x, y) `elem` getSnake state
                then do
                    putStrLn "trying again"
                    genFood state
                else do
                    putStrLn "done"
                    return $ setFood (Just (x, y)) state

-- move snake accordingly
doMove :: State -> State
doMove state =
    case getCounter state of
        0 ->
            let newSnake = move (getSnake state) (getDirection state)
             in setSnake newSnake state
        c ->
            let newSnake = eat (getSnake state) (getDirection state)
             in setCounter (c - 1) $ setSnake newSnake state

-- increment eat counter if snake is eating
eatFood :: State -> State
eatFood state =
    case getFood state of
        Nothing -> state
        (Just f) ->
            if getSnake state `eating` f
                then let newCounter = getCounter state + 5
                      in setFood Nothing $ setCounter newCounter state
                else state

-- Perform one step of the game
step :: Float -> State -> IO State
step _ state = do
    when (invalid state) (exitGame state)
    if getPaused state
        then return state
        else step' state
  where
    step' = genFood . eatFood . doMove
    invalid = not . valid gridWidth gridHeight . getSnake

-- exit the game and print out the player's score
exitGame :: State -> IO ()
exitGame state = do
    let score = (length $ getSnake state) - 1
    putStrLn $ "Your score is " ++ show score
    putStrLn "Thanks for playing!"
    exitSuccess

-- The Game!
game :: IO ()
game = playIO FullScreen black 11 initState drawState handleInput step
