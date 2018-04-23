module Game where

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Game hiding (Point)
import System.Exit
import System.Random
import Data.Maybe

import Snake
import Direction
import Grid

data State = State {getSnake :: Snake, getDirection :: Direction, getFood :: Maybe Point, getEatCounter :: Int}

-- Initial State for the game
initState :: State
initState = State [(0, 0)] DOWN Nothing 0

-- Create a rectangle given screen information and a point
makeRect :: Float -> Float -> Float -> Float -> Point -> Picture
makeRect w h sw sh (x, y) = translate transW transH rect
  where
    x' = fromIntegral x
    y' = fromIntegral $ negate y
    transW = -sw / 2 + (x' + 1) * w
    transH =  sh / 2 - (y' + 1) * h
    rect = rectangleSolid w h

-- cons a maybe to a list
consMaybe :: Maybe a -> [a] -> [a]
consMaybe (Just x) = (x :)
consMaybe Nothing  = id

-- Draw the current state
drawState :: State -> IO Picture
drawState state = do
  w <- blockWidth
  h <- blockHeight
  sw <- fromIntegral <$> screenWidth
  sh <- fromIntegral <$> screenHeight
  let food = color red . makeRect w h sw sh <$> getFood state
  let snake = color white . makeRect w h sw sh <$> getSnake state
  return $ pictures $ consMaybe food snake

-- Handle input
handleInput :: Event -> State -> IO State
handleInput (EventKey (SpecialKey KeyEsc) Down _ _) = const exitSuccess
handleInput (EventKey k Down _ _) = return . setDirection (keyToDir k)
handleInput _ = return . id

-- Return a direction given a key
keyToDir :: Key -> Maybe Direction
keyToDir (Char 'w') = Just UP
keyToDir (Char 'a') = Just LEFT
keyToDir (Char 's') = Just DOWN
keyToDir (Char 'd') = Just RIGHT
keyToDir _ = Nothing

-- Attempt to set the snake in a state to a direction
setDirection :: Maybe Direction -> State -> State
setDirection Nothing state = state
setDirection (Just dir) state = 
  case getSnake state of
    [x] -> State (getSnake state) dir (getFood state) (getEatCounter state)
    (x:y:_) -> if push dir x == y then state else State (getSnake state) dir (getFood state) (getEatCounter state)

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

-- Moves or eat the snake, depending on the current eatCounter
-- Changes eatCounter accordingly
moveOrEat :: Snake -> Int -> Direction -> (Snake, Int)
moveOrEat snake 0 dir = (move snake dir, 0)
moveOrEat snake x dir = (eat snake dir, x - 1)

-- Perform one step of the game
step :: Float -> State -> IO State
step _ state = do
  let snake = getSnake state
  let dir = getDirection state
  let food = getFood state
  let eatCounter = getEatCounter state

  newFood <- genFood food snake
  let (newSnake, newEatCounter) = moveOrEat snake eatCounter dir

  if not (valid gridWidth gridHeight snake) then do
    putStrLn $ "Your score is: " ++ show (length newSnake)
    putStrLn "Thanks for playing!"
    exitSuccess
  else if snake `eating` newFood then
    return $ State newSnake dir Nothing (newEatCounter + 5)
  else 
    return $ State newSnake dir (Just newFood) newEatCounter

-- The Game!
game :: IO ()
game = playIO FullScreen black 12 initState drawState handleInput step
