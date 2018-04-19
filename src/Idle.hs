module Idle (idle, generateFood) where

import Graphics.UI.GLUT
import Control.Concurrent
import Data.IORef
import System.Random
import Snake2
import Screen
import Direction

xRange = (-xSize + 1, xSize - 1) :: (Int, Int)
yRange = (-ySize + 1, ySize - 1) :: (Int, Int)

-- generates a new location for food
generateFood :: Snake -> IO Point
generateFood snake = do
  rx <- get $ randomRIO xRange
  ry <- get $ randomRIO yRange
  let point = (rx,ry) :: Point
  if snake `intersecting` point then generateFood snake else return point

-- idle call back
idle :: IORef Snake -> IORef Point -> IORef Int -> IORef Direction -> IdleCallback
idle snake food eatCounter dir = do
  threadDelay 90000
  d <- get dir
  f <- get food
  s <- get snake
  if s `eating` f then do
    eatCounter $~! (+5)
    generateFood s >>= (food $=)
  else return ()
  e <- get eatCounter
  if e > 0 then do
    snake $~! (`eat` d)
    eatCounter $~! ((+) (-1))
    else snake $~! (`move` d)
  if not $ valid xSize ySize s then do
    putStrLn $ (++) "snek ded\nscore: " $ show $ score s
    exit
  else postRedisplay Nothing
