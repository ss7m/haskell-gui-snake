module Idle (idle, generateFood) where

import Graphics.UI.GLUT
import Control.Concurrent.Thread.Delay
import Data.IORef
import System.Random
import Snake
import Screen
import Direction

xRange = (-xSize + 1, xSize - 1) :: (Int, Int)
yRange = (-ySize + 1, ySize - 1) :: (Int, Int)

generateFood :: Snake -> IO Point
generateFood snake = do
  rx <- get $ randomRIO xRange
  ry <- get $ randomRIO yRange
  let point = (rx,ry) :: Point
  if snake `intersecting` point then generateFood snake else return point

idle :: IORef Snake -> IORef Point -> IORef Direction -> IdleCallback
idle snake food dir = do
  delay (100000::Integer)
  d <- get dir
  f <- get food
  s <- get snake
  if s `eating` f
    then do
      snake $~! (`eat` d)
      f <- get $ generateFood s
      food $= f
    else snake $~! (`move` d)
  if not $ valid xSize ySize s
    then do
      putStrLn $ (++) "snek ded\nscore: " $ show $ score s
      exit
    else postRedisplay Nothing
