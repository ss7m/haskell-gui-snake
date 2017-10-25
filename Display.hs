module Display (idle, display, generateFood) where

import Graphics.UI.GLUT
import Control.Concurrent.Thread.Delay
import Data.IORef
import System.Random
import Control.Monad
import Snake
import Direction

type GLpoint = (GLfloat, GLfloat)

gWidth = 0.05 :: GLfloat
offset = (gWidth/2) - 0.005 :: GLfloat

toQuad :: GLpoint -> [GLpoint]
toQuad (x,y) = [(x-offset,y-offset),(x+offset,y-offset),(x+offset,y+offset),(x-offset,y+offset)]

scalePoint :: Point -> GLpoint
scalePoint (x,y) = (gWidth*(fromIntegral x), gWidth*(fromIntegral y))

toGraphics :: Snake -> [[GLpoint]]
toGraphics snake = map toQuad $ map scalePoint $ toPoints snake

unpack :: [[a]] -> [a]
unpack [] = []
unpack (x:xs) = x ++ unpack xs

display :: IORef Snake -> IORef Point -> DisplayCallback
display snake food = do
  clear [ColorBuffer]
  s <- get snake
  f <- get food
  preservingMatrix $ do
    renderPrimitive Quads $
      mapM_ (\(x,y) -> vertex $ Vertex3 x y 0) ((unpack.toGraphics) s)
    renderPrimitive Quads $ 
      mapM_ (\(x,y) -> vertex $ Vertex3 x y 0) ((toQuad.scalePoint) f)
  swapBuffers

range = (-19, 19) :: (Int, Int)
randIdx = randomRIO range :: IO Int

generateFood :: Snake -> IO Point
generateFood snake = do
  rx <- get randIdx
  ry <- get randIdx
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
  if not $ valid s
    then do
      putStrLn $ (++) "snek ded\nscore: " $ show $ score s
      exit
    else postRedisplay Nothing
