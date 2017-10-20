module Display (idle, display) where

import Graphics.UI.GLUT
import Control.Concurrent.Thread.Delay
import Data.IORef
import Control.Monad
import SnakeDef

display :: IORef Snake -> DisplayCallback
display snake = do
  clear [ColorBuffer]
  pureSnake <- get snake
  preservingMatrix $ do
    renderPrimitive Quads $
      mapM_ (\(x,y) -> vertex $ Vertex3 x y 0) (toGraphics pureSnake)
  swapBuffers

idle :: IORef Snake -> IORef Direction -> IdleCallback
idle snake dir = do
  delay (90000::Integer)
  d <- get dir
  snake $~! (`move` d)
  s <- get snake
  if not $ valid s
    then do
      putStrLn "snek ded"
      exit
    else postRedisplay Nothing
