module Display (display) where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Snake
import Direction
import Screen

type GLpoint = (GLfloat, GLfloat)

gWidth  = (1.0 / (fromIntegral xSize)) :: GLfloat
gHeight = (1.0 / (fromIntegral ySize)) :: GLfloat
xOffset = (gWidth/2) - 0.005 :: GLfloat
yOffset = (gHeight/2) - 0.005 :: GLfloat

toQuad :: GLpoint -> [GLpoint]
toQuad (x,y) = [(x-xOffset,y-yOffset),(x+xOffset,y-yOffset),(x+xOffset,y+yOffset),(x-xOffset,y+yOffset)]

scalePoint :: Point -> GLpoint
scalePoint (x,y) = (gWidth*(fromIntegral x), gHeight*(fromIntegral y))

toGraphics :: [Point] -> [GLpoint]
toGraphics points = concat $ map toQuad $ map scalePoint $ points

display :: IORef Snake -> IORef Point -> DisplayCallback
display snake food = do
  clear [ColorBuffer]
  s <- get snake
  f <- get food
  preservingMatrix $ do
    renderPrimitive Quads $
      mapM_ (\(x,y) -> vertex $ Vertex3 x y 0) ((toGraphics.toPoints) s)
    renderPrimitive Quads $ 
      mapM_ (\(x,y) -> vertex $ Vertex3 x y 0) (toGraphics [f])
  swapBuffers
