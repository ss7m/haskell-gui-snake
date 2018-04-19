module Display (display) where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Snake2
import Direction
import Screen

type GLpoint = (GLfloat, GLfloat)

gWidth  = (1.0 / (fromIntegral xSize)) :: GLfloat
gHeight = (1.0 / (fromIntegral ySize)) :: GLfloat
xOffset = (gWidth/2) - 0.005 :: GLfloat
yOffset = (gHeight/2) - 0.005 :: GLfloat

-- transforms a point into four points forming a square around the point
toQuad :: GLpoint -> [GLpoint]
toQuad (x,y) = [(x-xOffset,y-yOffset),(x+xOffset,y-yOffset),(x+xOffset,y+yOffset),(x-xOffset,y+yOffset)]

-- scales an integral point into a point for graphics
scalePoint :: Point -> GLpoint
scalePoint (x,y) = (gWidth*(fromIntegral x), gHeight*(fromIntegral y))

-- transforms a list of points into a list of points describing rectangles around the points
toGraphics :: [Point] -> [GLpoint]
toGraphics points = concat $ map toQuad $ map scalePoint $ points

-- display call back
display :: IORef Snake -> IORef Point -> DisplayCallback
display snake food = do
  clear [ColorBuffer]
  s <- get snake
  f <- get food
  preservingMatrix $ do
    renderPrimitive Quads $
      forM_ ((toGraphics.toPoints) s) $ \(x,y) -> do 
        color $ Color3 ((x+1)/2) ((y+1)/2) 0.5
        vertex $ Vertex3 x y 0
    color $ Color3 (1 :: GLfloat) 1 1
    renderPrimitive Quads $ 
      mapM_ (\(x,y) -> vertex $ Vertex3 x y 0) (toGraphics [f])
  swapBuffers
