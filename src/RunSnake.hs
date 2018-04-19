module RunSnake (runSnake) where

import Graphics.UI.GLUT
import Data.IORef
import Display
import Bindings
import Snake2
import Direction
import Idle

newSnake :: Snake
newSnake = Snake [(0,0)]

runSnake :: IO ()
runSnake = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Snake!"
  fullScreen
  snake <- newIORef newSnake
  dir <- newIORef DOWN
  food <- generateFood newSnake >>= newIORef
  eatCounter <- newIORef (0 :: Int)
  keyboardMouseCallback $= Just (keyboardMouse snake dir)
  idleCallback $= Just (idle snake food eatCounter dir)
  displayCallback $= display snake food
  mainLoop
