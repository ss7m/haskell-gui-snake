import Graphics.UI.GLUT
import Data.IORef
import Display
import Bindings
import Snake
import Direction

newSnake = Snake (0,0) [] :: Snake

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Snake!"
  snake <- newIORef newSnake
  dir <- newIORef DOWN
  f <- get $ generateFood newSnake
  food <- newIORef f
  keyboardMouseCallback $= Just (keyboardMouse snake dir)
  idleCallback $= Just (idle snake food dir)
  displayCallback $= display snake food
  mainLoop
