import Graphics.UI.GLUT
import Data.IORef
import SnakeDef
import Display
import Bindings

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Snake!"
  snake <- newIORef (Snake ((0,0)::GLpoint) [])
  dir <- newIORef DOWN
  keyboardMouseCallback $= Just (keyboardMouse snake dir)
  idleCallback $= Just (idle snake dir)
  displayCallback $= display snake
  mainLoop
