module MainGL (main) where

import Data.IORef
import Display
import Events
import Graphics.UI.GLUT
import Linear
import Model (loadModel, makeMegaAMOGUS)
import Skeleton

main :: IO ()
main = do
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  createWindow progName
  depthFunc $= Just Less
  cursor $= None

  lighting $= Enabled
  Graphics.UI.GLUT.position (Light 0) $= Vertex4 5 20 0 3
  light (Light 0) $= Enabled
  Graphics.UI.GLUT.normalize $= Enabled

  cam <- newIORef (Transform (V3 0 0 (-10)) (axisAngle (V3 0 1 0) (pi :: GLdouble)))
  keyState <- newIORef (MoveKeys Up Up Up Up)
  lastPos <- newIORef (Nothing :: Maybe Position)

  Right amogus <- loadModel "amogus.stl"
  let megaAMOGUS = makeMegaAMOGUS amogus
  specialCallback $= Just (onMoveKeys Down keyState) -- Esto es cuando se apretan las specialKeys (como por ejemplo las flechitas xd)
  specialUpCallback $= Just (onMoveKeys Up keyState)
  keyboardCallback $= Just (onWasd Down keyState)
  keyboardUpCallback $= Just (onWasd Up keyState)
  passiveMotionCallback $= Just (onMouse cam lastPos)
  idleCallback $= Just (onUpdate cam keyState)
  displayCallback $= display cam megaAMOGUS
  reshapeCallback $= Just reshape
  mainLoop
