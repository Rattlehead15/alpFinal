module MainGL (main) where

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Either
import Data.IORef
import Display
import Events
import GHC.Base (returnIO)
import Graphics.UI.GLUT
import Linear
import Model (loadModel, makeMegaAMOGUS, mongus)
import Reactive.Banana
import Reactive.Banana.Frameworks
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

  megaAMOGUS <- loadModel "amogus.stl" >>= (\(Right model) -> newIORef $ makeMegaAMOGUS model)
  specialCallback $= Just (onMoveKeys Down keyState) -- Esto es cuando se apretan las specialKeys (como por ejemplo las flechitas xd)
  specialUpCallback $= Just (onMoveKeys Up keyState)
  (timeAddHandler, fireTime) <- newAddHandler

  network <- compile $ do
    wasdDownEvent <- fromAddHandler $ AddHandler ((const .) >>> Just >>> (keyboardCallback $=) >>> (>> return (keyboardCallback $= Nothing)))
    wasdUpEvent <- fromAddHandler $ AddHandler ((const .) >>> Just >>> (keyboardUpCallback $=) >>> (>> return (keyboardUpCallback $= Nothing)))
    time <- fromChanges 0 timeAddHandler
    m <- get megaAMOGUS
    let mamongus = mongus m <$> time
    changes mamongus >>= reactimate' . fmap (fmap (megaAMOGUS $=))
    reactimate $ onWasd Down keyState <$> wasdDownEvent
    reactimate $ onWasd Up keyState <$> wasdUpEvent
  actuate network

  passiveMotionCallback $= Just (onMouse cam lastPos)
  idleCallback $= Just (onUpdate fireTime cam keyState)
  displayCallback $= display cam megaAMOGUS
  reshapeCallback $= Just reshape
  mainLoop
