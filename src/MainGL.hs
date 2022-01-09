module MainGL (main) where

import Codec.Picture
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Either
import Data.IORef
import Data.Vector.Storable
import Display
import Events
import GHC.Base (returnIO)
import Graphics.UI.GLUT
import Linear
import Model (loadModel, makeManco, makeMegaAMOGUS, mongus, saludar)
import OBJ (readOBJ)
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
  Graphics.UI.GLUT.position (Light 0) $= Vertex4 0 0 0 3
  light (Light 0) $= Enabled
  Graphics.UI.GLUT.normalize $= Enabled

  cam <- newIORef (Transform (V3 0 0 (-10)) (axisAngle (V3 0 1 0) (pi :: GLdouble)))
  keyState <- newIORef (MoveKeys Up Up Up Up)
  lastPos <- newIORef (Nothing :: Maybe Position)

  name <- genObjectName
  textureBinding Texture2D $= Just name
  Right (ImageRGBA8 (Image w h img)) <- readPng "model/textures/casual_man_color.png"
  unsafeWith img $ \ptr -> texImage2D Texture2D NoProxy 0 RGBA8 (TextureSize2D (fromIntegral w) (fromIntegral h)) 0 (PixelData RGBA UnsignedByte ptr)

  Right torso <- loadModel "manco.stl"
  Right hombro <- loadModel "hombro.stl"
  Right mano <- loadModel "mano.stl"
  chabon <- readOBJ "model/chabon.obj"
  manco <- newIORef $ makeManco torso hombro mano
  specialCallback $= Just (onMoveKeys Down keyState) -- Esto es cuando se apretan las specialKeys (como por ejemplo las flechitas xd)
  specialUpCallback $= Just (onMoveKeys Up keyState)
  (timeAddHandler, fireTime) <- newAddHandler
  network <- compile $ do
    wasdDownEvent <- fromAddHandler $ AddHandler ((const .) >>> Just >>> (keyboardCallback $=) >>> (>> return (keyboardCallback $= Nothing)))
    wasdUpEvent <- fromAddHandler $ AddHandler ((const .) >>> Just >>> (keyboardUpCallback $=) >>> (>> return (keyboardUpCallback $= Nothing)))
    time <- fromChanges 0 timeAddHandler
    m <- get manco
    let saludo = saludar m <$> time
    changes saludo >>= reactimate' . fmap (fmap (manco $=))
    reactimate $ onWasd Down keyState <$> wasdDownEvent
    reactimate $ onWasd Up keyState <$> wasdUpEvent
  actuate network

  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  Graphics.UI.GLUT.normalize $= Enabled
  texture Texture2D $= Enabled
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  shadeModel $= Smooth
  generateMipmap Texture2D $= Enabled

  passiveMotionCallback $= Just (onMouse cam lastPos)
  idleCallback $= Just (onUpdate fireTime cam keyState)
  displayCallback $= display cam manco chabon
  reshapeCallback $= Just reshape
  mainLoop
