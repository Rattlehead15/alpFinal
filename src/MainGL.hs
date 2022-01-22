module MainGL (main) where

import Codec.Picture
import Collada
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Either
import Data.IORef
import Data.Maybe
import Data.Tree.Lens
import Data.Vector.Storable
import Display
import EX (kick, model)
import Events
import GHC.Base (returnIO)
import Graphics.UI.GLUT
import Linear
import Reactive.Banana
import Reactive.Banana.Frameworks
import Skeleton
import Types

main :: IO ()
main = do
  --- SETUP WINDOW AND BASIC SCENE SETTINGS ---
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, Multisampling]
  createWindow progName
  depthFunc $= Just Less
  cursor $= None
  multisample $= Enabled
  lighting $= Enabled
  Graphics.UI.GLUT.position (Light 0) $= Vertex4 0 0 1 1
  light (Light 0) $= Enabled
  Graphics.UI.GLUT.normalize $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  Graphics.UI.GLUT.normalize $= Enabled

  --- SETUP STATE ---
  cam <- newIORef (Transform (V3 0 1 2) (axisAngle (V3 0 0 0) 0))
  keyState <- newIORef (MoveKeys Up Up Up Up)
  lastPos <- newIORef (Nothing :: Maybe Position)

  --- SETUP MODEL FROM COLLADA FILE ---
  name <- genObjectName
  textureBinding Texture2D $= Just name
  Right (ImageRGBA8 (Image w h img)) <- readPng "model/textures/casual_man_color.png"
  unsafeWith img $ \ptr -> texImage2D Texture2D NoProxy 0 RGBA8 (TextureSize2D (fromIntegral w) (fromIntegral h)) 0 (PixelData RGBA UnsignedByte ptr)
  pose <- newIORef $ model ^. joints
  shadeModel $= Smooth
  generateMipmap Texture2D $= Enabled
  texture Texture2D $= Enabled
  textureFilter Texture2D $= ((Linear', Nothing), Linear')

  --- SETUP CALLBACKS ---
  specialCallback $= Just (onMoveKeys Down keyState) --- Used for arrow keys
  specialUpCallback $= Just (onMoveKeys Up keyState)
  (timeAddHandler, fireTime) <- newAddHandler
  passiveMotionCallback $= Just (onMouse cam lastPos)
  idleCallback $= Just (onUpdate cam keyState)
  displayCallback $= display fireTime cam model pose
  reshapeCallback $= Just reshape

  --- SETUP FRP FOR WASD MOVEMENT AND MODEL ANIMATION ---
  network <- compile $ do
    wasdDownEvent <- fromAddHandler $ AddHandler ((const .) >>> Just >>> (keyboardCallback $=) >>> (>> return (keyboardCallback $= Nothing)))
    wasdUpEvent <- fromAddHandler $ AddHandler ((const .) >>> Just >>> (keyboardUpCallback $=) >>> (>> return (keyboardUpCallback $= Nothing)))
    time <- fmap ((/ 1000) . fromIntegral) <$> fromChanges 0 timeAddHandler
    let animation = flip (kick ^. runAnim) (model ^. joints) <$> time
    changes animation >>= reactimate' . fmap (fmap (pose $=))
    reactimate $ onWasd Down keyState <$> wasdDownEvent
    reactimate $ onWasd Up keyState <$> wasdUpEvent
  actuate network
  mainLoop
