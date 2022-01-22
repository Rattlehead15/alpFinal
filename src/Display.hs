{-# LANGUAGE FlexibleContexts #-}

module Display where

import Collada
import Control.Monad
import Data.IORef
import Drawing
import Graphics.UI.GLUT
import Linear hiding (frustum)
import Reactive.Banana.Frameworks (Handler)
import Skeleton
import Types

empty :: IO ()
empty = do loadIdentity; clearColor $= Color4 0.1 0.1 0.1 0; clear [ColorBuffer, DepthBuffer]

display :: Handler Int -> IORef Transform -> Collada -> IORef Skeleton -> DisplayCallback
display fireTime camRef model poseRef = do
  get elapsedTime >>= fireTime
  loadIdentity
  (Transform p r) <- get camRef
  useM44 (inv44 $ mkTransformation r p)
  clearColor $= Color4 0.75 1 1 0
  clear [ColorBuffer, DepthBuffer]
  materialDiffuse Front $= Color4 1 1 1 1
  preservingMatrix $ get poseRef >>= renderCollada model
  materialDiffuse Front $= Color4 0 1 0 1
  preservingMatrix $ do
    translate (Vector3 0 (-100) (0 :: GLdouble))
    cube 100
  swapBuffers

reshape :: ReshapeCallback
reshape screenSize@(Size w h) = do
  viewport $= (Position 0 0, screenSize)
  matrixMode $= Projection
  loadIdentity
  let near = 0.001
      far = 1000
      fov = 90
      ang = fov * pi / 360
      top = near / (cos ang / sin ang)
      aspect = fromIntegral w / fromIntegral h
      right = top * aspect
  frustum (- right) right (- top) top near far
  matrixMode $= Modelview 0
