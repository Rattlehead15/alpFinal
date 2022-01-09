{-# LANGUAGE FlexibleContexts #-}

module Display where

import Control.Monad
import Data.IORef
import Drawing
import Graphics.Formats.STL (STL)
import Graphics.UI.GLUT
import Linear hiding (frustum)
import Model
import OBJ
import Skeleton

empty :: IO ()
empty = do loadIdentity; clearColor $= Color4 0.1 0.1 0.1 0; clear [ColorBuffer, DepthBuffer]

display :: IORef Transform -> IORef Skeleton -> OBJ -> DisplayCallback
display camRef amogusRef chabon = do
  loadIdentity
  (Transform p r) <- get camRef
  useM44 (inv44 $ mkTransformation r p)
  clearColor $= Color4 0.1 0.1 0.1 0
  clear [ColorBuffer, DepthBuffer]
  amogus <- get amogusRef
  preservingMatrix $ renderOBJ chabon
  preservingMatrix $ do translate (Vector3 0 0 (10 :: GLdouble)); renderSkeleton amogus
  preservingMatrix $ do
    translate (Vector3 0 (-1) (0 :: GLdouble))
    scale 100 1 (100 :: GLdouble)
    cube 1
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
