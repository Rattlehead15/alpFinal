{-# LANGUAGE FlexibleContexts #-}

module Display where

import Control.Monad
import Data.IORef
import Drawing
import Graphics.Formats.STL (STL)
import Graphics.UI.GLUT
import Linear hiding (frustum)
import Model
import Skeleton

empty :: IO ()
empty = do loadIdentity; clearColor $= Color4 0.1 0.1 0.1 0; clear [ColorBuffer, DepthBuffer]

display :: IORef (Transform GLdouble) -> IORef (Skeleton GLdouble) -> DisplayCallback
display camRef amogusRef = do
  loadIdentity
  (Transform p r) <- get camRef
  useM44 (inv44 $ mkTransformation r p)
  clearColor $= Color4 0.1 0.1 0.1 0
  clear [ColorBuffer, DepthBuffer]
  amogus <- get amogusRef
  preservingMatrix $ renderSkeleton amogus
  preservingMatrix $ do
    materialDiffuse Front $= Color4 0.1 0.9 0.1 (1 :: GLfloat)
    translate (Vector3 0 (-1) (0 :: GLfloat))
    scale 100 1 (100 :: GLfloat)
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
