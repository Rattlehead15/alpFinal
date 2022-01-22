{-# LANGUAGE TemplateHaskell #-}

module Skeleton where

import Control.Lens
import Data.Tree
import Graphics.Rendering.OpenGL
import Linear

data Transform = Transform {_position :: V3 GLdouble, _rotation :: Quaternion GLdouble}

makeLenses ''Transform

useM44 :: M44 GLdouble -> IO ()
useM44 (V4 (V4 a11 a12 a13 a14) (V4 a21 a22 a23 a24) (V4 a31 a32 a33 a34) (V4 a41 a42 a43 a44)) = do
  m <-
    newMatrix
      RowMajor
      [ a11,
        a12,
        a13,
        a14,
        a21,
        a22,
        a23,
        a24,
        a31,
        a32,
        a33,
        a34,
        a41,
        a42,
        a43,
        a44
      ]
  multMatrix (m :: GLmatrix GLdouble)

toVertex3 :: V3 a -> Vertex3 a
toVertex3 (V3 x y z) = Vertex3 x y z

toVector3 :: V3 a -> Vector3 a
toVector3 (V3 x y z) = Vector3 x y z
