{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (makeLenses)
import Data.Tree (Tree)
import GHC.Arr (Array)
import Graphics.Rendering.OpenGL (GLdouble, Normal3, TexCoord2, Vertex3)
import Linear (M44)

type Time = GLdouble

type Skeleton = Tree Joint

type RunAnim = Time -> Skeleton -> Skeleton

data Animation = Anim {_duration :: Time, _runAnim :: RunAnim}

type Curve = Time -> Time

data VInfo = VInfo {_vertex :: Int, _normal :: Int, _texCoord :: Int} deriving (Show)

data Joint = Joint {_name :: String, _transform :: M44 GLdouble} deriving (Show)

data Collada = Collada
  { _vertices :: Array Int (Vertex3 GLdouble),
    _normals :: Array Int (Normal3 GLdouble),
    _uvs :: Array Int (TexCoord2 GLdouble),
    _triangles :: Array Int (VInfo, VInfo, VInfo),
    _weights :: Array Int GLdouble,
    _vertexWeights :: Array Int [(Int, Int)],
    _jointNames :: Array Int String,
    _joints :: Tree Joint,
    _invBinds :: Array Int (M44 GLdouble)
  }
  deriving (Show)

makeLenses ''VInfo
makeLenses ''Joint
makeLenses ''Collada
makeLenses ''Animation
