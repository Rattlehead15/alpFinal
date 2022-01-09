module Model where

import Control.Lens
import Control.Monad
import Data.Attoparsec.ByteString
import Data.ByteString
import Data.Serialize
import Data.Serialize.Get
import Data.Tree
import Data.Tree.Lens
import Graphics.Formats.STL
import Graphics.UI.GLUT
import Linear hiding (rotate)
import Reactive.Banana
import Skeleton
import Prelude hiding (readFile)

loadModel :: FilePath -> IO (Either String STL)
loadModel = fmap (runGet Data.Serialize.get) . readFile

renderModel :: STL -> IO ()
renderModel (STL _ tris) =
  renderPrimitive Triangles $
    forM_
      tris
      ( \(Graphics.Formats.STL.Triangle n ((x1, y1, z1), (x2, y2, z2), (x3, y3, z3))) -> do
          case n of
            Nothing -> return ()
            (Just (n1, n2, n3)) -> Graphics.UI.GLUT.normal (Normal3 n1 n2 n3)
          vertex (Vertex3 x1 y1 z1)
          vertex (Vertex3 x2 y2 z2)
          vertex (Vertex3 x3 y3 z3)
      )

minimogus :: STL -> IO ()
minimogus amogus = preservingMatrix $ do
  materialDiffuse Front $= Color4 0.5 0.1 0.1 (1 :: GLfloat)
  rotate 180 (Vector3 0 1 (0 :: GLfloat))
  rotate (-90) (Vector3 1 0 (0 :: GLfloat))
  scale 0.01 0.01 (0.01 :: GLfloat)
  renderModel amogus

makeMegaAMOGUS :: STL -> Skeleton
makeMegaAMOGUS amogus =
  Node
    ( Bone
        (minimogus amogus)
        (Transform (V3 0 0 0) noRot)
    )
    [ Node
        ( Bone
            (preservingMatrix $ do scale 0.5 0.8 (0.5 :: GLdouble); minimogus amogus)
            (Transform (V3 (-2) 4 0) (axisAngle (V3 0 0 1) (pi / 2)))
        )
        [ Node
            ( Bone
                (preservingMatrix $ do scale 0.5 0.8 (0.5 :: GLdouble); minimogus amogus)
                (Transform (V3 0 4 0) (axisAngle (V3 0 1 0) (pi / 2) * axisAngle (V3 0 0 1) (- pi / 2)))
            )
            []
        ],
      Node
        ( Bone
            (preservingMatrix $ do rotate (-90) (Vector3 0 0 (1 :: GLdouble)); scale 0.5 0.8 (0.5 :: GLdouble); minimogus amogus)
            (Transform (V3 0 4 0) (axisAngle (V3 0 0 0) 0))
        )
        []
    ]

mongus :: Skeleton -> Int -> Skeleton
mongus m t = m & (branches . _head . root . Skeleton.transform . rotation) *~ axisAngle (V3 0 1 0) (fromIntegral t * 0.01)

noRot :: Quaternion GLdouble
noRot = axisAngle (V3 0 0 0) 0

makeManco :: STL -> STL -> STL -> Skeleton
makeManco manco hombro mano =
  Node
    ( Bone (renderModel manco) (Transform (V3 0 0 0) noRot)
    )
    [ Node
        (Bone (renderModel hombro) (Transform (V3 0.18205 1.50746 0.03537) noRot))
        [ Node
            ( Bone
                (renderModel mano)
                ( Transform
                    (V3 0.22733 (-0.14413) 0.01551)
                    (axisAngle (V3 0 1 0) (toRad (-81.2)) * axisAngle (V3 1 0 0) (toRad (-30)))
                )
            )
            []
        ]
    ]

toRad :: GLdouble -> GLdouble
toRad = (* (pi / 180))

wobble :: GLdouble -> GLdouble -> Int -> GLdouble
wobble min max t = let tf = fromIntegral t / 1000 in min + max * (0.5 * (1 + cos tf))

wobbleAngle :: GLdouble -> GLdouble -> Int -> GLdouble
wobbleAngle minth maxth = wobble (toRad minth) (toRad maxth)

saludar :: Skeleton -> Int -> Skeleton
saludar m t = m & (branches . _head . branches . _head . root . Skeleton.transform . rotation) *~ axisAngle (V3 0 1 0) (wobbleAngle 0 60 (2 * t))
