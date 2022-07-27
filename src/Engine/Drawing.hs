module Engine.Drawing where

import Graphics.UI.GLUT

points :: Int -> [(GLfloat, GLfloat, GLfloat)]
points n = [(sin (2 * pi * k / n'), cos (2 * pi * k / n'), 0) | k <- [1 .. n']]
  where
    n' = fromIntegral n

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

cube :: GLfloat -> IO ()
cube w =
  renderPrimitive Quads $
    mapM_
      vertex3f
      [ (w, w, w),
        (w, w, - w),
        (w, - w, - w),
        (w, - w, w),
        (w, w, w),
        (w, w, - w),
        (- w, w, - w),
        (- w, w, w),
        (w, w, w),
        (w, - w, w),
        (- w, - w, w),
        (- w, w, w),
        (- w, w, w),
        (- w, w, - w),
        (- w, - w, - w),
        (- w, - w, w),
        (w, - w, w),
        (w, - w, - w),
        (- w, - w, - w),
        (- w, - w, w),
        (w, w, - w),
        (w, - w, - w),
        (- w, - w, - w),
        (- w, w, - w)
      ]

cubeFrame :: GLfloat -> IO ()
cubeFrame w =
  renderPrimitive Lines $
    mapM_
      vertex3f
      [ (w, - w, w),
        (w, w, w),
        (w, w, w),
        (- w, w, w),
        (- w, w, w),
        (- w, - w, w),
        (- w, - w, w),
        (w, - w, w),
        (w, - w, w),
        (w, - w, - w),
        (w, w, w),
        (w, w, - w),
        (- w, w, w),
        (- w, w, - w),
        (- w, - w, w),
        (- w, - w, - w),
        (w, - w, - w),
        (w, w, - w),
        (w, w, - w),
        (- w, w, - w),
        (- w, w, - w),
        (- w, - w, - w),
        (- w, - w, - w),
        (w, - w, - w)
      ]
