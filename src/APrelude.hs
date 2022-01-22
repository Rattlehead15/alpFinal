{-# LANGUAGE RankNTypes #-}

module APrelude where

import Collada
import Control.Arrow hiding (loop, (|||))
import Control.Lens hiding (transform)
import Control.Monad hiding (sequence)
import Data.Attoparsec.ByteString
import Data.ByteString hiding (drop)
import Data.Maybe
import Data.Serialize
import Data.Serialize.Get
import Data.Tree
import Data.Tree.Lens
import GHC.Arr
import Graphics.Formats.STL
import Graphics.UI.GLUT hiding (rotate)
import Linear hiding (rotate)
import Reactive.Banana
import Skeleton
import Types
import Prelude hiding (readFile, sequence, (||))

toRad :: GLdouble -> GLdouble
toRad = (* (pi / 180))

oscillateBetween :: GLdouble -> GLdouble -> Time -> GLdouble
oscillateBetween min max t = min + (max - min) * (0.5 * (1 + cos t))

oscillateBetweenAngles :: GLdouble -> GLdouble -> Time -> GLdouble
oscillateBetweenAngles minth maxth = oscillateBetween (toRad minth) (toRad maxth)

easeInOut :: Curve
easeInOut t = t * t / (2 * (t * t - t) + 1)

rotate :: Lens' Skeleton Joint -> Quaternion GLdouble -> Skeleton -> Skeleton
rotate s q = s . transform %~ (!*! m33_to_m44 (fromQuaternion q))

scale :: Lens' Skeleton Joint -> GLdouble -> Skeleton -> Skeleton
scale s f = s . transform %~ (!*! m33_to_m44 (f *!! identity))

xAxis :: V3 GLdouble
xAxis = V3 1 0 0

yAxis :: V3 GLdouble
yAxis = V3 0 1 0

zAxis :: V3 GLdouble
zAxis = V3 0 0 1

infixl 7 ==>

(==>) :: Animation -> Animation -> Animation
(==>) x@(Anim df f) (Anim dg g) = Anim (df + dg) (\t -> if t < df then f t else g (t - df) . f df)

infixl 8 |||

(|||) :: Animation -> Animation -> Animation
(|||) (Anim dx fx) (Anim dy fy) = Anim (max dx dy) (\t -> fy (min dy t) . fx (min dx t))

loop :: Animation -> Animation
loop x@(Anim d f) = Anim (1 / 0) (\t -> if t < d then f t else (loop x ^. runAnim) (t - d))

around :: (Quaternion GLdouble -> a) -> V3 GLdouble -> GLdouble -> a
around r axis = r . axisAngle axis . toRad

infixl 9 `for`

for :: RunAnim -> Time -> Animation
for = flip Anim

(|>>|) :: Animation -> GLdouble -> Animation
(|>>|) (Anim dx fx) m = Anim (dx / m) (fx . (* m))
