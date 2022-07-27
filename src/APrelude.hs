{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module APrelude where

import Control.Lens hiding (transform)
import Linear hiding (rotate)
import Types

toRad :: Number -> Number
toRad = (* (pi / 180))

oscillateBetween :: Number -> Number -> Time -> Number
oscillateBetween min max t = min + (max - min) * (0.5 * (1 + cos t))

oscillateBetweenAngles :: Number -> Number -> Time -> Number
oscillateBetweenAngles minth maxth = oscillateBetween (toRad minth) (toRad maxth)

wobble :: Curve
wobble f = let ?t = sin (2 * pi * ?t) in f

easeInOut :: Curve
easeInOut f = let ?t = if ?t < 0.5 then 4 * (?t ** 3) else 1 - ((-2 * ?t + 2) ** 3) / 2 in f

easeIn :: Curve
easeIn f = let ?t = ?t ** 3 in f

easeOut :: Curve
easeOut f = let ?t = 1 - ((1 - ?t) ** 3) in f

easeInStrong :: Curve
easeInStrong f = let ?t = if ?t == 0 then 0 else 2 ** (10 * ?t - 10) in f

easeOutStrong :: Curve
easeOutStrong f = let ?t = if ?t == 1 then 1 else 1 - 2 ** (-10 * ?t) in f

easeInBack :: Curve
easeInBack f = let ?t = (1.70158 * ?t ** 3 - 2.70158 * ?t ** 2) in f

easeOutBack :: Curve
easeOutBack f = let ?t = (1 + 2.70158 * (?t - 1) ** 3 + 1.70158 * (?t - 1) ** 2) in f

translate :: BodyPart -> V3 Number -> RunAnim (Skeleton -> Skeleton)
translate s v = s . transform . translation %~ (^+^ v)

rotate :: BodyPart -> Quaternion Number -> RunAnim (Skeleton -> Skeleton)
rotate s q = s . transform %~ (!*! m33_to_m44 (fromQuaternion q))

scale :: BodyPart -> Number -> RunAnim (Skeleton -> Skeleton)
scale s f = s . transform %~ (!*! m33_to_m44 (f *!! identity))

xAxis :: V3 Number
xAxis = V3 1 0 0

yAxis :: V3 Number
yAxis = V3 0 1 0

zAxis :: V3 Number
zAxis = V3 0 0 1

infixl 7 ==>

(==>) :: Animation -> Animation -> Animation
(==>) (Anim df f) (Anim dg g) = Anim (df + dg) (if ?t < df then f else (let ?t = ?t - df in g) . (let ?t = df in f))

infixl 8 |||

(|||) :: Animation -> Animation -> Animation
(|||) (Anim dx fx) (Anim dy fy) = Anim (max dx dy) ((let ?t = min dy ?t in fy) . (let ?t = min dx ?t in fx))

loop :: Animation -> Animation
loop x@(Anim d f) = Anim (1 / 0) (if ?t < d then f else let ?t = ?t - d in (let (Anim _ g) = loop x in g))

around :: (Quaternion Number -> a) -> V3 Number -> Number -> a
around r axis = r . axisAngle axis . toRad

infixl 9 `for`

for :: RunAnim (Skeleton -> Skeleton) -> Time -> Animation
for r t = Anim t (let ?t = if ?t >= t then 1 else ?t / t in r)

(|>>|) :: Animation -> Number -> Animation
(|>>|) (Anim dx fx) m = Anim (dx / m) (let ?t = ?t * m in fx)

infixl 9 `andAlso`

andAlso :: (b -> c) -> (a -> b) -> a -> c
andAlso = (.)

-- | Linearly interpolates during the animation from 0 to the specified value
to ::
  -- | The number to interpolate to in the animation
  Number ->
  RunAnim Number
to = (* ?t)
