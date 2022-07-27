{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module APrelude where

import Control.Lens hiding (transform)
import Linear hiding (rotate)
import Types

toRad :: Number -> Number
toRad = (* (pi / 180))

-- | Changes the passage of time from 0-1 to a full cycle of a sine wave. Note that the time will become negative.
wobble :: Curve
wobble f = let ?t = sin (2 * pi * ?t) in f

-- | Curve with easing on the inset and outset of the movement.
easeInOut :: Curve
easeInOut f = let ?t = if ?t < 0.5 then 4 * (?t ** 3) else 1 - ((-2 * ?t + 2) ** 3) / 2 in f

-- | Curve with easing on the inset of the movement.
easeIn :: Curve
easeIn f = let ?t = ?t ** 3 in f

-- | Curve with easing on the outset of the movement.
easeOut :: Curve
easeOut f = let ?t = 1 - ((1 - ?t) ** 3) in f

-- | Curve with strong easing on the inset of the movement.
easeInStrong :: Curve
easeInStrong f = let ?t = if ?t == 0 then 0 else 2 ** (10 * ?t - 10) in f

-- | Curve with strong easing on the outset of the movement.
easeOutStrong :: Curve
easeOutStrong f = let ?t = if ?t == 1 then 1 else 1 - 2 ** (-10 * ?t) in f

-- | Curve with strong easing and undershoot on the inset of the movement. Note that time will be negative during undershoot.
easeInBack :: Curve
easeInBack f = let ?t = (1.70158 * ?t ** 3 - 2.70158 * ?t ** 2) in f

-- | Curve with strong easing and overshoot on the outset of the movement. Note that time will go above 1 during overshoot.
easeOutBack :: Curve
easeOutBack f = let ?t = (1 + 2.70158 * (?t - 1) ** 3 + 1.70158 * (?t - 1) ** 2) in f

-- | Offsets a body part's position by some vector.
translate :: BodyPart -> V3 Number -> RunAnim (Skeleton -> Skeleton)
translate s v = s . transform . translation %~ (^+^ v)

-- | Rotates a body part with some quaternion.
rotate :: BodyPart -> Quaternion Number -> RunAnim (Skeleton -> Skeleton)
rotate s q = s . transform %~ (!*! m33_to_m44 (fromQuaternion q))

-- | Scales a body part by some factor.
scale :: BodyPart -> Number -> RunAnim (Skeleton -> Skeleton)
scale s f = s . transform %~ (!*! m33_to_m44 (f *!! identity))

-- | Shorthand for (V3 1 0 0).
xAxis :: V3 Number
xAxis = V3 1 0 0

-- | Shorthand for (V3 0 1 0).
yAxis :: V3 Number
yAxis = V3 0 1 0

-- | Shorthand for (V3 0 0 1).
zAxis :: V3 Number
zAxis = V3 0 0 1

infixl 7 ==>

-- | Composes two animations sequentially.
(==>) :: Animation -> Animation -> Animation
(==>) (Anim df f) (Anim dg g) = Anim (df + dg) (if ?t < df then f else (let ?t = ?t - df in g) . (let ?t = df in f))

infixl 8 |||

-- | Composes two animations in parallel.
(|||) :: Animation -> Animation -> Animation
(|||) (Anim dx fx) (Anim dy fy) = Anim (max dx dy) ((let ?t = min dy ?t in fy) . (let ?t = min dx ?t in fx))

-- | Loops an animation indefinitely.
loop :: Animation -> Animation
loop x@(Anim d f) = Anim (1 / 0) (if ?t < d then f else let ?t = ?t - d in (let (Anim _ g) = loop x in g))

-- | Transforms a function that takes a quaternion into a function that takes an axis vector and a rotation amount.
around :: (Quaternion Number -> a) -> V3 Number -> Number -> a
around r axis = r . axisAngle axis . toRad

infixl 9 `for`

-- | Creates an animation with a specific duration.
for :: RunAnim (Skeleton -> Skeleton) -> Time -> Animation
for r t = Anim t (let ?t = if ?t >= t then 1 else ?t / t in r)

-- | Increases the playback speed of an animation by some amount.
(|>>|) :: Animation -> Number -> Animation
(|>>|) (Anim dx fx) m = Anim (dx / m) (let ?t = ?t * m in fx)

infixl 9 `andAlso`

andAlso :: (b -> c) -> (a -> b) -> a -> c
andAlso = (.)

-- | Linearly interpolates during the animation from 0 to the specified value
to :: Number -> RunAnim Number
to = (* ?t)
