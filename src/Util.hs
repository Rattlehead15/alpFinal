module Util where

norm :: Floating c => c -> c -> c -> (c, c, c)
norm x y z = (x / m, y / m, z / m) where m = sqrt $ x * x + y * y + z * z

dotProduct :: Num a => (a, a, a) -> (a, a, a) -> a
dotProduct (x, y, z) (a, b, c) = a * x + b * y + c * z

crossProduct :: Num c => (c, c, c) -> (c, c, c) -> (c, c, c)
crossProduct (a1, a2, a3) (b1, b2, b3) = (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)

rodrigues :: Floating a => (a, a, a) -> a -> (a, a, a) -> (a, a, a)
rodrigues k theta v = (v .* cos theta) .+ ((k `crossProduct` v) .* sin theta) .+ (k .* ((k `dotProduct` v) * (1 - cos theta)))

(.+) :: (Num a, Num b, Num c) => (a, b, c) -> (a, b, c) -> (a, b, c)
(.+) (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)

(.*) :: Num c => (c, c, c) -> c -> (c, c, c)
(.*) (v1, v2, v3) s = (v1 * s, v2 * s, v3 * s)
