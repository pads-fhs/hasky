-- | The module 'Sphere' provides a data type for the primitive sphere and functions
-- to check the intersection with a ray and to get the normal vector at a point of
-- the sphere.
module Sphere where

import Vector
import Ray



-- | Data structure for a sphere.
data Sphere 
    -- | The constructor 'Sphere' creates a 'Sphere' data type which consists of its
    -- center point Vector3 and its radius as Double.
    = Sphere Vector3 Double deriving (Eq, Show, Read)

-- | Checks if a Ray hits the Sphere and returns a Bool and the distance of the intersection
-- as Double.
intersect :: Sphere -> Ray -> (Bool, Double)
intersect (Sphere sc sr) (Ray rp rd) = 
    let t = rp - sc
        a = (-1) * (Vector.dot t (norm rd))
        b = (a*a) - (Vector.dot t t) + (sr*sr)
    in  if b > 0 then
            let x1 = a - (sqrt b)
                x2 = a + (sqrt b)
            in  if x2 > 0 then
                    if x1 < 0 then
                        (False, x2) -- wir sind in der sphere
                    else
                        (True, x1)
                else (False, 0)
        else (False, 0)

-- | Gets the Normal of the Sphere at a given Positon.
getNormal :: Sphere -> Vector3 -> Vector3
getNormal (Sphere center r) atPosition = Vector.mul (atPosition - center) (1/r)

-- sx = Sphere (Vector3 0 0 1) 1
-- sy = Sphere (Vector3 4 5 6) 10
