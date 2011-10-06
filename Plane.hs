-- | The module 'Plane' provides a data type for the primitive plane and functions
-- to check the intersection with a ray
module Plane where

import Vector
import Ray



-- | Data structure for a plane.
data Plane 
    -- | The constructor 'Plane' creates a 'Plane' data type which consists of its
    -- normal Vector3 and its distance from the origin.
    = Plane Vector3 Double deriving (Eq, Show, Read)

-- | Checks if a Ray hits the Plane and returns a Bool and the distance of the intersection
-- as Double.
intersect :: Plane -> Ray -> (Bool, Double)
intersect (Plane pn pd) (Ray rp rd) = 
    let n = Vector.mul pn (-1)
        a = Vector.dot n rd
    in  if a /= 0 then
            let b = ((-1) * ((Vector.dot n rp) + pd)) / a
            in  if b > 0 then (True, b)
                else (False, 0)
        else (False, 0)
  
-- px = Plane (Vector3 0 0 1) 1
-- py = Plane (Vector3 4 5 6) 10
