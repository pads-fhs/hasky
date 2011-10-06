-- | The module 'Ray' provides a data type for representing a ray which 
-- can be used for several tracing jobs.
module Ray where

import Vector

-- | Data structure for a ray.
data Ray 
    -- | The constructor 'Ray' creates a Ray data type consisting of its
    -- position and its direction.
    = Ray Position Direction deriving (Eq, Show, Read)

-- | Position is just another name for a Vector3.
type Position = Vector3

-- | Direction is just another name for a Vector3.
type Direction = Vector3

-- rx = Ray (Vector3 1 2 3) (Vector3 4 5 6)
-- ry = Ray (Vector3 4 5 6) (Vector3 1 2 3)
