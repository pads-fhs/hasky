-- | The module 'Primitive' provides a data type for storing multiple types
-- of primitives in one data type.
module Primitive where

import Vector
import Option


-- | Data structure for a primitive.
data Primitive 
    -- | The constructor 'Primitive' creates a Primitive with the type
    -- 'PrimType' and a custom list of Options.
    = Primitive PrimType [Option] deriving (Eq, Show, Read)
    
-- | Data structure for the type of a primitive.
data PrimType
    -- | Defines the available types of a primitive.
    = TSphere | TPlane | TNone  deriving (Eq, Show, Read)



-- primx = Primitive TSphere [Option "color" (VS "ffffff"), Option "radius" (VD 1.0)]
-- primy = Primitive TPlane [Option "position" (VV (Vector3 0 0 1)), Option "d" (VD 6.0)]
