-- | This module provides data types to store and handle lights.
module Light where

import Vector
import Option

-- | Data structure for a light.
data Light
    -- | The constructor 'Light' creates a Light data which consists of a
    -- list of 'Option' to configure itself.
    = Light [Option] deriving (Eq, Show, Read)


-- lightx = Light [Option "color" (VS "ffffff"), Option "radius" (VD 1.0)]
-- lighty = Light [Option "color" (VS "ff0000"), Option "radius" (VD 25.0)]