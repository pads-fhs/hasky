-- | The module 'Scene' provides a data type for storing a scene with all
-- of its objects, lights and its camera.
module Scene where

import Primitive
import Camera
import Light

-- | Data structure for a scene.
data Scene 
    -- | The constructor 'Scene' creates a Scene data type out of 
    -- a camera, a list for Light data types and a list for 
    -- Primitive data types.
    = Scene Camera [Light] [Primitive] deriving (Eq, Show, Read)

--scenex =  Scene (Camera (Vector3 1.0 2.0 3.0) (Vector3 4.0 5.0 6.0) (Vector3 4.0 5.0 6.0) (Vector3 4.0 5.0 6.0)) [Light [Option "color" (VS "ffffff"),Option "radius" (VD 1.0)],Light [Option "color" (VS "ff0000"),Option "radius" (VD 25.0)]] [Primitive TSphere [Option "color" (VS "ffffff"),Option "radius" (VD 1.0)],Primitive TPlane [Option "position" (VV (Vector3 0.0 0.0 1.0)),Option "d" (VD 6.0)]]
