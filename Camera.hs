-- | The module 'Camera' provides a data type to store a camera, functions
-- to create a Camera data type from simple parameters and the functions
-- used to trace a ray for the color of a pixel.
module Camera where

import Vector
import Ray
import Color
import Primitive
import Sphere
import Option
import Plane
import Xor
import Light

-- | Data structure for a Camera.
data Camera 
    -- | The constructor 'Camera' of Camera creates a Camera data which
    -- consists of the vectors: location, direction, right and down.
    = Camera Location Direction Right Down deriving (Eq, Show, Read)
    
-- | Just another name for a Vector3.
type Location = Vector3
-- | Just another name for a Vector3.
type Right = Vector3
-- | Just another name for a Vector3.
type Down = Vector3

-- | Returns a Camera data type with the simple input of position
-- look-at x-resolution y-resolution
getSimpleCamera pos lookat x y = 
    let camdir = (Vector.norm (lookat-pos))
        sky = Vector3 0 1 0
        vx = sky * camdir -- (Vector3 vx1 vx2 vx3)
        camright = (if vx == Vector.o then
                        let (Vector3 dirx _ dirz) = camdir in
                            if dirx == 0 && dirz == 0 then
                                Vector3 1 0 0
                            else
                                (Vector3 0 1 0) * camdir
                    else
                        norm vx)
    in
        -- trace
            (Camera
                pos
                (Vector.mul camdir 1) -- zoom
                (Vector.mul camright (x/y)) -- aspectratio, evtl * -1 für right handed
                (camright * camdir)
            )
            -- lo
            -- ll
            -- x
            -- y
                    
-- | Creates a Ray for the actual pixel and starts the tracing for its color.
camTrace :: Camera -> [Primitive] -> [Light] -> Double -> Double -> Color
camTrace (Camera loc dir right down) lo ll x y =
    let ray = Ray loc ((dir+(Vector.mul right (x-0.5)))+(Vector.mul down (y-0.5))) in
        traceForColor ray lo ll 10 1

-- | Checks all lights of a scene if they affect the current primitive and
-- calculates the new color.
forAllLights
  :: Color      -- ^ color akkumulator, should start at black
  -> Color      -- ^ material color from pixel
  -> Vector3    -- ^ point of intersection
  -> Vector3    -- ^ normal of intersection
  -> [Light]    -- ^ list of lights of the scene
  -> Color      -- ^ the resulting color
forAllLights returnColor _     _  _ [] = returnColor
forAllLights returnColor color pi n ll = 
    let (Light optsLight) = head ll
        a = pi - (getVector3 "position" optsLight)
        --d = 1 / ((Vector.len a) * (Vector.len a))
        dc = (Vector.scalar a (Vector.mul n (-1)))
        
        dot = (Vector.dot a n) * (-1)
        
        --lightdir = Vector.norm $ (getVector3 "position" optsLight) + (Vector.mul  pi (-1))
        --cosangle = Vector.dot (Vector.norm n) lightdir
        
        light = getColor "color" optsLight
        d = (Color dc dc dc)
    in
        if dot > 0 then
            forAllLights (returnColor + (color * light * d)) color pi n (tail ll)
        else
            forAllLights returnColor color pi n (tail ll)

-- | Gets the Color for an primitive intersection point.
getColorAt
     :: Ray         -- ^ the ray to trace
     -> [Primitive] -- ^ list of all primitives of the scene
     -> [Light]     -- ^ list of all lights of the scene
     -> Integer     -- ^ maximum recursions for reflections, should be like 5 .. 10
     -> Double      -- ^ the distance of the intersection
     -> Primitive   -- ^ the primitive which got an intersection with the ray
     -> Double      -- ^ the effect multiplicator of reflection, should start 1.0
     -> Color       -- ^ the resulting color
getColorAt ray lo ll recu dist prim effect =
    let (Primitive typ opts) = prim
        (Ray rl rd) = ray
        
        -- zeug für die reflection --------------------------
        pi = rl + (Vector.mul rd (dist * 0.999))
        normal =    if typ == TPlane then
                        getVector3 "normal" opts
                    else if typ == TSphere then
                        Sphere.getNormal (Sphere (getVector3 "position" opts) (getDouble "radius" opts)) pi
                    else
                        Vector.o
        reflection = getDouble "reflection" opts
        reflectedRay = Ray pi (Vector.mirror rd normal)
        -- -------------------------------------------------
        
        color = (getColorForPrim prim pi) + (Color 0.1 0.1 0.1) -- ambient
        returnColor =   if reflection /= 0 && recu /= 0 then
                            color + ((traceForColor reflectedRay lo ll (recu - 1) (effect * reflection)) * (Color reflection reflection reflection))
                        else
                            color
    in
        --forAllLights returnColor color pi normal ll
        forAllLights Color.black returnColor pi normal ll
        
-- | Checks all primitives of a scene for an intersection, gets the closest and traces for its color.
traceForColor
    :: Ray          -- ^ the ray to trace
    -> [Primitive]  -- ^ list of all primitives of the scene
    -> [Light]      -- ^ list of all lights of the scene
    -> Integer      -- ^ maximum recursion depth (for reflection)
    -> Double       -- ^ the effect multiplicator of reflection, should start 1.0
    -> Color        -- ^ the resulting color
traceForColor ray lo ll recu effect =
    let (dist, prim) = trace lo 50 ray (50, Primitive TNone [])
        (Primitive typ _) = prim
    in
            if typ == TNone then
                Color.black
            else
                getColorAt ray lo ll recu dist prim effect


-- | Gets the Color of a primitive in a given point (texture or basic color). 
getColorForPrim (Primitive typ opts) (Vector3 px py pz) =
    if typ == TSphere then
        getColor "color" opts
    else if typ == TPlane then
        if (((truncate px) `mod` 2 == 0) `xor` ((truncate py) `mod` 2 == 0) `xor` ((truncate pz) `mod` 2 == 0)) == ((px < 0) `xor` (py < 0) `xor` (pz < 0)) then
            getColor "color" opts
        else
            getColor "checkercolor" opts
    else
        getColor "color" opts

-- | Traces a ray for all primitives of a scene to find the nearest hit.
trace
     :: [Primitive]         -- ^ a list of all primitives in the scene
     -> Double              -- ^ start distance, should be maximum  sight distance (high value)
     -> Ray                 -- ^ the ray to trace
     -> (Double, Primitive) -- ^ start of the result, should be (MAX_SIGHT_DIST, Primitive TNone []) for 'nothing hit'
     -> (Double, Primitive) -- ^ the result as distance and the hit primitive
trace [] d _ akku = akku
trace pl d r akku =
    let (Primitive t ol) = head pl in
        if t == TSphere then
            let (hit, dist) = Sphere.intersect (Sphere (getVector3 "position" ol) (getDouble "radius" ol)) r in
                if hit && dist < d then
                    trace (tail pl) dist r (dist, (Primitive t ol))
                else
                    trace (tail pl) d r akku
        else if t == TPlane then
            let (hit, dist) = Plane.intersect (Plane (getVector3 "normal" ol) (getDouble "d" ol)) r in
                if hit && dist < d then
                    trace (tail pl) dist r (dist, (Primitive t ol))
                else
                    trace (tail pl) d r akku
        else
            trace (tail pl) d r akku
    

-- cx = Camera (Vector3 1 2 3) (Vector3 4 5 6) (Vector3 4 5 6) (Vector3 4 5 6)
-- cy = Camera (Vector3 4 5 6) (Vector3 1 2 3) (Vector3 4 5 6) (Vector3 4 5 6)
