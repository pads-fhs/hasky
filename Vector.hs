-- | This module provides math functions for three dimensional vectors and
-- a basic storage data type for these vectors.
module Vector where


-- | Data structure for a three dimensional vector.
data Vector3 
    -- | The constructor 'Vector3' creates a Vector3 data type with
    -- its components x, y and z as Double.
    = Vector3 Double Double Double deriving (Eq, Show, Read)

instance Num Vector3 where
  Vector3 a1 b1 c1 + Vector3 a2 b2 c2 = Vector3 (a1+a2) (b1+b2) (c1+c2)
  Vector3 a1 b1 c1 - Vector3 a2 b2 c2 = Vector3 (a1-a2) (b1-b2) (c1-c2)
  Vector3 a1 a2 a3 * Vector3 b1 b2 b3 = Vector3 (a2*b3-a3*b2) (a3*b1-a1*b3) (a1*b2-a2*b1)
  abs _ = 0
  signum _ = 1
  fromInteger _ = 0

-- | Returns the dot product of two vectors.
dot :: Vector3 -> Vector3 -> Double
dot (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = a1*b1 + a2*b2 + a3*b3

-- | Returns the length of a vector.
len :: Vector3 -> Double
len (Vector3 a1 a2 a3) = sqrt(a1*a1 + a2*a2 + a3*a3)

-- | Returns a normalized vector of the input vector.
norm :: Vector3 -> Vector3
norm (Vector3 a1 a2 a3) = let l = len(Vector3 a1 a2 a3) in (Vector3 (a1 / l) (a2/l) (a3/l))

-- | Returns a inversed normalized vector of the input vector.
normInv :: Vector3 -> Vector3
normInv (Vector3 a1 a2 a3) = let l = 1/len(Vector3 a1 a2 a3) in (Vector3 (a1 / l) (a2/l) (a3/l))

-- | Returns the scalar product of two vectors.
scalar :: Vector3 -> Vector3 -> Double
scalar v1 v2  = (dot v1 v2) / ((len v1) + (len v2))

-- | Returns the input vector multiplied with a scalar value.
mul :: Vector3 -> Double -> Vector3
mul (Vector3 a1 a2 a3) d = Vector3 (a1*d) (a2*d) (a3*d)

-- | Returns a with vector2 (normal) mirrored vector of vector1.
mirror :: Vector3 -> Vector3 -> Vector3
mirror (Vector3 a1 a2 a3) (Vector3 n1 n2 n3) = 
    let (Vector3 nn1 nn2 nn3) = (norm (Vector3 n1 n2 n3))
    in    
        let u = (-2) * (dot (Vector3 a1 a2 a3) (Vector3 nn1 nn2 nn3))
        in (Vector3 (a1 + u * nn1) (a2 + u * nn2) (a3 + u * nn3))

-- | The origin vector (0, 0, 0)
o :: Vector3
o = Vector3 0 0 0
  
-- vx = Vector3 1 2 3
-- vy = Vector3 4 5 6
