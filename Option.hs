-- | This module provides functions and data types to handle multiple different
-- named options for an object of a scene.
module Option where

import Vector
import Color


-- | Data structure for an option.
data Option  = Option String Value deriving (Eq, Show, Read)

-- | Data structure for a value of an option.
data Value = VS String | VB Bool | VV Vector3 | VD Double | VC Color deriving (Eq, Show, Read)


-- | Gets a String with the name 'na' out of the option list 'ol'.
getString :: String -> [Option] -> String
getString na ol = 
    let l = [fval | (Option fname fval) <- ol, fname == na]
    in  if length l > 0 then
            let (VS x) = head l in  x
        else
            ("")

-- | Gets a Bool with the name 'na' out of the option list 'ol'.
getBool :: String -> [Option] -> Bool
getBool na ol = 
    let l = [fval | (Option fname fval) <- ol, fname == na]
    in  if length l > 0 then
            let (VB x) = head l in  x
        else
            (False)

-- | Gets a Double with the name 'na' out of the option list 'ol'.
getDouble :: String -> [Option] -> Double        
getDouble na ol = 
    let l = [fval | (Option fname fval) <- ol, fname == na]
    in  if length l > 0 then
            let (VD x) = head l in  x
        else
            (0.0)
            
-- | Gets a Vector3 with the name 'na' out of the option list 'ol'.
getVector3 :: String -> [Option] -> Vector3    
getVector3 na ol = 
    let l = [fval | (Option fname fval) <- ol, fname == na]
    in  if length l > 0 then
            let (VV x) = head l in  x
        else
            (Vector.o)

-- | Gets a Color with the name 'na' out of the option list 'ol'.
getColor :: String -> [Option] -> Color
getColor na ol = 
    let l = [fval | (Option fname fval) <- ol, fname == na]
    in  if length l > 0 then
            let (VC x) = head l in  x
        else
            (Color.black)

            
--ox=[Option "radius" (VD 1.0), Option "color" (VS "ffffff"), Option "position" (VV (Vector3 1 2 3))]
