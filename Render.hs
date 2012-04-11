-- | This module provides render functions such as the main render loop.
module Render( renderImage ) where

import System( getArgs )
import Camera
import Color
import Data.ByteString as B (pack, ByteString)--hiding (foldl, map, reverse, take, length, readFile, putStrLn) --(pack, ByteString, concat)
import Data.ByteString.Lazy  as BL (pack, cons', empty, fromChunks, writeFile, ByteString)
import Option
import Vector
import Light
import Scene
import Primitive
import Sphere
import Plane
import List (elemIndex)
import GHC.Word
import Bitmap

{- --threading stuff
import Data.Maybe
import Control.Monad.Par

import Control.DeepSeq --(NFData) 


--import Control.Parallel.Strategies hiding (parMap)
--import Control.Seq as Seq
--import Control.Exception

instance NFData B.ByteString 

parMapChunk :: NFData b => Int -> (a -> b) -> [a] -> Par [b]
parMapChunk n f xs = fmap concat $ parMap (map f) (chunk n xs)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs where (as,bs) = splitAt n xs
-}

-- | Function to render the whole image        
renderImage
  :: Camera            -- ^ the camera of the scene
     -> [Primitive]    -- ^ list of primitives in the scene
     -> [Light]        -- ^ list of lights in the scene
     -> Int        -- ^ x resolution
     -> Int        -- ^ y resolution
     -> BL.ByteString        -- ^ the returned pixel information for the image

renderImage cam lo ll xresolution yresolution =  
    --foldl unpackBytesTest BL.empty $ map (\(x, y) -> let { (RGB r g b) = getRGB $ Camera.camTrace cam lo ll x y } in (r, g, b)) [((xres - x + 0.5)/xres, (y + 0.5)/yres) | y <- [0..yres-1], x <- [1..xres]]
    --B.concat $ map (\(x, y) -> let { (RGB r g b) = getRGB $ Camera.camTrace cam lo ll x y } in B.pack[r, g, b, 255]) [((xres - x + 0.5)/xres, (y + 0.5)/yres) | y <- [0..yres-1], x <- [1..xres]]
    let xres = fromIntegral xresolution
        yres = fromIntegral yresolution
        pixelPairs = [((x + 0.5)/xres, (yres - y + 0.5)/yres) | y <- [0..yres-1], x <- [1..xres]]
    in
        BL.fromChunks $ map (\(x, y) -> let { (RGB r g b) = getRGB $ Camera.camTrace cam lo ll x y } in Bitmap.getByteString xresolution (round x) r g b) pixelPairs
        --BL.fromChunks $ ((runPar $ parMap (\(x, y) -> let { (RGB r g b) = getRGB $ Camera.camTrace cam lo ll x y } in Bitmap.getByteString xresolution (round x) r g b) [((x + 0.5)/xres, (yres - y + 0.5)/yres) | y <- [0..yres-1], x <- [1..xres]]))
        
        --BL.fromChunks $ ((evaluate $ deep $ runEval $ parMap (\(x, y) -> let { (RGB r g b) = getRGB $ Camera.camTrace cam lo ll x y } in Bitmap.getByteString xresolution (round x) r g b) [((x + 0.5)/xres, (yres - y + 0.5)/yres) | y <- [0..yres-1], x <- [1..xres]]))
        --BL.fromChunks $ ((parMap rpar (\(x, y) -> let { (RGB r g b) = getRGB $ Camera.camTrace cam lo ll x y } in Bitmap.getByteString xresolution (round x) r g b) [((x + 0.5)/xres, (yres - y + 0.5)/yres) | y <- [0..yres-1], x <- [1..xres]]))
        
        --BL.fromChunks $ ((runPar $ parMapChunk 4 (\(x, y) -> let { (RGB r g b) = getRGB $ Camera.camTrace cam lo ll x y } in Bitmap.getByteString xresolution (round x) r g b) [((x + 0.5)/xres, (yres - y + 0.5)/yres) | y <- [0..yres-1], x <- [1..xres]]))

