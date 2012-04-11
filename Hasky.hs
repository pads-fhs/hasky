-- | This module is the Main module of Hasky and provides the main rendering
-- loop and the main function which starts the rendering process.
module Main( main ) where

import System( getArgs )
import Camera
import Color
--import Codec.BMP
import Data.ByteString as BSStrict hiding (foldl, map, reverse, take, length, readFile, putStrLn) --(pack, ByteString, concat)
import Data.ByteString.Lazy  as BSLazy (pack, cons', empty, fromChunks, writeFile, ByteString)
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
import Data.Time

import Data.Maybe
import Control.Monad.Par

import Control.DeepSeq (NFData) 

instance NFData BSStrict.ByteString 


-- | Main render loop, returns a List of Word8 representing the r,g,b pixels of the final image
forY
  :: Camera            -- ^ the camera of the scene
     -> [Primitive] -- ^ list of primitives in the scene
     -> [Light]        -- ^ list of lights in the scene
     -> Double        -- ^ x resolution
     -> Double        -- ^ y resolution
     -> Double        -- ^ actual y index
     -> [Word8]        -- ^ the returned pixel information for the whole image
forY cam lo ll xres yres yact =
    if yact >= yres || yact < 0 then
        []
    else
        do
            let line = forX cam lo ll xres yres 0 yact
            ((forY cam lo ll xres yres (yact + 1)) ++ line)

-- | Function to render one line of the image, used in 'forY'            
forX
  :: Camera            -- ^ the camera of the scene
     -> [Primitive]    -- ^ list of primitives in the scene
     -> [Light]        -- ^ list of lights in the scene
     -> Double        -- ^ x resolution
     -> Double        -- ^ y resolution
     -> Double        -- ^ actual x index
     -> Double        -- ^ actual y index
     -> [Word8]        -- ^ the returned pixel information for one line
forX cam lo ll xres yres xact yact =
    if xact >= xres || xact < 0 then
        []
    else
        do
            -- putStr ((show (round xact)) ++ ":" ++ (show (round yact)) ++ "  ")
            let c = Camera.camTrace cam lo ll ((xact + 0.5) / xres) ((yact + 0.5) / yres)
            let (RGB r g b) = getRGB c
            (r:g:b:255:(forX cam lo ll xres yres (xact + 1) yact))

-- | Function to render the whole image        
renderImage
  :: Camera            -- ^ the camera of the scene
     -> [Primitive]    -- ^ list of primitives in the scene
     -> [Light]        -- ^ list of lights in the scene
     -> Double        -- ^ x resolution
     -> Double        -- ^ y resolution
     -> [Word8]        -- ^ the returned pixel information for the image
renderImage cam lo ll xres yres =  
    --foldl unpackBytes [] $ map (\(x, y) -> let { c = Camera.camTrace cam lo ll x y ; (RGB r g b) = getRGB c } in (r, g, b)) [((x + 0.5)/xres, (yres - y + 0.5)/yres) | y <- [1..yres], x <- [0..xres-1]]
    foldl unpackBytes [] $ map (\(x, y) -> let { (RGB r g b) = getRGB $ Camera.camTrace cam lo ll x y } in (r, g, b)) [((xres - x + 0.5)/xres, (y + 0.5)/yres) | y <- [0..yres-1], x <- [1..xres]]

-- | Function to render the whole image        
renderImageTest
  :: Camera            -- ^ the camera of the scene
     -> [Primitive]    -- ^ list of primitives in the scene
     -> [Light]        -- ^ list of lights in the scene
     -> Int        -- ^ x resolution
     -> Int        -- ^ y resolution
     -> BSLazy.ByteString        -- ^ the returned pixel information for the image

renderImageTest cam lo ll xresolution yresolution =  
    --foldl unpackBytesTest BSLazy.empty $ map (\(x, y) -> let { (RGB r g b) = getRGB $ Camera.camTrace cam lo ll x y } in (r, g, b)) [((xres - x + 0.5)/xres, (y + 0.5)/yres) | y <- [0..yres-1], x <- [1..xres]]
    --BSStrict.concat $ map (\(x, y) -> let { (RGB r g b) = getRGB $ Camera.camTrace cam lo ll x y } in BSStrict.pack[r, g, b, 255]) [((xres - x + 0.5)/xres, (y + 0.5)/yres) | y <- [0..yres-1], x <- [1..xres]]
    let xres = fromIntegral xresolution
        yres = fromIntegral yresolution
    in
        --BSLazy.fromChunks $ map (\(x, y) -> let { (RGB r g b) = getRGB $ Camera.camTrace cam lo ll x y } in Bitmap.getByteString xresolution (round x) r g b) [((x + 0.5)/xres, (yres - y + 0.5)/yres) | y <- [0..yres-1], x <- [1..xres]]
        BSLazy.fromChunks $ ((runPar $ parMap (\(x, y) -> let { (RGB r g b) = getRGB $ Camera.camTrace cam lo ll x y } in Bitmap.getByteString xresolution (round x) r g b) [((x + 0.5)/xres, (yres - y + 0.5)/yres) | y <- [0..yres-1], x <- [1..xres]]))
    
    

--unpackBytesTest :: ByteString -> (Word8, Word8, Word8) -> ByteString
--unpackBytesTest l (r, g, b) = BSLazy.cons' r $ BSLazy.cons' g $ BSLazy.cons' b $ BSLazy.cons' 255 l

unpackBytes :: [Word8] -> (Word8, Word8, Word8) -> [Word8]
unpackBytes l (r, g, b) = r:g:b:255:l


-- calcPixel
  -- :: Camera            -- ^ the camera of the scene
     -- -> [Primitive]    -- ^ list of primitives in the scene
     -- -> [Light]        -- ^ list of lights in the scene
     -- -> Double        -- ^ x resolution
     -- -> Double        -- ^ y resolution
     -- -> Double        -- ^ actual x index
     -- -> Double        -- ^ actual y index
     -- -> (Word8, Word8, Word8)       -- ^ the returned pixel information for one line

 -- :: (Double, Double) -> 
-- calcPixel cam lo ll x y = 
    -- let c = Camera.camTrace cam lo ll x y
    -- let (RGB r g b) = getRGB c
    -- (r, g, b)

getCleanFilename "" = ""
getCleanFilename str =
    let revstr = reverse str
        x = List.elemIndex '.' revstr
    in
        case x of
            Just i -> take ((length str) - i - 1) str
            Nothing -> str
          
{-          
scenetest = 
    (Scene 
        (Camera (Vector3 0 1 (-4)) (Vector.o) (Vector3 (4/3) 0 0) (Vector3 0 (-1) 0))
        [   Light [Option "position" (VV (Vector3 5 5 (-10))), Option "color" (VC (Color 1 1 1))]
        ]
        [   Primitive TSphere [Option "position" (VV (Vector3 0 0 0)), Option "color" (VC (Color 1 0 0)), Option "radius" (VD 1.0), Option "reflection" (VD 0.5)],
            Primitive TPlane  [Option "normal" (VV (Vector3 0 1 0)), Option "d" (VD (-2)),  Option "color" (VC (Color 1 1 1)), Option "checkercolor" (VC (Color 0 0 0))]
        ]
    )
    
scenetest2 = 
    (Scene 
        (Camera (Vector3 0 0 (-4)) Vector.o Vector.o Vector.o)
        [   Light [Option "position" (VV (Vector3 (-15) ( 15) ( -5))), Option "color" (VC (Color 1 1 1))],
            Light [Option "position" (VV (Vector3 ( 15) ( 15) ( -5))), Option "color" (VC (Color 1 1 1))],
            Light [Option "position" (VV (Vector3 (-15) (-15) ( -5))), Option "color" (VC (Color 1 1 1))],
            Light [Option "position" (VV (Vector3 ( 15) (-15) ( -5))), Option "color" (VC (Color 1 1 1))]
        ]
        [   Primitive TSphere [Option "position" (VV (Vector3 0 (-1.6) 0)), Option "color" (VC (Color 1 1 1)), Option "radius" (VD 1.6), Option "reflection" (VD 0.5)],
            Primitive TSphere [Option "position" (VV (Vector3 (-1.3) 0.5 0.8)), Option "color" (VC (Color 0 1 0)), Option "radius" (VD 1.2), Option "reflection" (VD 0.5)],
            Primitive TSphere [Option "position" (VV (Vector3 1.2 0 (-0.8))), Option "color" (VC (Color 0 0 1)), Option "radius" (VD 0.6), Option "reflection" (VD 0.5)],
            Primitive TSphere [Option "position" (VV (Vector3 (-0.4) (-0.2) (-1.2))), Option "color" (VC (Color 1 0 0)), Option "radius" (VD 0.4), Option "reflection" (VD 0.5)]
        ]
    )
    
scenetest3 = 
    (Scene 
        (Camera (Vector3 0 1 (-5)) Vector.o Vector.o Vector.o)
        [   Light [Option "position" (VV (Vector3 (  5) (  5) (-10))), Option "color" (VC (Color 1 1 1))],
            Light [Option "position" (VV (Vector3 (-15) (2.5) ( 30))), Option "color" (VC (Color 0 0.5 0))],
            Light [Option "position" (VV (Vector3 ( 15) (2.5) ( 30))), Option "color" (VC (Color 0 0 0.5))]
        ]
        [   Primitive TSphere [Option "position" (VV (Vector3 (-0.75) 0 0)), Option "color" (VC (Color 0 0 1)), Option "radius" (VD 1), Option "reflection" (VD 0.5)],
            Primitive TSphere [Option "position" (VV (Vector3 0.25 0 0)), Option "color" (VC (Color 1 0 0)), Option "radius" (VD 1), Option "reflection" (VD 0.5)],
            Primitive TSphere [Option "position" (VV (Vector3 (-0.25) (-0.75) 0)), Option "color" (VC (Color 1 1 0)), Option "radius" (VD 1), Option "reflection" (VD 0.5)],
            Primitive TSphere [Option "position" (VV (Vector3 (-2.5) (0.2) 1)), Option "color" (VC (Color 0.2 0.13 0.37)), Option "radius" (VD 0.25), Option "reflection" (VD 0.15)],
            Primitive TSphere [Option "position" (VV (Vector3 (2.5) (-2) (-1))), Option "color" (VC (Color 0 1 1)), Option "radius" (VD 1), Option "reflection" (VD 0.45)],
            Primitive TPlane  [Option "normal" (VV (Vector3 0 1 0)), Option "d" (VD (-2)),  Option "color" (VC (Color 0 0 0)), Option "checkercolor" (VC (Color 1 1 1)), Option "reflection" (VD 0.2)]
        ]
    )
xre = 80
yre = 60
-}

testStuff :: IO()
testStuff = do
    sceneFile <- readFile "scenes\\scene3.scene"
    let scene = read sceneFile :: Scene
    let width = 800 :: Int
    let height = 600 :: Int
    let xres = fromIntegral width
    let yres = fromIntegral height
    start <- getCurrentTime
    let (Scene (Camera campos camlookat _ _) llist plist) = scene
    putStrLn "beginne"
    let m = renderImageTest (getSimpleCamera campos camlookat xres yres) plist llist width height
    --writeFile "tmp2.txt" (show m)
    --BSLazy.writeFile "tmp3.txt" m
    saveBitmap "taa.bmp" width height m
    --let bmp    = packRGBA32ToBMP width height m
    --writeBMP ("tmp.bmp") bmp
    stop <- getCurrentTime
    print $ diffUTCTime stop start
    putStrLn "fertig"
    --x <- readLn :: IO(String)
    return ()
    --putStrLn $ show m



-- | The main function, loads a scene from file and renders this scene to <filename>.bmp in the given width and height
main :: IO()
main = do
    testStuff
{-
    args <- getArgs
    --print $ show args
    if (length args) < 3
      then
        putStrLn "Usage:\n\thasky [scene file] [width] [height]\n"
      else do
        sceneFile <- readFile $ head args
        let scene = read sceneFile :: Scene
        let width = read $ head $ tail args :: Int
        let height = read $ head $ tail $ tail args :: Int
        let xres = fromIntegral width
        let yres = fromIntegral height
        let (Scene (Camera campos camlookat _ _) llist plist) = scene
        let rgba   = Data.ByteString.pack $ renderImage (getSimpleCamera campos camlookat xres yres) plist llist xres yres
        let bmp    = packRGBA32ToBMP width height rgba
        writeBMP ((getCleanFilename $ head args) ++ ".bmp") bmp
        return ()
    
    -}
    -- Right bmp  <- readBMP "tmp.bmp"
    -- let rgba   =  unpackBMPToRGBA32 bmp
    -- let (width, height) = bmpDimensions bmp
    -- Prelude.putStrLn (show rgba)
    -- let lst = unpack rgba
    -- return lst
    
    
    
    -- let (Scene (Camera campos camlookat _ _) llist plist) = scenetest

    -- let rgba   = Data.ByteString.pack (forY (getSimpleCamera campos camlookat xre yre) plist llist xre yre 0)
    
    -- let bmp    = packRGBA32ToBMP (round xre) (round yre) rgba
    -- writeBMP "test.bmp" bmp
    -- return ()
