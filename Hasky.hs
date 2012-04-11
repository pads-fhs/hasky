-- | This module is the Main module of Hasky and provides the main rendering
-- loop and the main function which starts the rendering process.
module Main( main ) where

import System( getArgs )
import Camera
import Color
import Data.ByteString as BSStrict (pack, ByteString)--hiding (foldl, map, reverse, take, length, readFile, putStrLn) --(pack, ByteString, concat)
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
import Render
import Data.Time


getCleanFilename "" = ""
getCleanFilename str =
    let revstr = reverse str
        x = List.elemIndex '.' revstr
    in
        case x of
            Just i -> take ((length str) - i - 1) str
            Nothing -> str
          



-- | The main function, loads a scene from file and renders this scene to <filename>.bmp in the given width and height
main :: IO()
main = do
    -- testStuff

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
        start <- getCurrentTime
        let (Scene (Camera campos camlookat _ _) llist plist) = scene
        let imagePixels = renderImage (getSimpleCamera campos camlookat width height) plist llist width height
        saveBitmap ((getCleanFilename $ head args) ++ ".bmp") width height imagePixels
        stop <- getCurrentTime
        putStr "Done. (it took "
        print $ diffUTCTime stop start
        putStrLn ")";
        return ()
    

-- used to test the main without args sometimes    
{-
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
    --putStrLn $ show m-}

    
    
    
    
-- Old test scenes
    
    
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