-- | The module 'Bitmap' provides functions to save a lazy bytestring 
-- to a bitmap image.
module Bitmap (
    saveBitmap,
    getByteString
    ) where

import Data.ByteString.Lazy as BL
import Data.ByteString as B
import Data.Binary.Put
import Data.Bits
import Data.Int
import Data.Word
import System.IO

calcImageSize :: Int -> Int -> Int
calcImageSize width height = 
    if (width * 3) .&. 3 /= 0 then (width * 3 + 4 - ((width * 3) .&. 3)) * height
    else width * 3 * height
    
getWord32FromInt :: Int -> Word32
getWord32FromInt x = fromIntegral ((fromIntegral x) :: Int32)

serialiseHeader :: Int -> Int -> Put
serialiseHeader width height = do
  -- the bitmap file header (14 byte)
  putWord16le 19778 -- "BM"
  putWord32le $ fromIntegral (14 + 40 + (calcImageSize width height)) -- whole size, headers + image data
  putWord16le 0 -- reserved 1
  putWord16le 0 -- reserved 2
  putWord32le (14 + 40) -- off bits, full header size
  --putWord8 3
  -- the bitmap info header (40 byte)
  putWord32le 40 -- size of bitmap info header
  putWord32le $ getWord32FromInt width -- image width
  putWord32le $ getWord32FromInt height -- image height
  putWord16le 1 -- planes
  putWord16le 24 -- bit count
  putWord32le 0 -- compression
  putWord32le $ fromIntegral $ calcImageSize width height -- image size
  putWord32le $ getWord32FromInt 2834 -- XPelsPerMeter
  putWord32le $ getWord32FromInt 2834 -- YPelsPerMeter
  putWord32le 0 -- ClrUsed
  putWord32le 0 -- ClrImportant
  
  
--convertRGBtoBMP :: BL.ByteString -> Int -> Int -> BL.ByteString
--convertRGBtoBMP oldBytes width height =

fillMissingBytes 0 = []
fillMissingBytes i = 0:fillMissingBytes (i - 1)

getByteString width x r g b =
    let i = (width * 3) .&. 3
    in
      B.pack $ if x == width && (i /= 0)
                 then [b, g, r] ++ fillMissingBytes i
               else [b, g, r]
    
  
saveBitmap :: String -> Int -> Int -> BL.ByteString -> IO()
saveBitmap filename width height bytes = do
    h <- openFile filename WriteMode
    BL.hPut h $ runPut $ serialiseHeader width height
    BL.hPut h bytes
    hClose h