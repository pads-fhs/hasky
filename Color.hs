-- | The module 'Color' supplies functions and data types for representation
-- and calculation with a pixels color while calculating and in the final bitmap
module Color where

import GHC.Word

-- | Data structure for a Double color.
data Color
    -- | The constructor 'Color' creates a Color data of three Double for
    -- red, green and blue in the range of 0.0 till 1.0
    = Color Double Double Double deriving (Eq, Show, Read)

-- | Data structure for a Word8 color.
data RGB
    -- | The constructor 'RGB' creates a RGB data of three Word8 for
    -- red, green and blue in the range of 0 till 255 for the final
    -- bitmap
    = RGB Word8 Word8 Word8 deriving (Eq, Show, Read)

instance Num Color where
  -- -- | Simply adds two colors and returns the resulting color
  -- (+) :: Color -> Color -> Color
  Color a1 b1 c1 + Color a2 b2 c2 = Color (a1+a2) (b1+b2) (c1+c2)
  -- -- | Simply subtracts the second color from the first and returns the resulting color
  -- (-) :: Color -> Color -> Color
  Color a1 b1 c1 - Color a2 b2 c2 = Color (a1-a2) (b1-b2) (c1-c2)
  -- -- | Simply multiplies two colors and returns the resulting color
  -- (*) :: Color -> Color -> Color
  Color a1 a2 a3 * Color b1 b2 b3 = Color (a1*b1) (a2*b2) (a3*b3)
  abs _ = 0
  signum _ = 1
  fromInteger _ = 0

-- | Converts an Integer color component (r, g or b) into a valid value (0..255)
helperRGB :: Integer -> Word8
helperRGB x =
    if x > 255 then
        255 :: Word8
    else if x < 0 then
        0 :: Word8
    else
        fromInteger x
       
-- | Converts a Color (range 0.0 .. 1.0) to a RGB (range 0 .. 255)
getRGB :: Color -> RGB
getRGB (Color a1 a2 a3) =
    let ra = helperRGB (round (a1 * 255))
        ga = helperRGB (round (a2 * 255))
        ba = helperRGB (round (a3 * 255))
    in (RGB ra ga ba)

-- | the basic color black represented as 'Color'
black :: Color
black = Color 0 0 0

-- | the basic color white represented as 'Color'
white :: Color
white = Color 1 1 1
  
--cx = Color 0.1 0.2 0.3
--cy = Color 0.4 0.5 0.6

--a = helperRGB (round (234.0 * 255))
--b = getRGB (Color 1.0 1 235)

