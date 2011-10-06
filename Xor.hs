-- | This module provides a simple XOR function for two Bools.
module Xor where

-- | The binary XOR function for two Bools.
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False