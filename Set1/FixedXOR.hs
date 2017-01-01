module FixedXOR (fixedXorString) where

import           Data.Bits (xor)
import           Data.Char
import           Data.Hex  (hex)
import           HexToBase64 (hexToStr)

fixedXorString :: String -> String -> String
fixedXorString [] _          = []
fixedXorString _ []          = []
fixedXorString (x:xs) (y:ys) = xorChar x y : fixedXorString xs ys

xorChar :: Char -> Char -> Char
xorChar a b = chr (ord a `xor` ord b)

main =
  print . hex $ fixedXorString (hexToStr "1c0111001f010100061a024b53535009181c") (hexToStr "686974207468652062756c6c277320657965")
  -- "746865206b696420646f6e277420706c6179"
