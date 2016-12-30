module Set1.FixedXOR (xorString) where

import           Data.Bits (xor)
import           Data.Char

xorString :: String -> String -> String
xorString [] []         = []
xorString (x:xs) (y:ys) = xorChar x y : xorString xs ys

xorChar :: Char -> Char -> Char
xorChar a b = intToDigit (digitToInt a `xor` digitToInt b)

main = print $ xorString "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965" == "746865206b696420646f6e277420706c6179"
