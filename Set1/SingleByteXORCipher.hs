module SingleByteXORCipher (singleByteXor) where

import           Control.Monad         (mapM_)
import           Data.Bits             (xor)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt, intToDigit)
import           Foreign.C.String      (castCCharToChar, castCharToCChar)
import           HexToBase64           (hexToBase64, hexToStr)

xorString :: String -> Int -> String
xorString [] _ = []
xorString (x:xs) c = intToDigit (digitToInt x `xor` c) : xorString xs c

singleByteXor :: String -> [String]
singleByteXor s = map (xorString s) [0..15]

main =
  mapM_ (putStrLn . hexToStr) . singleByteXor $ "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
