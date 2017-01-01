module SingleByteXORCipher (singleByteXor) where

import           Control.Monad         (mapM_)
import           Data.Bits             (xor)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (chr, digitToInt, intToDigit)
import           Data.Hex              (hex, unhex)
import           Data.Maybe            (fromMaybe)
import           FixedXOR              (fixedXorString)
import           HexToBase64           (hexToBase64, hexToStr)

-- |print all the possible cases from key 32 to 127
xorString :: String -> Int -> String
xorString a i = hex $ fixedXorString (fromMaybe "" (unhex a)) (replicate (length a) (chr i))

singleByteXor :: String -> [String]
singleByteXor s = map (xorString s) [32..127]

main =
  mapM_ (putStrLn . hexToStr) . singleByteXor $ "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
