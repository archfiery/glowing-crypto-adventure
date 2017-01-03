module SingleByteXORCipher (singleByteXor) where

import           Control.Monad         (mapM_)
import           Data.Bits             (xor)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (chr, digitToInt, intToDigit, toLower)
import           Data.Hex              (hex, unhex)
import           Data.List             (sortBy)
import qualified Data.Map.Lazy         as M
import           Data.Maybe            (fromMaybe)
import           Data.Ord              (comparing)
import qualified Data.Set              as S
import           FixedXOR              (fixedXorString)
import           HexToBase64           (hexToBase64, hexToStr)

-- |print all the possible cases from key 32 to 127
xorString :: String -> Int -> String
xorString a i = hex $ fixedXorString (hexToStr a) (replicate len char) where
  len = length a
  char = chr i

singleByteXor :: String -> String
singleByteXor = fst . head . stringScore

singleByteXor' :: String -> [String]
singleByteXor' s = map (xorString s) [32..127]

stringScore :: String -> [(String, Int)]
stringScore s = let candidate = map hexToStr (singleByteXor' s) in
                    sortBy (flip (comparing snd)) $ zip candidate (map score candidate)

enLetterDescByFreq :: String
enLetterDescByFreq = "etaoinshrdlcumwfgypbvkjxqz"

-- https://en.wikipedia.org/wiki/Letter_frequency#Relative_frequencies_of_letters_in_the_English_language
enLetterFreq :: M.Map Char Double
enLetterFreq = M.fromList $ zip enLetterDescByFreq v where
  v = [ 0.12702, 0.9056, 0.8167, 0.7507, 0.6966, 0.6749, 0.6327, 0.6094, 0.5987, 0.4253, 0.4025, 0.2782, 0.2758,
        0.2406, 0.2360, 0.2228, 0.2015, 0.1974, 0.1929, 0.1492, 0.0978, 0.0772, 0.0153, 0.0150, 0.0095, 0.0074 ]

makeFreqMap :: String -> M.Map Char Double
makeFreqMap str = M.fromList $ map (\(a, b) -> (a, db b / len)) (map'' ['a'..'z'] (toLower <$> str)) where
  db x             = fromIntegral x :: Double
  len              = db $ length str
  map'' [] _       = []
  map'' (x:xs) str = (x, (length . filter (==x)) str) : map'' xs str

score :: String -> Int
score s = (compareScore . reverse . fst . unzip) (sortBy (comparing snd) ((M.toList . makeFreqMap) s)) where
  compareScore a = S.size (S.intersection ((S.fromList . begin) enLetterDescByFreq) ((S.fromList . begin) a))
                 + S.size (S.intersection ((S.fromList . end)   enLetterDescByFreq) ((S.fromList . end)   a)) where
                   begin = fst . splitAt 6
                   end   = snd . splitAt 20

printFreq :: String -> IO()
printFreq = printFreq' . M.toList . makeFreqMap where
  printFreq' [] = putStr ""
  printFreq' ((a, b):xs) = (putStr . show) a >> putStr ": " >> print b >> printFreq' xs

freqMap :: M.Map Char Double -> Double -> M.Map Char Double
freqMap m s = (/ (fromIntegral . length) m) <$> m

printTuple :: (String, Int) -> IO()
printTuple (a, b) = putStr a >> putStr ": " >> print b

main =
  putStrLn $ singleByteXor "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
