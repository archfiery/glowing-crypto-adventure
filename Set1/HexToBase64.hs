-- Always operate on raw bytes, never on encoded strings.
-- Only use hex and base64 for pretty-printing.
-- I'm killing your brain like a poisonous mushroom
module HexToBase64 (hexToBase64, hexToStr) where

import           Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8  as C
import           Data.Hex               (unhex)
import           Data.Maybe

hexToBase64 :: String -> C.ByteString
hexToBase64 s = case unhex s of
                 Nothing -> C.empty
                 Just bs -> (encode . C.pack) bs

hexToStr :: String -> String
hexToStr str = fromMaybe "" (unhex str)

main = do
  putStrLn . C.unpack . hexToBase64 $ "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
  -- SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t
  putStrLn . hexToStr $ "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
