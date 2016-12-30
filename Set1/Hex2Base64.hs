-- Always operate on raw bytes, never on encoded strings.
-- Only use hex and base64 for pretty-printing.
-- I'm killing your brain like a poisonous mushroom
module Set1.Hex2Base64 (hex2Base64) where

import           Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8  as B
import           Data.Hex               (unhex)

hex2Base64 :: String -> B.ByteString
hex2Base64 s = case unhex s of
                 Nothing -> B.empty
                 Just bs -> (encode . B.pack) bs

hex2Str :: String -> String
hex2Str str = case unhex str of
                Nothing -> ""
                Just s  -> s

main = do
  putStrLn . B.unpack . hex2Base64 $ "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
