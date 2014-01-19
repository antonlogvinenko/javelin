module Javelin.ByteCode.Utils
where

import Data.ByteString.Lazy (ByteString, unpack, pack)
import Data.Word (Word32, Word16, Word8)
import Control.Monad
import qualified Data.Map.Lazy as Map (findWithDefault, fromList, Map(..), keys, lookup)
import Data.Bits
import Data.Maybe
import Data.List (lookup)
import Javelin.ByteCode.Data

import Control.Applicative
import qualified Data.Binary.Get as G

type Parser a = [Word8] -> Either String ([Word8], a)
type RepeatingParser a = Word16 -> Parser a

convert :: G.Get a -> Parser a
convert get bytes = do
  case G.runGetOrFail get $ pack bytes of
    Left (bs, _, msg) -> Left msg
    Right (bs, _, value) -> Right (unpack bs, value)

getCountAndList :: (Word16 -> Parser [x]) -> Parser [x]
getCountAndList f bytes = do
  (bytes1, count) <- getWord bytes
  f count bytes1

require :: Int -> [Word8] ->  a -> Either String a
require len bs value = if length bs < len
                       then Left "Unexpected EOF"
                       else Right value

takeBytes count bs = require count bs $ (drop count bs, take count bs)

getByte :: Parser Word8
getByte = convert G.getWord8
getWord :: Parser Word16
getWord = convert G.getWord16be
getDWord :: Parser Word32
getDWord = convert G.getWord32be
                     

addFlagIfMatches :: Word16 -> Map.Map Word16 a -> [a] -> Word16 -> [a]
addFlagIfMatches number flagsMap list mask = if (mask .&. number) == 0
                                             then list
                                             else case Map.lookup mask flagsMap of
                                               Just x -> x : list
                                               Nothing -> list
--todo remove after migration to Get !
foldMask :: Word16 -> Map.Map Word16 a -> [a]
foldMask bytes flagsMap = foldl (addFlagIfMatches bytes flagsMap) [] (Map.keys flagsMap)

foldMask' ::Map.Map Word16 a -> Word16 -> [a]
foldMask' flagsMap bytes = foldl (addFlagIfMatches bytes flagsMap) [] (Map.keys flagsMap)

getNTimes :: Parser a -> RepeatingParser [a]
getNTimes parser n bytes = do
  if n == 1
    then Right (bytes, [])
    else do
    (bytes1, obj) <- parser bytes
    (bytes2, objs) <- getNTimes parser (n - 1) bytes1
    return (bytes2, obj : objs)

-- lineNumberTableAttribute - add parsing length
constrNTimes :: ([a] -> b) -> Parser a -> Parser b
constrNTimes f parser bytes = do
  (bytes1, len) <- getWord bytes
  (bytes2, object) <- getNTimes parser len bytes1
  return (bytes2, f object)

getFromPool :: [Constant] -> Word16 -> Maybe Constant
getFromPool list idx = if okIdx < length list
                       then Just $ list !! okIdx
                       else Nothing
  where okIdx = fromIntegral idx

bytesToString bytes = ""
