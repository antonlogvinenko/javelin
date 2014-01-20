module Javelin.ByteCode.Utils
where

import Data.ByteString.Lazy (ByteString, unpack, pack)
import Data.Word (Word32, Word16, Word8)
import qualified Data.Map.Lazy as Map (findWithDefault, fromList, Map(..), keys, lookup)
import Data.Bits
import Data.Maybe
import Control.Applicative
import Data.Binary.Get 

import Javelin.ByteCode.Data

getByte = getWord8
getWord = getWord16be
getDWord = getWord32be

addFlagIfMatches :: Word16 -> Map.Map Word16 a -> [a] -> Word16 -> [a]
addFlagIfMatches number flagsMap list mask = if (mask .&. number) == 0
                                             then list
                                             else case Map.lookup mask flagsMap of
                                               Just x -> x : list
                                               Nothing -> list
foldMask ::Map.Map Word16 a -> Word16 -> [a]
foldMask flagsMap bytes = foldl (addFlagIfMatches bytes flagsMap) [] (Map.keys flagsMap)

getCountAndList :: (Word16 -> Get [x]) -> Get [x]
getCountAndList f = getWord >>= f

nTimes :: Get a -> Word16 -> Get [a]
nTimes get n =
  if n == 0
  then return []
  else (:) <$> get <*> nTimes get (n - 1)
    
severalTimes :: Get a -> Get [a]
severalTimes get = do
  len <- getWord
  nTimes get len

getFromPool :: [x] -> Word16 -> Maybe x
getFromPool list idx = if okIdx < length list
                       then Just $ list !! okIdx
                       else Nothing
  where okIdx = fromIntegral idx

bytesToString bytes = ""
