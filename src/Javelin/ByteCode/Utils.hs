module Javelin.ByteCode.Utils
where

import Data.ByteString.Lazy (ByteString, unpack, pack)
import Data.Word (Word32, Word16, Word8)
import qualified Data.Map.Lazy as Map (findWithDefault, fromList, Map(..), keys, lookup)
import Data.Bits
import Data.Maybe
import Javelin.ByteCode.Data
import Control.Applicative
import qualified Data.Binary.Get as G

type Parser a = [Word8] -> Either String ([Word8], a)

getCountAndList :: (Word16 -> G.Get [x]) -> G.Get [x]
getCountAndList f = G.getWord16be >>= f

addFlagIfMatches :: Word16 -> Map.Map Word16 a -> [a] -> Word16 -> [a]
addFlagIfMatches number flagsMap list mask = if (mask .&. number) == 0
                                             then list
                                             else case Map.lookup mask flagsMap of
                                               Just x -> x : list
                                               Nothing -> list
--todo remove after migration to Get !

foldMask ::Map.Map Word16 a -> Word16 -> [a]
foldMask flagsMap bytes = foldl (addFlagIfMatches bytes flagsMap) [] (Map.keys flagsMap)

getNTimes :: G.Get a -> Word16 -> G.Get [a]
getNTimes parser n =
  if n == 0
  then return []
    else do
         obj <- parser
         objs <- getNTimes parser (n - 1)
         return (obj:objs)

constrNTimes :: ([a] -> b) -> G.Get a -> G.Get b
constrNTimes f parser = do
  len <- G.getWord16be
  object <- getNTimes parser len
  return $ f object

getFromPool :: [x] -> Word16 -> Maybe x
getFromPool list idx = if okIdx < length list
                       then Just $ list !! okIdx
                       else Nothing
  where okIdx = fromIntegral idx

bytesToString bytes = ""
