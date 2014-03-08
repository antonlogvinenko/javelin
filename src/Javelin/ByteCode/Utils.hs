module Javelin.ByteCode.Utils
where

import Data.ByteString (ByteString, unpack, pack)
import Data.Word (Word32, Word16, Word8)
import qualified Data.Map.Lazy as Map (findWithDefault, fromList, Map(..), keys, lookup)
import Data.Bits
import Data.Maybe
import Control.Applicative
import Data.Binary.Get
import Data.ByteString.UTF8 (toString)

import Javelin.ByteCode.Data
import Debug.Trace

getByte = getWord8
getWord = getWord16be
getDWord = getWord32be
getDDWord = getWord64be

times :: Get a -> Word16 -> Get [a]
times _ 0 = return []
--times get n = (:) <$> get <*> times get (n - 1)
times get n = do
  x <- get
  xs <- times get (n - 1)
  return $ x : xs
    
several :: Get a -> Get [a]
several get = do
  len <- getWord
  times get len

addFlagIfMatches :: Word16 -> Map.Map Word16 a -> [a] -> Word16 -> [a]
addFlagIfMatches number flagsMap list mask = if (mask .&. number) == 0
                                             then list
                                             else case Map.lookup mask flagsMap of
                                               Just x -> x : list
                                               Nothing -> list

foldMask :: Map.Map Word16 a -> Word16 -> [a]
foldMask flagsMap bytes = foldl (addFlagIfMatches bytes flagsMap) [] (Map.keys flagsMap)

getFromPool :: [x] -> Word16 -> Maybe x
getFromPool list idx = if okIdx <= length list
                       then Just $ list !! (okIdx - 1)
                       else Nothing
  where okIdx = fromIntegral idx

say x = traceShow x x
debug x = seq (say x) x
debug' m x = seq (say m) x
return' x = seq (say x) (return x)

bytesToString :: ByteString -> String
bytesToString = toString
