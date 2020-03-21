module Javelin.Lib.ByteCode.Utils where

import Data.Binary.Get (Get, getWord16be, getWord32be, getWord64be)
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import qualified Data.Map.Lazy as Map (Map, keys, lookup)
import Data.Word (Word16)
import Debug.Trace

-- getWord8 = getWord8
getWord16 = getWord16be

getWord32 = getWord32be

getWord64 = getWord64be

times :: Get a -> Word16 -> Get [a]
times _ 0 = return []
times get n = do
  x <- get
  xs <- times get (n - 1)
  return $ x : xs

several :: Get a -> Get [a]
several get = do
  len <- getWord16
  times get len

addFlagIfMatches :: Word16 -> Map.Map Word16 a -> [a] -> Word16 -> [a]
addFlagIfMatches number flagsMap list mask =
  if (mask .&. number) == 0
    then list
    else case Map.lookup mask flagsMap of
           Just x -> x : list
           Nothing -> list

foldMask :: Map.Map Word16 a -> Word16 -> [a]
foldMask flagsMap bytes =
  foldl (addFlagIfMatches bytes flagsMap) [] (Map.keys flagsMap)

getFromPool :: [x] -> Word16 -> Maybe x
getFromPool list idx =
  if okIdx <= length list
    then Just $ list !! (okIdx - 1)
    else Nothing
  where
    okIdx = fromIntegral idx

say x = traceShow x x

debug x = seq (say x) x

debug' m x = seq (say m) x

return' x = seq (say x) (return x)

debugM m = do
  v <- m
  return' v

bytesToString :: ByteString -> String
bytesToString = toString
