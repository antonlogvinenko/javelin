module Javelin.Lib.ByteCode.Utils where

import qualified Data.Binary.Get as Get
import qualified Data.Bits as Bits
import qualified Data.Map.Lazy as Map
import qualified Data.Word as Word
import qualified Debug.Trace as Trace

times :: Get.Get a -> Word.Word16 -> Get.Get [a]
times _ 0 = return []
times get n = do
  x <- get
  xs <- times get (n - 1)
  return $ x : xs

several :: Get.Get a -> Get.Get [a]
several get = do
  len <- Get.getWord16be
  times get len

addFlagIfMatches :: Word.Word16 -> Map.Map Word.Word16 a -> [a] -> Word.Word16 -> [a]
addFlagIfMatches number flagsMap list mask =
  if (mask Bits..&. number) == 0
    then list
    else case Map.lookup mask flagsMap of
           Just x -> x : list
           Nothing -> list

foldMask :: Map.Map Word.Word16 a -> Word.Word16 -> [a]
foldMask flagsMap bytes =
  foldl (addFlagIfMatches bytes flagsMap) [] (Map.keys flagsMap)

getFromPool :: [x] -> Word.Word16 -> Maybe x
getFromPool list idx =
  if okIdx <= length list
    then Just $ list !! (okIdx - 1)
    else Nothing
  where
    okIdx = fromIntegral idx

say x = Trace.traceShow x x

debug x = seq (say x) x

debug' m x = seq (say m) x

return' x = seq (say x) (return x)

debugM m = do
  v <- m
  return' v