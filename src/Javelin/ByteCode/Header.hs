module Javelin.ByteCode.Header (magicNumber, version, parseClassAccessFlags)
where

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils

import Data.Word (Word32, Word16, Word8)
import qualified Data.Map.Lazy as Map (fromList)

-- Class file header functions
magicNumber :: Parser Int
magicNumber bs = if take 4 bs == [0xCA, 0xFE, 0xBA, 0xBE]
                 then Right (drop 4 bs, 42)
                 else Left "Not a Java class format"
                        
version :: Parser Word16
version = getWord

classFlagsList = Map.fromList [(0x0001, ClassPublic), (0x0010, ClassFinal), (0x0020, ClassSuper),
                               (0x0200, ClassInterface), (0x0400, ClassAbstract),
                               (0x1000, ClassSynthetic), (0x2000, ClassAnnotation),
                               (0x4000, ClassEnum)]

parseClassAccessFlags :: Parser [ClassAccessFlags]
parseClassAccessFlags bytes = do
  (bytes1, flagsBytes) <- getWord bytes
  let flags = foldMask flagsBytes classFlagsList
  return $ (bytes1, flags)       
