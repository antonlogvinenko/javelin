module Javelin.ByteCode.Header (magicNumber, parseClassAccessFlags)
where

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils
import Data.Binary.Get
import Data.ByteString.Lazy (unpack)
import Control.Applicative

import Data.Word (Word32, Word16, Word8)
import qualified Data.Map.Lazy as Map (fromList)

magicNumber :: Parser Int
magicNumber = convert $ do
  magic <- getWord32be
  if magic == 0xCAFEBABE
    then return 42
    else fail "Not a Java class format"

classFlagsList = Map.fromList [(0x0001, ClassPublic), (0x0010, ClassFinal), (0x0020, ClassSuper),
                               (0x0200, ClassInterface), (0x0400, ClassAbstract),
                               (0x1000, ClassSynthetic), (0x2000, ClassAnnotation),
                               (0x4000, ClassEnum)]

parseClassAccessFlags :: Parser [ClassAccessFlags]
parseClassAccessFlags = convert $ foldMask' classFlagsList <$> getWord16be
