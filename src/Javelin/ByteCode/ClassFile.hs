module Javelin.ByteCode.ClassFile (parse, magicNumber, version, classBody)
where

import Data.Word (Word32, Word16, Word8)
import Data.Map (fromList)

import qualified Data.ByteString.Lazy as BS (unpack, pack, length)
import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils
import Javelin.ByteCode.ConstantPool
import Javelin.ByteCode.FieldMethod
import Javelin.ByteCode.Attribute
import Control.Applicative
import Data.Binary.Get

getInterfaces :: Word16 -> Get [Word16]
getInterfaces = getNTimes getWord16be

classBody :: Get ClassBody
classBody = do
  pool <- getCountAndList getConstantPool
  ClassBody pool
    <$> parseClassAccessFlags <*> getWord16be <*> getWord16be
    <*> getCountAndList getInterfaces
    <*> getCountAndList (getNTimes $ getField pool)
    <*> getCountAndList (getNTimes $ getMethod pool)
    <*> getCountAndList (getNTimes $ getAttribute pool)

magicNumber :: Get Int
magicNumber = do
  magic <- getWord32be
  if magic == 0xCAFEBABE
    then return 42
    else fail "Not a Java class format"

classFlagsList = fromList [(0x0001, ClassPublic), (0x0010, ClassFinal), (0x0020, ClassSuper),
                           (0x0200, ClassInterface), (0x0400, ClassAbstract),
                           (0x1000, ClassSynthetic), (0x2000, ClassAnnotation),
                           (0x4000, ClassEnum)]

parseClassAccessFlags :: Get [ClassAccessFlags]
parseClassAccessFlags = foldMask classFlagsList <$> getWord16be

version :: Get Word16
version = getWord16be

parseByteCode :: Get ByteCode
parseByteCode = magicNumber >> ByteCode <$> version <*> version <*> classBody

parse :: Parser ByteCode
parse bytes = do
  case runGetOrFail parseByteCode $ BS.pack bytes of
    Left (bs, _, msg) -> Left msg
    Right (bs, _, value) -> if BS.length bs == 0
                            then Right (BS.unpack bs, value)
                            else Left "Bytes left"
