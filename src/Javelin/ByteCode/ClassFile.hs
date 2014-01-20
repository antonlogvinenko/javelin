module Javelin.ByteCode.ClassFile (parse, magicNumber, version, classBody)
where

import Data.Word (Word32, Word16, Word8)
import Data.Map (fromList)
import Control.Applicative
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BS (unpack, pack, length)

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils
import Javelin.ByteCode.ConstantPool
import Javelin.ByteCode.FieldMethod
import Javelin.ByteCode.Attribute

getInterfaces :: Word16 -> Get [Word16]
getInterfaces = nTimes getWord

classBody :: Get ClassBody
classBody = do
  pool <- getCountAndList getConstantPool
  ClassBody pool
    <$> parseClassAccessFlags <*> getWord <*> getWord
    <*> getCountAndList getInterfaces
    <*> getCountAndList (nTimes $ getField pool)
    <*> getCountAndList (nTimes $ getMethod pool)
    <*> getCountAndList (nTimes $ getAttribute pool)

magicNumber :: Get Int
magicNumber = do
  magic <- getDWord
  if magic == 0xCAFEBABE
    then return 42
    else fail "Not a Java class format"

classFlagsList = fromList [(0x0001, ClassPublic), (0x0010, ClassFinal), (0x0020, ClassSuper),
                           (0x0200, ClassInterface), (0x0400, ClassAbstract),
                           (0x1000, ClassSynthetic), (0x2000, ClassAnnotation),
                           (0x4000, ClassEnum)]

parseClassAccessFlags :: Get [ClassAccessFlags]
parseClassAccessFlags = foldMask classFlagsList <$> getWord

version :: Get Word16
version = getWord

parseByteCode :: Get ByteCode
parseByteCode = magicNumber >> ByteCode <$> version <*> version <*> classBody

parse :: [Word8] -> Either String ByteCode
parse bytes = do
  case runGetOrFail parseByteCode $ BS.pack bytes of
    Left (bs, _, msg) -> Left msg
    Right (bs, _, value) -> if BS.length bs == 0
                            then Right value
                            else Left "Bytes left"
