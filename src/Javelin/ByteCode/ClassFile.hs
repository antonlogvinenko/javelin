module Javelin.ByteCode.ClassFile (parse, magicNumber, version, classBody)
where

import Data.Word (Word32, Word16, Word8)
import Data.Map (fromList)
import Control.Applicative
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BS (unpack, pack, length, ByteString)

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils
import Javelin.ByteCode.ConstantPool
import Javelin.ByteCode.FieldMethod
import Javelin.ByteCode.Attribute

getInterface = getWord

classBody :: Get ClassBody
classBody = do
  pool <- several getConstant
  ClassBody pool
    <$> parseClassAccessFlags <*> getWord <*> getWord
    <*> several getInterface
    <*> several (getField pool)
    <*> several (getMethod pool)
    <*> several (getAttr pool)

magicNumber :: Get Int
magicNumber = do
  magic <- getDWord
  if magic == 0xCAFEBABE
    then return 42
    else fail "Not a Java class format"

classFlagsList = fromList [(0x0001, ClassPublic), (0x0010, ClassFinal), (0x0020, ClassSuper),
                           (0x0200, ClassInterface), (0x0400, ClassAbstract),
                           (0x1000, ClassSynthetic), (0x2000, ClassAnn),
                           (0x4000, ClassEnum)]

parseClassAccessFlags :: Get [ClassAccessFlags]
parseClassAccessFlags = foldMask classFlagsList <$> getWord

version :: Get Word16
version = getWord

parseByteCode :: Get ByteCode
parseByteCode = magicNumber >> ByteCode <$> version <*> version <*> classBody

parse :: [Word8] -> Either (BS.ByteString, ByteOffset, String) (BS.ByteString, ByteOffset, ByteCode)
parse bytes = do
  runGetOrFail parseByteCode $ BS.pack bytes
