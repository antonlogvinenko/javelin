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

import Debug.Trace (trace)

getInterface = getWord

classBody :: Get ClassBody
classBody = do
  poolLength <- getWord
  pool <- getConstants poolLength
  flags <- parseClassAccessFlags
  thisClass <- getWord
  superClass <- getWord
  interfaces <- several getInterface
  fields <- several $ getField pool
  methods <- several $ getMethod pool
  attributes <- several $ getAttr pool
  return $ ClassBody pool flags thisClass superClass interfaces fields methods attributes

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
--parseByteCode = magicNumber >> ByteCode <$> version <*> version <*> classBody
parseByteCode = do
  magicNumber
  minor <- version
  major <- version
  body <- classBody
  return $ ByteCode minor major body

parse :: [Word8] -> Either (BS.ByteString, ByteOffset, String) (BS.ByteString, ByteOffset, ByteCode)
parse bytes = do
  runGetOrFail parseByteCode $ BS.pack bytes
