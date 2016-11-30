module Javelin.ByteCode.ClassFile (parse, magicNumber, version, classBody)
where

import Data.Map (fromList, Map)
import Control.Applicative
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BS (pack, ByteString)
import Data.Word

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils
import Javelin.ByteCode.ConstantPool
import Javelin.ByteCode.FieldMethod
import Javelin.ByteCode.Attribute

getInterface :: Get Word16
getInterface = getWord16

classBody :: Get ClassBody
classBody = do
  poolLength <- getWord16
  pool <- getConstants poolLength
  flags <- parseClassAccessFlags
  thisClass <- getWord16
  superClass <- getWord16
  interfaces <- several getInterface
  fields <- several $ getField pool
  methods <- several $ getMethod pool
  attributes <- several $ getAttr pool
  return $ ClassBody (ConstantPool pool) flags thisClass superClass interfaces fields methods attributes

magicNumber :: Get Int
magicNumber = do
  magic <- getWord32
  if magic == 0xCAFEBABE
    then return 42
    else fail "Not a Java class format"

classFlagsList :: Map Word16 ClassAccessFlags
classFlagsList = fromList [(0x0001, AccPublic), (0x0010, AccFinal), (0x0020, AccSuper),
                           (0x0200, AccInterface), (0x0400, AccAbstract),
                           (0x1000, AccSynthetic), (0x2000, AccAnn),
                           (0x4000, AccEnum)]

parseClassAccessFlags :: Get [ClassAccessFlags]
parseClassAccessFlags = foldMask classFlagsList <$> getWord16

version :: Get Word16
version = getWord16

parseByteCode :: Get ByteCode
parseByteCode = do
  magicNumber
  minor <- version
  major <- version
  body <- classBody
  return $ ByteCode minor major body

parse :: [Word8] -> Either (BS.ByteString, ByteOffset, String) (BS.ByteString, ByteOffset, ByteCode)
parse bytes = runGetOrFail parseByteCode $ BS.pack bytes

