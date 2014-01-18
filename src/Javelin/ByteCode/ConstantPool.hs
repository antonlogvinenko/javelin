module Javelin.ByteCode.ConstantPool (getConstantPool)
where

import Data.Word (Word32, Word16, Word8)
import qualified Data.Map.Lazy as Map (findWithDefault, fromList, Map(..))

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils

-- Constant pool
getConstantPool :: RepeatingParser [Constant]
getConstantPool = getNTimes getConstant

getConstant :: Parser Constant
getConstant bytes = do
  (bytes1, tag) <- getByte bytes
  getConstantParser (fromIntegral tag) bytes1

getConstantParser :: Int -> Parser Constant
getConstantParser idx = Map.findWithDefault failingConstParser idx constantTypeParser
constantTypeParser :: Map.Map Int (Parser Constant)
constantTypeParser = Map.fromList [(1, utf8InfoParser), (3, integerInfoParser), (4, floatInfoParser),
                               (5, longInfoParser), (6, doubleInfoParser), (7, classInfoParser),
                               (8, stringInfoParser), (9, fieldrefParser), (10, methodrefParser),
                               (11, interfaceMethodrefParser), (12, nameAndTypeInfoParser), (15, identityParser),
                               (16, identityParser), (18, identityParser)]
                     
identityParser bytes = Right (bytes, Utf8Info [])
failingConstParser _ = Left "Undefined constant"

twoTwoBytesInfoParser :: (Word16 -> Word16 -> Constant) -> Parser Constant
twoTwoBytesInfoParser constConstr bytes = do
  (bytes1, firstWord16) <- getWord bytes
  (bytes2, secondWord16) <- getWord bytes
  return $ (bytes2, constConstr firstWord16 secondWord16)

twoFourBytesInfoParser :: (Word32 -> Word32 -> Constant) -> Parser Constant
twoFourBytesInfoParser constConstr bytes = do
  (bytes1, bytesHigh) <- getBytes 4 bytes
  (bytes2, bytesLow) <- getBytes 4 bytes1
  return $ (bytes2, constConstr bytesHigh bytesLow)

twoBytesInfoParser :: (Word16 -> Constant) -> Parser Constant
twoBytesInfoParser constConstr bytes = do
  (bytes1, twoBytes) <- getWord bytes
  return $ (bytes1, constConstr twoBytes)

fourBytesInfoParser :: (Word32 -> Constant) -> Parser Constant
fourBytesInfoParser constConstr bytes = do
  (bytes1, value) <- getBytes 4 bytes
  return $ (bytes1, constConstr value)

fieldrefParser = twoTwoBytesInfoParser Fieldref
methodrefParser = twoTwoBytesInfoParser Methodref
interfaceMethodrefParser = twoTwoBytesInfoParser InterfaceMethodref
nameAndTypeInfoParser = twoTwoBytesInfoParser NameAndTypeInfo
classInfoParser = twoBytesInfoParser ClassInfo
stringInfoParser = twoBytesInfoParser StringInfo
integerInfoParser = fourBytesInfoParser IntegerInfo
floatInfoParser = fourBytesInfoParser FloatInfo
longInfoParser = twoFourBytesInfoParser LongInfo
doubleInfoParser = twoFourBytesInfoParser DoubleInfo

utf8InfoParser bytes = do
  (bytes1, len) <- getWord bytes
  (bytes2, lenBytes) <- takeBytes (fromIntegral len) bytes1
  return $ (bytes2, Utf8Info $ bytesToString lenBytes)
methodHandleInfoParser bytes = do
  (bytes1, kind) <- getByte bytes
  (bytes2, index) <- getWord bytes1
  return $ (bytes2, MethodHandleInfo kind index)
methodTypeInfoParser bytes = do
  (bytes1, index) <- getWord bytes
  return $ (bytes1, MethodTypeInfo index)
invokeDynamicInfoParser = twoTwoBytesInfoParser InvokeDynamicInfo
