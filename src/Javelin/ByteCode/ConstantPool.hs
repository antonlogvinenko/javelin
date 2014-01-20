module Javelin.ByteCode.ConstantPool
where

import Data.Word (Word32, Word16, Word8)
import Data.Map.Lazy (findWithDefault, fromList, Map(..))
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Binary.Get

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils

getConstantPool :: Word16 -> Get [Constant]
getConstantPool = nTimes getConstant

getConstant :: Get Constant
getConstant = do
  tag <- getByte
  getConstantParser tag

getConstantParser :: Word8 -> Get Constant
getConstantParser idx = findWithDefault failingConstParser idx constantTypeParser

failingConstParser :: Get Constant
failingConstParser = fail "Undefined constant"

constantTypeParser :: Map Word8 (Get Constant)
constantTypeParser = fromList [(1, utf8InfoParser), (3, integerInfoParser),
                               (4, floatInfoParser), (5, longInfoParser),
                               (6, doubleInfoParser), (7, classInfoParser),
                               (8, stringInfoParser), (9, fieldrefParser),
                               (10, methodrefParser), (11, interfaceMethodrefParser),
                               (12, nameAndTypeInfoParser), (15, identityParser),
                               (16, identityParser), (18, identityParser)]

utf8InfoParser :: Get Constant
utf8InfoParser = do
  byteStringLen <- getWord
  byteString <- getByteString $ fromIntegral byteStringLen
  let string = bytesToString2 byteString
  return $ Utf8Info string

identityParser :: Get Constant
identityParser = return $ Utf8Info ""

bytesToString2 :: ByteString -> String
bytesToString2 = undefined

twoTwoBytesInfoParser :: (Word16 -> Word16 -> Constant) -> Get Constant
twoTwoBytesInfoParser constConstr = constConstr <$> getWord <*> getWord

twoFourBytesInfoParser :: (Word32 -> Word32 -> Constant) -> Get Constant
twoFourBytesInfoParser constConstr = constConstr <$> getDWord <*> getDWord

twoBytesInfoParser :: (Word16 -> Constant) -> Get Constant
twoBytesInfoParser constConstr = constConstr <$> getWord

fourBytesInfoParser :: (Word32 -> Constant) -> Get Constant
fourBytesInfoParser constConstr = constConstr <$> getDWord

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

methodHandleInfoParser = MethodHandleInfo <$> getByte <*> getWord
methodTypeInfoParser = MethodTypeInfo <$> getWord
invokeDynamicInfoParser = twoTwoBytesInfoParser InvokeDynamicInfo
