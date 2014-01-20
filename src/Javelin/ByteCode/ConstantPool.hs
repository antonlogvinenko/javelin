module Javelin.ByteCode.ConstantPool
where

import Data.Word (Word32, Word16, Word8)
import Data.Map.Lazy (findWithDefault, fromList, Map(..))

import Control.Applicative
import Data.ByteString (ByteString)
import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils
import Data.Binary.Get

getConstantPool :: Word16 -> Get [Constant]
getConstantPool = getNTimes getConstant

getConstant :: Get Constant
getConstant = do
  tag <- getWord8
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
  byteStringLen <- getWord16be
  byteString <- getByteString $ fromIntegral byteStringLen
  let string = bytesToString2 byteString
  return $ Utf8Info string

identityParser :: Get Constant
identityParser = return $ Utf8Info ""

bytesToString2 :: ByteString -> String
bytesToString2 = undefined

twoTwoBytesInfoParser :: (Word16 -> Word16 -> Constant) -> Get Constant
twoTwoBytesInfoParser constConstr = constConstr <$> getWord16be <*> getWord16be

twoFourBytesInfoParser :: (Word32 -> Word32 -> Constant) -> Get Constant
twoFourBytesInfoParser constConstr = constConstr <$> getWord32be <*> getWord32be

twoBytesInfoParser :: (Word16 -> Constant) -> Get Constant
twoBytesInfoParser constConstr = constConstr <$> getWord16be

fourBytesInfoParser :: (Word32 -> Constant) -> Get Constant
fourBytesInfoParser constConstr = constConstr <$> getWord32be

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

methodHandleInfoParser = MethodHandleInfo <$> getWord8 <*> getWord16be
methodTypeInfoParser = MethodTypeInfo <$> getWord16be
invokeDynamicInfoParser = twoTwoBytesInfoParser InvokeDynamicInfo
