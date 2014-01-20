module Javelin.ByteCode.ConstantPool
where

import Data.Word (Word32, Word16, Word8)
import qualified Data.Map.Lazy as Map (findWithDefault, fromList, Map(..))

import Control.Applicative
import Data.ByteString (ByteString)
import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils
import qualified Data.Binary.Get as G

getConstantPool :: Word16 -> G.Get [Constant]
getConstantPool = getNTimes getConstant

getConstant :: G.Get Constant
getConstant = do
  tag <- G.getWord8
  getConstantParser tag

getConstantParser :: Word8 -> G.Get Constant
getConstantParser idx = Map.findWithDefault failingConstParser idx constantTypeParser

failingConstParser :: G.Get Constant
failingConstParser = fail "Undefined constant"

constantTypeParser :: Map.Map Word8 (G.Get Constant)
constantTypeParser = Map.fromList
                      [(1, utf8InfoParser), (3, integerInfoParser),
                       (4, floatInfoParser), (5, longInfoParser),
                       (6, doubleInfoParser), (7, classInfoParser),
                       (8, stringInfoParser), (9, fieldrefParser),
                       (10, methodrefParser), (11, interfaceMethodrefParser),
                       (12, nameAndTypeInfoParser), (15, identityParser),
                       (16, identityParser), (18, identityParser)]

utf8InfoParser :: G.Get Constant
utf8InfoParser = do
  byteStringLen <- G.getWord16be
  byteString <- G.getByteString $ fromIntegral byteStringLen
  let string = bytesToString2 byteString
  return $ Utf8Info string

identityParser :: G.Get Constant
identityParser = return $ Utf8Info ""

bytesToString2 :: ByteString -> String
bytesToString2 = undefined

twoTwoBytesInfoParser :: (Word16 -> Word16 -> Constant) -> G.Get Constant
twoTwoBytesInfoParser constConstr = constConstr <$> G.getWord16be <*> G.getWord16be

twoFourBytesInfoParser :: (Word32 -> Word32 -> Constant) -> G.Get Constant
twoFourBytesInfoParser constConstr = constConstr <$> G.getWord32be <*> G.getWord32be

twoBytesInfoParser :: (Word16 -> Constant) -> G.Get Constant
twoBytesInfoParser constConstr = constConstr <$> G.getWord16be

fourBytesInfoParser :: (Word32 -> Constant) -> G.Get Constant
fourBytesInfoParser constConstr = constConstr <$> G.getWord32be

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

methodHandleInfoParser = MethodHandleInfo <$> G.getWord8 <*> G.getWord16be
methodTypeInfoParser = MethodTypeInfo <$> G.getWord16be
invokeDynamicInfoParser = twoTwoBytesInfoParser InvokeDynamicInfo
