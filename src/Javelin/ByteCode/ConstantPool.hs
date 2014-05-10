module Javelin.ByteCode.ConstantPool (getConstants)
where

import Data.Word (Word32, Word16, Word8)
import Data.Map.Lazy (findWithDefault, fromList, Map)
import Control.Applicative
import Data.Binary.Get
import Unsafe.Coerce

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils


getConstants 1 = return []
getConstants len = do
  constant <- getConstant
  let double = ([constant, constant] ++) <$> (getConstants $ len - 2)
      single = (constant :) <$> (getConstants $ len - 1)
  case constant of
    LongInfo _ -> double
    DoubleInfo _ -> double
    otherwise -> single

getConstant :: Get Constant
getConstant = do
  tag <- getByte
  findWithDefault (failingConstParser tag) tag constantTypeParser

failingConstParser :: Word8 -> Get Constant
failingConstParser x = fail $ "Undefined constant with index " ++ (show x) ++ "\n"

constantTypeParser :: Map Word8 (Get Constant)
constantTypeParser = fromList [(1, utf8InfoParser), (3, integerInfoParser),
                               (4, floatInfoParser), (5, longInfoParser),
                               (6, doubleInfoParser), (7, classInfoParser),
                               (8, stringInfoParser), (9, fieldrefParser),
                               (10, methodrefParser), (11, interfaceMethodrefParser),
                               (12, nameAndTypeInfoParser), (15, methodHandleInfoParser),
                               (16, methodTypeInfoParser), (18, invokeDynamicInfoParser)]

utf8InfoParser :: Get Constant
utf8InfoParser = do
  byteStringLen <- getWord
  byteString <- getByteString $ fromIntegral byteStringLen
  return $ Utf8Info $ bytesToString byteString

twoTwoBytesInfoParser :: (Word16 -> Word16 -> Constant) -> Get Constant
twoTwoBytesInfoParser constConstr = constConstr <$> getWord <*> getWord
twoFourBytesInfoParser :: (Word32 -> Word32 -> Constant) -> Get Constant
twoFourBytesInfoParser constConstr = constConstr <$> getDWord <*> getDWord

twoBytesInfoParser :: (Word16 -> Constant) -> Get Constant
twoBytesInfoParser constConstr = constConstr <$> getWord
fourBytesInfoParser :: (Word32 -> Constant) -> Get Constant
fourBytesInfoParser constConstr = constConstr <$> getDWord

fieldrefParser = twoTwoBytesInfoParser Fieldref
methodrefParser = Methodref <$> getWord <*> getWord

interfaceMethodrefParser = twoTwoBytesInfoParser InterfaceMethodref
nameAndTypeInfoParser = twoTwoBytesInfoParser NameAndTypeInfo
classInfoParser = twoBytesInfoParser ClassInfo
stringInfoParser = twoBytesInfoParser StringInfo

integerInfoParser = IntegerInfo <$> unsafeCoerce <$> getDWord
floatInfoParser = FloatInfo <$> unsafeCoerce <$> getDWord
longInfoParser = LongInfo <$> unsafeCoerce <$> getDDWord
doubleInfoParser = DoubleInfo <$> unsafeCoerce <$> getDDWord

methodHandleInfoParser = MethodHandleInfo <$> getByte <*> getWord
methodTypeInfoParser = MethodTypeInfo <$> getWord
invokeDynamicInfoParser = twoTwoBytesInfoParser InvokeDynamicInfo
