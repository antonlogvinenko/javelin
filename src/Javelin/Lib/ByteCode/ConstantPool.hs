module Javelin.Lib.ByteCode.ConstantPool
  ( getConstants
  ) where

import qualified Data.Binary.Get as Get
import qualified Data.Map.Lazy as Map
import qualified Data.Word as Word
import qualified Unsafe.Coerce as Unsafe

import Javelin.Lib.ByteCode.Data
import qualified Data.ByteString.UTF8 as BS

getConstants 1 = return []
getConstants len = do
  constant <- getConstant
  let double = ([constant, constant] ++) <$> getConstants (len - 2)
      single = (constant :) <$> getConstants (len - 1)
  case constant of
    LongInfo _ -> double
    DoubleInfo _ -> double
    _ -> single

getConstant :: Get.Get Constant
getConstant = do
  tag <- Get.getWord8
  Map.findWithDefault (failingConstParser tag) tag constantTypeParser

failingConstParser :: Word.Word8 -> Get.Get Constant
failingConstParser x = fail $ "Undefined constant with index " ++ show x ++ "\n"

constantTypeParser :: Map.Map Word.Word8 (Get.Get Constant)
constantTypeParser =
  Map.fromList
    [ (1, utf8InfoParser)
    , (3, integerInfoParser)
    , (4, floatInfoParser)
    , (5, longInfoParser)
    , (6, doubleInfoParser)
    , (7, classInfoParser)
    , (8, stringInfoParser)
    , (9, fieldrefParser)
    , (10, methodrefParser)
    , (11, interfaceMethodrefParser)
    , (12, nameAndTypeInfoParser)
    , (15, methodHandleInfoParser)
    , (16, methodTypeInfoParser)
    , (18, invokeDynamicInfoParser)
    ]

utf8InfoParser :: Get.Get Constant
utf8InfoParser = do
  byteStringLen <- Get.getWord16be
  byteString <- Get.getByteString $ fromIntegral byteStringLen
  return $ Utf8Info $ BS.toString byteString

twoTwoBytesInfoParser :: (Word.Word16 -> Word.Word16 -> Constant) -> Get.Get Constant
twoTwoBytesInfoParser constConstr = constConstr <$> Get.getWord16be <*> Get.getWord16be

twoFourBytesInfoParser :: (Word.Word32 -> Word.Word32 -> Constant) -> Get.Get Constant
twoFourBytesInfoParser constConstr = constConstr <$> Get.getWord32be <*> Get.getWord32be

twoBytesInfoParser :: (Word.Word16 -> Constant) -> Get.Get Constant
twoBytesInfoParser constConstr = constConstr <$> Get.getWord16be

fourBytesInfoParser :: (Word.Word32 -> Constant) -> Get.Get Constant
fourBytesInfoParser constConstr = constConstr <$> Get.getWord32be

fieldrefParser = twoTwoBytesInfoParser Fieldref

methodrefParser = Methodref <$> Get.getWord16be <*> Get.getWord16be

interfaceMethodrefParser :: Get.Get Constant
interfaceMethodrefParser = twoTwoBytesInfoParser InterfaceMethodref

nameAndTypeInfoParser :: Get.Get Constant
nameAndTypeInfoParser = twoTwoBytesInfoParser NameAndTypeInfo

classInfoParser = twoBytesInfoParser ClassInfo

stringInfoParser = twoBytesInfoParser StringInfo

integerInfoParser = IntegerInfo <$> Unsafe.unsafeCoerce <$> Get.getWord32be

floatInfoParser = FloatInfo <$> Unsafe.unsafeCoerce <$> Get.getWord32be

longInfoParser = LongInfo <$> Unsafe.unsafeCoerce <$> Get.getWord64be

doubleInfoParser = DoubleInfo <$> Unsafe.unsafeCoerce <$> Get.getWord64be

methodHandleInfoParser = MethodHandleInfo <$> Get.getWord8 <*> Get.getWord16be

methodTypeInfoParser = MethodTypeInfo <$> Get.getWord16be

invokeDynamicInfoParser = twoTwoBytesInfoParser InvokeDynamicInfo
