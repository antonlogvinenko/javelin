module Javelin.ByteCode (parse, require, getBytes, magicNumber, version, classBody)
where

import Data.ByteString (ByteString, unpack)
import Data.Word (Word32, Word16, Word8)
import Control.Monad
import Data.Map.Lazy


-- Modeling java class format
data ByteCode = ByteCode {minVer :: Word16, majVer :: Word16, body :: ClassBody}
                deriving (Show, Eq)

data ClassBody = ClassBody {constPool :: [Constant],
                           classAccessFlags :: [ClassAccessFlags],
                           this :: Word16,
                           super :: Word16,
                           interfaces :: [Word16],
                           fields :: [FieldInfo],
                           methods :: [MethodInfo],
                           attributes :: [AttributeInfo]}
                 deriving (Show, Eq)

data ClassAccessFlags = Public | Final
                      | Super | Interface | Abstract
                      | Synthetic | Annotation | Enum deriving (Show, Eq)

data Constant = Utf8Info {len :: Word16, stringBytes :: [Word8]}
              | IntegerInfo {bytes :: Word32}
              | FloatInfo {bytes :: Word32}
              | LongInfo {highBytes :: Word32, lowBytes :: Word32}
              | DoubleInfo {highBytes :: Word32, lowBytes :: Word32}
              | ClassInfo {nameIndex :: Word16}
              | StringInfo {stringIndex :: Word16}
              | Fieldref {classIndex :: Word16, nameAndTypeIndex :: Word16}
              | Methodref {classIndex :: Word16, nameAndTypeIndex :: Word16}
              | InterfaceMethodref {classIndex :: Word16, nameAndTypeIndex :: Word16}
              | NameAndTypeInfo { nameIndex :: Word16, nameAndTypeDescriptorIndex :: Word16}
              | MethodHandleInfo { referenceKind :: Word8, refereneIndex :: Word16 }
              | MethodTypeInfo { methodTypeDescriptorIndex :: Word16 }
              | InvokeDynamicInfo { bootstrapMethodAttrIndex :: Word16, nameAndTypeIndex :: Word16 } deriving (Show, Eq)

data FieldInfo = FieldInfo { fieldAccessFlags :: Word16,
                             fieldNameIndex :: Word16,
                             descriptorIndex :: Word16,
                             fieldAttributes :: [AttributeInfo]
                           } deriving (Show, Eq)

data MethodInfo = MethodInfo { methodAccessFlags :: Word16,
                               methodNameIndex :: Word16,
                               methodInfoDescriptorIndex :: Word16,
                               methodAttributes :: [AttributeInfo]
                             } deriving (Show, Eq)

data AttributeInfo = AttributeInfo deriving (Show, Eq)


-- Basic utility functions
getCountAndList :: (Word16 -> Parser [x]) -> [Word8] -> Either String ([Word8], [x])
getCountAndList f bytes = do
  (bytes1, count) <- getBytes 2 bytes
  f count bytes1

require :: Int -> [Word8] ->  a -> Either String a
require len bs value = if length bs < len
                       then Left "Unexpected EOF"
                       else Right value

getBytes count bs = require count bs $
                    let high = bs !! 0
                        low = bs !! 1
                        ver = (fromIntegral high) * 256 + fromIntegral low
                    in (drop count bs, ver)

takeBytes count bs = require count bs $ (drop count bs, take count bs)
                     

type Parser a = [Word8] -> Either String ([Word8], a)


-- Class file header functions
magicNumber :: Parser Int
magicNumber bs = if take 4 bs == [0xCA, 0xFE, 0xBA, 0xBE]
                 then Right (drop 4 bs, 42)
                 else Left "Not a Java class format"
                        
version :: Parser Word16
version = getBytes 2

classFlagsList = fromList [(0x0001, Public), (0x0010, Final), (0x0020, Super),
                           (0x0200, Interface), (0x0400, Abstract),
                           (0x1000, Synthetic), (0x2000, Annotation), (0x4000, Enum)]

getClassFlagsList bytes = Right []

parseClassAccessFlags bytes = do
  (bytes1, flagsBytes) <- getBytes 2 bytes
  let flags = flagsList2 flagsBytes
  return $ (bytes1, flags)


-- Constant pool
getConstantPool :: Word16 -> Parser [Constant]
getConstantPool len bytes = do
  if len == 1
    then Right (bytes, [])
    else do
    (bytes1, pool) <- getConstant bytes
    (bytes2, pools) <- getConstantPool (len - 1) bytes1
    return (bytes2, (pool:pools))

getConstant bytes = do
  (bytes1, tag) <- getBytes 1 bytes
  getConstantParser tag bytes1

getConstantParser :: Int -> Parser Constant
getConstantParser idx = findWithDefault failingConstParser idx constantTypeParser
constantTypeParser :: Map Int (Parser Constant)
constantTypeParser = fromList [(1, utf8InfoParser), (3, integerInfoParser), (4, floatInfoParser),
                               (5, longInfoParser), (6, doubleInfoParser), (7, classInfoParser),
                               (8, stringInfoParser), (9, fieldrefParser), (10, methodrefParser),
                               (11, interfaceMethodrefParser), (12, nameAndTypeInfoParser), (15, identityParser),
                               (16, identityParser), (18, identityParser)]
                     
identityParser bytes = Right (bytes, Utf8Info 1 [])
failingConstParser _ = Left "Undefined constant"

twoTwoBytesInfoParser :: (Word16 -> Word16 -> Constant) -> Parser Constant
twoTwoBytesInfoParser constConstr bytes = do
  (bytes1, firstWord16) <- getBytes 2 bytes
  (bytes2, secondWord16) <- getBytes 2 bytes
  return $ (bytes2, constConstr firstWord16 secondWord16)

twoFourBytesInfoParser :: (Word32 -> Word32 -> Constant) -> Parser Constant
twoFourBytesInfoParser constConstr bytes = do
  (bytes1, bytesHigh) <- getBytes 4 bytes
  (bytes2, bytesLow) <- getBytes 4 bytes1
  return $ (bytes2, constConstr bytesHigh bytesLow)

twoBytesInfoParser :: (Word16 -> Constant) -> Parser Constant
twoBytesInfoParser constConstr bytes = do
  (bytes1, twoBytes) <- getBytes 2 bytes
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
  (bytes1, len) <- getBytes 2 bytes
  (bytes2, lenBytes) <- takeBytes (fromIntegral len) bytes1
  return $ (bytes2, Utf8Info len lenBytes)
methodHandleInfoParser bytes = do
  (bytes1, kind) <- getBytes 1 bytes
  (bytes2, index) <- getBytes 2 bytes1
  return $ (bytes2, MethodHandleInfo kind index)
methodTypeInfoParser bytes = do
  (bytes1, index) <- getBytes 2 bytes
  return $ (bytes1, MethodTypeInfo index)
invokeDynamicInfoParser = twoTwoBytesInfoParser InvokeDynamicInfo




-- Fields
getFields :: Word16 -> Parser [FieldInfo]
getFields len bytes = Right (bytes, [])

-- Interfaces
getInterfaces :: Word16 -> Parser [Word16]
getInterfaces len bytes = Right (bytes, [])

-- Methods
getMethods :: Word16 -> Parser [MethodInfo]
getMethods len bytes = Right (bytes, [])

-- Attributes
getAttributes :: Word16 -> Parser [AttributeInfo]
getAttributes len bytes = Right (bytes, [])


classBody :: Parser ClassBody
classBody bytes = do
  (bytes1, pool) <- getCountAndList getConstantPool bytes
  (bytes2, flags) <- parseClassAccessFlags bytes1
  (bytes3, this) <- getBytes 2 bytes2
  (bytes4, super) <- getBytes 2 bytes3
  (bytes5, interfaces) <- getCountAndList getInterfaces bytes4
  (bytes6, fields) <- getCountAndList getFields bytes5
  (bytes7, methods) <- getCountAndList getMethods bytes6
  (bytesLast, attributes) <- getCountAndList getAttributes bytes7
  return (bytesLast, ClassBody pool flags this super interfaces fields methods attributes)


-- Parser of all things alive
parse :: [Word8] -> Either String ByteCode
parse bytes = do
  (bytes0, _) <- magicNumber bytes
  (bytes1, minor) <- version bytes0
  (bytes2, major) <- version bytes1
  (bytes3, body) <- classBody bytes2
  if length bytes3 == 0
    then Left "Bytes left"
    else return $ ByteCode minor major body
