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
                           flags :: Word16,
                           this :: Word16,
                           super :: Word16}
                 deriving (Show, Eq)

data Constant = Utf8 | Integer | Float | Long | Double | Class {nameIndex :: Word16} | String
              | Fieldref {classIndex :: Word16, nameAndTypeIndex :: Word16}
              | Methodref {classIndex :: Word16, nameAndTypeIndex :: Word16}
              | InterfaceMethodref {classIndex :: Word16, nameAndTypeIndex :: Word16}
              | NameAndType
              | MethodHandle | MethodType | InvokeDynamic
              deriving (Show, Eq)


data Field = Field deriving (Show, Eq)

data Attribute = Attribute deriving (Show, Eq)

data Method = Method deriving (Show, Eq)


-- Basic utility functions
require :: Int -> [Word8] ->  a -> Either String a
require len bs value = if length bs < len
                       then Left "Unexpected EOF"
                       else Right value

getBytes count bs = require 2 bs $
                     let high = bs !! 0
                         low = bs !! 1
                         ver = (fromIntegral high) * 256 + fromIntegral low
                     in (drop 2 bs, ver)

type Parser a = [Word8] -> Either String ([Word8], a)


-- Class file header functions
magicNumber :: Parser Int
magicNumber bs = if take 4 bs == [0xCA, 0xFE, 0xBA, 0xBE]
                 then Right (drop 4 bs, 42)
                 else Left "Not a Java class format"
                        
version :: Parser Word16
version = getBytes 2


-- Constant pool
constantTypeParser :: Map Int (Parser Constant)
constantTypeParser = fromList [(1, identityParser), (3, identityParser), (4, identityParser),
                               (5, identityParser), (6, identityParser), (7, identityParser),
                               (8, classInfoParser), (9, fieldrefParser), (10, methodrefParser),
                               (11, interfaceMethodrefParser), (12, identityParser), (15, identityParser),
                               (16, identityParser), (18, identityParser)]
getConstantParser :: Int -> Parser Constant
getConstantParser idx = findWithDefault failingConstParser idx constantTypeParser

identityParser :: Parser Constant
identityParser bytes = Right (bytes, Utf8)

failingConstParser :: Parser Constant
failingConstParser _ = Left "Undefined constant"

classInfoParser :: Parser Constant
classInfoParser bytes = do
  (bytes1, nameIndex) <- getBytes 2 bytes
  return $ (bytes1, Class nameIndex)

fieldrefParser = fieldMethodRefParser Fieldref
methodrefParser = fieldMethodRefParser Methodref
interfaceMethodrefParser = fieldMethodRefParser InterfaceMethodref

fieldMethodRefParser constConstr bytes = do
  (bytes1, classIndex) <- getBytes 2 bytes
  (bytes2, nameAndTypeIndex) <- getBytes 2 bytes
  return $ (bytes2, constConstr classIndex nameAndTypeIndex)

getConstant bytes = do
  (bytes1, tag) <- getBytes 1 bytes
  getConstantParser tag bytes1

getConstantPool :: Word16 -> Parser [Constant]
getConstantPool len bytes = do
  if len == 1
    then Right (bytes, [])
    else do
    (bytes1, pool) <- getConstant bytes
    (bytes2, pools) <- getConstantPool (len - 1) bytes1
    return (bytes2, (pool:pools))

-- Interfaces
getInterfaces :: Word16 -> Parser [Word16]
getInterfaces len bytes = Right (bytes, [])

getFields :: Word16 -> Parser [Field]
getFields len bytes = Right (bytes, [])

getMethods :: Word16 -> Parser [Method]
getMethods len bytes = Right (bytes, [])

getAttributes :: Word16 -> Parser [Attribute]
getAttributes len bytes = Right (bytes, [])

getCountAndList :: (Word16 -> Parser [x]) -> [Word8] -> Either String ([Word8], [x])
getCountAndList f bytes = do
  (bytes1, count) <- getBytes 2 bytes
  f count bytes1

classBody :: Parser ClassBody
classBody bytes = do
  (bytes1, pool) <- getCountAndList getConstantPool bytes
  (bytes2, flags) <- getBytes 2 bytes1
  (bytes3, this) <- getBytes 2 bytes2
  (bytes4, super) <- getBytes 2 bytes3
  (bytes5, interfaces) <- getCountAndList getInterfaces bytes4
  (bytes6, fields) <- getCountAndList getFields bytes5
  (bytes7, methods) <- getCountAndList getMethods bytes6
  (bytesLast, attributes) <- getCountAndList getAttributes bytes7
  return (bytesLast, ClassBody pool flags this super)


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
