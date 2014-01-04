module Javelin.ByteCode2 (parse)
where

import Data.ByteString (ByteString, unpack)
import Data.Word (Word32, Word16, Word8)
import Control.Monad
                   
data ByteCode = ByteCode {minVer :: Integer, majVer :: String, body :: ClassBody} deriving (Show)
data ClassBody = ClassBody {constantPool :: [ConstantPool]} deriving (Show)
data ConstantPool = ConstantPool deriving (Show)  

type Parser a = [Word8] -> Either String ([Word8], a)

parseMinor :: Parser Integer
parseMinor bytes = Right (bytes, 42)

parseMajor :: Parser String
parseMajor bytes = Right (bytes, "cake")

parseConstantPoolSize :: Parser Integer
parseConstantPoolSize bytes = Right (bytes, 2)

parseConstantPool :: Integer -> Parser [ConstantPool]
parseConstantPool len bytes = Right (bytes, [])
                              
parseBody :: Parser ClassBody
parseBody bytes = do
  (bytes1, len) <- parseConstantPoolSize bytes
  (bytes2, pool) <- parseConstantPool len bytes1
  return (bytes2, ClassBody pool)

parse :: [Word8] -> Either String ByteCode
parse bytes = do
  (bytes1, minor) <- parseMinor bytes
  (bytes2, major) <- parseMajor bytes1
  (bytes3, body) <- parseBody bytes2
  if length bytes3 == 0 then Left "asdad" else return $ ByteCode minor major body
