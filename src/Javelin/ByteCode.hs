module Javelin.ByteCode (parse, require, get2Bytes, magicNumber, version,
                         constantPool, constantPoolSize, classBody)
where

import Data.ByteString (ByteString, unpack)
import Data.Word (Word32, Word16, Word8)
import Control.Monad
                   
data ByteCode = ByteCode {minVer :: Word16, majVer :: Word16, body :: ClassBody}
                deriving (Show, Eq)
data ClassBody = ClassBody {constool :: [ConstantPool]}
                 deriving (Show, Eq)
data ConstantPool = ConstantPool
                    deriving (Show, Eq)

type Parser a = [Word8] -> Either String ([Word8], a)

require :: Int -> [Word8] ->  a -> Either String a
require len bs value = if length bs < len
                       then Left "Unexpected EOF"
                       else Right value

get2Bytes bs = require 2 bs $
               let high = bs !! 0
                   low = bs !! 1
                   ver = (fromIntegral high) * 256 + fromIntegral low
               in (drop 2 bs, ver)

magicNumber :: Parser Int
magicNumber bs = if take 4 bs == [0xCA, 0xFE, 0xBA, 0xBE]
                 then Right (drop 4 bs, 42)
                 else Left "Not a Java class format"
                        
version :: Parser Word16
version = get2Bytes

constantPoolSize :: Parser Word16
constantPoolSize = get2Bytes

constantPool :: Word16 -> Parser [ConstantPool]
constantPool len bytes = Right (bytes, [])
                              
classBody :: Parser ClassBody
classBody bytes = do
  (bytes1, len) <- constantPoolSize bytes
  (bytes2, pool) <- constantPool len bytes1
  return (bytes2, ClassBody pool)

parse :: [Word8] -> Either String ByteCode
parse bytes = do
  (bytes0, _) <- magicNumber bytes
  (bytes1, minor) <- version bytes0
  (bytes2, major) <- version bytes1
  (bytes3, body) <- classBody bytes2
  if length bytes3 == 0 then Left "Bytes left" else return $ ByteCode minor major body
