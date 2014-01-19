module Javelin.ByteCode.ClassFile (parse, require, magicNumber, version, classBody)
where

import Data.Word (Word32, Word16, Word8)
import qualified Data.Map as Map (fromList)

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils
import Javelin.ByteCode.ConstantPool
import Javelin.ByteCode.FieldMethod
import Javelin.ByteCode.Attribute
import Control.Applicative
import qualified Data.Binary.Get as G

getInterfaces :: RepeatingParser [Word16]
getInterfaces = getNTimes $ getWord

classBody :: Parser ClassBody
classBody bytes = do
  (bytes1, pool) <- getCountAndList getConstantPool bytes
  (bytes2, flags) <- parseClassAccessFlags bytes1
  (bytes3, this) <- getWord bytes2
  (bytes4, super) <- getWord bytes3
  (bytes5, interfaces) <- getCountAndList getInterfaces bytes4
  (bytes6, fields) <- getCountAndList (getNTimes $ getField pool) bytes5
  (bytes7, methods) <- getCountAndList (getNTimes $ getMethod pool) bytes6
  (bytesLast, attributes) <- getCountAndList (getNTimes $ getAttribute pool) bytes7
  return (bytesLast, ClassBody pool flags this super interfaces fields methods attributes)

classBody' :: G.Get ClassBody
classBody' = undefined

magicNumber :: Parser Int
magicNumber = convert $ do
  magic <- G.getWord32be
  if magic == 0xCAFEBABE
    then return 42
    else fail "Not a Java class format"

classFlagsList = Map.fromList [(0x0001, ClassPublic), (0x0010, ClassFinal), (0x0020, ClassSuper),
                               (0x0200, ClassInterface), (0x0400, ClassAbstract),
                               (0x1000, ClassSynthetic), (0x2000, ClassAnnotation),
                               (0x4000, ClassEnum)]

parseClassAccessFlags :: Parser [ClassAccessFlags]
parseClassAccessFlags = convert $ foldMask' classFlagsList <$> G.getWord16be

version :: G.Get Word16
version = G.getWord16be

parse' :: G.Get ByteCode
parse' = ByteCode <$> version <*> version <*> classBody'

parse :: Parser ByteCode  
parse bytes = do
  (bytes0, _) <- magicNumber bytes
  (bytes1, minor) <- convert version $ bytes0
  (bytes2, major) <- convert version $ bytes1
  (bytes3, body) <- classBody bytes2
  if length bytes3 == 0
    then Left "Bytes left"
    else return (bytes3, ByteCode minor major body)
