module Javelin.ByteCode.ClassFile (parse, require, magicNumber, version, classBody)
where

import Data.Word (Word32, Word16, Word8)

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils
import Javelin.ByteCode.Header
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
