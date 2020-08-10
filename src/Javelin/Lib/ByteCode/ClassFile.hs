module Javelin.Lib.ByteCode.ClassFile
  ( parse
  , parseRaw
  , magicNumber
  , version
  , classBody
  ) where

import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as LBS (ByteString, null, pack)
import Data.Map (Map, fromList)
import Data.Word

import Javelin.Lib.ByteCode.Attribute
import Javelin.Lib.ByteCode.ConstantPool
import Javelin.Lib.ByteCode.Data
import Javelin.Lib.ByteCode.FieldMethod
import Javelin.Lib.ByteCode.Utils

getInterface :: Get.Get Word16
getInterface = Get.getWord16be

classBody :: Get.Get ClassBody
classBody = do
  poolLength <- Get.getWord16be
  pool <- getConstants poolLength
  flags <- parseClassAccessFlags
  thisClass <- Get.getWord16be
  superClass <- Get.getWord16be
  interfaces <- several getInterface
  fields <- several $ getField pool
  methods <- several $ getMethod pool
  attributes <- several $ getAttr pool
  return $
    ClassBody
      (ConstantPool pool)
      flags
      thisClass
      superClass
      interfaces
      fields
      methods
      attributes

magicNumber :: Get.Get Int
magicNumber = do
  magic <- Get.getWord32be
  if magic == 0xCAFEBABE
    then return 42
    else fail "Not a Java class format"

classFlagsList :: Map Word16 ClassAccessFlags
classFlagsList =
  fromList
    [ (0x0001, AccPublic)
    , (0x0010, AccFinal)
    , (0x0020, AccSuper)
    , (0x0200, AccInterface)
    , (0x0400, AccAbstract)
    , (0x1000, AccSynthetic)
    , (0x2000, AccAnn)
    , (0x4000, AccEnum)
    ]

parseClassAccessFlags :: Get.Get [ClassAccessFlags]
parseClassAccessFlags = foldMask classFlagsList <$> Get.getWord16be

version :: Get.Get Word16
version = Get.getWord16be

parseByteCode :: Get.Get ByteCode
parseByteCode = do
  magicNumber
  minor <- version
  major <- version
  body <- classBody
  return $ ByteCode minor major body

parseRaw ::
     [Word8]
  -> Either (LBS.ByteString, Get.ByteOffset, String) ( LBS.ByteString
                                                 , Get.ByteOffset
                                                 , ByteCode)
parseRaw bytes = Get.runGetOrFail parseByteCode $ LBS.pack bytes

parse :: [Word8] -> Either String ByteCode
parse = either formatParseError validateParseResult . parseRaw

formatParseError ::
     (LBS.ByteString, Get.ByteOffset, String) -> Either String ByteCode
formatParseError (_, _, msg) = Left msg

validateParseResult ::
     (LBS.ByteString, Get.ByteOffset, ByteCode) -> Either String ByteCode
validateParseResult (bs, off, bc) =
  if LBS.null bs
    then Right bc
    else Left $ "Bytes left after parsing. Offset: " ++ show off
