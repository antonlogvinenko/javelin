module Javelin.Lib.ByteCode.ClassFile
  ( parse
  , parseRaw
  , magicNumber
  , version
  , classBody
  ) where

import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as LBS (ByteString, null, pack)
import qualified Data.Map as Map
import qualified Data.Word as Word

import Javelin.Lib.ByteCode.Attribute
import Javelin.Lib.ByteCode.ConstantPool
import qualified Javelin.Lib.ByteCode.Data as ByteCode
import Javelin.Lib.ByteCode.FieldMethod
import qualified Javelin.Lib.ByteCode.Utils as Utils

getInterface :: Get.Get Word.Word16
getInterface = Get.getWord16be

classBody :: Get.Get ByteCode.ClassBody
classBody = do
  poolLength <- Get.getWord16be
  pool <- getConstants poolLength
  flags <- parseClassAccessFlags
  thisClass <- Get.getWord16be
  superClass <- Get.getWord16be
  interfaces <- Utils.several getInterface
  fields <- Utils.several $ getField pool
  methods <- Utils.several $ getMethod pool
  attributes <- Utils.several $ getAttr pool
  return $
    ByteCode.ClassBody
      (ByteCode.ConstantPool pool)
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

classFlagsList :: Map.Map Word.Word16 ByteCode.ClassAccessFlags
classFlagsList =
  Map.fromList
    [ (0x0001, ByteCode.AccPublic)
    , (0x0010, ByteCode.AccFinal)
    , (0x0020, ByteCode.AccSuper)
    , (0x0200, ByteCode.AccInterface)
    , (0x0400, ByteCode.AccAbstract)
    , (0x1000, ByteCode.AccSynthetic)
    , (0x2000, ByteCode.AccAnn)
    , (0x4000, ByteCode.AccEnum)
    ]

parseClassAccessFlags :: Get.Get [ByteCode.ClassAccessFlags]
parseClassAccessFlags = Utils.foldMask classFlagsList <$> Get.getWord16be

version :: Get.Get Word.Word16
version = Get.getWord16be

parseByteCode :: Get.Get ByteCode.ByteCode
parseByteCode = do
  magicNumber
  minor <- version
  major <- version
  body <- classBody
  return $ ByteCode.ByteCode minor major body

parseRaw ::
     [Word.Word8]
  -> Either (LBS.ByteString, Get.ByteOffset, String) ( LBS.ByteString
                                                 , Get.ByteOffset
                                                 , ByteCode.ByteCode)
parseRaw bytes = Get.runGetOrFail parseByteCode $ LBS.pack bytes

parse :: [Word.Word8] -> Either String ByteCode.ByteCode
parse = either formatParseError validateParseResult . parseRaw

formatParseError ::
     (LBS.ByteString, Get.ByteOffset, String) -> Either String ByteCode.ByteCode
formatParseError (_, _, msg) = Left msg

validateParseResult ::
     (LBS.ByteString, Get.ByteOffset, ByteCode.ByteCode) -> Either String ByteCode.ByteCode
validateParseResult (bs, off, bc) =
  if LBS.null bs
    then Right bc
    else Left $ "Bytes left after parsing. Offset: " ++ show off
