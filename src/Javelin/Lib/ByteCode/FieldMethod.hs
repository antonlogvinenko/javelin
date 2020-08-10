module Javelin.Lib.ByteCode.FieldMethod where

import qualified Data.Binary.Get as Get
import qualified Data.Map.Lazy as Map
import qualified Data.Word as Word

import qualified Javelin.Lib.ByteCode.Attribute as Attribute
import qualified Javelin.Lib.ByteCode.Data as ByteCode
import qualified Javelin.Lib.ByteCode.Utils as Utils

fieldInfoAF :: Map.Map Word.Word16 ByteCode.FieldInfoAccessFlag
fieldInfoAF =
  Map.fromList
    [ (0x0001, ByteCode.FieldPublic)
    , (0x0002, ByteCode.FieldPrivate)
    , (0x0004, ByteCode.FieldProtected)
    , (0x0008, ByteCode.FieldStatic)
    , (0x0010, ByteCode.FieldFinal)
    , (0x0040, ByteCode.FieldVolatile)
    , (0x0080, ByteCode.FieldTransient)
    , (0x1000, ByteCode.FieldSynthetic)
    , (0x4000, ByteCode.FieldEnum)
    ]

methodInfoAF =
  Map.fromList
    [ (0x0001, ByteCode.MethodPublic)
    , (0x0002, ByteCode.MethodPrivate)
    , (0x0004, ByteCode.MethodProtected)
    , (0x0008, ByteCode.MethodStatic)
    , (0x0010, ByteCode.MethodFinal)
    , (0x0020, ByteCode.MethodSynchronized)
    , (0x0040, ByteCode.MethodBridge)
    , (0x0080, ByteCode.MethodVarargs)
    , (0x0100, ByteCode.MethodNative)
    , (0x0400, ByteCode.MethodAbstract)
    , (0x0800, ByteCode.MethodStrict)
    , (0x1000, ByteCode.MethodSynthetic)
    ]

getFieldMethod ::
     Map.Map Word.Word16 flag
  -> ([flag] -> Word.Word16 -> Word.Word16 -> [ByteCode.AttrInfo] -> x)
  -> [ByteCode.Constant]
  -> Get.Get x
getFieldMethod accessFlags constr pool = do
  maskBytes <- Get.getWord16be
  let mask = Utils.foldMask accessFlags maskBytes
  nameIndex <- Get.getWord16be
  descriptorIndex <- Get.getWord16be
  attrsCount <- Get.getWord16be
  attributes <- Utils.times (Attribute.getAttr pool) attrsCount
  return $ constr mask nameIndex descriptorIndex attributes

getField :: [ByteCode.Constant] -> Get.Get ByteCode.FieldInfo
getField = getFieldMethod fieldInfoAF ByteCode.FieldInfo

getMethod :: [ByteCode.Constant] -> Get.Get ByteCode.MethodInfo
getMethod = getFieldMethod methodInfoAF ByteCode.MethodInfo
