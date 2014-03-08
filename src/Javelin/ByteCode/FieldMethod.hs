module Javelin.ByteCode.FieldMethod
where

import Data.Map.Lazy (Map, fromList)
import Data.Word (Word16)
import Control.Applicative
import Data.Binary.Get

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils
import Javelin.ByteCode.Attribute

fieldInfoAF = fromList [(0x0001, FieldPublic), (0x0002, FieldPrivate),
                        (0x0004, FieldProtected), (0x0008, FieldStatic),
                        (0x0010, FieldFinal), (0x0040, FieldVolatile),
                        (0x0080, FieldTransient), (0x1000, FieldSynthetic),
                        (0x4000, FieldEnum)]
                          
methodInfoAF = fromList [(0x0001, MethodPublic), (0x0002, MethodPrivate),
                         (0x0004, MethodProtected), (0x0008, MethodStatic),
                         (0x0010, MethodFinal), (0x0020, MethodSynchronized),
                         (0x0040, MethodBridge), (0x0080, MethodVarargs),
                         (0x0100, MethodNative), (0x0400, MethodAbstract),
                         (0x0800, MethodStrict), (0x1000, MethodSynthetic)]

getFieldMethod :: Map Word16 flag ->
                  ([flag] -> Word16 -> Word16 -> [AttrInfo] -> x) ->
                  [Constant]
                  -> Get x
getFieldMethod accessFlags constr pool = do
  maskBytes <- getWord
  let mask = foldMask accessFlags maskBytes
  nameIndex <- getWord
  descriptorIndex <- getWord
  attrsCount <- getWord
  attributes <- times (getAttr pool) attrsCount
  return $ constr mask nameIndex descriptorIndex attributes

getField :: [Constant] -> Get FieldInfo
getField = getFieldMethod fieldInfoAF FieldInfo

getMethod :: [Constant] -> Get MethodInfo
getMethod = getFieldMethod methodInfoAF MethodInfo
