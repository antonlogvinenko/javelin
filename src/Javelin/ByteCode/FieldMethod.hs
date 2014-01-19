module Javelin.ByteCode.FieldMethod
where

import qualified Data.Map.Lazy as Map (fromList, Map(..))

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils
import Javelin.ByteCode.Attribute
import qualified Data.Binary.Get as G

fieldInfoAccessFlagsMap = Map.fromList [(0x0001, FieldPublic), (0x0002, FieldPrivate),
                                      (0x0004, FieldProtected), (0x0008, FieldStatic),
                                      (0x0010, FieldFinal), (0x0040, FieldVolatile),
                                      (0x0080, FieldTransient), (0x1000, FieldSynthetic),
                                      (0x4000, FieldEnum)]

methodInfoAccessFlagsMap = Map.fromList [(0x0001, MethodPublic), (0x0002, MethodPrivate),
                                         (0x0004, MethodProtected), (0x0008, MethodStatic),
                                         (0x0010, MethodFinal), (0x0020, MethodSynchronized),
                                         (0x0040, MethodBridge), (0x0080, MethodVarargs),
                                         (0x0100, MethodNative), (0x0400, MethodAbstract),
                                         (0x0800, MethodStrict), (0x1000, MethodSynthetic)]

getFieldMethod pool accessFlagsMap constr bytes = do
  (bytes1, flagBytes) <- getWord bytes
  let accessFlags = foldMask flagBytes accessFlagsMap
  (bytes2, nameIndex) <- getWord bytes1
  (bytes3, descriptorIndex) <- getWord bytes2
  (bytes4, attributesCount) <- getWord bytes3
  (bytesLast, attributes) <- (getNTimes $ getAttribute pool) attributesCount bytes4
  return $ (bytesLast, constr accessFlags nameIndex descriptorIndex attributes)

getField :: [Constant] -> Parser FieldInfo
getField pool = getFieldMethod pool fieldInfoAccessFlagsMap FieldInfo

getMethod :: [Constant] -> Parser MethodInfo
getMethod pool = getFieldMethod pool methodInfoAccessFlagsMap MethodInfo

getField' :: [Constant] -> G.Get FieldInfo
getField' = undefined

getMethod' :: [Constant] -> G.Get MethodInfo
getMethod' = undefined
