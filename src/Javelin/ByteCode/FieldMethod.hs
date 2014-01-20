module Javelin.ByteCode.FieldMethod
where

import Data.Map.Lazy (Map, fromList)
import Data.Word (Word16)
import Control.Applicative
import Data.Binary.Get

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils
import Javelin.ByteCode.Attribute

fieldInfoAccessFlagsMap = fromList [(0x0001, FieldPublic), (0x0002, FieldPrivate),
                                    (0x0004, FieldProtected), (0x0008, FieldStatic),
                                    (0x0010, FieldFinal), (0x0040, FieldVolatile),
                                    (0x0080, FieldTransient), (0x1000, FieldSynthetic),
                                    (0x4000, FieldEnum)]
                          
methodInfoAccessFlagsMap = fromList [(0x0001, MethodPublic), (0x0002, MethodPrivate),
                                     (0x0004, MethodProtected), (0x0008, MethodStatic),
                                     (0x0010, MethodFinal), (0x0020, MethodSynchronized),
                                     (0x0040, MethodBridge), (0x0080, MethodVarargs),
                                     (0x0100, MethodNative), (0x0400, MethodAbstract),
                                     (0x0800, MethodStrict), (0x1000, MethodSynthetic)]

getFieldMethod :: [Constant] -> Map Word16 flag ->
                   ([flag] -> Word16 -> Word16 -> [AttributeInfo] -> x) -> Get x
getFieldMethod pool accessFlagsMap constr =
  constr
  <$> (getWord >>= (\c -> return $ foldMask accessFlagsMap c))
  <*> getWord
  <*> getWord
  <*> (getWord >>= (\c -> (getNTimes $ getAttribute pool) c))
  
getField :: [Constant] -> Get FieldInfo
getField pool = getFieldMethod pool fieldInfoAccessFlagsMap FieldInfo

getMethod :: [Constant] -> Get MethodInfo
getMethod pool = getFieldMethod pool methodInfoAccessFlagsMap MethodInfo
