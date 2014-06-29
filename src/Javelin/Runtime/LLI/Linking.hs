module Javelin.Runtime.LLI.Linking
where

import qualified Data.Map.Lazy as Map (fromList, Map, lookup, insert, (!))
import Data.Array.IArray as Array ((!), (//))
import Data.Word (Word16)

import Javelin.Runtime.Structures
import Javelin.Runtime.LLI.Resolve
import Javelin.ByteCode.Data
import Javelin.Util

linking :: ClassName -> Runtime -> Either VMError Runtime
linking name rt = verification name rt >>= preparing name

-- §5.4.1 verification skipped in the first iteration
verification :: ClassName -> Runtime -> Either VMError Runtime
verification name rt = Right rt

preparing :: ClassName -> Runtime -> Either VMError Runtime
preparing name rt@(Runtime {classLoading = classLoadingInfo}) =
  let classLoaderInfo = classLoadingInfo Map.! name
      (rt1, ref) = malloc rt
  in do
    rt2 <- writeStaticFields name rt1 ref
    return rt2{classLoading = Map.insert name classLoaderInfo{staticRef = Just ref} classLoadingInfo}


writeStaticFields :: String -> Runtime -> Ref -> Either VMError Runtime
writeStaticFields name rt ref = let (s, h) = heap rt
                                    jobject = h ! ref
                                in do
                                   sym <- getSymTable rt name
                                   bc <- getByteCode rt name 
                                   let staticSearch fi = FieldStatic `elem` fieldAccessFlags fi
                                       staticFields = filter staticSearch $ fields $ body bc
                                     in foldl (prepareStaticField sym ref) (return rt) staticFields

getDefaultValue :: String -> JValue
getDefaultValue = undefined
-- 2 rewrite here

prepareStaticField :: SymTable -> Ref -> Either VMError Runtime -> FieldInfo -> Either VMError Runtime
prepareStaticField sym ref ert fi = do
  rt <- ert
  fieldName <- getStringLiteral sym $ fieldNameIndex fi
  descriptorName <- getStringLiteral sym $ fieldDescriptorIndex fi
  let defaultValue = getDefaultValue descriptorName
  writeField rt ref (fieldName, defaultValue)
-- 1 parse descriptor - or save already parsed while creating sym table? good question
-- 2 determine default value by descriptor
-- 3 do writeField rt ref (name, value) several times for default values
