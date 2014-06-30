module Javelin.Runtime.LLI.Linking
where

import qualified Data.Map.Lazy as Map (fromList, Map, lookup, insert, (!))
import Data.Array.IArray as Array ((!), (//))
import Data.Word (Word16)
import Control.Applicative ((<$>))
import Control.Arrow ((>>>))

import Javelin.Runtime.Structures
import Javelin.Runtime.LLI.Resolve
import Javelin.ByteCode.Data
import Javelin.ByteCode.DescSign
import Javelin.Util

linking :: ClassName -> Runtime -> Either VMError Runtime
linking name rt = verification name rt >>= preparing name

-- §5.4.1 Verification
verification :: ClassName -> Runtime -> Either VMError Runtime
verification name rt = Right rt

-- §5.4.2 Preparation
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
                                   body >>> fields >>> filter staticSearch >>> foldl (prepareStaticField sym ref) (return rt) $ bc

getDefaultValue :: Runtime -> String -> Either VMError JValue
getDefaultValue rt name = case parseFieldDescriptor name of
  Left err -> Left $ StateError rt ""
  Right d -> case fieldType d of
    BaseType t -> maybeToEither (StateError rt "") $ Map.lookup t baseDefaultValues
    _ -> return nullReference

prepareStaticField :: SymTable -> Ref -> Either VMError Runtime -> FieldInfo -> Either VMError Runtime
prepareStaticField sym ref ert fi = do
  rt <- ert
  fieldName <- fieldNameIndex >>> getStringLiteral sym $ fi
  descriptorName <- getStringLiteral sym $ fieldDescriptorIndex fi
  defaultValue <- getDefaultValue rt descriptorName
  writeField rt ref (fieldName, defaultValue)
