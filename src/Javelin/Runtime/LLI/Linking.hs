module Javelin.Runtime.LLI.Linking
where

import qualified Data.Map.Lazy as Map (fromList, Map, lookup, insert, (!))
import Data.Array.IArray as Array ((!), (//))

import Javelin.Runtime.Structures
import Javelin.Runtime.LLI.Resolve
import Javelin.ByteCode.Data
import Javelin.Util

linking :: ClassName -> Runtime -> Either LoadingError Runtime
linking name rt = verification name rt >>= preparing name

-- §5.4.1 verification skipped in the first iteration
verification :: ClassName -> Runtime -> Either LoadingError Runtime
verification name rt = Right rt

preparing :: ClassName -> Runtime -> Either LoadingError Runtime
preparing name rt@(Runtime {classLoading = classLoadingInfo}) =
  let classLoaderInfo = classLoadingInfo Map.! name
      (rt1, ref) = malloc rt
  in do
    rt2 <- maybeToEither undefined $ writeStaticFields name rt1 ref
    return rt2{classLoading = Map.insert name classLoaderInfo{staticRef = Just ref} classLoadingInfo}


writeStaticFields :: String -> Runtime -> Ref -> Maybe Runtime
writeStaticFields name rt ref = let (s, h) = heap rt
                                    jobject = h ! ref
                                in do
                                   sym <- Map.lookup name $ symbolics rt
                                   bc <- Map.lookup name $ bytecodes rt
                                   let staticSearch fi = FieldStatic `elem` fieldAccessFlags fi
                                       staticFields = filter staticSearch $ fields $ body bc
                                     in return $ foldl (prepareStaticField sym ref) rt staticFields

prepareStaticField :: SymTable -> Ref -> Runtime -> FieldInfo -> Runtime
prepareStaticField sym ref rt fi = let nameIndex = fieldNameIndex fi
                                       descriptorIndex = fieldDescriptorIndex fi
                                   in undefined
-- take string literals from sym table for 2 indices
-- do writeField rt ref (name, value) several times for default values
