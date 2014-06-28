module Javelin.Runtime.LLI.Linking
where

import qualified Data.Map.Lazy as Map (fromList, Map, lookup, insert, (!))
import Data.Array.IArray as Array ((!), (//))

import Javelin.Runtime.Structures
import Javelin.Runtime.LLI.Resolve
import Javelin.ByteCode.Data

linking :: ClassName -> Runtime -> Either LoadingError Runtime
linking name rt = verification name rt >>= preparing name

-- §5.4.1 verification skipped in the first iteration
verification :: ClassName -> Runtime -> Either LoadingError Runtime
verification name rt = Right rt

preparing :: ClassName -> Runtime -> Either LoadingError Runtime
preparing name rt@(Runtime {classLoading = classLoadingInfo}) =
  let classLoaderInfo = classLoadingInfo Map.! name
      (rt1, ref) = malloc rt
      rt2 = writeStaticFields name rt1 ref
  in Right $ rt2{classLoading = Map.insert name classLoaderInfo{staticRef = Just ref} classLoadingInfo}


writeStaticFields :: String -> Runtime -> Ref -> Runtime
writeStaticFields name rt ref = let (s, h) = heap rt
                                    jobject = h ! ref
                                in case Map.lookup name $ bytecodes rt of
                                  Nothing -> undefined
                                  Just bc -> let allFields = fields $ body bc
                                                 -- find all static fields in bytecode
                                                 staticFields = undefined
                                             in foldl prepareStaticField rt staticFields

prepareStaticField :: Runtime -> FieldInfo -> Runtime
prepareStaticField rt fi = undefined
-- do writeField rt ref (name, value) several times for default values
