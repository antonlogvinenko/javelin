module Javelin.Runtime.LLI.Linking
where

import Data.Map.Lazy as Map (fromList, Map, lookup, insert, (!))

import Javelin.Runtime.Structures
import Javelin.Runtime.LLI.Resolve


linking :: ClassName -> Runtime -> Either LoadingError Runtime
linking name rt = verification name rt >>= preparing name

-- §5.4.1 verification skipped in the first iteration
verification :: ClassName -> Runtime -> Either LoadingError Runtime
verification name rt = Right rt

preparing :: ClassName -> Runtime -> Either LoadingError Runtime
preparing name rt@(Runtime {classLoading = classLoadingInfo}) =
  let classLoaderInfo = classLoadingInfo ! name
      (rt, ref) = malloc rt
      -- do writeField rt ref (name, value) several times, use final rt
  in Right $ rt{classLoading = Map.insert name classLoaderInfo{staticRef = Just ref} classLoadingInfo}
-- find all static fields in bytecode
-- set runtime default values to static fields
