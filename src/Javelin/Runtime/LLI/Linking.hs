module Javelin.Runtime.LLI.Linking
where

import Javelin.Runtime.Structures
import Javelin.Runtime.LLI.Resolve


linking :: ClassName -> Runtime -> Either LoadingError Runtime
linking name rt = verification name rt >>= preparing name

-- §5.4.1 verification skipped in the first iteration
verification :: ClassName -> Runtime -> Either LoadingError Runtime
verification name rt = Right rt

preparing :: ClassName -> Runtime -> Either LoadingError Runtime
preparing name rt = undefined
-- find all static fields in bytecode
-- create area in heap
-- set runtime proper values to static fields
