module Javelin.Runtime.LLI.Linking
where

import Javelin.Runtime.Structures
import Javelin.Runtime.LLI.Resolve


link :: ClassName -> Runtime -> Either LoadingError Runtime
link name rt = verify name rt >>= prepare name

-- §5.4.1 verification skipped in the first iteration
verify :: ClassName -> Runtime -> Either LoadingError Runtime
verify name rt = Right rt

prepare :: ClassName -> Runtime -> Either LoadingError Runtime
prepare name rt = undefined
