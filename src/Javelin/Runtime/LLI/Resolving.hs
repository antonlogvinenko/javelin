module Javelin.Runtime.LLI.Resolving
where

import Javelin.Runtime.Structures
import Control.Arrow ((>>>))
import Javelin.Util


resolve :: ClassName -> Runtime -> Either VMError Runtime
resolve = undefined

-- for some successful resolution or error must be persistent between invokations
-- errors: generate classchange, react to linkage error
-- class, interface, field, method, interface method
