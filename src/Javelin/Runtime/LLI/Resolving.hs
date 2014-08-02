module Javelin.Runtime.LLI.Resolving
where

import Javelin.Runtime.Structures
import Control.Arrow ((>>>))
import Javelin.Util


resolveClassInterface :: ClassName -> Runtime -> Either VMError Runtime
resolveClassInterface = undefined

resolveField :: ClassName -> Runtime -> Either VMError Runtime
resolveField = undefined

resolveMethod :: ClassName -> Runtime -> Either VMError Runtime
resolveMethod = undefined

resolveInterfaceMethod :: ClassName -> Runtime -> Either VMError Runtime
resolveInterfaceMethod = undefined


-- for some successful resolution or error must be persistent between invokations
-- errors: generate classchange, react to linkage error
-- class, interface, field, method, interface method
