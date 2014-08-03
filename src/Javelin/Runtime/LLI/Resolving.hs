module Javelin.Runtime.LLI.Resolving
where

import Javelin.Runtime.Structures
import Data.Map as Map
import Control.Arrow ((>>>))
import Javelin.Util


-- first need to check whether resolution already happened, 3 possible outcomes:
-- 1. not yet resolved
-- 2. successfilly resolved
-- 3. resolution failed on the previous attempt 
resolveClassInterface :: ClassName -> Runtime -> Either VMError Runtime
resolveClassInterface name rt = do
  case rt $> classResolving >>> (Map.lookup name) of
    Just Nothing -> return rt
    Just (Just err) -> linkageLeft err
    Nothing -> undefined

    

resolveField :: ClassName -> Runtime -> Either VMError Runtime
resolveField = undefined

resolveMethod :: ClassName -> Runtime -> Either VMError Runtime
resolveMethod = undefined

resolveInterfaceMethod :: ClassName -> Runtime -> Either VMError Runtime
resolveInterfaceMethod = undefined


-- for some successful resolution or error must be persistent between invokations
-- errors: generate classchange, react to linkage error
-- class, interface, field, method, interface method
