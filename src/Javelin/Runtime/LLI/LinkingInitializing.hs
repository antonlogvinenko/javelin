module Javelin.Runtime.LLI.LinkingInitializing where

import qualified Data.Map.Lazy              as Map (Map, fromList, insert, lookup, (!))
import           Javelin.Util

import           Javelin.ByteCode.Data
import           Javelin.Runtime.DescSign
import           Javelin.Runtime.Structures

linking :: ClassId -> Runtime -> Either VMError Runtime
linking classId rt = verify classId rt >>= prepare classId

--runtime: check preparation status
--parse field descriptor to get value type: do it early

-- §5.4.1 Verify
verify :: ClassId -> Runtime -> Either VMError Runtime
verify classId rt = Right rt

-- §5.4.2 Preparation
prepare :: ClassId -> Runtime -> Either VMError Runtime
prepare classId rt =
  if isClassPrepared classId rt
  then return rt
  else updateClassFields classId rt (map initStaticField . filter (isFieldStatic . fieldAccess))
       >>= markClassPrepared classId

initStaticField :: Field -> Field
initStaticField field = let value = case fieldType field of
                                      BaseType bt -> baseDefaultValues Map.! bt
                                      _ -> nullReference
                        in field{staticValue = Just value}
