module Javelin.Runtime.LLI.LinkingInitializing where

import           Control.Applicative        ((<$>))
import qualified Data.Map.Lazy              as Map (Map, fromList, insert,
                                                    lookup, (!))
import           Data.Word                  (Word16)
import           Javelin.Util

import           Flow
import           Javelin.ByteCode.Data
import           Javelin.Runtime.DescSign
import           Javelin.Runtime.Structures

linking :: ClassId -> Runtime -> Either VMError Runtime
linking classId rt = verify classId rt >>= prepare classId

-- §5.4.1 Verify
verify :: ClassId -> Runtime -> Either VMError Runtime
verify classId rt = Right rt

-- §5.4.2 Preparation
prepare :: ClassId -> Runtime -> Either VMError Runtime
prepare classId rt = do
  c <- getClass rt classId
  rt2 <- updateClassFields classId rt (map initStaticField)
  return $ markClassPrepared classId rt2

initStaticField :: Field -> Field
initStaticField = undefined
