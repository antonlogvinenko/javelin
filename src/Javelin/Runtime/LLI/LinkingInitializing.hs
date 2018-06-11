module Javelin.Runtime.LLI.LinkingInitializing where

import           Data.Map.Strict            ((!))

import           Javelin.Runtime.DescSign   (FieldType (..))
import           Javelin.Runtime.Structures

init :: ClassId -> Runtime -> Either VMError Runtime
init classId rt = linking classId rt

linking :: ClassId -> Runtime -> Either VMError Runtime
linking classId rt = verify classId rt >>= prepare classId

verify :: ClassId -> Runtime -> Either VMError Runtime
verify classId rt = Right rt

prepare :: ClassId -> Runtime -> Either VMError Runtime
prepare classId rt =
  if isClassPrepared classId rt
    then return rt
    else updateClassFields
           classId
           rt
           (map initStaticField . filter (isFieldStatic . fieldAccess)) >>=
         markClassPrepared classId

initStaticField :: Field -> Field
initStaticField field =
  let value =
        case fieldType field of
          BaseType bt -> baseDefaultValues ! bt
          _           -> nullReference
   in field {staticValue = Just value}
