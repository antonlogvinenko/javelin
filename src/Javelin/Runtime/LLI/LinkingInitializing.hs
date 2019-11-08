module Javelin.Runtime.LLI.LinkingInitializing where

import           Data.Map.Strict            ((!))

import           Javelin.Runtime.DescSign   (FieldType (..))
import           Javelin.Runtime.Structures
import           Javelin.Runtime.LLI.Loading    (loadClassOrArray)
import           Control.Monad.Trans.Except    (ExceptT (..), throwE, withExceptT, except)
import           Control.Monad.Trans.Class     (lift)

init :: ClassId -> Runtime -> ExceptT VMError IO Runtime
init classId rt = linking classId rt

linking :: ClassId -> Runtime -> ExceptT VMError IO Runtime
linking classId rt = verify classId rt >>= prepare classId

verify :: ClassId -> Runtime -> ExceptT VMError IO Runtime
verify classId rt = loadClassOrArray classId rt --todo not doing actual verification yet

prepare :: ClassId -> Runtime -> ExceptT VMError IO Runtime
prepare classId rt =
  if isClassPrepared classId rt
  then return rt
  else do
    ExceptT . return $ updateClassFields --todo replace with 'except' when transformers = 0.5.6.2
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
