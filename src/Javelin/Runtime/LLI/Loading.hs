{-# LANGUAGE TemplateHaskell #-}

module Javelin.Runtime.LLI.Loading
where

import Javelin.Runtime.LLI.ClassPath
import Javelin.Runtime.Structures
import Javelin.ByteCode.Data
import Javelin.ByteCode.ClassFile (parseRaw)

import Data.Word (Word16)
import Data.Map as Map (insert, lookup, member, Map(..), empty, (!))
import Data.ByteString (ByteString, unpack)

import Control.Monad.Trans.Except (ExceptT(..), throwE, withExceptT)
import Control.Monad.Trans.Class (lift)
import Javelin.Util ((#))
import Data.Either.Utils (maybeToEither)
import Flow ((|>))
import Control.Lens hiding ((|>), (#))


-- 5.1 Deriving the Run-Time Constant Pool
--- The constant_pool table (§4.4) in the binary representation
--- of a class or interface is used to construct the run-time
--- constant pool upon class or interface creation (§5.3). 
deriveUtf8 :: [Constant] -> Word16 -> String
deriveUtf8 p idx = stringValue $ p # idx
-- note: fix, value at index could be not Utf8Info, stringValue not applicable
-- or throw an exception: invalid bytecode

deriveSymTable :: ConstantPool -> SymTable
deriveSymTable (ConstantPool p) = map (deriveRef p) p

deriveRef :: [Constant] -> Constant -> SymbolicReference
deriveRef p (ClassInfo idx) =
  ClassOrInterface $ deriveUtf8 p idx
deriveRef p (Fieldref classIdx nameAndTypeIdx) =
  FieldReference $ deriveFromClass classIdx nameAndTypeIdx p
deriveRef p (Methodref classIdx typeIdx) =
  ClassMethodReference $ deriveFromClass classIdx typeIdx p
deriveRef p (InterfaceMethodref classIdx typeIdx) =
  InterfaceMethodReference $ deriveFromClass classIdx typeIdx p
deriveRef p (MethodHandleInfo x y) = undefined
deriveRef p (MethodTypeInfo idx) =
  MethodTypeReference $ deriveUtf8 p idx
deriveRef p (InvokeDynamicInfo x y) = undefined
deriveRef p (StringInfo idx) =
  StringLiteral $ deriveUtf8 p idx
deriveRef p (DoubleInfo val) =
  DoubleLiteral val
-- note: repr in IEEE 754 double point
deriveRef p (FloatInfo val) =
  FloatLiteral val
-- note: repr in IEEE 754 single point
deriveRef p (LongInfo val) =
  LongLiteral val
deriveRef p (IntegerInfo val) =
  IntegerLiteral val
-- Following don't have referrers
deriveRef p (Utf8Info val) =
  StringLiteral val
deriveRef p (NameAndTypeInfo _ _) =
  EmptyLiteral
    
internString :: SymTable -> String -> (Word16, SymTable)
internString st str = case findString 0 str st of
  Just idx -> (idx, st)
  Nothing -> appendString str st
findString _ _ [] = Nothing
findString i str (s:st) = case s of
  (StringLiteral str') -> if str == str'
                          then Just i
                          else findString (i + 1) str st
  _ -> findString (i + 1) str st
appendString :: String -> SymTable -> (Word16, SymTable)
appendString str st = (fromIntegral $ length st, newSymTable)
  where newSymTable = st ++ [StringLiteral str]

deriveFromClass :: (Integral i) => i -> i -> [Constant] -> ClassPartReference
deriveFromClass classIdx nameAndTypeIdx p =
  let classInfo = p # classIdx
      nameAndTypeInfo = p # nameAndTypeIdx
      className = stringValue $ p # (nameIndex classInfo)
      memberName = stringValue $ p # (nameIndex nameAndTypeInfo)
      memberDescriptor = stringValue $ p # (nameAndTypeDescriptorIndex nameAndTypeInfo)
  in ClassPartReference className memberName memberDescriptor
-- note: stringValue usage, can fail for invalid bytecode; need a way to handle/notify



-- 5.3.5 Deriving a Class from a class File Representation
deriveClass :: ClassId -> Runtime -> ClassLoader -> ByteString -> ExceptT VMError IO Runtime
deriveClass request@(ClassId initCL name) rt defCL bs = do
  checkInitiatingClassLoader initCL name rt
  bc <- checkClassFileFormat bs rt
  checkClassVersion bc rt
  syms <- checkRepresentedClass name rt bc
  checkSuperClass request defCL bc syms rt
    >>= checkSuperInterfaces request defCL bc syms
    >>= recordClassLoading name bc syms initCL defCL


      
checkInitiatingClassLoader :: ClassLoader -> ClassName -> Runtime -> ExceptT VMError IO Runtime
checkInitiatingClassLoader initCL name rt = do
  if rt ^. loadedClasses . (to $ Map.member (ClassId initCL name))
    then throwE $ Linkage rt LinkageError
    else lift $ return rt

checkClassFileFormat :: ByteString -> Runtime -> ExceptT VMError IO ByteCode
checkClassFileFormat bs rt = let body = parseRaw $ unpack bs in
  case body of
    Left (_, _, msg) -> throwE $ Linkage rt  ClassFormatError
    Right (_, _, byteCode) -> lift $ return byteCode
    
checkClassVersion :: ByteCode -> Runtime -> ExceptT VMError IO ()
checkClassVersion bc rt = if minVer bc < 0 || majVer bc > 1050
                          then throwE $ Linkage rt UnsupportedClassVersionError
                          else lift $ return ()

checkRepresentedClass :: ClassName -> Runtime -> ByteCode -> ExceptT VMError IO SymTable
checkRepresentedClass name rt bc =
  let pool = constPool $ body bc
      symTable = deriveSymTable pool
      thisIndex = this $ body bc
  in case symTable # thisIndex of
    (ClassOrInterface actualName) -> if actualName == name
                                     then lift $ return symTable
                                     else throwE $ Linkage rt $
                                          NoClassDefFoundError (name ++ actualName ++ (show thisIndex))
    _ -> throwE $ Linkage rt ClassFormatError
-- last case is due to invalid bytecode; throw an exception and terminate?

checkSuperClass :: ClassId -> ClassLoader -> ByteCode -> SymTable -> Runtime -> ExceptT VMError IO Runtime
checkSuperClass request defCL bc sym rt =
  let superClassIdx = super $ body bc
      name = getName request
  in case (name, superClassIdx) of
    ("java/lang/Object", 0) -> lift $ return rt
    ("java/lang/Object", _) -> throwE $ Linkage rt $ ClassFormatError
    (_, 0) -> throwE $ Linkage rt $ ClassFormatError
    (_, idx) -> case sym # idx of
      -- what if other constructor? bytecode error
      (ClassOrInterface parent) -> do
        let parentId = ClassId defCL parent
        rt <- resolveClass parentId rt
        case isInterface rt parentId of
          Left error -> throwE error
          Right True -> throwE $ Linkage rt IncompatibleClassChangeError
          Right False -> let thisAccessFlags = classAccessFlags $ body $ bc
                             thisIsInterface = elem AccInterface thisAccessFlags
                         in case (thisIsInterface, parent) of
                           (True, "java/lang/Object") -> lift $ return rt
                           (True, _) -> throwE $ InternalError rt InterfaceMustHaveObjectAsSuperClass
                           (False, parentName) -> if parentName == name
                                                  then throwE $ Linkage rt ClassCircularityError
                                                  else lift $ return rt
      _ -> throwE $ undefined


checkSuperInterfaces :: ClassId -> ClassLoader -> ByteCode -> SymTable -> Runtime -> ExceptT VMError IO Runtime
checkSuperInterfaces request defCL bc syms rt = let superInterfaces = interfaces $ body bc
                                                in foldl (checkSuperInterface request defCL bc syms) (lift $ return rt) superInterfaces
checkSuperInterface :: ClassId -> ClassLoader -> ByteCode -> SymTable -> ExceptT VMError IO Runtime -> Word16 -> ExceptT VMError IO Runtime
checkSuperInterface request defCL bc sym eitherRt interfaceIdx = do
  rt <- eitherRt
  let name = getName request
  case sym # interfaceIdx of
    --other constructor? bytecode error
    (ClassOrInterface parent) -> do
      let parentClassId = ClassId defCL parent
      rt <- resolveClass parentClassId rt
      case isInterface rt parentClassId of
        Left e -> throwE e
        Right False -> throwE $ Linkage rt IncompatibleClassChangeError
        Right True -> if parent == name
                     then throwE $ Linkage rt ClassCircularityError
                     else lift $ return rt
    _ -> throwE $ undefined

recordClassLoading :: ClassName -> ByteCode -> SymTable -> ClassLoader -> ClassLoader -> Runtime -> ExceptT VMError IO Runtime
recordClassLoading name bc sym defCL initCL rt =
    let c = LoadedClass defCL initCL (name, defCL) sym bc (reformatWithSymlinks bc) (getFields bc sym) (getMethods bc sym)
    in addLoadedClass (ClassId initCL name) c rt

reformatWithSymlinks :: ByteCode -> Class
reformatWithSymlinks bc =
  let classBody = body bc
      cp = constPool classBody
      sym = deriveSymTable cp
      ClassOrInterface className = sym # (this classBody)
      superIdx = super classBody
      superName = if superIdx == 0
                  then "java/lang/Object"
                  else classOrInterfaceName $ sym # superIdx
      classAttrs = attrs classBody
      classInterfaces = classBody |> interfaces |> map (classOrInterfaceName .  (sym #))
      classFields = classBody |> fields |> map (deriveClassFields sym classBody)
      classMethods = classBody |> methods |> map (deriveClassMethods sym classBody)
  in Class className superName [] "sourceFile" (ClassAccess False False False False False False False False) classFields classMethods

deriveClassFields :: SymTable -> ClassBody -> FieldInfo -> Field
deriveClassFields sym body fieldInfo =
  let nameIdx = fieldNameIndex fieldInfo
      descriptorIndex = fieldDescriptorIndex fieldInfo
      fieldName = string $ sym # nameIdx
      fieldDescriptor = string $ sym # descriptorIndex
  in Field fieldName fieldDescriptor Nothing (FieldAccess False False False False False False False False False)

deriveClassMethods :: SymTable -> ClassBody -> MethodInfo -> Method
deriveClassMethods sym body fieldInfo = Method "name" "descriptor" (MethodAccess False False False False False False False False False False False False) []

getFields :: ByteCode -> SymTable -> Map PartReference FieldInfo
getFields bc sym =
  bc
  |> body
  |> fields
  |> foldl fieldsFold Map.empty
  where fieldsFold = \acc field -> Map.insert (buildPartReference field) field acc
        buildPartReference (FieldInfo _ nameIdx descrIdx _ ) =
          PartReference (string $ sym # nameIdx) (string $ sym # descrIdx)

getMethods :: ByteCode -> SymTable -> Map PartReference MethodInfo
getMethods bc sym =   bc
  |> body
  |> methods
  |> foldl fieldsFold Map.empty
  where fieldsFold = \acc field -> Map.insert (buildPartReference field) field acc
        buildPartReference (MethodInfo _ nameIdx descrIdx _ ) =
          PartReference (string $ sym # nameIdx) (string $ sym # descrIdx)

-- 5.3 Creation and Loading top level code
load :: ClassId -> Runtime -> ExceptT VMError IO Runtime
load request@(ClassId initCL name) rt =
  let loaderFn = if isArray name then loadArray else loadClass
  in case rt ^. loadedClasses . to (Map.lookup request) of
    Just _ -> lift $ return rt
    Nothing -> loaderFn request rt

loadClass :: ClassLoadMethod
loadClass request@(ClassId BootstrapClassLoader _) rt = loadClassWithBootstrap request rt
loadClass request rt = loadClassWithUserDefCL request rt

isArray :: ClassName -> Bool
isArray name = head name == '['

type ClassLoadMethod = ClassId -> Runtime -> ExceptT VMError IO Runtime


wrapClassNotFound :: Runtime -> VMError -> VMError
wrapClassNotFound rt x@(ClassNotFoundException _) = Linkage rt $ NoClassDefFoundClassNotFoundError x
wrapClassNotFound _ x = x

-- 5.3.1 Loading Using the Bootstrap Class Loader
loadClassWithBootstrap :: ClassId -> Runtime -> ExceptT VMError IO Runtime
loadClassWithBootstrap request@(ClassId _ name) rt@(Runtime {_classPathLayout = layout}) = 
  do
    bytes <- withExceptT (wrapClassNotFound rt) (getClassBytes name $ rt ^. classPathLayout)
    deriveClass request rt BootstrapClassLoader bytes

-- 5.3.2 Loading Using a User-defined Class Loader
loadClassWithUserDefCL :: ClassLoadMethod
loadClassWithUserDefCL request rt = undefined

-- 5.3.3 Creating Array Classes
loadArray :: ClassLoadMethod
loadArray request@(ClassId initCL name) rt = do
  let arrayRepr@(ArrayRepr {depth = d}) = parseSignature name
  (rt, defCL) <- loadAndGetDefCLOfComponentType request rt arrayRepr
  let c = LoadedArrayClass defCL initCL (name, defCL) d
  addLoadedClass (ClassId initCL name) c rt

loadAndGetDefCLOfComponentType :: ClassId -> Runtime -> ArrayRepr -> ExceptT VMError IO (Runtime, ClassLoader)
loadAndGetDefCLOfComponentType classId@(ClassId initCL name) rt repr@(ArrayRepr {depth = d, componentType = refType}) = do
  case refType of
    Just ref -> do
      let componentClassId = ClassId initCL ref
      rt <- loadClass componentClassId rt
      definingComponentCL <- ExceptT . return $ getDefiningClassLoader rt componentClassId
      lift $ return (rt, definingComponentCL)
    Nothing ->  return (rt, BootstrapClassLoader)

-- to be rewritten with introduction of parsed types
data ArrayRepr = ArrayRepr { depth :: Int,
                             componentType :: Maybe String }
parseSignature :: String -> ArrayRepr
parseSignature name = bla name (ArrayRepr 0 Nothing)
bla :: String -> ArrayRepr -> ArrayRepr
bla [] r = r
bla ('[' : s) r = bla s r{depth = (depth r) + 1}
bla ('L' : s) r = r{componentType = Just $ take ((length s) - 1) s}
bla (_ : xs) r = bla xs r
  
-- 5.3.4 Loading Constraints
-- not implemented yet


-- first need to check whether resolution already happened, 3 possible outcomes:
resolveClass :: ClassId -> Runtime -> ExceptT VMError IO Runtime
resolveClass request@(ClassId initCL name) rt =
  case rt ^? classResolving . ix request of
    Just (ClassResOk _ _) -> return rt
    Just (ClassResFail err) -> throwE err
    Nothing -> do
      rt <- load request rt
      let (ArrayRepr d mt) = parseSignature name
      if d > 0
        then case mt of
        Nothing -> lift $ return rt
        Just t -> resolveClass (ClassId initCL t) rt
        else lift $ return rt


  
-- API fn for resolving a field
resolveField :: ClassId -> PartReference -> Runtime -> ExceptT VMError IO Runtime
resolveField classId partRef rt = do
  r <- resolveFieldSearch classId partRef rt
  case r of
    Nothing -> throwE $ Linkage rt NoSuchFieldError
    Just rt -> return rt

-- Recursive fn for looking for a field
resolveFieldSearch :: ClassId -> PartReference -> Runtime -> ExceptT VMError IO (Maybe Runtime)
resolveFieldSearch classId partRef rt = do
  rt <- resolveClass classId rt
  maybeRt <- resolveFieldInClass classId partRef rt
  case maybeRt of
    Nothing -> resolveClassFieldInParents classId partRef rt
    Just rt -> return $ Just rt

-- Resolving field in a specific class

-- todo terminate on Object parent; for interfaces?
-- todo what error to record?
resolveFieldInClass :: ClassId -> PartReference -> Runtime -> ExceptT VMError IO (Maybe Runtime)
resolveFieldInClass classId partRef rt = 
  if classDefinesField classId partRef rt
  then Just <$> addResolvedClassField classId partRef rt
  else return Nothing

resolveClassFieldInParents :: ClassId -> PartReference -> Runtime -> ExceptT VMError IO (Maybe Runtime)
resolveClassFieldInParents classId partRef rt = do
  superInterfaces <- ExceptT $ return $ getSuperInterfaces rt classId
  interfaceResolution <- superInterfaces
                         |> map (\superInterface -> ClassId (getInitCL classId) superInterface)
                         |> map (\classId -> resolveFieldSearch classId partRef rt)
                         |> findSuccessfulResolution
                         |> ExceptT
  case interfaceResolution of
    Just r -> return $ Just r
    Nothing -> case getSuperClass rt classId of
                 Left err -> throwE err
                 Right superClass -> resolveFieldSearch (ClassId (getInitCL classId) superClass) partRef rt

findSuccessfulResolution :: [ExceptT VMError IO (Maybe Runtime)] -> IO (Either VMError (Maybe Runtime))
findSuccessfulResolution [] = return $ Right Nothing
findSuccessfulResolution ((ExceptT io):xs) = do
  eitherResult <- io
  case eitherResult of
    Left err -> return $ Left err
    Right (Just rt) -> return $ Right $ Just rt
    Right Nothing -> findSuccessfulResolution xs


type MethodResolution = Runtime -> ClassId -> PartReference -> ExceptT VMError IO (Maybe Runtime)

resolveMethod :: Runtime -> ClassId -> PartReference -> ExceptT VMError IO Runtime
resolveMethod rt classId partRef = do
  rt <- resolveClass classId rt
  requireNotInterface rt classId
  inClass <- resolveMethodInClassOrSuperclass rt classId partRef
  case inClass of
    Just rt -> return rt
    Nothing -> do
      inInterfaces <- resolveMethodInSuperInterfaces rt classId partRef
      case inInterfaces of
        Just rt -> undefined
        Nothing -> undefined

resolveMethodInClassOrSuperclass :: MethodResolution
resolveMethodInClassOrSuperclass = undefined

-- not important for phase I, skipping
resolveMethodSignPolymorphic ::MethodResolution
resolveMethodSignPolymorphic rt classId partRef = lift $ return Nothing

resolveMethodNameDescriptor :: MethodResolution
resolveMethodNameDescriptor rt classId partRef = undefined

resolveInSuperClass :: MethodResolution
resolveInSuperClass = undefined

resolveMethodInSuperInterfaces :: MethodResolution
resolveMethodInSuperInterfaces = undefined

requireNotInterface :: Runtime -> ClassId -> ExceptT VMError IO Runtime
requireNotInterface rt classId = case isInterface rt classId of
  Right True -> lift $ return rt
  Right False -> throwE $ Linkage rt IncompatibleClassChangeError
  Left err -> throwE err
