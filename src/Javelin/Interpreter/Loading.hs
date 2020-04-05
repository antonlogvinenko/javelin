{-# LANGUAGE TemplateHaskell #-}

module Javelin.Interpreter.Loading where

import Javelin.Interpreter.ClassPathLoading (getClassBytes)
import Javelin.Lib.ByteCode.ClassFile (parseRaw)
import Javelin.Lib.ByteCode.Data
import Javelin.Lib.ByteCode.DescSign
import Javelin.Lib.Structures

import Data.ByteString as BSS (ByteString, unpack)
import Data.ByteString.Lazy as BSL (readFile, toStrict)
import Data.Map as Map (Map(..), (!), empty, insert, lookup, member)
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe)
import Data.Word (Word16)

import Codec.Archive.Zip
import Control.Lens ((^.), (^?), ix, to)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Data.Either.Utils (maybeToEither)
import Flow ((|>))
import Javelin.Capability.Classes
import Javelin.Interpreter.JVMApp

instance ClassLoading JVM where
  loadClass classId rt = liftIO $ runExceptT $ loadClassOrArray classId rt
  initClass classId rt = liftIO $ runExceptT $ initClassOrArray classId rt
  getClassBytes name classPath = liftIO $ getClassBytesIO name classPath
  -- 5.3 Creation and Loading top level code

getClassBytesIO name (ClassPathLayout classes _) = do
  case classes |> Map.lookup name |> maybeToEither (ClassNotFoundException name) of
    Left err -> return $ Left err
    Right v -> getClassFromSource name v

-- ExceptT (Either VMError (IO ByteString))
getClassFromSource ::
     ClassName -> ClassSource -> IO (Either VMError BSS.ByteString)
getClassFromSource name (ClassFile path)
  --todo should we check that class x.y.z.class is loaded from x/y/z.class file?
 = Right . BSL.toStrict <$> BSL.readFile path
--  if classToPath name == path
--    else throwE $ ClassNotFoundException "cake"
getClassFromSource name (JarFile path) = do
  raw <- BSL.readFile path
  return $ maybeToEither (ClassNotFoundException name) $ do
    let arc = toArchive raw
        classPath = classToPath name
    entry <- findEntryByPath classPath arc
    return $ BSL.toStrict $ fromEntry entry

classToPath :: ClassName -> FilePath
classToPath name = name ++ ".class"

initClassOrArray :: ClassId -> Runtime -> ExceptT VMError IO Runtime
initClassOrArray = linking

loadClassOrArray :: ClassId -> Runtime -> ExceptT VMError IO Runtime
loadClassOrArray request@(ClassId initCL name) rt =
  let loaderFn =
        if isArray name
          then loadArray
          else loadClazz
   in case rt ^. loadedClasses . to (Map.lookup request) of
        Just _ -> lift $ return rt
        Nothing -> loaderFn request rt

linking :: ClassId -> Runtime -> ExceptT VMError IO Runtime
linking classId rt = verify classId rt >>= prepare classId

verify :: ClassId -> Runtime -> ExceptT VMError IO Runtime
verify classId rt = loadClassOrArray classId rt --todo not doing actual verification yet

prepare :: ClassId -> Runtime -> ExceptT VMError IO Runtime
prepare classId rt =
  if isClassPrepared classId rt
    then return rt
    else ExceptT . return $ Right $ markClassPrepared classId $
         updateClassFields --todo replace with 'except' when transformers = 0.5.6.2
           classId
           rt
           (map initStaticField . filter (isFieldStatic . fieldAccess))

initStaticField :: Field -> Field
initStaticField field =
  let value =
        case fieldType field of
          BaseType bt -> baseDefaultValues ! bt
          _ -> nullReference
   in field {staticValue = Just value}

-- 5.1 Deriving the Run-Time Constant Pool
--- The constant_pool table (§4.4) in the binary representation
--- of a class or interface is used to construct the run-time
--- constant pool upon class or interface creation (§5.3).
deriveUtf8 :: [Constant] -> Word16 -> String
deriveUtf8 p idx = stringValue $ p `at` idx

-- note: fix, value at index could be not Utf8Info, stringValue not applicable
-- or throw an exception: invalid bytecode
deriveSymTable :: ConstantPool -> SymTable
deriveSymTable (ConstantPool p) = map (deriveRef p) p

deriveRef :: [Constant] -> Constant -> SymbolicReference
deriveRef p (ClassInfo idx) = ClassOrInterface $ deriveUtf8 p idx
deriveRef p (Fieldref classIdx nameAndTypeIdx) =
  FieldReference $ deriveFromClass classIdx nameAndTypeIdx p
deriveRef p (Methodref classIdx typeIdx) =
  ClassMethodReference $ deriveFromClass classIdx typeIdx p
deriveRef p (InterfaceMethodref classIdx typeIdx) =
  InterfaceMethodReference $ deriveFromClass classIdx typeIdx p
deriveRef p (MethodHandleInfo x y) = undefined
deriveRef p (MethodTypeInfo idx) = MethodTypeReference $ deriveUtf8 p idx
deriveRef p (InvokeDynamicInfo x y) = undefined
deriveRef p (StringInfo idx) = StringLiteral $ deriveUtf8 p idx
deriveRef p (DoubleInfo val) = DoubleLiteral val
-- note: repr in IEEE 754 double point
deriveRef p (FloatInfo val) = FloatLiteral val
-- note: repr in IEEE 754 single point
deriveRef p (LongInfo val) = LongLiteral val
deriveRef p (IntegerInfo val) = IntegerLiteral val
-- Following don't have referrers
deriveRef p (Utf8Info val) = StringLiteral val
deriveRef p (NameAndTypeInfo _ _) = EmptyLiteral

internString :: SymTable -> String -> (Word16, SymTable)
internString st str =
  case findString 0 str st of
    Just idx -> (idx, st)
    Nothing -> appendString str st

findString _ _ [] = Nothing
findString i str (s:st) =
  case s of
    (StringLiteral str') ->
      if str == str'
        then Just i
        else findString (i + 1) str st
    _ -> findString (i + 1) str st

appendString :: String -> SymTable -> (Word16, SymTable)
appendString str st = (fromIntegral $ length st, newSymTable)
  where
    newSymTable = st ++ [StringLiteral str]

deriveFromClass :: (Integral i) => i -> i -> [Constant] -> ClassPartReference
deriveFromClass classIdx nameAndTypeIdx p =
  let classInfo = p `at` classIdx
      nameAndTypeInfo = p `at` nameAndTypeIdx
      className = stringValue $ p `at` nameIndex classInfo
      memberName = stringValue $ p `at` nameIndex nameAndTypeInfo
      memberDescriptor =
        stringValue $ p `at` nameAndTypeDescriptorIndex nameAndTypeInfo
   in ClassPartReference (PartReference className memberName) memberDescriptor

-- note: stringValue usage, can fail for invalid bytecode; need a way to handle/notify
-- 5.3.5 Deriving a Class from a class File Representation
checkAndRecordLoadedClass ::
     ClassId
  -> Runtime
  -> ClassLoader
  -> BSS.ByteString
  -> ExceptT VMError IO Runtime
checkAndRecordLoadedClass request@(ClassId initCL name) rt defCL bs = do
  checkInitiatingClassLoader initCL name rt
  bc <- checkClassFileFormat bs rt
  checkClassVersion bc rt
  classInfo <- ExceptT . return $ deriveClass bc
  checkSuperClass request defCL rt classInfo >>=
    checkSuperInterfaces request defCL classInfo >>=
    recordClassLoading name classInfo initCL defCL

checkInitiatingClassLoader ::
     ClassLoader -> ClassName -> Runtime -> ExceptT VMError IO Runtime
checkInitiatingClassLoader initCL name rt = do
  if rt ^. loadedClasses . to (Map.member (ClassId initCL name))
    then throwE $ Linkage rt LinkageError
    else lift $ return rt

checkClassFileFormat :: BSS.ByteString -> Runtime -> ExceptT VMError IO ByteCode
checkClassFileFormat bs rt =
  let body = parseRaw $ unpack bs
   in case body of
        Left (_, _, msg) -> throwE $ Linkage rt ClassFormatError
        Right (_, _, byteCode) -> lift $ return byteCode

--todo also check class name is correct
checkClassVersion :: ByteCode -> Runtime -> ExceptT VMError IO ()
checkClassVersion bc rt =
  if minVer bc < 0 || majVer bc > 1050
    then throwE $ Linkage rt UnsupportedClassVersionError
    else lift $ return ()

checkRepresentedClass ::
     ClassName -> Runtime -> ByteCode -> ExceptT VMError IO SymTable
checkRepresentedClass name rt bc =
  let pool = constPool $ body bc
      symTable = deriveSymTable pool
      thisIndex = this $ body bc
   in case symTable `at` thisIndex of
        (ClassOrInterface actualName) ->
          if actualName == name
            then lift $ return symTable
            else throwE $ Linkage rt $
                 NoClassDefFoundError (name ++ actualName ++ show thisIndex)
        _ -> throwE $ Linkage rt ClassFormatError

-- last case is due to invalid bytecode; throw an exception and terminate?
checkSuperClass ::
     ClassId -> ClassLoader -> Runtime -> Class -> ExceptT VMError IO Runtime
checkSuperClass request defCL rt classInfo =
  let classSuperName = superName classInfo
      name = getName request
   in case (name, classSuperName) of
        ("java/lang/Object", Nothing) -> lift $ return rt
        ("java/lang/Object", _) -> throwE $ Linkage rt ClassFormatError
        (_, Nothing) -> throwE $ Linkage rt ClassFormatError
        (_, Just parent) -> do
          let parentId = ClassId defCL parent
          rt <- resolveClass parentId rt
          case isInterface rt parentId of
            Left error -> throwE error
            Right True -> throwE $ Linkage rt IncompatibleClassChangeError
            Right False ->
              let thisIsInterface =
                    classInfo |> classVisibility |> isClassInterface
               in case (thisIsInterface, parent) of
                    (True, "java/lang/Object") -> lift $ return rt
                    (True, _) ->
                      throwE $
                      InternalError rt InterfaceMustHaveObjectAsSuperClass
                    (False, parentName) ->
                      if parentName == name
                        then throwE $ Linkage rt ClassCircularityError
                        else lift $ return rt

checkSuperInterfaces ::
     ClassId -> ClassLoader -> Class -> Runtime -> ExceptT VMError IO Runtime
checkSuperInterfaces request defCL classInfo rt =
  let superInterfaces = classInterfaces classInfo
   in foldl
        (checkSuperInterface request defCL classInfo)
        (lift $ return rt)
        superInterfaces

checkSuperInterface ::
     ClassId
  -> ClassLoader
  -> Class
  -> ExceptT VMError IO Runtime
  -> String
  -> ExceptT VMError IO Runtime
checkSuperInterface request defCL classInfo eitherRt parentInterface = do
  rt <- eitherRt
  let name = getName request
  let parentClassId = ClassId defCL parentInterface
  rt <- resolveClass parentClassId rt
  case isInterface rt parentClassId of
    Left e -> throwE e
    Right False -> throwE $ Linkage rt IncompatibleClassChangeError
    Right True ->
      if parentInterface == name
        then throwE $ Linkage rt ClassCircularityError
        else lift $ return rt

recordClassLoading ::
     ClassName
  -> Class
  -> ClassLoader
  -> ClassLoader
  -> Runtime
  -> ExceptT VMError IO Runtime
recordClassLoading name classInfo defCL initCL rt =
  let c = LoadedClass defCL initCL (name, defCL) classInfo
   in ExceptT $ return $ Right $ addLoadedClass (ClassId initCL name) c rt

deriveClass :: ByteCode -> Either VMError Class
deriveClass bc =
  let classBody = body bc
      cp = constPool classBody
      sym = deriveSymTable cp
      ClassOrInterface className = sym `at` this classBody
      superIdx = super classBody
      superName =
        if superIdx == 0
          then Nothing
          else Just $ classOrInterfaceName $ sym `at` superIdx
      classAttrs = attrs classBody
      accessFlags = classAccessFlags classBody
      classInterfaces =
        classBody |> interfaces |> map (classOrInterfaceName . (sym `at`))
      classMethods =
        classBody |> methods |>
        map (checkAndRecordLoadedClassMethods sym classBody)
   in do classFields <-
           mapM
             (checkAndRecordLoadedClassFields sym classBody)
             (fields classBody)
         return $
           Class
             sym
             className
             superName
             classInterfaces
             "sourceFile"
             (deriveClassAccess accessFlags)
             classFields
             classMethods

deriveClassAccess :: [ClassAccessFlags] -> ClassAccess
deriveClassAccess flags =
  ClassAccess
    (elem AccPublic flags)
    (elem AccFinal flags)
    (elem AccSuper flags)
    (elem AccInterface flags)
    (elem AccAbstract flags)
    (elem AccSynthetic flags)
    (elem AccAnn flags)
    (elem AccEnum flags)

checkAndRecordLoadedClassFields ::
     SymTable -> ClassBody -> FieldInfo -> Either VMError Field
checkAndRecordLoadedClassFields sym body fieldInfo =
  let fieldName = fieldInfo |> fieldNameIndex |> (sym `at`) |> string
      fieldDescriptor =
        fieldInfo |> fieldDescriptorIndex |> (sym `at`) |> string
      fieldAttrs = attrs body
      parsedFieldDescriptor = parseFieldDescriptor fieldDescriptor
   in case parsedFieldDescriptor of
        Left err -> undefined
        Right (FieldDescriptor descriptor) ->
          Right $
          Field
            fieldName
            fieldDescriptor
            descriptor
            (deriveDefaultFieldValue sym fieldAttrs)
            (deriveFieldAccess $ fieldAccessFlags fieldInfo)
            Nothing

deriveDefaultFieldValue :: SymTable -> [AttrInfo] -> Maybe ConstantValue
deriveDefaultFieldValue sym [] = Nothing
deriveDefaultFieldValue sym a@((ConstantValue idx):as) =
  Just $ ConstantString ""
deriveDefaultFieldValue sym a@(_:as) = deriveDefaultFieldValue sym as

deriveFieldAccess :: [FieldInfoAccessFlag] -> FieldAccess
deriveFieldAccess flags =
  FieldAccess
    (elem FieldPublic flags)
    (elem FieldPrivate flags)
    (elem FieldProtected flags)
    (elem FieldStatic flags)
    (elem FieldFinal flags)
    (elem FieldVolatile flags)
    (elem FieldTransient flags)
    (elem FieldSynthetic flags)
    (elem FieldEnum flags)

checkAndRecordLoadedClassMethods ::
     SymTable -> ClassBody -> MethodInfo -> Method
checkAndRecordLoadedClassMethods sym body methodInfo =
  let methodName = methodInfo |> methodNameIndex |> (sym `at`) |> string
      methodDescriptor =
        methodInfo |> methodInfoDescriptorIndex |> (sym `at`) |> string
      attrs = methodAttrs methodInfo
      codeAttr = head $ filter isCodeAttrInfo attrs
   in Method
        methodName
        methodDescriptor
        (deriveMethodAccess $ methodAccessFlags methodInfo)
        (maxStack codeAttr)
        (maxLocals codeAttr)
        (code codeAttr)
        (exceptionTable codeAttr)
        []

deriveMethodAccess :: [MethodInfoAccessFlag] -> MethodAccess
deriveMethodAccess flags =
  MethodAccess
    (elem MethodPublic flags)
    (elem MethodPrivate flags)
    (elem MethodProtected flags)
    (elem MethodStatic flags)
    (elem MethodFinal flags)
    (elem MethodSynchronized flags)
    (elem MethodBridge flags)
    (elem MethodVarargs flags)
    (elem MethodNative flags)
    (elem MethodAbstract flags)
    (elem MethodStrict flags)
    (elem MethodSynthetic flags)

getFields :: ByteCode -> SymTable -> Map PartReference FieldInfo
getFields bc sym = bc |> body |> fields |> foldl fieldsFold Map.empty
  where
    fieldsFold = \acc field -> Map.insert (buildPartReference field) field acc
    buildPartReference (FieldInfo _ nameIdx descrIdx _) =
      PartReference (string $ sym `at` nameIdx) (string $ sym `at` descrIdx)

getMethods :: ByteCode -> SymTable -> Map PartReference MethodInfo
getMethods bc sym = bc |> body |> methods |> foldl fieldsFold Map.empty
  where
    fieldsFold = \acc field -> Map.insert (buildPartReference field) field acc
    buildPartReference (MethodInfo _ nameIdx descrIdx _) =
      PartReference (string $ sym `at` nameIdx) (string $ sym `at` descrIdx)

loadClazz :: ClassId -> Runtime -> ExceptT VMError IO Runtime
loadClazz request@(ClassId BootstrapClassLoader _) rt =
  loadClassWithBootstrap request rt
loadClazz request rt = loadClassWithUserDefCL request rt

isArray :: ClassName -> Bool
isArray name = head name == '['

wrapClassNotFound :: Runtime -> VMError -> VMError
wrapClassNotFound rt x@(ClassNotFoundException _) =
  Linkage rt $ NoClassDefFoundClassNotFoundError x
wrapClassNotFound _ x = x

-- 5.3.1 Loading Using the Bootstrap Class Loader
loadClassWithBootstrap :: ClassId -> Runtime -> ExceptT VMError IO Runtime
loadClassWithBootstrap request@(ClassId _ name) rt@Runtime {_classPathLayout = layout} = do
  bytes <- ExceptT $ getClassBytesIO name $ rt ^. classPathLayout
  checkAndRecordLoadedClass request rt BootstrapClassLoader bytes

-- 5.3.2 Loading Using a User-defined Class Loader
loadClassWithUserDefCL :: ClassId -> Runtime -> ExceptT VMError IO Runtime
loadClassWithUserDefCL request rt = undefined

-- 5.3.3 Creating Array Classes
loadArray :: ClassId -> Runtime -> ExceptT VMError IO Runtime
loadArray request@(ClassId initCL name) rt = do
  let arrayRepr@(ArrayRepr {depth = d}) = parseSignature name
  (rt, defCL) <- loadAndGetDefCLOfComponentType request rt arrayRepr
  let c = LoadedArrayClass defCL initCL (name, defCL) d
  ExceptT $ return $ Right $ addLoadedClass (ClassId initCL name) c rt

loadAndGetDefCLOfComponentType ::
     ClassId
  -> Runtime
  -> ArrayRepr
  -> ExceptT VMError IO (Runtime, ClassLoader)
loadAndGetDefCLOfComponentType classId@(ClassId initCL name) rt repr@(ArrayRepr { depth = d
                                                                                , componentType = refType
                                                                                }) = do
  case refType of
    Just ref -> do
      let componentClassId = ClassId initCL ref
      rt <- loadClazz componentClassId rt
      definingComponentCL <-
        ExceptT . return $ getDefiningClassLoader rt componentClassId
      lift $ return (rt, definingComponentCL)
    Nothing -> return (rt, BootstrapClassLoader)

-- to be rewritten with introduction of parsed types
data ArrayRepr =
  ArrayRepr
    { depth :: Int
    , componentType :: Maybe String
    }

parseSignature :: String -> ArrayRepr
parseSignature name = bla name (ArrayRepr 0 Nothing)

bla :: String -> ArrayRepr -> ArrayRepr
bla [] r = r
bla ('[':s) r = bla s r {depth = depth r + 1}
bla ('L':s) r = r {componentType = Just $ take (length s - 1) s}
bla (_:xs) r = bla xs r

-- 5.3.4 Loading Constraints
-- not implemented yet
-- first need to check whether resolution already happened, 3 possible outcomes:
resolveClass :: ClassId -> Runtime -> ExceptT VMError IO Runtime
resolveClass request@(ClassId initCL name) rt =
  case rt ^? classResolving . ix request of
    Just (ClassResOk _ _) -> return rt
    Just (ClassResFail err) -> throwE err
    Nothing -> do
      rt <- loadClassOrArray request rt
      let (ArrayRepr d mt) = parseSignature name
      if d > 0
        then case mt of
               Nothing -> lift $ return rt
               Just t -> resolveClass (ClassId initCL t) rt
        else lift $ return rt

-- API fn for resolving a field
resolveField ::
     ClassId -> PartReference -> Runtime -> ExceptT VMError IO Runtime
resolveField classId partRef rt = do
  rt <- resolveClass classId rt
  ExceptT $ return $ do
    mowner <- resolveFieldSearch classId partRef rt
    case mowner of
      Nothing -> Left $ Linkage rt NoSuchFieldError
      Just owner ->
        Right $
        addResolvedClassField classId (ClassPartReference partRef owner) rt

-- Recursive fn for looking for a field
resolveFieldSearch ::
     ClassId -> PartReference -> Runtime -> Either VMError (Maybe String)
resolveFieldSearch classId partRef rt = do
  mowner <- resolveFieldInClass classId partRef rt
  case mowner of
    Nothing -> resolveClassFieldInParents classId partRef rt
    Just owner -> return $ Just owner

-- Resolving field in a specific class
-- todo terminate on Object parent; for interfaces?
-- todo what error to record?
resolveFieldInClass ::
     ClassId -> PartReference -> Runtime -> Either VMError (Maybe String)
resolveFieldInClass classId@(ClassId _ owner) partRef rt =
  if classDefinesField classId (ClassPartReference partRef owner) rt
    then return $ Just owner
    else return Nothing

resolveClassFieldInParents ::
     ClassId -> PartReference -> Runtime -> Either VMError (Maybe String)
resolveClassFieldInParents classId partRef rt = do
  superInterfaces <- getSuperInterfaces rt classId
  interfaceResolution <-
    superInterfaces |>
    map (\superInterface -> ClassId (getInitCL classId) superInterface) |>
    map (\classId -> resolveFieldSearch classId partRef rt) |>
    findSuccessfulResolution
  case interfaceResolution of
    Just r -> return $ Just r
    Nothing ->
      case getSuperClass rt classId of
        Left err -> Left err
        Right Nothing -> return Nothing
        Right (Just superClass) ->
          resolveFieldSearch (ClassId (getInitCL classId) superClass) partRef rt

findSuccessfulResolution ::
     [Either VMError (Maybe String)] -> Either VMError (Maybe String)
findSuccessfulResolution [] = Right Nothing
findSuccessfulResolution (either:xs) =
  case either of
    Left err -> Left err
    Right Nothing -> findSuccessfulResolution xs
    Right owner -> Right owner

type MethodResolution
   = Runtime -> PartReference -> ClassId -> Class -> Either VMError (Maybe String)

resolveMethod ::
     Runtime -> PartReference -> ClassId -> ExceptT VMError IO Runtime
resolveMethod rt partRef classId = do
  rt <- resolveClass classId rt
  ExceptT $ return $ do
    classInfo <- getClass rt classId
    mclass <- resolveMethodSearch rt partRef classId classInfo
    case mclass of
      Nothing -> Left $ Linkage rt NoSuchMethodError
      Just owner ->
        Right $
        addResolvedClassMethod classId (ClassPartReference partRef owner) rt

resolveMethodSearch :: MethodResolution
resolveMethodSearch rt partRef classId classInfo = do
  requireNotInterface rt classId
  mclass <- resolveMethodInClassOrSuperclass rt partRef classId classInfo
  case mclass of
    Just _ -> return mclass
    Nothing -> resolveMethodInSuperInterfaces rt partRef classId classInfo

resolveMethodInClassOrSuperclass :: MethodResolution
resolveMethodInClassOrSuperclass rt partRef classId classInfo = do
  mclass <- resolveMethodSignPolymorphic rt partRef classId classInfo
  case mclass of
    Just _ -> return mclass
    Nothing -> do
      mclass <- resolveMethodNameDescriptor rt partRef classId classInfo
      case mclass of
        Nothing -> resolveInSuperClass rt partRef classId classInfo
        Just _ -> return mclass

-- not important for phase I, skipping
resolveMethodSignPolymorphic :: MethodResolution
resolveMethodSignPolymorphic rt partRef classId classInfo = return Nothing

resolveMethodNameDescriptor :: MethodResolution
resolveMethodNameDescriptor rt partRef classId classInfo =
  let matchedMethods =
        classInfo |> methodsList |> filter (methodMatches partRef)
   in return $
      case matchedMethods of
        (m:_) -> Just $ getName classId
        [] -> Nothing

methodMatches :: PartReference -> Method -> Bool
methodMatches partRef@(PartReference name descr) m =
  methodName m == name && methodDescriptor m == descr

resolveInSuperClass :: MethodResolution
resolveInSuperClass rt partRef classId@(ClassId initCl name) classInfo =
  case superName classInfo of
    Nothing -> return Nothing
    Just parentClassName ->
      resolveMethodSearch rt partRef (ClassId initCl parentClassName) classInfo

resolveMethodInSuperInterfaces :: MethodResolution
resolveMethodInSuperInterfaces rt partRef classId classInfo = do
  mclass <- resolveMaxSpecificSuperinterfaceMethod rt partRef classId classInfo
  case mclass of
    Nothing ->
      resolveNonPrivateNonStaticSuperinterfaceMethod
        rt
        partRef
        classId
        classInfo
    Just _ -> return mclass

resolveMaxSpecificSuperinterfaceMethod :: MethodResolution
resolveMaxSpecificSuperinterfaceMethod rt partRef classId@(ClassId init name) classInfo =
  return $
  case maxSpecific rt partRef classId of
    Just (Just owner) -> Just owner
    _ -> Nothing

maxSpecific :: Runtime -> PartReference -> ClassId -> Maybe (Maybe String)
maxSpecific rt partRef classId@(ClassId initCL _) =
  case getClass rt classId of
    Left error -> Nothing
    Right classInfo ->
      case classInfo |> methodsList |> filter (methodMatches partRef) of
        [method] -> classInfo |> className |> Just |> Just
        [] ->
          let found =
                classInfo |> classInterfaces |> map (ClassId initCL) |>
                map (maxSpecific rt partRef)
              failures = found |> filter isNothing |> length
              successes = found |> catMaybes |> filter isJust
              successesLength = length successes
           in if failures > 0
                then Nothing
                else case successesLength of
                       0 -> Just Nothing
                       1 -> successes |> head |> Just
                       _ -> Nothing
        _ -> Nothing

resolveNonPrivateNonStaticSuperinterfaceMethod :: MethodResolution
resolveNonPrivateNonStaticSuperinterfaceMethod rt classId partRef classInfo =
  return $ findNonPrivateNonStatic rt partRef classId

findNonPrivateNonStatic :: Runtime -> ClassId -> PartReference -> Maybe String
findNonPrivateNonStatic rt classId@(ClassId initCL _) partRef =
  case getClass rt classId of
    Left error -> Nothing
    Right classInfo ->
      case classInfo |> methodsList |> filter (methodMatches partRef) of
        (m:_) -> classInfo |> className |> Just
        _ ->
          classInfo |> classInterfaces |> map (ClassId initCL) |>
          map (\classId -> findNonPrivateNonStatic rt classId partRef) |>
          catMaybes |>
          listToMaybe

requireNotInterface :: Runtime -> ClassId -> Either VMError Runtime
requireNotInterface rt classId =
  case isInterface rt classId of
    Right True -> return rt
    Right False -> Left $ Linkage rt IncompatibleClassChangeError
    Left err -> Left err
