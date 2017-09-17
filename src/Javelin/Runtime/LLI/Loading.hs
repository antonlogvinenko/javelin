{-# LANGUAGE TemplateHaskell #-}

module Javelin.Runtime.LLI.Loading
where

import Javelin.Runtime.LLI.ClassPath (getClassBytes)
import Javelin.Runtime.Structures
import Javelin.ByteCode.Data
import Javelin.ByteCode.ClassFile (parseRaw)

import Data.Maybe (isJust, catMaybes, isNothing, listToMaybe)
import Data.Word (Word16)
import Data.Map as Map (insert, lookup, member, Map(..), empty, (!))
import Data.ByteString (ByteString, unpack)

import Control.Monad.Trans.Except (ExceptT(..), throwE, withExceptT)
import Control.Monad.Trans.Class (lift)
import Javelin.Util ((#))
import Data.Either.Utils (maybeToEither)
import Flow ((|>))
import Control.Lens ((^?), ix, (^.), to)


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
  in ClassPartReference (PartReference className memberName) memberDescriptor
-- note: stringValue usage, can fail for invalid bytecode; need a way to handle/notify


-- 5.3.5 Deriving a Class from a class File Representation
checkAndRecordLoadedClass :: ClassId -> Runtime -> ClassLoader -> ByteString -> ExceptT VMError IO Runtime
checkAndRecordLoadedClass request@(ClassId initCL name) rt defCL bs = do
  checkInitiatingClassLoader initCL name rt
  bc <- checkClassFileFormat bs rt
  checkClassVersion bc rt
  let classInfo = deriveClass bc
  checkSuperClass request defCL rt classInfo
    >>= checkSuperInterfaces request defCL classInfo
    >>= recordClassLoading name classInfo initCL defCL
      
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

--todo also check class name is correct
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

checkSuperClass :: ClassId -> ClassLoader -> Runtime -> Class -> ExceptT VMError IO Runtime
checkSuperClass request defCL rt classInfo =
  let classSuperName = superName classInfo
      name = getName request
  in case (name, classSuperName) of
    ("java/lang/Object", Nothing) -> lift $ return rt
    ("java/lang/Object", _) -> throwE $ Linkage rt $ ClassFormatError
    (_, Nothing) -> throwE $ Linkage rt $ ClassFormatError
    (_, Just parent) -> do
      let parentId = ClassId defCL parent
      rt <- resolveClass parentId rt
      case isInterface rt parentId of
        Left error -> throwE error
        Right True -> throwE $ Linkage rt IncompatibleClassChangeError
        Right False -> let thisIsInterface = classInfo |> classVisibility |> isClassInterface
                       in case (thisIsInterface, parent) of
                            (True, "java/lang/Object") -> lift $ return rt
                            (True, _) -> throwE $ InternalError rt InterfaceMustHaveObjectAsSuperClass
                            (False, parentName) -> if parentName == name
                                                   then throwE $ Linkage rt ClassCircularityError
                                                   else lift $ return rt

checkSuperInterfaces :: ClassId -> ClassLoader -> Class -> Runtime -> ExceptT VMError IO Runtime
checkSuperInterfaces request defCL classInfo rt = let superInterfaces = classInterfaces classInfo
                                                  in foldl (checkSuperInterface request defCL classInfo) (lift $ return rt) superInterfaces
checkSuperInterface :: ClassId -> ClassLoader -> Class -> ExceptT VMError IO Runtime -> String -> ExceptT VMError IO Runtime
checkSuperInterface request defCL classInfo eitherRt parentInterface = do
  rt <- eitherRt
  let name = getName request
  let parentClassId = ClassId defCL parentInterface
  rt <- resolveClass parentClassId rt
  case isInterface rt parentClassId of
    Left e -> throwE e
    Right False -> throwE $ Linkage rt IncompatibleClassChangeError
    Right True -> if parentInterface == name
                  then throwE $ Linkage rt ClassCircularityError
                  else lift $ return rt

recordClassLoading :: ClassName -> Class -> ClassLoader -> ClassLoader -> Runtime -> ExceptT VMError IO Runtime
recordClassLoading name classInfo defCL initCL rt =
    let c = LoadedClass defCL initCL (name, defCL) classInfo
    in addLoadedClass (ClassId initCL name) c rt

deriveClass :: ByteCode -> Class
deriveClass bc =
  let classBody = body bc
      cp = constPool classBody
      sym = deriveSymTable cp
      ClassOrInterface className = sym # (this classBody)
      superIdx = super classBody
      superName = if superIdx == 0
                  then Nothing
                  else Just $ classOrInterfaceName $ sym # superIdx
      classAttrs = attrs classBody
      classInterfaces = classBody |> interfaces |> map (classOrInterfaceName .  (sym #))
      classFields = classBody |> fields |> map (checkAndRecordLoadedClassFields sym classBody)
      classMethods = classBody |> methods |> map (checkAndRecordLoadedClassMethods sym classBody)
  in Class className superName [] "sourceFile" (ClassAccess False False False False False False False False) classFields classMethods

checkAndRecordLoadedClassFields :: SymTable -> ClassBody -> FieldInfo -> Field
checkAndRecordLoadedClassFields sym body fieldInfo =
  let fieldName = fieldInfo |> fieldNameIndex |> (sym #) |> string
      fieldDescriptor = fieldInfo |> fieldDescriptorIndex |> (sym #) |> string
      fieldAttrs = attrs body
  in Field fieldName fieldDescriptor (deriveDefaultFieldValue sym fieldAttrs) (FieldAccess False False False False False False False False False)

deriveDefaultFieldValue :: SymTable -> [AttrInfo] -> Maybe ConstantValue
deriveDefaultFieldValue sym [] = Nothing
deriveDefaultFieldValue sym (a@((ConstantValue idx):as)) = Just $ ConstantString ""
deriveDefaultFieldValue sym (a@(_:as)) = deriveDefaultFieldValue sym as

checkAndRecordLoadedClassMethods :: SymTable -> ClassBody -> MethodInfo -> Method
checkAndRecordLoadedClassMethods sym body methodInfo =
  let methodName = methodInfo |> methodNameIndex |> (sym #) |> string
      methodDescriptor = methodInfo |> methodInfoDescriptorIndex |> (sym #) |> string
  in Method methodName methodDescriptor (MethodAccess False False False False False False False False False False False False) []

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
getMethods bc sym =
  bc
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
    checkAndRecordLoadedClass request rt BootstrapClassLoader bytes

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
  rt <- resolveClass classId rt
  ExceptT $ return $ do
    mowner <- resolveFieldSearch classId partRef rt
    case mowner of
      Nothing -> Left $ Linkage rt NoSuchFieldError
      Just owner -> addResolvedClassField classId (ClassPartReference partRef owner) rt

-- Recursive fn for looking for a field
resolveFieldSearch :: ClassId -> PartReference -> Runtime -> Either VMError (Maybe String)
resolveFieldSearch classId partRef rt = do
  mowner <- resolveFieldInClass classId partRef rt
  case mowner of
    Nothing -> resolveClassFieldInParents classId partRef rt
    Just owner -> return $ Just owner

-- Resolving field in a specific class

-- todo terminate on Object parent; for interfaces?
-- todo what error to record?
resolveFieldInClass :: ClassId -> PartReference -> Runtime -> Either VMError (Maybe String)
resolveFieldInClass classId@(ClassId _ owner) partRef rt = 
  if classDefinesField classId (ClassPartReference partRef owner) rt
  then return $ Just owner
  else return Nothing

resolveClassFieldInParents :: ClassId -> PartReference -> Runtime -> Either VMError (Maybe String)
resolveClassFieldInParents classId partRef rt = do
  superInterfaces <- getSuperInterfaces rt classId
  interfaceResolution <- superInterfaces
                         |> map (\superInterface -> ClassId (getInitCL classId) superInterface)
                         |> map (\classId -> resolveFieldSearch classId partRef rt)
                         |> findSuccessfulResolution
  case interfaceResolution of
    Just r -> return $ Just r
    Nothing -> case getSuperClass rt classId of
                 Left err -> Left err
                 Right Nothing -> return Nothing
                 Right (Just superClass) -> resolveFieldSearch (ClassId (getInitCL classId) superClass) partRef rt

findSuccessfulResolution :: [Either VMError (Maybe String)] -> Either VMError (Maybe String)
findSuccessfulResolution [] = Right Nothing
findSuccessfulResolution (either:xs) =
  case either of
    Left err -> Left err
    Right Nothing -> findSuccessfulResolution xs
    Right owner -> Right owner




type MethodResolution = Runtime -> PartReference -> ClassId -> Class -> Either VMError (Maybe String)

resolveMethod :: Runtime -> PartReference -> ClassId -> ExceptT VMError IO Runtime
resolveMethod rt partRef classId = do
  rt <- resolveClass classId rt
  ExceptT $ return $ do
    classInfo <- getClass rt classId
    mclass <- resolveMethodSearch rt partRef classId classInfo
    case mclass of
      Nothing -> Left $ Linkage rt NoSuchMethodError
      Just owner -> addResolvedClassMethod classId (ClassPartReference partRef owner) rt
  

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
resolveMethodSignPolymorphic ::MethodResolution
resolveMethodSignPolymorphic rt partRef classId classInfo = return Nothing

resolveMethodNameDescriptor :: MethodResolution
resolveMethodNameDescriptor rt partRef classId classInfo =
  let matchedMethods = classInfo |> methodsList |> filter (methodMatches partRef)
  in return $ case matchedMethods of
    (m:_) -> Just $ getName classId
    [] -> Nothing

methodMatches :: PartReference -> Method -> Bool
methodMatches partRef@(PartReference name descr) m =
  methodName m == name && methodDescriptor m == descr

resolveInSuperClass :: MethodResolution
resolveInSuperClass rt partRef classId@(ClassId initCl name) classInfo =
  case superName classInfo of
    Nothing -> return Nothing
    Just parentClassName -> resolveMethodSearch rt partRef (ClassId initCl parentClassName) classInfo

resolveMethodInSuperInterfaces :: MethodResolution
resolveMethodInSuperInterfaces rt partRef classId classInfo = do
  mclass <- resolveMaxSpecificSuperinterfaceMethod rt partRef classId classInfo
  case mclass of
    Nothing -> resolveNonPrivateNonStaticSuperinterfaceMethod rt partRef classId classInfo
    Just _ -> return mclass

resolveMaxSpecificSuperinterfaceMethod :: MethodResolution
resolveMaxSpecificSuperinterfaceMethod rt partRef classId@(ClassId init name) classInfo =
  return $ case maxSpecific rt partRef classId of
    Just (Just owner) -> Just owner
    _ -> Nothing

maxSpecific :: Runtime -> PartReference -> ClassId -> Maybe (Maybe String)
maxSpecific rt partRef classId@(ClassId initCL _) =
  case getClass rt classId of
    Left error -> Nothing
    Right classInfo -> case classInfo |> methodsList |> filter (methodMatches partRef) of
                         [method] -> classInfo |> className |> Just |> Just
                         [] -> let found = classInfo
                                           |> classInterfaces
                                           |> map (ClassId initCL)
                                           |> map (maxSpecific rt partRef)
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
    Right classInfo -> case classInfo |> methodsList |> filter (methodMatches partRef) of
                         (m:_) -> classInfo |> className |> Just
                         _ -> classInfo
                              |> classInterfaces
                              |> map (ClassId initCL)
                              |> map (\classId -> findNonPrivateNonStatic rt classId partRef)
                              |> catMaybes |> listToMaybe
  

requireNotInterface :: Runtime -> ClassId -> Either VMError Runtime
requireNotInterface rt classId = case isInterface rt classId of
  Right True -> return rt
  Right False -> Left $ Linkage rt IncompatibleClassChangeError
  Left err -> Left err
