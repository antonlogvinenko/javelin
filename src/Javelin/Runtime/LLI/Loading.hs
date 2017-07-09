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

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Javelin.Util
import Data.Either.Utils (maybeToEither)
import Flow
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
deriveRef p (Utf8Info _) =
  EmptyLiteral
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
checkInitiatingClassLoader initCL name rt@(Runtime {loadedClasses = cls}) = do
  if Map.member (ClassId initCL name) cls
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
        case isInterface parentId rt of
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
      case isInterface parentClassId rt of
        Left e -> throwE e
        Right False -> throwE $ Linkage rt IncompatibleClassChangeError
        Right True -> if parent == name
                     then throwE $ Linkage rt ClassCircularityError
                     else lift $ return rt
    _ -> throwE $ undefined

recordClassLoading :: ClassName -> ByteCode -> SymTable -> ClassLoader -> ClassLoader -> Runtime -> ExceptT VMError IO Runtime
recordClassLoading name bc sym defCL initCL
  rt@(Runtime {loadedClasses = cls}) =
    let clInfo = Right $ LoadedClass defCL initCL (name, defCL) sym bc
                 (getFields bc sym) (getMethods bc sym)
    in lift $ return $ rt {loadedClasses = insert (ClassId initCL name) clInfo cls}

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
load request@(ClassId initCL name) rt@(Runtime {loadedClasses = cls}) =
  let loaderFn = if isArray name then loadArray else loadClass
  in case Map.lookup request cls of
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
loadClassWithBootstrap request@(ClassId _ name) rt@(Runtime {classPathLayout = layout}) = 
  do
    bytes <- withExceptT (wrapClassNotFound rt) (getClassBytes name layout)
    deriveClass request rt BootstrapClassLoader bytes

-- 5.3.2 Loading Using a User-defined Class Loader
loadClassWithUserDefCL :: ClassLoadMethod
loadClassWithUserDefCL request rt = undefined

-- 5.3.3 Creating Array Classes
loadArray :: ClassLoadMethod
loadArray request@(ClassId initCL name) rt@(Runtime {loadedClasses = cls}) = do
  let arrayRepr@(ArrayRepr {depth = d}) = parseSignature name
  (rt, defCL) <- loadAndGetDefCLOfComponentType request rt arrayRepr
  let clInfo = Right $ LoadedArrayClass defCL initCL (name, defCL) d
  lift $ return $ rt {loadedClasses = insert (ClassId initCL name) clInfo cls}

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
  case rt |> _classResolving |> Map.lookup request of
    Just (ClassResOk _ _) -> lift $ return rt
    Just (ClassResFail err) -> throwE err
    Nothing -> do
      rt <- load request rt
      let (ArrayRepr d mt) = parseSignature name
      if d > 0
        then case mt of
        Nothing -> lift $ return rt
        Just t -> resolveClass (ClassId initCL t) rt
        else lift $ return rt

recordClassFieldResolved :: ClassId -> PartReference -> Runtime -> Runtime
recordClassFieldResolved classId partRef rt =
  rt & classResolving . ix classId . resolvedFields %~ insert partRef ClassPartResOk

classDefinesField :: ClassId -> PartReference -> Runtime -> Bool
classDefinesField classId partRef rt =
  let fieldResStatus = rt ^? classResolving . ix classId . resolvedFields . ix partRef
  in Nothing /= fieldResStatus

resolveClassFieldInParents :: ClassId -> PartReference -> Runtime -> ExceptT VMError IO Runtime
resolveClassFieldInParents classId partRef rt = undefined

resolveField :: ClassId -> PartReference -> Runtime -> ExceptT VMError IO Runtime
resolveField classId partRef rt = do
  rt <- resolveClass classId rt
  resolveFieldInClass classId partRef rt

resolveFieldInClass :: ClassId -> PartReference -> Runtime -> ExceptT VMError IO Runtime
resolveFieldInClass classId partRef rt =
  if classDefinesField classId partRef rt
  then lift $ return $ recordClassFieldResolved classId partRef rt
  else resolveClassFieldInParents classId partRef rt


resolveMethod :: ClassId -> PartReference -> Runtime -> ExceptT VMError IO Runtime
resolveMethod = undefined

resolveInterfaceMethod :: ClassId -> Runtime -> ExceptT VMError IO Runtime
resolveInterfaceMethod = undefined
