module Javelin.Runtime.LLI.Loading
where

import Javelin.ByteCode.Data
import Javelin.Runtime.Structures
import Javelin.Util
import Javelin.Runtime.LLI.ClassPath
import Javelin.Runtime.LLI.Resolving
import Javelin.ByteCode.ClassFile (parse)

import Control.Monad.Trans.Maybe
import Control.Monad (liftM)
import Data.Word (Word16)
import Data.Map.Lazy as Map (fromList, insert, lookup)
import Control.Applicative ((<$>))
import Data.ByteString.Lazy (ByteString, unpack)
import Javelin.Util
import Control.Arrow ((>>>))


-- Loading
load :: Maybe ClassName -> ClassName -> Runtime -> IO (Either VMError Runtime)
load trigger name rt =
  let classLoadFunction = if isArray name then loadArray else loadClass
      properClassLoader = getProperClassLoader trigger rt
  in case properClassLoader of
          Nothing -> return $ linkageLeft $ InternalError ClassLoaderNotFound
          Just cl -> case newClassRequest trigger name rt of
            Left e -> return $ Left e
            Right request -> classLoadFunction request rt cl

isArray :: ClassName -> Bool
isArray name = head name == '['

getProperClassLoader :: Maybe ClassName -> Runtime -> Maybe ClassLoader
getProperClassLoader Nothing _ = Just BootstrapClassLoader
getProperClassLoader (Just trigger)
  rt@(Runtime {classLoading = classLoading}) =
  defining <$> Map.lookup trigger classLoading

type ClassLoadMethod = ClassRequest -> Runtime -> ClassLoader -> IO (Either VMError Runtime)


-- §5.3.3 Creating Array Classes
loadArray :: ClassLoadMethod
loadArray name rt classLoader = undefined

loadClass :: ClassLoadMethod
loadClass request rt cl@BootstrapClassLoader =
  case trigger request of
    Nothing -> undefined
    Just triggerClass -> case getInitiatingClassLoader rt triggerClass of
      Nothing -> loadClassWithBootstrap request rt
      Just initCl -> if initCl == cl
                     then return $ Right rt
                     else loadClassWithBootstrap request rt
loadClass name rt cl = undefined


-- §5.3.1 Loading Using the Bootstrap Class Loader
loadClassWithBootstrap :: ClassRequest -> Runtime -> IO (Either VMError Runtime)
loadClassWithBootstrap request rt@(Runtime {classPathLayout = layout}) = do
  maybeBytes <- runMaybeT $ getClassBytes (name request) layout
  let eitherBytes = maybeToEither (Linkage $ NoClassDefFoundClassNotFoundError ClassNotFoundException) maybeBytes
  return $ do
    bytes <- eitherBytes
    derive request rt BootstrapClassLoader BootstrapClassLoader bytes



-- §5.3.5 Deriving a Class from a class File Representation
derive :: ClassRequest -> Runtime -> ClassLoader -> ClassLoader -> ByteString -> Either VMError Runtime
derive request rt initCl defCl bs = do
  checkInitiatingClassLoader initCl (name request) rt
  bc <- checkClassFileFormat bs rt
  checkClassVersion bc
  syms <- checkRepresentedClass (name request) rt bc
  checkSuperClass undefined bc syms rt
    >>= checkSuperInterfaces request bc syms
    >>= recordClassLoading (name request) bc syms initCl defCl

checkInitiatingClassLoader :: ClassLoader -> ClassName -> Runtime -> Either VMError Runtime
checkInitiatingClassLoader initCL name rt = do
  let actualInitCl = getInitiatingClassLoader rt name
  if Just initCL == actualInitCl
    then linkageLeft LinkageError
    else Right rt

checkClassFileFormat :: ByteString -> Runtime -> Either VMError ByteCode
checkClassFileFormat bs rt = let body = parse $ unpack bs in
  case body of
    Left (_, _, msg) -> linkageLeft ClassFormatError
    Right (_, _, byteCode) -> Right byteCode
    
checkClassVersion :: ByteCode -> Either VMError ()
checkClassVersion bc = if minVer bc < 0 || majVer bc > 1050
                       then linkageLeft UnsupportedClassVersionError
                       else Right ()

checkRepresentedClass :: ClassName -> Runtime -> ByteCode -> Either VMError SymTable
checkRepresentedClass name rt bc = let pool = constPool $ body bc
                                       symbolics = deriveSymTable pool
                                       thisIndex = this $ body bc
                                   in case Map.lookup (fromIntegral thisIndex) symbolics of
                                     Just (ClassOrInterface x) -> return symbolics
                                     _ -> linkageLeft $ InternalError CantCheckClassRepresentation

checkSuperClass :: ClassRequest -> ByteCode -> SymTable -> Runtime -> Either VMError Runtime
checkSuperClass request bc sym rt = let superClassIdx = super $ body bc
                                    in case (name request, superClassIdx) of
                                      ("java.lang.Object", 0) -> Right rt
                                      (_, 0) -> linkageLeft $ InternalError OnlyClassObjectHasNoSuperClass
                                      ("java.lang.Object", _) -> linkageLeft $ InternalError ClassObjectHasNoSuperClasses
                                      (_, idx) -> case Map.lookup superClassIdx sym of
                                        Just (ClassOrInterface parent) -> do
                                          rt <- resolveClassInterface undefined rt
                                          case isInterface parent rt of
                                            Nothing -> linkageLeft $ InternalError CouldNotFindAccessFlags
                                            Just True -> linkageLeft IncompatibleClassChangeError
                                            Just False -> let thisAccessFlags = classAccessFlags $ body $ bc
                                                              thisIsInterface = elem ClassInterface thisAccessFlags
                                                          in case (thisIsInterface, parent) of
                                                            (True, "java.lang.Object") -> Right rt
                                                            (True, _) -> linkageLeft $ InternalError InterfaceMustHaveObjectAsSuperClass
                                                            (False, parent) -> if parent == name request
                                                                               then linkageLeft ClassCircularityError
                                                                               else Right rt


checkSuperInterfaces :: ClassRequest -> ByteCode -> SymTable -> Runtime -> Either VMError Runtime
checkSuperInterfaces request bc syms rt = let superInterfaces = interfaces $ body bc
                                          in foldl (checkSuperInterface undefined bc syms) (Right rt) superInterfaces
checkSuperInterface :: ClassRequest -> ByteCode -> SymTable -> Either VMError Runtime -> Word16 -> Either VMError Runtime
checkSuperInterface request bc sym eitherRt interfaceIdx = do
  rt <- eitherRt
  case Map.lookup interfaceIdx sym of
    Just (ClassOrInterface parent) -> do
      rt <- resolveClassInterface request rt
      case isInterface parent rt of
        Nothing -> linkageLeft $ InternalError CouldNotFindAccessFlags
        Just True -> if parent == name request
                     then linkageLeft ClassCircularityError
                     else Right rt
        Just False -> linkageLeft $ IncompatibleClassChangeError
      undefined
    _ -> linkageLeft $ InternalError SymTableHasNoClassEntryAtIndex

recordClassLoading :: ClassName -> ByteCode -> SymTable -> ClassLoader -> ClassLoader -> Runtime -> Either VMError Runtime
recordClassLoading name bc sym defCl initCl
  rt@(Runtime {classLoading = cls, symbolics = syms, bytecodes = bcs}) =
    let clInfo = ClassLoaderInfo defCl initCl (name, defCl) Loaded False Nothing
    in Right $ rt {classLoading = insert name clInfo cls,
                   symbolics = insert name sym syms,
                   bytecodes = insert name bc bcs}
  


-- §5.1 The Runtime Constant Pool

deriveSymTable :: ConstantPool -> SymTable
deriveSymTable p = deriveReduce p (length p - 1) $ fromList []

deriveReduce :: ConstantPool -> Int -> SymTable -> SymTable
deriveReduce _ (-1) d = d
deriveReduce p i d = deriveReduce p (i - 1) d2
  where item = p !! i
        d2 = case deriveReference p item of
          Just ref -> insert (fromIntegral i) ref d
          Nothing -> d

deriveReference :: ConstantPool -> Constant -> Maybe SymbolicReference
deriveReference p c = case c of
  ClassInfo idx ->
    ClassOrInterface <$> deriveUtf8 p idx
  Fieldref classIdx typeIdx ->
    FieldReference <$> deriveFromClass classIdx typeIdx p
  Methodref classIdx typeIdx ->
    ClassMethodReference <$> deriveFromClass classIdx typeIdx p
  InterfaceMethodref classIdx typeIdx ->
    InterfaceMethodReference <$> deriveFromClass classIdx typeIdx p
  MethodHandleInfo x y -> undefined
  MethodTypeInfo idx -> MethodTypeReference <$> deriveUtf8 p idx
  InvokeDynamicInfo x y -> undefined
  StringInfo idx -> StringLiteral <$> deriveUtf8 p idx
  DoubleInfo val -> Just $ DoubleLiteral val
  FloatInfo val -> Just $ FloatLiteral val
  LongInfo val -> Just $ LongLiteral val
  IntegerInfo val -> Just $ IntegerLiteral val
  _ -> Nothing

deriveFromClass :: (Integral i) => i -> i -> ConstantPool -> Maybe PartReference
deriveFromClass classIdx typeIdx p = do
  classInfo <- p !? classIdx
  nameAndTypeInfo <- p !? typeIdx
  case (classInfo, nameAndTypeInfo) of
    (ClassInfo classNameIdx, NameAndTypeInfo methodNameIdx descriptorIdx) -> do
      className <- p !? classNameIdx
      methodName <- p !? methodNameIdx
      descriptor <- p!? descriptorIdx
      case (className, methodName, descriptor) of
        (Utf8Info a, Utf8Info b, Utf8Info c) -> return $ PartReference a b c
        _ -> Nothing
    _ -> Nothing

deriveUtf8 :: ConstantPool -> Word16 -> Maybe String
deriveUtf8 p idx = do
  name <- p !? idx
  case name of
    Utf8Info name -> Just name
    _ -> Nothing
