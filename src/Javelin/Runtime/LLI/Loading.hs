module Javelin.Runtime.LLI.Loading
where

import Javelin.ByteCode.Data
import Javelin.Runtime.Structures
import Javelin.Util
import Javelin.Runtime.LLI.ClassPath
import Javelin.Runtime.LLI.Resolving
import Javelin.ByteCode.ClassFile (parse)

import Control.Monad.Trans.Maybe
import Data.Word (Word16)
import Data.Map.Lazy as Map (fromList, insert, lookup)
import Control.Applicative ((<$>))
import Data.ByteString.Lazy (ByteString, unpack)
import Javelin.Util



-- Loading
load :: Maybe ClassName -> ClassName -> Runtime -> IO (Either VMError Runtime)
load trigger name rt =
  let classLoadFunction = if isArray name then loadArray else loadClass
      properClassLoader = getProperClassLoader trigger rt
  in case properClassLoader of
          Nothing -> return $ linkageLeft $ InternalError ClassLoaderNotFound
          Just cl -> classLoadFunction name rt cl

isArray :: ClassName -> Bool
isArray name = head name == '['

getProperClassLoader :: Maybe ClassName -> Runtime -> Maybe Int
getProperClassLoader Nothing _ = Just 0
getProperClassLoader (Just trigger)
  rt@(Runtime {classLoading = classLoading, classLoaders = classLoaders}) =
  defining <$>  Map.lookup trigger classLoading

type ClassLoadMethod = ClassName -> Runtime -> Int -> IO (Either VMError Runtime)


-- §5.3.3 Creating Array Classes
loadArray :: ClassLoadMethod
loadArray name rt classLoader = undefined

loadClass :: ClassLoadMethod
loadClass name rt cl@0 = 
  case getInitiatingClassLoader name rt of
    Nothing -> loadClassWithBootstrap name rt
    Just initCl -> if initCl == cl
                   then return $ Right rt
                   else loadClassWithBootstrap name rt
loadClass name rt cl = undefined


-- §5.3.1 Loading Using the Bootstrap Class Loader
loadClassWithBootstrap :: ClassName -> Runtime -> IO (Either VMError Runtime)
loadClassWithBootstrap name rt@(Runtime {classPathLayout = layout}) = do
  maybeBytes <- runMaybeT $ getClassBytes name layout
  let eitherBytes = maybeToEither (Linkage $ NoClassDefFoundClassNotFoundError ClassNotFoundException) maybeBytes
  return $ do
    bytes <- eitherBytes
    derive name rt 0 0 bytes



-- §5.3.5 Deriving a Class from a class File Representation
derive :: ClassName -> Runtime -> Int -> Int -> ByteString -> Either VMError Runtime
derive name rt initCl defCl bs = do
  checkInitiatingClassLoader initCl name rt
  bc <- checkClassFileFormat bs rt
  checkClassVersion bc
  syms <- checkRepresentedClass name rt bc
  checkSuperClass name bc syms rt
    >>= checkSuperInterfaces name bc syms
    >>= recordClassLoading name bc syms initCl defCl

checkInitiatingClassLoader initCl name rt = if Just initCl == getInitiatingClassLoader name rt
                                            then linkageLeft LinkageError
                                            else Right ()
checkClassFileFormat :: ByteString -> Runtime -> Either VMError ByteCode
checkClassFileFormat bs rt = let body = parse $ unpack bs in
  case body of
    Left (_, _, msg) -> linkageLeft ClassFormatError
    Right (_, _, byteCode) -> Right byteCode
    
checkClassVersion :: ByteCode -> Either VMError ()
checkClassVersion bc = if minVer bc < 0 || majVer bc > 100500
                       then linkageLeft UnsupportedClassVersionError
                       else Right ()

checkRepresentedClass :: ClassName -> Runtime -> ByteCode -> Either VMError SymTable
checkRepresentedClass name rt bc = let pool = constPool $ body bc
                                       symbolics = deriveSymTable pool
                                       thisIndex = this $ body bc
                                   in case Map.lookup (fromIntegral thisIndex) symbolics of
                                     Just (ClassOrInterface x) -> return symbolics
                                     _ -> linkageLeft $ InternalError CantCheckClassRepresentation

checkSuperClass :: ClassName -> ByteCode -> SymTable -> Runtime -> Either VMError Runtime
checkSuperClass name bc sym rt = let superClassIdx = super $ body bc
                                 in case (name, superClassIdx) of
                                   ("java.lang.Object", 0) -> Right rt
                                   (_, 0) -> linkageLeft $ InternalError OnlyClassObjectHasNoSuperClass
                                   ("java.lang.Object", _) -> linkageLeft $ InternalError ClassObjectHasNoSuperClasses
                                   (name, idx) -> case Map.lookup superClassIdx sym of
                                     Just (ClassOrInterface parent) -> do
                                       rt <- resolve parent rt
                                       case isInterface parent rt of
                                         Nothing -> linkageLeft $ InternalError CouldNotFindAccessFlags
                                         Just True -> linkageLeft IncompatibleClassChangeError
                                         Just False -> let thisAccessFlags = classAccessFlags $ body $ bc
                                                           thisIsInterface = elem ClassInterface thisAccessFlags
                                                       in case (thisIsInterface, parent) of
                                                         (True, "java.lang.Object") -> Right rt
                                                         (True, _) -> linkageLeft $ InternalError InterfaceMustHaveObjectAsSuperClass
                                                         (False, parent) -> if parent == name
                                                                                 then linkageLeft ClassCircularityError
                                                                                 else Right rt
                                     _ -> linkageLeft $ InternalError SymTableHasNoClassEntryAtIndex


checkSuperInterfaces :: ClassName -> ByteCode -> SymTable -> Runtime -> Either VMError Runtime
checkSuperInterfaces name bc syms rt = let superInterfaces = interfaces $ body bc
                                  in foldl (checkSuperInterface name bc syms) (Right rt) superInterfaces
checkSuperInterface :: ClassName => ByteCode -> SymTable -> Either VMError Runtime -> Word16 -> Either VMError Runtime
checkSuperInterface name bc sym eitherRt interfaceIdx = do
  rt <- eitherRt
  case Map.lookup interfaceIdx sym of
    Just (ClassOrInterface parent) -> do
      rt <- resolve parent rt
      case isInterface parent rt of
        Nothing -> linkageLeft $ InternalError CouldNotFindAccessFlags
        Just True -> if parent == name
                     then linkageLeft ClassCircularityError
                     else Right rt
        Just False -> linkageLeft $ IncompatibleClassChangeError
      undefined
    _ -> linkageLeft $ InternalError SymTableHasNoClassEntryAtIndex

recordClassLoading :: ClassName -> ByteCode -> SymTable -> Int -> Int -> Runtime -> Either VMError Runtime
recordClassLoading name bc sym defCl initCl
  rt@(Runtime {classLoading = cls, symbolics = syms, bytecodes = bcs, constantPool = cps}) =
    let clInfo = ClassLoaderInfo defCl initCl (name, defCl) Loaded False Nothing
    in Right $ rt {classLoading = insert name clInfo cls,
                   symbolics = insert name sym syms,
                   bytecodes = insert name bc bcs,
                   constantPool = insert name (constPool $ body bc) cps}
  


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
