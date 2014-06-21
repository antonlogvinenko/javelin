module Javelin.Runtime.LLI.Loading
where

import Javelin.ByteCode.Data
import Javelin.Runtime.Structures
import Javelin.Util
import Javelin.Runtime.LLI.ClassPath
import Javelin.ByteCode.ClassFile (parse)

import Control.Monad.Trans.Maybe
import Data.Word (Word16)
import Data.Map.Lazy as Map (fromList, insert, lookup)
import Control.Applicative ((<$>))
import Data.ByteString.Lazy (ByteString, unpack)
import Javelin.Util



-- Loading

data LoadingError = ClassNotFoundException
                  | LinkageError
                  | ClassFormatError
                  | UnsupportedClassVersionError
                  | NoClassDefFoundError
                  | IncompatibleClassChangeError
                  | ClassCircularityError
                  | InternalError { internal :: InternalLoadingError }
                  | ResolutionError
                  | UnknownError { message :: String }
                  deriving (Show, Eq)

data InternalLoadingError = CantCheckClassRepresentation
                          | ClassLoaderNotFound
                          deriving (Show, Eq)

load :: Maybe ClassName -> ClassName -> Runtime -> IO (Either LoadingError Runtime)
load trigger name rt =
  let classLoadFunction = if isArray name then loadArray else loadClass
      properClassLoader = getProperClassLoader trigger rt
  in case properClassLoader of
          Nothing -> return $ Left $ InternalError ClassLoaderNotFound
          Just cl -> classLoadFunction name rt cl

isArray :: ClassName -> Bool
isArray name = head name == '['

getProperClassLoader :: Maybe ClassName -> Runtime -> Maybe Int
getProperClassLoader Nothing _ = Just 0
getProperClassLoader (Just trigger)
  rt@(Runtime {classLoading = classLoading, classLoaders = classLoaders}) =
  do
    classLoadingInfo <- Map.lookup trigger classLoading
    let definingCLIndex = defining classLoadingInfo
    return definingCLIndex

type ClassLoadMethod = ClassName -> Runtime -> Int -> IO (Either LoadingError Runtime)

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


loadClassWithBootstrap :: ClassName -> Runtime -> IO (Either LoadingError Runtime)
loadClassWithBootstrap name rt@(Runtime {layout = layout}) = do
  maybeBytes <- runMaybeT $ getClassBytes name layout
  let eitherBytes = maybeToEither ClassNotFoundException maybeBytes
  return $ do
    bytes <- eitherBytes
    derive name rt 0 0 bytes



-- §5.3.5 Deriving a Class from a class File Representation
derive :: ClassName -> Runtime -> Int -> Int -> ByteString -> Either LoadingError Runtime
derive name rt initCl defCl bs = do
  checkInitiatingClassLoader initCl name rt
  bc <- checkClassFileFormat bs rt
  checkClassVersion bc
  syms <- checkRepresentedClass name rt bc
  checkSuperClass name bc syms rt
    >>= checkSuperInterfaces name bc syms
    >>= recordClassLoading name bc syms initCl defCl

checkInitiatingClassLoader initCl name rt = if Just initCl == getInitiatingClassLoader name rt
                                            then Left LinkageError
                                            else Right ()
checkClassFileFormat :: ByteString -> Runtime -> Either LoadingError ByteCode
checkClassFileFormat bs rt = let body = parse $ unpack bs in
  case body of
    Left (_, _, msg) -> Left ClassFormatError
    Right (_, _, byteCode) -> Right byteCode
    
checkClassVersion :: ByteCode -> Either LoadingError ()
checkClassVersion bc = if minVer bc < 0 || majVer bc > 100500
                       then Left UnsupportedClassVersionError
                       else Right ()

checkRepresentedClass :: ClassName -> Runtime -> ByteCode -> Either LoadingError SymTable
checkRepresentedClass name rt bc = let pool = constPool $ body bc
                                       symbolics = deriveSymTable pool
                                       thisIndex = this $ body bc
                                   in case Map.lookup (fromIntegral thisIndex) symbolics of
                                     Just (ClassOrInterface x) -> return symbolics
                                     _ -> Left $ InternalError CantCheckClassRepresentation

checkSuperClass :: ClassName -> ByteCode -> SymTable -> Runtime -> Either LoadingError Runtime
checkSuperClass name bc sym rt = let superClassIdx = super $ body bc
                                 in case (name, superClassIdx) of
                                   ("java.lang.Object", 0) -> Right rt
                                   (_, 0) -> Left $ UnknownError "Only java.lang.Object has no super classes"
                                   ("java.lang.Object", _) -> Left $ UnknownError "java.lang.Object has no super classes"
                                   (name, idx) -> case Map.lookup superClassIdx sym of
                                     Just (ClassOrInterface parent) -> do
                                       rt <- resolve parent rt
                                       case isInterface parent rt of
                                         Nothing -> Left $ UnknownError "Couldn't find access flags for super class"
                                         Just True -> Left IncompatibleClassChangeError
                                         Just False -> let thisAccessFlags = classAccessFlags $ body $ bc
                                                           thisIsInterface = elem ClassInterface thisAccessFlags
                                                       in case (thisIsInterface, parent) of
                                                         (True, "java.lang.Object") -> Right rt
                                                         (True, _) -> Left $ UnknownError "Interface must have java.lang.Object as it's super class"
                                                         (False, parent) -> if parent == name
                                                                                 then Left ClassCircularityError
                                                                                 else Right rt
                                     _ -> Left $ UnknownError "SymTable doesn't have a class at the index"


checkSuperInterfaces :: ClassName -> ByteCode -> SymTable -> Runtime -> Either LoadingError Runtime
checkSuperInterfaces name bc syms rt = let superInterfaces = interfaces $ body bc
                                  in foldl (checkSuperInterface name bc syms) (Right rt) superInterfaces
checkSuperInterface :: ClassName => ByteCode -> SymTable -> Either LoadingError Runtime -> Word16 -> Either LoadingError Runtime
checkSuperInterface name bc sym eitherRt interfaceIdx = do
  rt <- eitherRt
  case Map.lookup interfaceIdx sym of
    Just (ClassOrInterface parent) -> do
      rt <- resolve parent rt
      case isInterface parent rt of
        Nothing -> Left $ UnknownError "Couldn't find access flags for super interface"
        Just True -> if parent == name
                     then Left ClassCircularityError
                     else Right rt
        Just False -> Left IncompatibleClassChangeError
      undefined
    _ -> Left $ UnknownError "SymTable doesn't have a interface symbol at the index"

recordClassLoading :: ClassName -> ByteCode -> SymTable -> Int -> Int -> Runtime -> Either LoadingError Runtime
recordClassLoading name bc sym defCl initCl
  rt@(Runtime {classLoading = cls, symbolics = syms, bytecodes = bcs, constantPool = cps}) =
    let clInfo = ClassLoaderInfo defCl initCl (name, defCl) Loaded False
    in Right $ rt {classLoading = insert name clInfo cls,
                   symbolics = insert name sym syms,
                   bytecodes = insert name bc bcs,
                   constantPool = insert name (constPool $ body bc) cps}
  


-- §5.4.3 Resolution

resolve :: ClassName -> Runtime -> Either LoadingError Runtime
resolve = undefined



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
    


