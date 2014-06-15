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




-- Loading

data LoadingError = ClassNotFoundException
                  | LinkageError
                  | ClassFormatError
                  | UnsupportedClassVersionError
                  | NoClassDefFoundError
                  | UnknownError { msg :: String }
                  deriving (Show, Eq)

load :: Maybe ClassName -> ClassName -> Runtime -> IO (Either LoadingError Runtime)
load trigger name rt =
  let classLoadFunction = if isArray name then loadArray else loadClass
      properClassLoader = getProperClassLoader trigger rt
  in case properClassLoader of
          Nothing -> return $ Left $ UnknownError "Class loader could not be found"
          Just cl -> classLoadFunction name rt cl

isArray :: ClassName -> Bool
isArray name = head name == '['

getProperClassLoader :: Maybe ClassName -> Runtime -> Maybe ClassLoader
getProperClassLoader Nothing (Runtime {classLoaders = loaders}) = loaders !? 0
getProperClassLoader (Just trigger)
  rt@(Runtime {classLoading = classLoading, classLoaders = classLoaders}) =
  do
    classLoadingInfo <- Map.lookup trigger classLoading
    let definingCLIndex = defining classLoadingInfo
    classLoaders !? definingCLIndex

type ClassLoadMethod = ClassName -> Runtime -> ClassLoader -> IO (Either LoadingError Runtime)

loadArray :: ClassLoadMethod
loadArray name rt classLoader = undefined

loadClass :: ClassLoadMethod
loadClass name rt (UserDefinedClassLoader cl) = undefined
loadClass name rt cl@BootstrapClassLoader =
  case getInitiatingClassLoader name rt of
    Nothing -> loadClassWithBootstrap name rt
    Just initCl -> if initCl == cl
                   then return $ Right rt
                   else loadClassWithBootstrap name rt

loadClassWithBootstrap :: ClassName -> Runtime -> IO (Either LoadingError Runtime)
loadClassWithBootstrap name rt@(Runtime {layout = layout}) = do
  maybeBytes <- runMaybeT $ getClassBytes name layout
  let eitherBytes = maybeToEither ClassNotFoundException maybeBytes
  return $ do
    bytes <- eitherBytes
    derive name rt BootstrapClassLoader bytes


-- §5.3.5 Deriving a Class from a class File Representation
derive :: ClassName -> Runtime -> ClassLoader -> ByteString -> Either LoadingError Runtime
derive name rt initCl bs = do
  checkInitiatingClassLoader initCl name rt
  checkClassFileFormat bs rt
    >>= checkClassVersion
    >>= checkRepresentedClass rt
    >>= checkSuperClasses rt
    >>= recordClassLoading rt

checkInitiatingClassLoader initCl name rt = if Just initCl == getInitiatingClassLoader name rt
                                            then Left LinkageError
                                            else Right ()
checkClassFileFormat :: ByteString -> Runtime -> Either LoadingError ByteCode
checkClassFileFormat bs rt = let body = parse $ unpack bs in
  case body of
    Left (_, _, msg) -> Left ClassFormatError
    Right (_, _, body) -> Right body
    
checkClassVersion :: ByteCode -> Either LoadingError ByteCode
checkClassVersion bc = if minVer bc < 0 || majVer bc > 100500
                       then Left UnsupportedClassVersionError
                       else Right bc

checkRepresentedClass :: Runtime -> ByteCode -> Either LoadingError ByteCode
checkRepresentedClass = undefined --NoClassDefFoundError

checkSuperClasses :: Runtime -> ByteCode -> Either LoadingError ByteCode
checkSuperClasses = undefined --IncompatibleClassChangeError --ClassCircularityError

checkSuperInterfaces :: Runtime -> ByteCode -> Either LoadingError ByteCode
checkSuperInterfaces = undefined --same as for classes

recordClassLoading :: Runtime -> ByteCode -> Either LoadingError Runtime
recordClassLoading = undefined
  



-- §5.1 The Runtime Constant Pool

derivePool :: ConstantPool -> DerivedPool
derivePool p = deriveReduce p (length p - 1) $ fromList []

deriveReduce :: ConstantPool -> Int -> DerivedPool -> DerivedPool
deriveReduce _ (-1) d = d
deriveReduce p i d = deriveReduce p (i - 1) d2
  where item = p !! i
        d2 = case deriveReference p item of
          Just ref -> insert i ref d
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
    


