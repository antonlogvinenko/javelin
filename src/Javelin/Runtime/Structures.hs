{-# LANGUAGE TemplateHaskell #-}

module Javelin.Runtime.Structures
where 

import Control.Applicative
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Array.IArray (Array)
import Data.Map.Lazy as Map (fromList, Map, lookup, insert, size)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Array.IArray
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

import Javelin.ByteCode.Data
import Javelin.Runtime.DescSign
import Javelin.Util
import Data.Either.Utils (maybeToEither)
import Control.Lens

data Runtime = Runtime { _classPathLayout :: ClassPathLayout,
                         _loadedClasses :: Map.Map ClassId (Either VMError LoadedClass),

                         _classResolving :: Map.Map ClassId ClassRes,

                         _heap :: (Int, Array Int JObject),
                         _threads :: [Thread] }
             deriving (Show, Eq)

data ClassId = ClassId { getInitCL :: ClassLoader,
                         getName :: ClassName }
             deriving (Show, Eq, Ord)

data ClassPartRes = ClassPartResOk
                  | ClassPartResFail VMError
                  deriving (Show, Eq)

data ClassPartReference = ClassPartReference { _partName :: String,
                                               _partDescriptor :: String,
                                               _ownerName :: String }
                        deriving (Show, Eq, Ord)

data PartReference = PartReference { _part :: String,
                                     _descriptor :: String }
                   deriving (Show, Eq, Ord)

data ClassRes = ClassResOk { _resolvedFields  :: Map.Map PartReference ClassPartRes,
                             _resolvedMethods :: Map.Map PartReference ClassPartRes }
              | ClassResFail { resolvingFailure :: VMError }
              deriving (Show, Eq)

data LoadedClass = LoadedClass { defining :: ClassLoader,
                                 initiating :: ClassLoader,
                                 runtimePackage :: (String, ClassLoader),
                                 symtable :: SymTable,
                                 bytecode :: ByteCode,
                                 classFields :: Map.Map PartReference FieldInfo,
                                 classMethods :: Map.Map PartReference MethodInfo }
                   | LoadedArrayClass { defining :: ClassLoader,
                                        initiating :: ClassLoader,
                                        runtimePackage :: (String, ClassLoader),
                                        dimensions :: Int }
                 deriving (Show, Eq)

data ClassPathLayout = ClassPathLayout { _classes :: Map ClassName ClassSource,
                                         _classPath :: [String] }
                     deriving Eq
instance Show ClassPathLayout where
  show cpl@(ClassPathLayout classes classPath) =
    "ClassPathLayout: " ++ show (size classes) ++ " classes loaded from " ++ show classPath

type ClassName = String
data ClassSource = JarFile { getPath :: FilePath }
                 | ClassFile { getPath :: FilePath }
                 deriving (Show, Eq)


-- ClassLoading Info
data ClassLoader = BootstrapClassLoader
                 | UserDefinedClassLoader { instanceReference :: Integer }
                 deriving (Show, Eq, Ord)


data InternalLoadingError = ClassLoaderNotFound
                          | OnlyClassObjectHasNoSuperClass String
                          | ClassObjectHasNoSuperClasses
                          | CouldNotFindAccessFlags String
                          | InterfaceMustHaveObjectAsSuperClass
                          | SymTableHasNoClassEntryAtIndex
                          | SpecifyMeError
                          deriving (Show, Eq)
data LinkageError = LinkageError
                  | ClassFormatError
                  | UnsupportedClassVersionError

                  | NoClassDefFoundClassNotFoundError { notFound :: VMError }
                  | NoClassDefFoundError String
                    
                  | IncompatibleClassChangeError
                  | AbstractMethodError
                  | IllegalAccessError
                  | InstantiationError
                  | NoSuchFieldError
                  | NoSuchMethodError
                    
                  | ClassCircularityError
                  | ExceptionInInitializerError
                  deriving (Show, Eq)

data VMError = InternalError { rt :: Runtime,
                               reason :: InternalLoadingError }
             | Linkage { rt :: Runtime,
                         le :: LinkageError }
             | ClassNotFoundException { notFoundClass :: String }
             deriving (Show, Eq)

data Thread = Thread { pc :: ProgramCounter,
                       frames :: FrameStack,
                       runtime :: Runtime }
              deriving (Show, Eq)
type ProgramCounter = Integer
type FrameStack = [Frame]
data Frame = Frame { currentClass :: Integer,
                     currentMethod :: Integer,
                     locals :: Locals,
                     operands :: [StackElement],
                     pool :: Integer }
             deriving (Show, Eq)
data StackElement = StackElement { stackElement :: Word64 } deriving (Show, Eq)
data Locals = Locals { vars :: Array Int Word32 } deriving (Show, Eq)

type Ref = Int

type JObject = Map String JValue
data JValue = JInt { getInt :: Int32 }
            | JLong { getLong :: Int64 }
            | JBoolean { getBoolean :: Int32}
            | JShort { getShort :: Int16 }
            | JByte { getByte :: Int8 }
            | JChar { getChar :: Word8 }
            | JDouble { getDouble :: Double }
            | JFloat { getFloat :: Float }
            | JReference { getReference :: Integer }
            deriving (Show, Eq)

type SymTable = [SymbolicReference]

data SymbolicReference = ClassOrInterface { classOrInterfaceName :: String }
                       | FieldReference { field :: ClassPartReference }
                       | ClassMethodReference { classMethod :: ClassPartReference }
                       | InterfaceMethodReference { interfaceMethod :: ClassPartReference }
                       | MethodHandleReference
                       | MethodTypeReference { typeReference :: String }
                       | CallSiteSpecifierReference
                       | StringLiteral { string :: String }
                       | DoubleLiteral { double :: Double }
                       | FloatLiteral { float :: Float }
                       | IntegerLiteral { integer :: Int32 }
                       | LongLiteral { long :: Int64 }
                       | EmptyLiteral
                       deriving (Show, Eq)


makeLenses ''Runtime
makeLenses ''ClassRes
makeLenses ''ClassPartReference
makeLenses ''LoadedClass
makeLenses ''ClassPathLayout


-- Thread
newThread frame = Thread 0 [frame] . newRuntime
malloc :: Runtime -> (Runtime, Ref)
malloc rt = let old = rt ^. heap . _1
            in (rt & heap . _1 %~ (+ 1), old)

-- Runtime
newRuntime :: ClassPathLayout -> Runtime
newRuntime layout = let emptyThreads = []
                        loadedClassesInfo = fromList []
                    in Runtime layout
                       loadedClassesInfo (fromList [])
                       (0, (array (0, 0) [(0, (fromList [("", JReference 0)]))]))
                       emptyThreads

getField :: Runtime -> Ref -> String -> Either VMError JValue
getField rt ref name = let h = rt ^. heap . _2
                       in if ref < (snd . bounds) h
                          then maybeToEither (InternalError rt SpecifyMeError) $ Map.lookup name (h ! ref)
                          else Left $ InternalError rt SpecifyMeError

rtlookup :: (Ord k) => (Runtime -> Map k v) -> Runtime -> k -> Either VMError v
rtlookup f rt k = maybeToEither (InternalError rt SpecifyMeError) $ Map.lookup k $ f rt

writeField :: Runtime -> Ref -> (String, JValue) -> Either VMError Runtime
writeField rt@(Runtime {_heap = (s, h)}) ref (name, value) = do
  let jobject = h ! ref
      newObject = Map.insert name value jobject
  return $ rt & heap . _2 %~ (// [(ref, newObject)])

getSymTable :: Runtime -> ClassId -> Either VMError SymTable
getSymTable rt classId = symtable <$> getLoadedClass rt classId

getByteCode :: Runtime -> ClassId -> Either VMError ByteCode
getByteCode rt classId = bytecode <$> getLoadedClass rt classId

getLoadedClass :: Runtime -> ClassId -> Either VMError LoadedClass
getLoadedClass rt classId =
  maybe (Left $ InternalError rt SpecifyMeError) id (findLoadedClass rt classId)

findLoadedClass :: Runtime -> ClassId -> Maybe (Either VMError LoadedClass)
findLoadedClass rt classId = rt ^? loadedClasses . ix classId

addLoadedClass :: ClassId -> LoadedClass -> Runtime -> ExceptT VMError IO Runtime
addLoadedClass classId loadedClass rt = lift $ return $
                                        rt & loadedClasses %~ insert classId (Right loadedClass)
addResolvedClassField :: ClassId -> PartReference -> Runtime -> Runtime
addResolvedClassField classId partRef rt =
  rt & classResolving . ix classId . resolvedFields %~ insert partRef ClassPartResOk

getDefiningClassLoader :: Runtime -> ClassId -> Either VMError ClassLoader
getDefiningClassLoader rt classId = defining <$> getLoadedClass rt classId



isInterface ::  ClassId -> Runtime -> Either VMError Bool
isInterface classId rt = (elem AccInterface) <$> classAccessFlags <$> body <$> bytecode <$> getLoadedClass rt classId


-- LLI ClassPath

linkageLeft = Left . Linkage

-- Heap contents

nullReference = JReference (-1)

baseDefaultValues :: Map.Map BaseType JValue
baseDefaultValues = Map.fromList [
  (ByteT, JByte 0), (CharT, JChar 0), (DoubleT, JDouble 0), (FloatT, JFloat 0),
  (IntT, JInt 0), (LongT, JLong 0), (ShortT, JShort 0), (BooleanT, JBoolean 0)
  ]


-- Data structures

getStringLiteral :: SymTable -> Word16 -> Either VMError String
getStringLiteral t i = maybeToEither undefined $ do
  let elem = t !! fromIntegral i
  case elem of
    StringLiteral x -> return x
    _ -> Nothing
