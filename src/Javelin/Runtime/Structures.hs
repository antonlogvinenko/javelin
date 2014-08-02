module Javelin.Runtime.Structures

where 

import Control.Applicative
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Array.IArray (Array)
import Data.Map.Lazy as Map (fromList, Map, lookup, insert)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Array.IArray

import Javelin.ByteCode.Data
import Javelin.Util



-- Thread
newThread frame = Thread 0 [frame] . newRuntime
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

-- Runtime
newRuntime :: ClassPathLayout -> Runtime
newRuntime layout = let emptyThreads = []
                        classLoadingInfo = fromList []
                    in Runtime layout [BootstrapClassLoader]
                       classLoadingInfo (fromList []) (fromList []) (fromList [])
                       (0, (array (0, 1000) []))
                       emptyThreads

getClassLoader :: ClassName -> Runtime -> (ClassLoaderInfo -> Int) -> Maybe Int
getClassLoader name rt f = f <$> (Map.lookup name $ classLoading rt)

getInitiatingClassLoader :: ClassName -> Runtime -> Maybe Int
getInitiatingClassLoader name rt = getClassLoader name rt initiating

getDefiningClassLoader :: ClassName -> Runtime -> Maybe Int
getDefiningClassLoader name rt = getClassLoader name rt defining



type Ref = Int

malloc :: Runtime -> (Runtime, Ref)
malloc rt@(Runtime {heap = (s, h)}) = (rt{heap = (s + 1, h)}, s)

getField :: Runtime -> Ref -> String -> Either VMError JValue
getField rt ref name = let h = snd $ heap rt
                       in if ref < (snd . bounds) h
                          then maybeToEither (StateError rt "") $ Map.lookup name (h ! ref)
                          else Left $ StateError rt ""

rtlookup :: (Ord k) => (Runtime -> Map k v) -> Runtime -> k -> Either VMError v
rtlookup f rt k = maybeToEither (StateError rt "") $ Map.lookup k $ f rt

writeField :: Runtime -> Ref -> (String, JValue) -> Either VMError Runtime
writeField rt@(Runtime {heap = (s, h)}) ref (name, value) = do
  let jobject = h ! ref
      newObject = Map.insert name value jobject
  return $ rt{heap = (s, h // [(ref, newObject)])}

getSymTable :: Runtime -> ClassName -> Either VMError SymTable
getSymTable = rtlookup symbolics

getByteCode :: Runtime -> ClassName -> Either VMError ByteCode
getByteCode = rtlookup bytecodes

data Runtime = Runtime { classPathLayout :: ClassPathLayout,

                         classLoaders :: [ClassLoader],
                         classLoading :: Map.Map ClassName ClassLoaderInfo,
                         classResolving :: Map.Map ClassName (Maybe VMError),

                         symbolics :: Map ClassName SymTable,
                         bytecodes :: Map.Map ClassName ByteCode,

                         heap :: (Int, Array Int JObject),
                         threads :: [Thread] }
             deriving (Show, Eq)

isInterface :: ClassName -> Runtime -> Maybe Bool
isInterface name rt = (elem ClassInterface) <$> classAccessFlags <$> body <$> (Map.lookup name $ bytecodes rt)


-- LLI ClassPath
type ClassPathLayout = Map ClassName ClassSource
type ClassName = String
data ClassSource = JarFile { getPath :: FilePath }
                 | ClassFile { getPath :: FilePath }
                 deriving (Show, Eq)
                          


-- LLI state
data LoadLinkInitializeState = Loaded | Linked | Initialized
                             deriving (Show, Eq)



-- ClassLoading Info
data ClassLoader = BootstrapClassLoader
                 | UserDefinedClassLoader { instanceReference :: Integer }
                 deriving (Show, Eq)

data ClassLoaderInfo = ClassLoaderInfo { defining :: Int,
                                         initiating :: Int,
                                         runtimePackage :: (String, Int),
                                         lliState :: LoadLinkInitializeState,
                                         resolved :: Bool,
                                         staticRef :: Maybe Int }
                     deriving (Show, Eq)


data ClassNotFoundException = ClassNotFoundException
                            deriving (Show, Eq)

data InternalLoadingError = CantCheckClassRepresentation
                          | ClassLoaderNotFound
                          | OnlyClassObjectHasNoSuperClass
                          | ClassObjectHasNoSuperClasses
                          | CouldNotFindAccessFlags
                          | InterfaceMustHaveObjectAsSuperClass
                          | SymTableHasNoClassEntryAtIndex
                          deriving (Show, Eq)
data LinkageError = LinkageError
                  | ClassFormatError
                  | UnsupportedClassVersionError

                  | NoClassDefFoundClassNotFoundError { notFound :: ClassNotFoundException }
                  | NoClassDefFoundError
                    
                  | IncompatibleClassChangeError
                  | AbstractMethodError
                  | IllegalAccessError
                  | InstantiationError
                  | NoSuchFieldError
                  | NoSuchMethodError
                    
                  | ClassCircularityError
                  | InternalError { internal :: InternalLoadingError }
                  | UnknownError { message :: String }
                  | ExceptionInInitializerError
                  deriving (Show, Eq)

data VMError = StateError { rt :: Runtime,
                            msg :: String }
             | Linkage { le :: LinkageError }
             deriving (Show, Eq)

linkageLeft = Left . Linkage

-- Heap contents
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

nullReference = JReference (-1)

baseDefaultValues :: Map.Map BaseType JValue
baseDefaultValues = Map.fromList [
  (ByteT, JByte 0), (CharT, JChar 0), (DoubleT, JDouble 0), (FloatT, JFloat 0),
  (IntT, JInt 0), (LongT, JLong 0), (ShortT, JShort 0), (BooleanT, JBoolean 0)
  ]

-- Class loading structures
type SymTable = Map Word16 SymbolicReference

data SymbolicReference = ClassOrInterface { classInterfaceName :: String }
                       | FieldReference { field :: PartReference }
                       | ClassMethodReference { classMethod :: PartReference }
                       | InterfaceMethodReference { interfaceMethod :: PartReference }
                       | MethodHandleReference
                       | MethodTypeReference { typeReference :: String }
                       | CallSiteSpecifierReference
                       | StringLiteral { string :: String }
                       | DoubleLiteral { double :: Double }
                       | FloatLiteral { float :: Float }
                       | IntegerLiteral { integer :: Int32 }
                       | LongLiteral { long :: Int64 }
                       deriving (Show, Eq)

getStringLiteral :: SymTable -> Word16 -> Either VMError String
getStringLiteral t i = maybeToEither undefined $ do
  v <- Map.lookup i t
  case v of
    StringLiteral x -> return x
    _ -> Nothing

data PartReference = PartReference { partName :: String,
                                     partDescriptor :: String,
                                     ownerName :: String }
                   deriving (Show, Eq)
