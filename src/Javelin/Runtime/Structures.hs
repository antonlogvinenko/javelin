module Javelin.Runtime.Structures

where 

import Control.Applicative
import Data.Word (Word16, Word32, Word64)
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
newRuntime :: Layout -> Runtime
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

getField :: Runtime -> Ref -> String -> Maybe JValue
getField rt ref name = let (s, h) = heap rt
                           jobject = h ! ref
                       in Map.lookup name jobject

writeField :: Runtime -> Ref -> (String, JValue) -> Either String Runtime
writeField rt@(Runtime {heap = (s, h)}) ref (name, value) = do
  let jobject = h ! ref
      newObject = Map.insert name value jobject
  return $ rt{heap = (s, h // [(ref, newObject)])}

data Runtime = Runtime { layout :: Layout,
                         classLoaders :: [ClassLoader],

                         classLoading :: Map.Map ClassName ClassLoaderInfo,
                         symbolics :: Map ClassName SymTable,
                         bytecodes :: Map.Map ClassName ByteCode,
                         constantPool :: Map.Map ClassName [Constant],

                         heap :: (Int, Array Int JObject),
                         threads :: [Thread] }
             deriving (Show, Eq)

isInterface :: ClassName -> Runtime -> Maybe Bool
isInterface name rt = (elem ClassInterface) <$> classAccessFlags <$> body <$> (Map.lookup name $ bytecodes rt)


-- LLI ClassPath
type Layout = Map ClassName ClassSource
type ClassName = String
data ClassSource = JarFile { getPath :: FilePath }
                 | ClassFile { getPath :: FilePath }
                 deriving (Show, Eq)
                          


-- LLI state
data LoadLinkInitializeState = Loaded | LinkVerified | LinkPrepared | Initialized
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
                          | OnlyClassObjectHasNoSuperClass
                          | ClassObjectHasNoSuperClasses
                          | CouldNotFindAccessFlags
                          | InterfaceMustHaveObjectAsSuperClass
                          | SymTableHasNoClassEntryAtIndex
                          deriving (Show, Eq)


-- Heap contents
type JObject = Map String JValue
data JValue = JInt { getInt :: Int32 }
            | JLong { getLong :: Int64 }
            | JBoolean { getBoolean :: Int32}
            | JShort { getShort :: Int16 }
            | JByte { getByte :: Int8 }
            | JDouble { getDouble :: Double }
            | JFloat { getFloat :: Float }
            | JReference { getReference :: Integer }
            deriving (Show, Eq)



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

data PartReference = PartReference { partName :: String,
                                     partDescriptor :: String,
                                     ownerName :: String }
                   deriving (Show, Eq)
