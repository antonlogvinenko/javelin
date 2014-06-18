module Javelin.Runtime.Structures

where 

import Control.Applicative
import Data.Word (Word32, Word64)
import Data.Array.IArray (Array)
import Data.Map.Lazy as Map (fromList, Map, lookup)
import Data.Int (Int8, Int16, Int32, Int64)

import Javelin.ByteCode.Data (Constant)
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
                       classLoadingInfo (fromList []) (fromList []) [] emptyThreads

getClassLoader name rt f = do
  index <- f <$> (Map.lookup name $ classLoading rt)
  classLoaders rt !? index

getInitiatingClassLoader :: ClassName -> Runtime -> Maybe ClassLoader
getInitiatingClassLoader name rt = getClassLoader name rt initiating

getDefiningClassLoader :: ClassName -> Runtime -> Maybe ClassLoader
getDefiningClassLoader name rt = getClassLoader name rt defining


data Runtime = Runtime { layout :: Layout,
                         classLoaders :: [ClassLoader],
                         classLoading :: Map.Map ClassName ClassLoaderInfo,

                         symbolics:: Map ClassName SymTable,
                         constantPool :: Map.Map ClassName [Constant],
                         heap :: [JObject],

                         threads :: [Thread] }
             deriving (Show, Eq)



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
                                         resolved :: Bool }
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
type SymTable = Map Int SymbolicReference

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
