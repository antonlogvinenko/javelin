{-# LANGUAGE TemplateHaskell #-}

module Javelin.Runtime.Structures where

import           Flow
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..))
import           Data.Array.IArray          (Array, array, bounds, (!), (//))
import           Data.Int                   (Int16, Int32, Int64, Int8)
import           Data.List                  (intersperse, findIndex)
import qualified Data.Map.Strict            as Map (Map, fromList, toList, insert, lookup, size, (!), (!?))
import           Data.Word                  (Word16, Word32, Word64, Word8)

import           Control.Lens               (ix, makeLenses, (%~), (&), (^.),
                                             (^?), _1, _2, _Right)
import           Data.Either.Utils          (maybeToEither)
import           Javelin.ByteCode.Data
import           Javelin.Runtime.DescSign

data Runtime = Runtime
  { _classPathLayout :: ClassPathLayout
  , _loadedClasses   :: Map.Map ClassId (Either VMError LoadedClass)
  , _classResolving  :: Map.Map ClassId ClassRes
  , _classPrepared   :: Map.Map ClassId Bool --todo change Bool to error as in ClassRes
  , _heap            :: (Int, Array Int JObject)
  , _threads         :: [Thread]
  } deriving (Show, Eq)

data ClassId = ClassId
  { getInitCL :: ClassLoader
  , getName   :: ClassName
  } deriving (Show, Eq, Ord)

data ClassRes
  = ClassResOk { _resolvedFields  :: Map.Map ClassPartReference ClassPartRes
               , _resolvedMethods :: Map.Map ClassPartReference ClassPartRes }
  | ClassResFail { resolvingFailure :: VMError }
  deriving (Show, Eq)

data ClassPartReference = ClassPartReference
  { _part      :: PartReference
  , _ownerName :: String
  } deriving (Show, Eq, Ord)

data PartReference = PartReference
  { _name       :: String
  , _descriptor :: String
  } deriving (Show, Eq, Ord)

data ClassPartRes
  = ClassPartResOk
  | ClassPartResFail VMError
  deriving (Show, Eq)

data Class = Class
  { symTable        :: SymTable
  , className       :: String
  , superName       :: Maybe String
  , classInterfaces :: [String]
  , sourceFile      :: String
  , classVisibility :: ClassAccess
  , _fieldsList     :: [Field]
  , methodsList     :: [Method]
  } deriving (Show, Eq)

data ClassAccess = ClassAccess
  { isClassPublic              :: Bool
  , isClassFinal               :: Bool
  , isClassTreatSuperSpecially :: Bool
  , isClassInterface           :: Bool
  , isClassAbstract            :: Bool
  , isClassSynthetic           :: Bool
  , isClassAnnotation          :: Bool
  , isClassEnum                :: Bool
  } deriving (Show, Eq)

data Method = Method
  { methodName       :: String
  , methodDescriptor :: String
  , methodAccess     :: MethodAccess
  , methodParameters :: [MethodParameter2]
  } deriving (Show, Eq)

--  methodCode :: [String], --todo
--  methodExceptions :: [String], --todo
data MethodParameter2 = MethodParameter2
  { parameterName        :: Maybe String
  , isParameterFinal     :: Bool
  , isParameterSynthetic :: Bool
  , isParameterMandated  :: Bool
  } deriving (Show, Eq)

data MethodAccess = MethodAccess
  { isMethodPublic       :: Bool
  , isMethodPrivate      :: Bool
  , isMethodProtected    :: Bool
  , isMethodStatic       :: Bool
  , isMethodFinal        :: Bool
  , isMethodSynchronized :: Bool
  , isMethodBridge       :: Bool
  , isMethodVarargs      :: Bool
  , isMethodNative       :: Bool
  , isMethodAbstract     :: Bool
  , isMethodStrict       :: Bool
  , isMethodSynthetic    :: Bool
  } deriving (Show, Eq)

data Field = Field
  { fieldName       :: String
  , fieldDescriptor :: String
  , fieldType       :: FieldType
  , constantValue   :: Maybe ConstantValue
  , fieldAccess     :: FieldAccess
  , staticValue     :: Maybe JValue
  } deriving (Show, Eq)

data ConstantValue
  = ConstantLong Word64
  | ConstantFloat Float
  | ConstantDouble Double
  | ConstantInteger Word32
  | ConstantString String
  deriving (Show, Eq)

data FieldAccess = FieldAccess
  { isFieldPublic                :: Bool
  , isFieldPrivate               :: Bool
  , isFieldProtected             :: Bool
  , isFieldStatic                :: Bool
  , isFieldFinal                 :: Bool
  , isFieldVolatile              :: Bool
  , isFieldTransient             :: Bool
  , isFieldSynthetic, isFieldEum :: Bool
  } deriving (Show, Eq)

data LoadedClass
  = LoadedClass { _defining       :: ClassLoader
                , _initiating     :: ClassLoader
                , _runtimePackage :: (String, ClassLoader)
                , _classInfo      :: Class }
  | LoadedArrayClass { _defining       :: ClassLoader
                     , _initiating     :: ClassLoader
                     , _runtimePackage :: (String, ClassLoader)
                     , _dimensions     :: Int }
  deriving (Show, Eq)

data ClassPathLayout = ClassPathLayout
  { _classes   :: Map.Map ClassName ClassSource
  , _classPath :: [String]
  } deriving (Eq)

instance Show ClassPathLayout where
  show cpl@(ClassPathLayout classes classPath) =
    "== Classpath ==\n" ++ show classPath ++ "\n\n" ++
    "== Loaded classes ==\n" ++
    (classes |> Map.toList |> map (\(c, p) -> c ++ "\n" ++ (show p)) |> intersperse "\n\n" |> concat)

type ClassName = String

data ClassSource
  = JarFile { getPath :: FilePath }
  | ClassFile { getPath :: FilePath }
  deriving (Show, Eq)

-- ClassLoading Info
data ClassLoader
  = BootstrapClassLoader
  | UserDefinedClassLoader { instanceReference :: Integer }
  deriving (Show, Eq, Ord)

data InternalLoadingError
  = ClassLoaderNotFound
  | OnlyClassObjectHasNoSuperClass String
  | ClassObjectHasNoSuperClasses
  | CouldNotFindAccessFlags String
  | InterfaceMustHaveObjectAsSuperClass
  | SymTableHasNoProperEntryAtIndex
  | CustomError String
  | SpecifyMeError
  deriving (Show, Eq)

data LinkageError
  = LinkageError
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

data VMError
  = InternalError { rt     :: Runtime
                  , reason :: InternalLoadingError }
  | Linkage { rt :: Runtime
            , le :: LinkageError }
  | ClassNotFoundException { notFoundClass :: String }
  deriving (Show, Eq)

data Thread = Thread
  { pc      :: ProgramCounter
  , frames  :: FrameStack
  , runtime :: Runtime
  } deriving (Show, Eq)

type ProgramCounter = Integer

type FrameStack = [Frame]

data Frame = Frame
  { currentClass  :: ClassId
  , currentMethod :: Integer
  , locals        :: Locals
  , operands      :: [StackElement]
  } deriving (Show, Eq)

data StackElement = StackElement
  { stackElement :: Word64
  } deriving (Show, Eq)

data Locals = Locals
  { vars :: Array Int Word32
  } deriving (Show, Eq)

type Ref = Int

type JObject = Map.Map String JValue

data JValue
  = JInt { getInt :: Int32 }
  | JLong { getLong :: Int64 }
  | JBoolean { getBoolean :: Int32 }
  | JShort { getShort :: Int16 }
  | JByte { getByte :: Int8 }
  | JChar { getChar :: Word8 }
  | JDouble { getDouble :: Double }
  | JFloat { getFloat :: Float }
  | JReference { getReference :: Integer }
  deriving (Show, Eq)

type SymTable = [SymbolicReference]

data SymbolicReference
  = ClassOrInterface { classOrInterfaceName :: String }
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

makeLenses ''Class

makeLenses ''ClassPathLayout

-- Querying class info
getSuperInterfaces :: Runtime -> ClassId -> Either VMError [String]
getSuperInterfaces rt classId = classInterfaces <$> getClass rt classId

isInterface :: Runtime -> ClassId -> Either VMError Bool
isInterface rt classId =
  isClassInterface <$> classVisibility <$> getClass rt classId

getSuperClass :: Runtime -> ClassId -> Either VMError (Maybe String)
getSuperClass rt classId = superName <$> getClass rt classId

getMethodBySignature :: Runtime -> ClassId -> PartReference -> Either VMError Integer
getMethodBySignature rt classId partRef = do
   classMethods <- methodsList <$> getClass rt classId
   case findIndex (\m -> (methodName m) == (_name partRef)) classMethods of
     Just idx -> Right $ fromIntegral idx
     Nothing -> Left $ InternalError rt $ CustomError $ "Cant find method" ++ (show partRef)

getClass :: Runtime -> ClassId -> Either VMError Class
getClass rt classId = _classInfo <$> getLoadedClass rt classId

getLoadedClass :: Runtime -> ClassId -> Either VMError LoadedClass
getLoadedClass rt classId =
  maybe (Left $ InternalError rt SpecifyMeError) id (findLoadedClass rt classId)

findLoadedClass :: Runtime -> ClassId -> Maybe (Either VMError LoadedClass)
findLoadedClass rt classId = rt ^? loadedClasses . ix classId

getDefiningClassLoader :: Runtime -> ClassId -> Either VMError ClassLoader
getDefiningClassLoader rt classId = _defining <$> getLoadedClass rt classId

getStringLiteral :: SymTable -> Word16 -> Either VMError String
getStringLiteral t i =
  maybeToEither undefined $ do
    let elem = t !! fromIntegral i
    case elem of
      StringLiteral x -> return x
      _               -> Nothing

-- Updating class info
newRuntime :: ClassPathLayout -> Runtime
newRuntime layout =
  let emptyThreads = []
      loadedClassesInfo = Map.fromList []
   in Runtime
        layout
        loadedClassesInfo
        (Map.fromList [])
        (Map.fromList [])
        (0, (array (0, 0) [(0, (Map.fromList [("", JReference 0)]))]))
        emptyThreads

addLoadedClass ::
     ClassId -> LoadedClass -> Runtime -> ExceptT VMError IO Runtime
addLoadedClass classId loadedClass rt =
  lift $ return $ rt & loadedClasses %~ Map.insert classId (Right loadedClass)

markClassPrepared :: ClassId -> Runtime -> Either VMError Runtime
markClassPrepared classId rt =
  return $ rt & classPrepared %~ Map.insert classId True

isClassPrepared :: ClassId -> Runtime -> Bool
isClassPrepared classId rt =
  case (_classPrepared rt) Map.!? classId of
    Nothing -> False
    Just x -> x

updateClassFields ::
     ClassId -> Runtime -> ([Field] -> [Field]) -> Either VMError Runtime
updateClassFields classId rt update =
  return $ rt & loadedClasses . ix classId . _Right . classInfo . fieldsList %~
  update

addResolvedClassField ::
     ClassId -> ClassPartReference -> Runtime -> Either VMError Runtime
addResolvedClassField classId classPartRef rt =
  return $ rt & classResolving . ix classId . resolvedFields %~
  Map.insert classPartRef ClassPartResOk

addResolvedClassMethod ::
     ClassId -> ClassPartReference -> Runtime -> Either VMError Runtime
addResolvedClassMethod classId classPartRef rt =
  return $ rt & classResolving . ix classId . resolvedMethods %~
  Map.insert classPartRef ClassPartResOk

classDefinesField :: ClassId -> ClassPartReference -> Runtime -> Bool
classDefinesField classId partRef rt =
  let fieldResStatus =
        rt ^? classResolving . ix classId . resolvedFields . ix partRef
   in Nothing /= fieldResStatus

-- Heap contents
newThread :: Frame -> ClassPathLayout -> Thread
newThread frame = Thread 0 [frame] . newRuntime

malloc :: Runtime -> (Runtime, Ref)
malloc rt =
  let old = rt ^. heap . _1
   in (rt & heap . _1 %~ (+ 1), old)

getField :: Runtime -> Ref -> String -> Either VMError JValue
getField rt ref name =
  let h = rt ^. heap . _2
   in if ref < (snd . bounds) h
        then maybeToEither (InternalError rt SpecifyMeError) $
             Map.lookup name (h ! ref)
        else Left $ InternalError rt SpecifyMeError

writeField :: Runtime -> Ref -> (String, JValue) -> Either VMError Runtime
writeField rt@(Runtime {_heap = (s, h)}) ref (name, value) = do
  let jobject = h ! ref
      newObject = Map.insert name value jobject
  return $ rt & heap . _2 %~ (// [(ref, newObject)])

-- LLI ClassPath
nullReference = JReference (-1)

baseDefaultValues :: Map.Map BaseType JValue
baseDefaultValues =
  Map.fromList
    [ (ByteT, JByte 0)
    , (CharT, JChar 0)
    , (DoubleT, JDouble 0)
    , (FloatT, JFloat 0)
    , (IntT, JInt 0)
    , (LongT, JLong 0)
    , (ShortT, JShort 0)
    , (BooleanT, JBoolean 0)
    ]
