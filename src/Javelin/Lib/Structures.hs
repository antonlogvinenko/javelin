{-# LANGUAGE TemplateHaskell #-}

module Javelin.Lib.Structures where

import qualified Data.Array.IArray as Array
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Word as Word
import qualified Data.Function as Function
import qualified Data.Maybe as Maybe

import Control.Lens ((%~), (&), (^.), (^?), _1, _2, _Right, ix, makeLenses)

import Data.Either.Utils (maybeToEither)
import Javelin.Lib.ByteCode.Data
import Javelin.Lib.ByteCode.DescSign

data Runtime =
  Runtime
    { _classPathLayout :: ClassPathLayout
    , _loadedClasses :: Map.Map ClassId (Either VMError LoadedClass)
    , _classResolving :: Map.Map ClassId ClassRes
    , _classPrepared :: Map.Map ClassId Bool --todo change Bool to error as in ClassRes
    , _heap :: (Int, Array.Array Int JObject)
    , _threads :: [Thread]
    }
  deriving (Show, Eq)

data ClassId =
  ClassId
    { getInitCL :: ClassLoader
    , getName :: ClassName
    }
  deriving (Show, Eq, Ord)

data ClassRes
  = ClassResOk
      { _resolvedFields :: Map.Map ClassPartReference ClassPartRes
      , _resolvedMethods :: Map.Map ClassPartReference ClassPartRes
      }
  | ClassResFail
      { resolvingFailure :: VMError
      }
  deriving (Show, Eq)

data ClassPartReference =
  ClassPartReference
    { _part :: PartReference
    , _ownerName :: String
    }
  deriving (Show, Eq, Ord)

data PartReference =
  PartReference
    { _name :: String
    , _descriptor :: String
    }
  deriving (Show, Eq, Ord)

data ClassPartRes
  = ClassPartResOk
  | ClassPartResFail VMError
  deriving (Show, Eq)

data Class =
  Class
    { symTable :: SymTable
    , className :: String
    , superName :: Maybe String
    , classInterfaces :: [String]
    , sourceFile :: String
    , classVisibility :: ClassAccess
    , _fieldsList :: [Field]
    , methodsList :: [Method]
    }
  deriving (Show, Eq)

data ClassAccess =
  ClassAccess
    { isClassPublic :: Bool
    , isClassFinal :: Bool
    , isClassTreatSuperSpecially :: Bool
    , isClassInterface :: Bool
    , isClassAbstract :: Bool
    , isClassSynthetic :: Bool
    , isClassAnnotation :: Bool
    , isClassEnum :: Bool
    }
  deriving (Show, Eq)

data Method =
  Method
    { methodName :: String
    , methodDescriptor :: String
    , methodAccess :: MethodAccess
    , stackSize :: Word.Word16
    , localsSize :: Word.Word16
    , instructions :: [Instruction]
    , exceptions :: [Exception]
    , methodParameters :: [MethodParameter2]
    }
  deriving (Show, Eq)

--  methodCode :: [String], --todo
--  methodExceptions :: [String], --todo
data MethodParameter2 =
  MethodParameter2
    { parameterName :: Maybe String
    , isParameterFinal :: Bool
    , isParameterSynthetic :: Bool
    , isParameterMandated :: Bool
    }
  deriving (Show, Eq)

data MethodAccess =
  MethodAccess
    { isMethodPublic :: Bool
    , isMethodPrivate :: Bool
    , isMethodProtected :: Bool
    , isMethodStatic :: Bool
    , isMethodFinal :: Bool
    , isMethodSynchronized :: Bool
    , isMethodBridge :: Bool
    , isMethodVarargs :: Bool
    , isMethodNative :: Bool
    , isMethodAbstract :: Bool
    , isMethodStrict :: Bool
    , isMethodSynthetic :: Bool
    }
  deriving (Show, Eq)

data Field =
  Field
    { fieldName :: String
    , fieldDescriptor :: String
    , fieldType :: FieldType
    , constantValue :: Maybe ConstantValue
    , fieldAccess :: FieldAccess
    , staticValue :: Maybe JValue
    }
  deriving (Show, Eq)

data ConstantValue
  = ConstantLong Word.Word64
  | ConstantFloat Float
  | ConstantDouble Double
  | ConstantInteger Word.Word32
  | ConstantString String
  deriving (Show, Eq)

data FieldAccess =
  FieldAccess
    { isFieldPublic :: Bool
    , isFieldPrivate :: Bool
    , isFieldProtected :: Bool
    , isFieldStatic :: Bool
    , isFieldFinal :: Bool
    , isFieldVolatile :: Bool
    , isFieldTransient :: Bool
    , isFieldSynthetic, isFieldEum :: Bool
    }
  deriving (Show, Eq)

data LoadedClass
  = LoadedClass
      { _defining :: ClassLoader
      , _initiating :: ClassLoader
      , _runtimePackage :: (String, ClassLoader)
      , _classInfo :: Class
      }
  | LoadedArrayClass
      { _defining :: ClassLoader
      , _initiating :: ClassLoader
      , _runtimePackage :: (String, ClassLoader)
      , _dimensions :: Int
      }
  deriving (Show, Eq)

data ClassPathLayout =
  ClassPathLayout
    { _classes :: Map.Map ClassName ClassSource
    , _classPath :: [String]
    }
  deriving (Eq)


instance Show ClassPathLayout where
  show cpl@(ClassPathLayout classes classPath) =
    "== Classpath ==\n" ++ show classPath ++ "\n\n" ++ "== Loaded classes ==\n" ++
    (classes |> Map.toList |> map (\(c, p) -> c ++ "\n" ++ show p) |>
     List.intersperse "\n\n" |>
     List.concat)
     where (|>) = (Function.&)

type ClassName = String

data ClassSource
  = JarFile
      { getPath :: FilePath
      }
  | ClassFile
      { getPath :: FilePath
      }
  deriving (Show, Eq)

-- ClassLoading Info
data ClassLoader
  = BootstrapClassLoader
  | UserDefinedClassLoader
      { instanceReference :: Integer
      }
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
  | NoClassDefFoundClassNotFoundError
      { notFound :: VMError
      }
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
  = InternalError
      { rt :: Runtime
      , reason :: InternalLoadingError
      }
  | Linkage
      { rt :: Runtime
      , le :: LinkageError
      }
  | ClassNotFoundException
      { notFoundClass :: String
      }
  deriving (Show, Eq)

data Thread =
  Thread
    { frames :: FrameStack
    , runtime :: Runtime
    }
  deriving (Show, Eq)

type ProgramCounter = Int

type FrameStack = [Frame]

data Frame =
  Frame
    { pc :: ProgramCounter
    , currentClass :: ClassId
    , currentMethod :: Int
    , locals :: Locals
    , operands :: [StackElement]
    }
  deriving (Show, Eq)

newtype StackElement =
  StackElement
    { stackElement :: Word.Word64
    }
  deriving (Show, Eq)

newtype Locals =
  Locals
    { vars :: Array.Array Int Word.Word32
    }
  deriving (Show, Eq)

type Ref = Int

type JObject = Map.Map String JValue

data JValue
  = JInt
      { getInt :: Int.Int32
      }
  | JLong
      { getLong :: Int.Int64
      }
  | JBoolean
      { getBoolean :: Int.Int32
      }
  | JShort
      { getShort :: Int.Int16
      }
  | JByte
      { getByte :: Int.Int8
      }
  | JChar
      { getChar :: Word.Word8
      }
  | JDouble
      { getDouble :: Double
      }
  | JFloat
      { getFloat :: Float
      }
  | JReference
      { getReference :: Integer
      }
  deriving (Show, Eq)

type SymTable = [SymbolicReference]

data SymbolicReference
  = ClassOrInterface
      { classOrInterfaceName :: String
      }
  | FieldReference
      { field :: ClassPartReference
      }
  | ClassMethodReference
      { classMethod :: ClassPartReference
      }
  | InterfaceMethodReference
      { interfaceMethod :: ClassPartReference
      }
  | MethodHandleReference
  | MethodTypeReference
      { typeReference :: String
      }
  | CallSiteSpecifierReference
  | StringLiteral
      { string :: String
      }
  | DoubleLiteral
      { double :: Double
      }
  | FloatLiteral
      { float :: Float
      }
  | IntegerLiteral
      { integer :: Int.Int32
      }
  | LongLiteral
      { long :: Int.Int64
      }
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

getMethodByIndex :: Runtime -> ClassId -> Int -> Either VMError Method
getMethodByIndex rt classId index = do
  classMethods <- methodsList <$> getClass rt classId
  return $ classMethods !! index

getMethodBySignature ::
     Runtime -> ClassId -> PartReference -> Either VMError (Int, Method)
getMethodBySignature rt classId partRef = do
  classMethods <- methodsList <$> getClass rt classId
  case List.findIndex (\m -> methodName m == _name partRef) classMethods of
    Just idx -> Right (fromIntegral idx, classMethods !! idx)
    Nothing ->
      Left $ InternalError rt $ CustomError $ "Cant find method" ++ show partRef

getClass :: Runtime -> ClassId -> Either VMError Class
getClass rt classId = _classInfo <$> getLoadedClass rt classId

getLoadedClass :: Runtime -> ClassId -> Either VMError LoadedClass
getLoadedClass rt classId =
  maybe (Left $ InternalError rt SpecifyMeError) id (findLoadedClass rt classId)

findLoadedClass :: Runtime -> ClassId -> Maybe (Either VMError LoadedClass)
findLoadedClass rt classId = rt ^? loadedClasses . ix classId

getDefiningClassLoader :: Runtime -> ClassId -> Either VMError ClassLoader
getDefiningClassLoader rt classId = _defining <$> getLoadedClass rt classId

getStringLiteral :: SymTable -> Word.Word16 -> Either VMError String
getStringLiteral t i =
  maybeToEither undefined $ do
    let elem = t !! fromIntegral i
    case elem of
      StringLiteral x -> return x
      _ -> Nothing

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
        (0, Array.array (0, 0) [(0, Map.fromList [("", JReference 0)])])
        emptyThreads

addLoadedClass :: ClassId -> LoadedClass -> Runtime -> Runtime
addLoadedClass classId loadedClass rt =
  rt & loadedClasses %~ Map.insert classId (Right loadedClass)

markClassPrepared :: ClassId -> Runtime -> Runtime
markClassPrepared classId rt = rt & classPrepared %~ Map.insert classId True

isClassPrepared :: ClassId -> Runtime -> Bool
isClassPrepared classId rt = Maybe.fromMaybe False (_classPrepared rt Map.!? classId)

updateClassFields :: ClassId -> Runtime -> ([Field] -> [Field]) -> Runtime
updateClassFields classId rt update =
  rt & loadedClasses . ix classId . _Right . classInfo . fieldsList %~ update

addResolvedClassField :: ClassId -> ClassPartReference -> Runtime -> Runtime
addResolvedClassField classId classPartRef rt =
  rt & classResolving . ix classId . resolvedFields %~
  Map.insert classPartRef ClassPartResOk

addResolvedClassMethod :: ClassId -> ClassPartReference -> Runtime -> Runtime
addResolvedClassMethod classId classPartRef rt =
  rt & classResolving . ix classId . resolvedMethods %~
  Map.insert classPartRef ClassPartResOk

classDefinesField :: ClassId -> ClassPartReference -> Runtime -> Bool
classDefinesField classId partRef rt =
  let fieldResStatus =
        rt ^? classResolving . ix classId . resolvedFields . ix partRef
   in Maybe.isJust fieldResStatus

-- Heap contents
newThread :: Frame -> ClassPathLayout -> Thread
newThread frame = Thread [frame] . newRuntime

malloc :: Runtime -> (Runtime, Ref)
malloc rt =
  let old = rt ^. heap . _1
   in (rt & heap . _1 %~ (+ 1), old)

getField :: Runtime -> Ref -> String -> Either VMError JValue
getField rt ref name =
  let h = rt ^. heap . _2
   in if ref < (snd . Array.bounds) h
        then maybeToEither (InternalError rt SpecifyMeError) $
             Map.lookup name ((Array.!) h ref)
        else Left $ InternalError rt SpecifyMeError

writeField :: Runtime -> Ref -> (String, JValue) -> Either VMError Runtime
writeField rt@Runtime {_heap = (s, h)} ref (name, value) = do
  let jobject = (Array.!) h ref
      newObject = Map.insert name value jobject
  return $ rt & heap . _2 %~ ( (flip (Array.//)) [(ref, newObject)])

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
