module Javelin.ByteCode (parse, require, getBytes, magicNumber, version, classBody)
where

import Data.ByteString (ByteString, unpack)
import Data.Word (Word32, Word16, Word8)
import Control.Monad
import qualified Data.Map.Lazy as Map (findWithDefault, fromList, Map(..), keys, lookup)
import Data.Bits
import Data.Maybe
import Data.List (lookup)


-- Java bytecode
data ByteCode = ByteCode {minVer :: Word16, majVer :: Word16, body :: ClassBody}
                deriving (Show, Eq)



-- class body
data ClassBody = ClassBody { constPool :: [Constant],
                             classAccessFlags :: [ClassAccessFlags],
                             this :: Word16,
                             super :: Word16,
                             interfaces :: [Word16],
                             fields :: [FieldInfo],
                             methods :: [MethodInfo],
                             attributes :: [AttributeInfo] }
                 deriving (Show, Eq)

data ClassAccessFlags = ClassPublic | ClassFinal
                      | ClassSuper | ClassInterface | ClassAbstract
                      | ClassSynthetic | ClassAnnotation | ClassEnum deriving (Show, Eq)

data Constant = Utf8Info { stringValue :: String}
              | IntegerInfo {bytes :: Word32}
              | FloatInfo {bytes :: Word32}
              | LongInfo {highBytes :: Word32, lowBytes :: Word32}
              | DoubleInfo {highBytes :: Word32, lowBytes :: Word32}
              | ClassInfo {nameIndex :: Word16}
              | StringInfo {stringIndex :: Word16}
              | Fieldref {classIndex :: Word16, nameAndTypeIndex :: Word16}
              | Methodref {classIndex :: Word16, nameAndTypeIndex :: Word16}
              | InterfaceMethodref {classIndex :: Word16, nameAndTypeIndex :: Word16}
              | NameAndTypeInfo { nameIndex :: Word16, nameAndTypeDescriptorIndex :: Word16}
              | MethodHandleInfo { referenceKind :: Word8, refereneIndex :: Word16 }
              | MethodTypeInfo { methodTypeDescriptorIndex :: Word16 }
              | InvokeDynamicInfo { bootstrapMethodAttrIndex :: Word16, nameAndTypeIndex :: Word16 } deriving (Show, Eq)

data FieldInfo = FieldInfo { fieldAccessFlags :: [FieldInfoAccessFlag],
                             fieldNameIndex :: Word16,
                             fieldDescriptorIndex :: Word16,
                             fieldAttributes :: [AttributeInfo]
                           } deriving (Show, Eq)

data FieldInfoAccessFlag = FieldPublic | FieldPrivate | FieldProtected
                         | FieldStatic | FieldFinal
                         | FieldVolatile | FieldTransient | FieldSynthetic
                         | FieldEnum
                         deriving (Show, Eq)

data MethodInfoAccessFlag = MethodPublic | MethodPrivate | MethodProtected
                          | MethodStatic | MethodFinal | MethodSynchronized
                          | MethodBridge | MethodVarargs | MethodNative
                          | MethodAbstract | MethodStrict | MethodSynthetic
                          deriving (Show, Eq)
  
data MethodInfo = MethodInfo { methodAccessFlags :: [MethodInfoAccessFlag],
                               methodNameIndex :: Word16,
                               methodInfoDescriptorIndex :: Word16,
                               methodAttributes :: [AttributeInfo]
                             } deriving (Show, Eq)



-- Attributes
data AttributeInfo = UnknownAttribute { unknownBytes :: [Word8] }
                   | ConstantValue { constantValueIndex :: Word16 }
                   | CodeAttribute { maxStack :: Word16,
                                     maxLocals :: Word16,
                                     code :: [Word8],
                                     exceptionTable :: [Exception],
                                     codeAttributes :: [AttributeInfo] }
                   | StackMapTable { entries :: [StackMapFrame] }
                   | Exceptions { exceptionIndexTable :: [Word16] }
                   | InnerClasses { classes :: [InnerClassInfo] }
                   | EnclosingMethod { enclosingClassIndex :: Word16,
                                       enclosingMethodIndex :: Word16 }
                   | Synthetic
                   | Signature { signatureIndex :: Word16 }
                   | SourceFile { sourceFileIndex :: Word16 }
                   | SourceDebugExtension { debugExtension :: String }
                   | LineNumberTable { lineNumberTable :: [LineNumber] }
                   | LocalVariableTable { localVariableTable :: [LocalVariableInfo] }
                   | LocalVariableTypeTable { localVariableTypeTable :: [LocalVariableInfo] }
                   | Deprecated
                   | RuntimeVisibleAnnotations { annotations :: [Annotation] }
                   | RuntimeInvisibleAnnotations { annotations :: [Annotation] }
                   | RuntimeVisibleParameterAnnotations { parameterAnnotations :: [[Annotation]] }
                   | RuntimeInvisibleParameterAnnotations { parameterAnnotations :: [[Annotation]] }
                   | AnnotationDefault { defaultValue :: [Word8] }
                   | BootstrapMethods { bootstrapMethods :: [BootstrapMethod] }
                   deriving (Show, Eq)

data BootstrapMethod = BootstrapMethod { methodRef :: Word16,
                                         arguments :: [Word16] }
                     deriving (Show, Eq)                            

data ElementValue = ElementConstValue { tag :: Char,
                                        value :: Word16 }
                  | ElementEnumConstValue { tag :: Char,
                                            typeNameIndex :: Word16,
                                            constNameIndex :: Word16 }
                  | ElementClassInfoIndex { tag :: Char,
                                            classInfoIndex :: Word16 }
                  | ElementAnnotationValue { tag :: Char,
                                             annotation :: Annotation }
                  | ElementArrayValue { tag :: Char,
                                        elementValues :: [ElementValue] }
                  deriving (Show, Eq)

data ElementValuePair = ElementValuePair { elementNameIndex :: Word16,
                                           elementValue :: ElementValue
                                         } deriving (Show, Eq)

data Annotation = Annotation { typeIndex :: Word16,
                               elementValuePairs :: [ElementValuePair]
                             } deriving (Show, Eq)

data Exception = Exception { startPc :: Word16,
                             endPc :: Word16,
                             handlerPc :: Word16,
                             catchType :: Word16
                           } deriving (Show, Eq)

data VerificationTypeInfo = TopVariableInfo
                          | IntegerVariableInfo
                          | FloatVariableInfo
                          | LongVariableInfo
                          | DoubleVariableInfo
                          | NullVariableInfo
                          | UninitializedThisVariableInfo
                          | ObjectVariableInfo { cpoolIndex :: Word16 }
                          | UninitializedVariableInfo { offset :: Word16 }
                          deriving (Show, Eq)

data StackMapFrame = SameFrame { frameType :: Word8 }
                   | SameLocals1StackItemFrame { frameType :: Word8,
                                                 stackItem :: VerificationTypeInfo }
                   | SameLocals1StackItemFrameExtended { frameType :: Word8,
                                                         offsetData :: Word16,
                                                         stackItem :: VerificationTypeInfo }
                   | ChopFrame { frameType :: Word8,
                                 offsetDelta :: Word16 }
                   | SameFrameExtended {frameType :: Word8,
                                        offsetDelta :: Word16 }
                   | AppendFrame { frameType :: Word8,
                                   offsetDelta :: Word16,
                                   locals :: [VerificationTypeInfo] }
                   | FullFrame { frameType :: Word8,
                                 offsetDelta :: Word16,
                                 locals :: [VerificationTypeInfo],
                                 stack :: [VerificationTypeInfo] }
                   deriving (Show, Eq)

data InnerClassInfo = InnerClassInfo { innerClassInfoIndex :: Word16,
                                       outerClassInfoIndex :: Word16,
                                       innerNameIndex :: Word16,
                                       innerClassAccessFlags :: [InnerClassAccessFlags]
                                     } deriving (Show, Eq)

data InnerClassAccessFlags = InnerClassPublic | InnerClassPrivate | InnerClassProtected
                           | InnerClassStatic | InnerClassFinal | InnerClassInterface
                           | InnerClassAbstract | InnerClassSynthetic
                           | InnerClassAnnotation | InnerClassEnum
                           deriving (Show, Eq)

data LineNumber = LineNumber { lineStartPc :: Word16,
                               lineNumber :: Word16
                             } deriving (Show, Eq)

data LocalVariableInfo = LocalVariableInfo { localVariableStartPc :: Word16,
                                             localVariableLength :: Word16,
                                             localVariableNameIndex :: Word16,
                                             variableSignatureIndex :: Word16,
                                             localVariableIndex :: Word16
                                           } deriving (Show, Eq)


-- Basic utility functions
getCountAndList :: (Word16 -> Parser [x]) -> [Word8] -> Either String ([Word8], [x])
getCountAndList f bytes = do
  (bytes1, count) <- getWord bytes
  f count bytes1

require :: Int -> [Word8] ->  a -> Either String a
require len bs value = if length bs < len
                       then Left "Unexpected EOF"
                       else Right value

getBytes count bs = require count bs $
                    let high = bs !! 0
                        low = bs !! 1
                        ver = (fromIntegral high) * 256 + fromIntegral low
                    in (drop count bs, ver)

getByte :: Parser Word8
getByte = getBytes 1

getWord :: Parser Word16
getWord = getBytes 2

getDWord :: Parser Word32
getDWord = getBytes 4

takeBytes count bs = require count bs $ (drop count bs, take count bs)
                     
type Parser a = [Word8] -> Either String ([Word8], a)
type RepeatingParser a = Word16 -> Parser a

addFlagIfMatches :: Word16 -> Map.Map Word16 a -> [a] -> Word16 -> [a]
addFlagIfMatches number flagsMap list mask = if (mask .&. number) == 0
                                             then list
                                             else case Map.lookup mask flagsMap of
                                               Just x -> x : list
                                               Nothing -> list
foldMask :: Word16 -> Map.Map Word16 a -> [a]
foldMask bytes flagsMap = foldl (addFlagIfMatches bytes flagsMap) [] (Map.keys flagsMap)

getNTimes :: Parser a -> RepeatingParser [a]
getNTimes parser n bytes = do
  if n == 1
    then Right (bytes, [])
    else do
    (bytes1, obj) <- parser bytes
    (bytes2, objs) <- getNTimes parser (n - 1) bytes1
    return (bytes2, obj : objs)

-- lineNumberTableAttribute - add parsing length
constrNTimes :: ([a] -> b) -> Parser a -> Parser b
constrNTimes f parser bytes = do
  (bytes1, len) <- getWord bytes
  (bytes2, object) <- getNTimes parser len bytes1
  return (bytes2, f object)

getFromPool :: [Constant] -> Word16 -> Maybe Constant
getFromPool list idx = if okIdx < length list
                       then Just $ list !! okIdx
                       else Nothing
  where okIdx = fromIntegral idx


-- Class file header functions
magicNumber :: Parser Int
magicNumber bs = if take 4 bs == [0xCA, 0xFE, 0xBA, 0xBE]
                 then Right (drop 4 bs, 42)
                 else Left "Not a Java class format"
                        
version :: Parser Word16
version = getWord

classFlagsList = Map.fromList [(0x0001, ClassPublic), (0x0010, ClassFinal), (0x0020, ClassSuper),
                               (0x0200, ClassInterface), (0x0400, ClassAbstract),
                               (0x1000, ClassSynthetic), (0x2000, ClassAnnotation),
                               (0x4000, ClassEnum)]

parseClassAccessFlags :: Parser [ClassAccessFlags]
parseClassAccessFlags bytes = do
  (bytes1, flagsBytes) <- getWord bytes
  let flags = foldMask flagsBytes classFlagsList
  return $ (bytes1, flags)



-- Constant pool
getConstantPool :: RepeatingParser [Constant]
getConstantPool = getNTimes getConstant

getConstant :: Parser Constant
getConstant bytes = do
  (bytes1, tag) <- getByte bytes
  getConstantParser (fromIntegral tag) bytes1

getConstantParser :: Int -> Parser Constant
getConstantParser idx = Map.findWithDefault failingConstParser idx constantTypeParser
constantTypeParser :: Map.Map Int (Parser Constant)
constantTypeParser = Map.fromList [(1, utf8InfoParser), (3, integerInfoParser), (4, floatInfoParser),
                               (5, longInfoParser), (6, doubleInfoParser), (7, classInfoParser),
                               (8, stringInfoParser), (9, fieldrefParser), (10, methodrefParser),
                               (11, interfaceMethodrefParser), (12, nameAndTypeInfoParser), (15, identityParser),
                               (16, identityParser), (18, identityParser)]
                     
identityParser bytes = Right (bytes, Utf8Info [])
failingConstParser _ = Left "Undefined constant"

twoTwoBytesInfoParser :: (Word16 -> Word16 -> Constant) -> Parser Constant
twoTwoBytesInfoParser constConstr bytes = do
  (bytes1, firstWord16) <- getWord bytes
  (bytes2, secondWord16) <- getWord bytes
  return $ (bytes2, constConstr firstWord16 secondWord16)

twoFourBytesInfoParser :: (Word32 -> Word32 -> Constant) -> Parser Constant
twoFourBytesInfoParser constConstr bytes = do
  (bytes1, bytesHigh) <- getBytes 4 bytes
  (bytes2, bytesLow) <- getBytes 4 bytes1
  return $ (bytes2, constConstr bytesHigh bytesLow)

twoBytesInfoParser :: (Word16 -> Constant) -> Parser Constant
twoBytesInfoParser constConstr bytes = do
  (bytes1, twoBytes) <- getWord bytes
  return $ (bytes1, constConstr twoBytes)

fourBytesInfoParser :: (Word32 -> Constant) -> Parser Constant
fourBytesInfoParser constConstr bytes = do
  (bytes1, value) <- getBytes 4 bytes
  return $ (bytes1, constConstr value)

fieldrefParser = twoTwoBytesInfoParser Fieldref
methodrefParser = twoTwoBytesInfoParser Methodref
interfaceMethodrefParser = twoTwoBytesInfoParser InterfaceMethodref
nameAndTypeInfoParser = twoTwoBytesInfoParser NameAndTypeInfo
classInfoParser = twoBytesInfoParser ClassInfo
stringInfoParser = twoBytesInfoParser StringInfo
integerInfoParser = fourBytesInfoParser IntegerInfo
floatInfoParser = fourBytesInfoParser FloatInfo
longInfoParser = twoFourBytesInfoParser LongInfo
doubleInfoParser = twoFourBytesInfoParser DoubleInfo

bytesToString bytes = ""

utf8InfoParser bytes = do
  (bytes1, len) <- getWord bytes
  (bytes2, lenBytes) <- takeBytes (fromIntegral len) bytes1
  return $ (bytes2, Utf8Info $ bytesToString lenBytes)
methodHandleInfoParser bytes = do
  (bytes1, kind) <- getByte bytes
  (bytes2, index) <- getWord bytes1
  return $ (bytes2, MethodHandleInfo kind index)
methodTypeInfoParser bytes = do
  (bytes1, index) <- getWord bytes
  return $ (bytes1, MethodTypeInfo index)
invokeDynamicInfoParser = twoTwoBytesInfoParser InvokeDynamicInfo



-- Fields, Methods
fieldInfoAccessFlagsMap = Map.fromList [(0x0001, FieldPublic), (0x0002, FieldPrivate),
                                      (0x0004, FieldProtected), (0x0008, FieldStatic),
                                      (0x0010, FieldFinal), (0x0040, FieldVolatile),
                                      (0x0080, FieldTransient), (0x1000, FieldSynthetic),
                                      (0x4000, FieldEnum)]

methodInfoAccessFlagsMap = Map.fromList [(0x0001, MethodPublic), (0x0002, MethodPrivate),
                                         (0x0004, MethodProtected), (0x0008, MethodStatic),
                                         (0x0010, MethodFinal), (0x0020, MethodSynchronized),
                                         (0x0040, MethodBridge), (0x0080, MethodVarargs),
                                         (0x0100, MethodNative), (0x0400, MethodAbstract),
                                         (0x0800, MethodStrict), (0x1000, MethodSynthetic)]

getFieldMethod pool accessFlagsMap constr bytes = do
  (bytes1, flagBytes) <- getWord bytes
  let accessFlags = foldMask flagBytes accessFlagsMap
  (bytes2, nameIndex) <- getWord bytes1
  (bytes3, descriptorIndex) <- getWord bytes2
  (bytes4, attributesCount) <- getWord bytes3
  (bytesLast, attributes) <- (getNTimes $ getAttribute pool) attributesCount bytes4
  return $ (bytesLast, constr accessFlags nameIndex descriptorIndex attributes)

getField :: [Constant] -> Parser FieldInfo
getField pool = getFieldMethod pool fieldInfoAccessFlagsMap FieldInfo

getMethod :: [Constant] -> Parser MethodInfo
getMethod pool = getFieldMethod pool methodInfoAccessFlagsMap MethodInfo


-- Interfaces
getInterfaces :: RepeatingParser [Word16]
getInterfaces = getNTimes $ getWord



-- Attributes
innerClassAccessFlagsMap = Map.fromList [(0x0001, InnerClassPublic), (0x0002, InnerClassPrivate),
                                         (0x0004, InnerClassProtected), (0x0008, InnerClassStatic),
                                         (0x0010, InnerClassFinal), (0x0200, InnerClassInterface),
                                         (0x0400, InnerClassAbstract), (0x1000, InnerClassSynthetic),
                                         (0x2000, InnerClassAnnotation), (0x4000, InnerClassEnum)]

attributesNamesMap = Map.fromList [("ConstantValue", constantValueAttribute),
                                   ("CodeAttribute", codeAttribute),
                                   ("StackMapTableAttribute", stackMapTableAttribute),
                                   ("Exceptions", exceptionsAttribute),
                                   ("InnerClasses", innerClassesAttribute),
                                   ("EnclosingMethod", enclosingMethodAttribute),
                                   ("Synthetic", syntheticAttribute),
                                   ("Signature", signatureAttribute),
                                   ("SourceFile", sourceFileAttribute),
                                   ("SourceDebugExtension", sourceDebugExtensionAttribute),
                                   ("LineNumberTable", lineNumberTableAttribute),
                                   ("LocalVariableTable", localVariableTableAttribute),
                                   ("LocalVariableTypeTable", localVariableTypeTableAttribute),
                                   ("Deprecated", deprecatedAttribute),
                                   ("RuntimeVisibleAnnotations", runtimeVisibleAnnotationsAttribute),
                                   ("RuntimeInvisibleAnnotations", runtimeInvisibleAnnotationsAttribute),
                                   ("RuntimeVisibleParameterAnnotations", runtimeVisibleParameterAnnotationsAttribute),
                                   ("RuntimeInvisibleParameterAnnotations", runtimeInvisibleParameterAnnotationsAttribute),
                                   ("AnnotationDefault", annotationDefaultAttribute),
                                   ("BootstrapMethods", bootstrapMethodsAttribute)]
constantValueAttribute pool len bytes = do
  (bytes1, value) <- getWord bytes
  return $ (bytes1, ConstantValue value)
--code attribute
getExceptionTable bytes = do
  (bytes1, startPc) <- getWord bytes
  (bytes2, endPc) <- getWord bytes1
  (bytes3, handlerPc) <- getWord bytes2
  (bytes4, catchType) <- getWord bytes3
  return $ (bytes4, Exception startPc endPc handlerPc catchType)
codeAttribute pool len bytes = do
  (bytes1, maxStack) <- getWord bytes
  (bytes2, maxLocals) <- getWord bytes1
  (bytes3, codeLength) <- getWord bytes2
  (bytes4, code) <- takeBytes 2 bytes3
  (bytes5, exceptionTableLength) <- getWord bytes4
  (bytes6, exceptionTable) <- getNTimes getExceptionTable exceptionTableLength bytes5
  (bytes7, attributesCount) <- getWord bytes6
  (bytes8, attributesInfo) <- getNTimes (getAttribute pool) attributesCount bytes7
  return $ (bytes8, CodeAttribute maxStack maxLocals code exceptionTable attributesInfo)
-- -> StackMapTable
-- -> verification type info
verificationTypeInfo = [(0, topVariableInfo), (1, integerVariableInfo),
                        (2, floatVariableInfo), (3, longVariableInfo),
                        (4, doubleVariableInfo), (5, nullVariableInfo),
                        (6, uninitializedThisVariableInfo), (7, objectVariableInfo),
                        (8, uninitializedVariableInfo)]
onlyTagByteInfo bytes constr = return (bytes, constr)
topVariableInfo tag bytes = onlyTagByteInfo bytes TopVariableInfo
integerVariableInfo tag bytes = onlyTagByteInfo bytes IntegerVariableInfo
floatVariableInfo tag bytes = onlyTagByteInfo bytes FloatVariableInfo
longVariableInfo tag bytes = onlyTagByteInfo bytes LongVariableInfo
doubleVariableInfo tag bytes = onlyTagByteInfo bytes DoubleVariableInfo
nullVariableInfo tag bytes = onlyTagByteInfo bytes NullVariableInfo
uninitializedThisVariableInfo tag bytes = onlyTagByteInfo bytes UninitializedThisVariableInfo
objectVariableInfo tag bytes = do
  (bytes1, cpoolIndex) <- getWord bytes
  return $ (bytes1, ObjectVariableInfo cpoolIndex)
uninitializedVariableInfo tag bytes = do
  (bytes1, offset) <- getWord bytes
  return $ (bytes, UninitializedVariableInfo offset)
parseVerificationTypeInfo bytes = do
  (bytes1, tag) <- getByte bytes
  case take 1 . filter (\tags -> tag == fst tags) $ verificationTypeInfo of
    [(_, parser)] -> parser tag bytes1
    [] -> Left "Cake!"
-- <-- verification type info
stackMapFrameList =  [([0..63], sameFrameParser),
                      ([64..127], sameLocals1StackItemFrame),
                      ([247], sameLocals1StackItemFrameExtended),
                      ([248..250], chopFrame),
                      ([251], sameFrameExtended),
                      ([252..254], appendFrame),
                      ([255], fullFrame)]
sameFrameParser tag bytes = return (bytes, SameFrame tag)
sameLocals1StackItemFrame tag bytes = do
  (bytes1, typeInfo) <- parseVerificationTypeInfo bytes
  return (bytes1, SameLocals1StackItemFrame tag typeInfo)
sameLocals1StackItemFrameExtended tag bytes = do
  (bytes1, offsetData) <- getWord bytes
  (bytes2, typeInfo) <- parseVerificationTypeInfo bytes1
  return (bytes2, SameLocals1StackItemFrameExtended tag offsetData typeInfo)
chopFrame tag bytes = do
  (bytes1, offsetDelta) <- getWord bytes
  return (bytes1, ChopFrame tag offsetDelta)
sameFrameExtended tag bytes = do
  (bytes1, offsetDelta) <- getWord bytes
  return (bytes1, SameFrameExtended tag offsetDelta)
appendFrame tag bytes = do
  (bytes1, offsetDelta) <- getWord bytes
  (bytes2, locals) <- getNTimes parseVerificationTypeInfo ((fromIntegral tag) - 251) bytes1
  return (bytes, AppendFrame tag offsetDelta [])
fullFrame tag bytes = do
  (bytes1, offsetDelta) <- getWord bytes
  (bytes2, localLength) <- getWord bytes1
  (bytes3, locals) <- getNTimes parseVerificationTypeInfo localLength bytes2
  (bytes4, stackLength) <- getWord bytes3
  (bytes5, stack) <- getNTimes parseVerificationTypeInfo stackLength bytes4
  return (bytes5, FullFrame tag offsetDelta locals stack)
lookupFrameParser tag =
  case take 1 . filter (\tags -> elem tag $ fst tags) $ stackMapFrameList of
    [] -> Nothing
    (x:_) -> Just $ snd x
getStackMapFrame bytes = do
  (bytes1, tag) <- getByte bytes
  case lookupFrameParser tag of
    Just parser -> parser tag bytes1
    Nothing -> Left "Oooops"
stackMapTableAttribute pool len bytes = do
  (bytes1, entries) <- getNTimes getStackMapFrame len bytes
  return $ (bytes1, StackMapTable entries)
-- -> StackMapTable  
exceptionsAttribute pool len bytes = do
  (bytes1, numberOfExceptions) <- getWord bytes
  (bytes2, exceptions) <- getNTimes (getWord) numberOfExceptions bytes1
  return $ (bytes2, Exceptions exceptions)

innerClass bytes = do
  (bytes1, innerClassInfo) <- getWord bytes
  (bytes2, outerClassInfo) <- getWord bytes1
  (bytes3, innerName) <- getWord bytes2
  (bytes4, innerAccessFlagsBytes) <- getWord bytes3
  let innerAccessFlags = foldMask innerAccessFlagsBytes innerClassAccessFlagsMap
  return (bytes4, InnerClassInfo innerClassInfo outerClassInfo innerName innerAccessFlags)
innerClassesAttribute pool len bytes = do
  (bytes1, length) <- getBytes 2 bytes
  (bytes2, classes) <- getNTimes innerClass length bytes1
  return (bytes2, InnerClasses classes)

enclosingMethodAttribute pool len bytes = do
  (bytes1, classIndex) <- getWord bytes
  (bytes2, methodIndex) <- getWord bytes1
  return (bytes2, EnclosingMethod classIndex methodIndex)

syntheticAttribute pool len bytes = Right (bytes, Synthetic)
  
signatureAttribute pool len bytes = do
  (bytes1, signatureIndex) <- getWord bytes
  return (bytes1, Signature signatureIndex)

sourceFileAttribute pool len bytes = do
  (bytes1, sourceFile) <- getWord bytes
  return (bytes, SourceFile sourceFile)

sourceDebugExtensionAttribute pool len bytes = do
  (bytes1, stringBytes) <- getNTimes getByte len bytes
  return (bytes1, SourceDebugExtension $ bytesToString stringBytes)

lineNumberParser bytes = do
  (bytes1, startPc) <- getWord bytes
  (bytes2, lineNumber) <- getWord bytes1
  return (bytes2, LineNumber startPc lineNumber)
lineNumberTableAttribute pool len bytes =
  constrNTimes LineNumberTable lineNumberParser bytes

localVariableInfoParser bytes = do
  (bytes1, startPc) <- getWord bytes
  (bytes2, lengthPc) <- getWord bytes1
  (bytes3, nameIndex) <- getWord bytes2
  (bytes4, signatureIndex) <- getWord bytes3
  (bytes5, index) <- getWord bytes4
  return (bytes5, LocalVariableInfo startPc lengthPc nameIndex signatureIndex index)
localVariableTableAttribute pool len bytes =
  constrNTimes LocalVariableTable localVariableInfoParser bytes
localVariableTypeTableAttribute pool len bytes =
  constrNTimes LocalVariableTypeTable localVariableInfoParser bytes

deprecatedAttribute pool len bytes = return (bytes, Deprecated)


elementValueParsersList = [("BCDFIJSZs", parseConstValue), ("e", parseEnumValue),
                          ("c", parseClassValue), ("@", parseAnnotationValue),
                          ("[", parseArrayValue)]
parseConstValue tag bytes = do
  (bytes1, value) <- getWord bytes
  return (bytes1, ElementConstValue tag value)
parseEnumValue tag bytes = do
  (bytes1, typeNameIndex) <- getWord bytes
  (bytes2, constNameIndex) <- getWord bytes1
  return (bytes2, ElementEnumConstValue tag typeNameIndex constNameIndex)
parseClassValue tag bytes = do
  (bytes1, classInfoIndex) <- getWord bytes
  return (bytes1, ElementClassInfoIndex tag classInfoIndex)
parseAnnotationValue tag bytes = do
  (bytes1, annotationValue) <- parseAnnotationAttribute bytes
  return (bytes1, ElementAnnotationValue tag annotationValue)
parseArrayValue tag bytes = do
  (bytes1, numValues) <- getWord bytes
  (bytes2, elementValues) <- getNTimes elementValueParser numValues bytes1
  return (bytes2, ElementArrayValue tag elementValues)
elementValueParser bytes = do
  (bytes1, tag) <- takeBytes 1 bytes
  let tagChar = bytesToString tag !! 0
  case take 1 $ filter (\strs -> elem tagChar $ fst strs) elementValueParsersList of
    [(_, parser)] -> parser tagChar bytes
    _ -> Left "AAAAA"
elementValuePairParser bytes = do
  (bytes1, elementNameIndex) <- getWord bytes
  (bytes2, elementValue) <- elementValueParser bytes1
  return (bytes2, ElementValuePair elementNameIndex elementValue)
parseAnnotationAttribute bytes = do
  (bytes1, typeIndex) <- getWord bytes
  (bytes2, elementValuePairsNum) <- getWord bytes1
  (bytes3, elementValuePairs) <- getNTimes elementValuePairParser elementValuePairsNum bytes2
  return (bytes3, Annotation typeIndex elementValuePairs)
runtimeVisibleAnnotationsAttribute pool len bytes =
  constrNTimes RuntimeVisibleAnnotations parseAnnotationAttribute bytes
runtimeInvisibleAnnotationsAttribute pool len bytes =
  constrNTimes RuntimeInvisibleAnnotations parseAnnotationAttribute bytes
runtimeVisibleParameterAnnotationsAttribute pool len bytes =
  let annotationsListParser = constrNTimes id parseAnnotationAttribute
  in constrNTimes RuntimeVisibleParameterAnnotations annotationsListParser bytes
runtimeInvisibleParameterAnnotationsAttribute pool len bytes =
  let annotationListParser = constrNTimes id parseAnnotationAttribute
  in constrNTimes RuntimeInvisibleParameterAnnotations annotationListParser bytes

annotationDefaultAttribute pool len bytes = do
  (bytes1, elementValue) <- takeBytes (fromIntegral len) bytes
  return (bytes1, AnnotationDefault elementValue)

bootstrapMethodParser bytes = do
  (bytes1, methodRef) <- getWord bytes
  (bytes2, argumentsCount) <- getWord bytes
  (bytes3, arguments) <- getNTimes getWord argumentsCount bytes2
  return (bytes3, BootstrapMethod methodRef arguments)
bootstrapMethodsAttribute pool len bytes =
  constrNTimes BootstrapMethods bootstrapMethodParser bytes

parseAttribute :: [Constant] -> String -> Word16 -> Parser AttributeInfo
parseAttribute pool text len bytes = case Map.lookup text attributesNamesMap of
  Just parser -> parser pool len bytes
  Nothing -> Right (drop okLen bytes, UnknownAttribute $ take okLen bytes)
  where okLen = (fromIntegral len) :: Int

getAttribute :: [Constant] -> Parser AttributeInfo
getAttribute pool bytes = do
  (bytes1, attributeNameIndex) <- getWord bytes
  (bytes2, attributeLength) <- getBytes 4 bytes1
  case getFromPool pool attributeNameIndex of
    Just (Utf8Info text) -> parseAttribute pool text attributeLength bytes2
    Just _ -> Left "some cake"
    Nothing -> Left "some other cake"

classBody :: Parser ClassBody
classBody bytes = do
  (bytes1, pool) <- getCountAndList getConstantPool bytes
  (bytes2, flags) <- parseClassAccessFlags bytes1
  (bytes3, this) <- getWord bytes2
  (bytes4, super) <- getWord bytes3
  (bytes5, interfaces) <- getCountAndList getInterfaces bytes4
  (bytes6, fields) <- getCountAndList (getNTimes $ getField pool) bytes5
  (bytes7, methods) <- getCountAndList (getNTimes $ getMethod pool) bytes6
  (bytesLast, attributes) <- getCountAndList (getNTimes $ getAttribute pool) bytes7
  return (bytesLast, ClassBody pool flags this super interfaces fields methods attributes)

-- Parser of all things alive
parse :: [Word8] -> Either String ByteCode
parse bytes = do
  (bytes0, _) <- magicNumber bytes
  (bytes1, minor) <- version bytes0
  (bytes2, major) <- version bytes1
  (bytes3, body) <- classBody bytes2
  if length bytes3 == 0
    then Left "Bytes left"
    else return $ ByteCode minor major body
