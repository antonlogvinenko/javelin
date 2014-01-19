module Javelin.ByteCode.Attribute
where

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils

import Data.Word (Word32, Word16, Word8)
import qualified Data.Map.Lazy as Map (findWithDefault, fromList, Map(..), keys, lookup)
import Data.Maybe
import qualified Data.Binary.Get as G

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
  (bytes1, length) <- getWord bytes
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
  (bytes2, attributeLength) <- getWord bytes1
  case getFromPool pool attributeNameIndex of
    Just (Utf8Info text) -> parseAttribute pool text attributeLength bytes2
    Just _ -> Left "some cake"
    Nothing -> Left "some other cake"

getAttribute' :: [Constant] -> G.Get AttributeInfo    
getAttribute' = undefined
