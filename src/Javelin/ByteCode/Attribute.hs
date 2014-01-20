module Javelin.ByteCode.Attribute
where

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils

import qualified Data.ByteString.Lazy as BS (ByteString, unpack)

import qualified Data.ByteString as BS2 (unpack)

import Data.Word (Word32, Word16, Word8)
import qualified Data.Map.Lazy as Map (findWithDefault, fromList, Map(..), keys, lookup)
import Data.Maybe
import qualified Data.Binary.Get as G
import Control.Applicative

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
verificationTypeInfo :: Map.Map Word8 (G.Get VerificationTypeInfo)
verificationTypeInfo = Map.fromList [(0, return TopVariableInfo), (1, return IntegerVariableInfo),
                                     (2, return FloatVariableInfo), (3, return LongVariableInfo),
                                     (4, return DoubleVariableInfo), (5, return NullVariableInfo),
                                     (6, return UninitializedThisVariableInfo),
                                     (7, objectVariableInfo),
                                     (8, uninitializedVariableInfo)]
objectVariableInfo = ObjectVariableInfo <$> G.getWord16be
uninitializedVariableInfo = UninitializedVariableInfo <$> G.getWord16be
failingVerificationInfo = fail "Unknown verification info"
parseVerificationTypeInfo' = do
  tag <- G.getWord8
  Map.findWithDefault failingVerificationInfo tag verificationTypeInfo
parseVerificationTypeInfo = convert parseVerificationTypeInfo'
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

exceptionsAttribute pool len = convert $ exceptionsAttribute' pool len
exceptionsAttribute' pool len = Exceptions <$> constrNTimes' id G.getWord16be

innerClass = convert innerClass'
innerClass' = InnerClassInfo <$> G.getWord16be <*> G.getWord16be <*> G.getWord16be
              <*> (foldMask' innerClassAccessFlagsMap <$> G.getWord16be)

innerClassesAttribute pool len = convert $ innerClassesAttribute' pool len
innerClassesAttribute' pool len = InnerClasses <$> getNTimes' innerClass' len

enclosingMethodAttribute' pool len = EnclosingMethod <$> G.getWord16be <*> G.getWord16be
enclosingMethodAttribute pool len = convert $ enclosingMethodAttribute' pool len

syntheticAttribute pool len = convert $ syntheticAttribute' pool len
syntheticAttribute' pool len = return Synthetic

signatureAttribute' pool len = Signature <$> G.getWord16be
signatureAttribute pool len = convert $ signatureAttribute' pool len

sourceFileAttribute pool len = convert $ sourceFileAttribute' pool len
sourceFileAttribute' pool len = SourceFile <$> G.getWord16be

sourceDebugExtensionAttribute' pool len =
  SourceDebugExtension <$> (bytesToString <$> getNTimes' G.getWord16be len)
sourceDebugExtensionAttribute pool len = convert $ sourceDebugExtensionAttribute' pool len

lineNumberParser' = LineNumber <$> G.getWord16be <*> G.getWord16be
lineNumberTableAttribute' pool len = constrNTimes' LineNumberTable lineNumberParser'
lineNumberTableAttribute pool len = convert $ lineNumberTableAttribute' pool len

localVariableInfoParser'= LocalVariableInfo <$> G.getWord16be <*> G.getWord16be <*> G.getWord16be <*> G.getWord16be <*> G.getWord16be
localVariableTableAttribute pool len = convert $ localVariableTableAttribute' pool len
localVariableTableAttribute' pool len =
  constrNTimes' LocalVariableTable localVariableInfoParser'
localVariableTypeTableAttribute pool len =
  convert $ localVariableTypeTableAttribute' pool len
localVariableTypeTableAttribute' pool len =
  constrNTimes' LocalVariableTypeTable localVariableInfoParser'

deprecatedAttribute' pool len = return Deprecated
deprecatedAttribute pool len = convert $ deprecatedAttribute' pool len

-- --> annotations
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
elementValueParser' = undefined
elementValueParser bytes = do
  (bytes1, tag) <- takeBytes 1 bytes
  let tagChar = bytesToString tag !! 0
  case take 1 $ filter (\strs -> elem tagChar $ fst strs) elementValueParsersList of
    [(_, parser)] -> parser tagChar bytes
    _ -> Left "AAAAA"

elementValuePairParser' = ElementValuePair <$> G.getWord16be <*> elementValueParser'
elementValuePairParser = convert elementValuePairParser'

parseAnnotationAttribute = convert parseAnnotationAttribute'
parseAnnotationAttribute' =
  Annotation <$> G.getWord16be <*> constrNTimes' id elementValuePairParser'

runtimeVisibleAnnotationsAttribute' pool len =
  constrNTimes' RuntimeVisibleAnnotations parseAnnotationAttribute'
runtimeVisibleAnnotationsAttribute pool len =
  convert $ runtimeVisibleAnnotationsAttribute' pool len

runtimeInvisibleAnnotationsAttribute' pool len =
  constrNTimes' RuntimeInvisibleAnnotations parseAnnotationAttribute'
runtimeInvisibleAnnotationsAttribute pool len =
  convert $ runtimeInvisibleAnnotationsAttribute' pool len

runtimeVisibleParameterAnnotationsAttribute' pool len =
  RuntimeVisibleParameterAnnotations <$>
  getNTimes' (constrNTimes' id parseAnnotationAttribute') len
runtimeVisibleParameterAnnotationsAttribute pool len =
  convert $ runtimeVisibleParameterAnnotationsAttribute' pool len

runtimeInvisibleParameterAnnotationsAttribute' pool len =
  RuntimeInvisibleParameterAnnotations <$>
  getNTimes' (constrNTimes' id parseAnnotationAttribute') len
runtimeInvisibleParameterAnnotationsAttribute pool len =
  convert $ runtimeInvisibleParameterAnnotationsAttribute' pool len

annotationDefaultAttribute' pool len = AnnotationDefault <$>
                                       (BS2.unpack <$> G.getByteString (fromIntegral len))
annotationDefaultAttribute pool len = convert $ annotationDefaultAttribute' pool len
-- <-- annotations


bootstrapMethodParser' = BootstrapMethod <$> G.getWord16be <*> constrNTimes' id G.getWord16be
bootstrapMethodsAttribute' pool len = constrNTimes' BootstrapMethods bootstrapMethodParser'
bootstrapMethodsAttribute pool len = convert $ bootstrapMethodsAttribute' pool len






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
