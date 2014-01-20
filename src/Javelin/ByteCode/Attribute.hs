module Javelin.ByteCode.Attribute
where

import Data.ByteString (unpack)
import Data.Word (Word32, Word16, Word8)
import qualified Data.Map.Lazy as Map (findWithDefault, fromList, Map, lookup)
import Data.Maybe
import Data.Binary.Get
import Control.Applicative

import Javelin.ByteCode.Data
import Javelin.ByteCode.Utils

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
constantValueAttribute pool len = ConstantValue <$> getWord
getExceptionTable = Exception <$>
                     getWord <*> getWord <*> getWord <*> getWord
codeAttribute pool len = CodeAttribute
                         <$> getWord <*> getWord
                         <*> constrNTimes id getByte
                         <*> constrNTimes id getExceptionTable
                         <*> constrNTimes id (getAttribute pool)

-- -> StackMapTable

-- -> verification type info
verificationTypeInfo :: Map.Map Word8 (Get VerificationTypeInfo)
verificationTypeInfo = Map.fromList [(0, return TopVariableInfo), (1, return IntegerVariableInfo),
                                     (2, return FloatVariableInfo), (3, return LongVariableInfo),
                                     (4, return DoubleVariableInfo), (5, return NullVariableInfo),
                                     (6, return UninitializedThisVariableInfo),
                                     (7, objectVariableInfo),
                                     (8, uninitializedVariableInfo)]
objectVariableInfo = ObjectVariableInfo <$> getWord
uninitializedVariableInfo = UninitializedVariableInfo <$> getWord
failingVerificationInfo = fail "Unknown verification info"
parseVerificationTypeInfo = do
  tag <- getByte
  Map.findWithDefault failingVerificationInfo tag verificationTypeInfo
-- <-- verification type info
stackMapFrameList = [([0..63], sameFrameParser),
                      ([64..127], sameLocals1StackItemFrame),
                      ([247], sameLocals1StackItemFrameExtended),
                      ([248..250], chopFrame),
                      ([251], sameFrameExtended),
                      ([252..254], appendFrame),
                      ([255], fullFrame)]
sameFrameParser tag = return $ SameFrame tag
sameLocals1StackItemFrame tag = SameLocals1StackItemFrame tag <$> parseVerificationTypeInfo
sameLocals1StackItemFrameExtended tag =
  SameLocals1StackItemFrameExtended tag <$> getWord <*> parseVerificationTypeInfo
chopFrame tag = ChopFrame tag <$> getWord
sameFrameExtended tag = SameFrameExtended tag <$> getWord  
appendFrame tag = AppendFrame tag
                   <$> getWord
                   <*> getNTimes parseVerificationTypeInfo ((fromIntegral tag) - 251)
fullFrame tag = FullFrame tag <$> getWord
                <*> constrNTimes id parseVerificationTypeInfo
                <*> constrNTimes id parseVerificationTypeInfo
failingStackMapFrame tag = fail "AAAAAAAA!!!!1111"
findWithDefault dft tag m =
  case take 1 . filter (elem tag . fst) $ m of
    [(_, f)] -> f
    _ -> dft
getStackMapFrame = do
  tag <- getByte
  findWithDefault failingStackMapFrame tag stackMapFrameList $ tag
stackMapTableAttribute pool len = StackMapTable <$> getNTimes getStackMapFrame len
-- -> StackMapTable  

exceptionsAttribute pool len = Exceptions <$> constrNTimes id getWord
innerClass = InnerClassInfo <$> getWord <*> getWord <*> getWord
              <*> (foldMask innerClassAccessFlagsMap <$> getWord)
innerClassesAttribute pool len = InnerClasses <$> getNTimes innerClass len
enclosingMethodAttribute pool len = EnclosingMethod <$> getWord <*> getWord
syntheticAttribute pool len = return Synthetic
signatureAttribute pool len = Signature <$> getWord
sourceFileAttribute pool len = SourceFile <$> getWord
sourceDebugExtensionAttribute pool len =
  SourceDebugExtension <$> (bytesToString <$> getNTimes getWord len)
lineNumberParser = LineNumber <$> getWord <*> getWord
lineNumberTableAttribute pool len = constrNTimes LineNumberTable lineNumberParser
localVariableInfoParser= LocalVariableInfo <$> getWord <*> getWord <*> getWord <*> getWord <*> getWord
localVariableTableAttribute pool len =
  constrNTimes LocalVariableTable localVariableInfoParser
localVariableTypeTableAttribute pool len =
  constrNTimes LocalVariableTypeTable localVariableInfoParser
deprecatedAttribute pool len = return Deprecated

-- --> annotations
elementValueParsersList = [("BCDFIJSZs", parseConstValue), ("e", parseEnumValue),
                            ("c", parseClassValue), ("@", parseAnnotationValue),
                            ("[", parseArrayValue)]
parseConstValue tag = ElementConstValue tag <$> getWord
parseEnumValue tag = ElementEnumConstValue tag <$> getWord <*> getWord
parseClassValue tag = ElementClassInfoIndex tag <$> getWord
parseAnnotationValue tag = ElementAnnotationValue tag <$> parseAnnotationAttribute
parseArrayValue tag = ElementArrayValue tag <$> constrNTimes id elementValueParser  
elementValueParser = do
  tag <- getByteString 1
  let tagChar = bytesToString tag !! 0
  case take 1 . filter (elem tagChar . fst) $ elementValueParsersList of
    [(_, parser)] -> parser tagChar
    _ -> fail "Aaaa"

elementValuePairParser = ElementValuePair <$> getWord <*> elementValueParser
parseAnnotationAttribute =
  Annotation <$> getWord <*> constrNTimes id elementValuePairParser

runtimeVisibleAnnotationsAttribute pool len =
  constrNTimes RuntimeVisibleAnnotations parseAnnotationAttribute

runtimeInvisibleAnnotationsAttribute pool len =
  constrNTimes RuntimeInvisibleAnnotations parseAnnotationAttribute

runtimeVisibleParameterAnnotationsAttribute pool len =
  RuntimeVisibleParameterAnnotations <$>
  getNTimes (constrNTimes id parseAnnotationAttribute) len

runtimeInvisibleParameterAnnotationsAttribute pool len =
  RuntimeInvisibleParameterAnnotations <$>
  getNTimes (constrNTimes id parseAnnotationAttribute) len

annotationDefaultAttribute pool len = AnnotationDefault <$>
                                       (unpack <$> getByteString (fromIntegral len))
-- <-- annotations


bootstrapMethodParser = BootstrapMethod <$> getWord <*> constrNTimes id getWord
bootstrapMethodsAttribute pool len = constrNTimes BootstrapMethods bootstrapMethodParser

parseAttribute :: [Constant] -> String -> Word16 -> Get AttributeInfo
parseAttribute pool text len = case Map.lookup text attributesNamesMap of
  Just parser -> parser pool len
  Nothing -> UnknownAttribute <$> getByteString (fromIntegral len)
  

getAttribute :: [Constant] -> Get AttributeInfo    
getAttribute pool = do
  attributeNameIndex <- getWord
  attributeLength <- getWord
  case getFromPool pool attributeNameIndex of
    Just (Utf8Info text) -> parseAttribute pool text attributeLength
    Just _ -> fail "some cake"
    Nothing -> fail "another cake"
      
