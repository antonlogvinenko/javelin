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
constantValueAttribute pool len = ConstantValue <$> G.getWord16be
getExceptionTable = Exception <$>
                     G.getWord16be <*> G.getWord16be <*> G.getWord16be <*> G.getWord16be
codeAttribute pool len = CodeAttribute
                         <$> G.getWord16be <*> G.getWord16be
                         <*> constrNTimes id G.getWord8
                         <*> constrNTimes id getExceptionTable
                         <*> constrNTimes id (getAttribute pool)

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
parseVerificationTypeInfo = do
  tag <- G.getWord8
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
  SameLocals1StackItemFrameExtended tag <$> G.getWord16be <*> parseVerificationTypeInfo
chopFrame tag = ChopFrame tag <$> G.getWord16be
sameFrameExtended tag = SameFrameExtended tag <$> G.getWord16be  
appendFrame tag = AppendFrame tag
                   <$> G.getWord16be
                   <*> getNTimes parseVerificationTypeInfo ((fromIntegral tag) - 251)
fullFrame tag = FullFrame tag <$> G.getWord16be
                <*> constrNTimes id parseVerificationTypeInfo
                <*> constrNTimes id parseVerificationTypeInfo
failingStackMapFrame tag = fail "AAAAAAAA!!!!1111"
findWithDefault dft tag m =
  case take 1 . filter (elem tag . fst) $ m of
    [(_, f)] -> f
    _ -> dft
getStackMapFrame = do
  tag <- G.getWord8
  findWithDefault failingStackMapFrame tag stackMapFrameList $ tag
stackMapTableAttribute pool len = StackMapTable <$> getNTimes getStackMapFrame len
-- -> StackMapTable  

exceptionsAttribute pool len = Exceptions <$> constrNTimes id G.getWord16be
innerClass = InnerClassInfo <$> G.getWord16be <*> G.getWord16be <*> G.getWord16be
              <*> (foldMask innerClassAccessFlagsMap <$> G.getWord16be)
innerClassesAttribute pool len = InnerClasses <$> getNTimes innerClass len
enclosingMethodAttribute pool len = EnclosingMethod <$> G.getWord16be <*> G.getWord16be
syntheticAttribute pool len = return Synthetic
signatureAttribute pool len = Signature <$> G.getWord16be
sourceFileAttribute pool len = SourceFile <$> G.getWord16be
sourceDebugExtensionAttribute pool len =
  SourceDebugExtension <$> (bytesToString <$> getNTimes G.getWord16be len)
lineNumberParser = LineNumber <$> G.getWord16be <*> G.getWord16be
lineNumberTableAttribute pool len = constrNTimes LineNumberTable lineNumberParser
localVariableInfoParser= LocalVariableInfo <$> G.getWord16be <*> G.getWord16be <*> G.getWord16be <*> G.getWord16be <*> G.getWord16be
localVariableTableAttribute pool len =
  constrNTimes LocalVariableTable localVariableInfoParser
localVariableTypeTableAttribute pool len =
  constrNTimes LocalVariableTypeTable localVariableInfoParser
deprecatedAttribute pool len = return Deprecated

-- --> annotations
elementValueParsersList = [("BCDFIJSZs", parseConstValue), ("e", parseEnumValue),
                            ("c", parseClassValue), ("@", parseAnnotationValue),
                            ("[", parseArrayValue)]
parseConstValue tag = ElementConstValue tag <$> G.getWord16be
parseEnumValue tag = ElementEnumConstValue tag <$> G.getWord16be <*> G.getWord16be
parseClassValue tag = ElementClassInfoIndex tag <$> G.getWord16be
parseAnnotationValue tag = ElementAnnotationValue tag <$> parseAnnotationAttribute
parseArrayValue tag = ElementArrayValue tag <$> constrNTimes id elementValueParser  
elementValueParser = do
  tag <- G.getByteString 1
  let tagChar = bytesToString tag !! 0
  case take 1 . filter (elem tagChar . fst) $ elementValueParsersList of
    [(_, parser)] -> parser tagChar
    _ -> fail "Aaaa"

elementValuePairParser = ElementValuePair <$> G.getWord16be <*> elementValueParser
parseAnnotationAttribute =
  Annotation <$> G.getWord16be <*> constrNTimes id elementValuePairParser

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
                                       (BS2.unpack <$> G.getByteString (fromIntegral len))
-- <-- annotations


bootstrapMethodParser = BootstrapMethod <$> G.getWord16be <*> constrNTimes id G.getWord16be
bootstrapMethodsAttribute pool len = constrNTimes BootstrapMethods bootstrapMethodParser

parseAttribute :: [Constant] -> String -> Word16 -> G.Get AttributeInfo
parseAttribute pool text len = case Map.lookup text attributesNamesMap of
  Just parser -> parser pool len
  Nothing -> UnknownAttribute <$> G.getByteString (fromIntegral len)
  

getAttribute :: [Constant] -> G.Get AttributeInfo    
getAttribute pool = do
  attributeNameIndex <- G.getWord16be
  attributeLength <- G.getWord16be
  case getFromPool pool attributeNameIndex of
    Just (Utf8Info text) -> parseAttribute pool text attributeLength
    Just _ -> fail "some cake"
    Nothing -> fail "another cake"
      
