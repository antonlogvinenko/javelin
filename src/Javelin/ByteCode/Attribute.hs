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
                                         (0x2000, InnerClassAnn), (0x4000, InnerClassEnum)]

getAttr :: [Constant] -> Get AttrInfo    
getAttr pool = do
  attrNameIndex <- getWord
  attrLength <- getWord
  case getFromPool pool attrNameIndex of
    Just (Utf8Info text) -> parseAttr pool text attrLength
    Just _ -> fail "some cake"
    Nothing -> fail "another cake"

parseAttr :: [Constant] -> String -> Word16 -> Get AttrInfo
parseAttr pool "CodeAttr" len = codeAttr pool len
parseAttr pool text len = case Map.lookup text attrsNamesMap of
  Just parser -> parser len
  Nothing -> UnknownAttr <$> getByteString (fromIntegral len)

attrsNamesMap = Map.fromList [("ConstantValue", constantValueAttr),
                              ("StackMapTableAttr", stackMapTableAttr),
                              ("Exceptions", exceptionsAttr),
                              ("InnerClasses", innerClassesAttr),
                              ("EnclosingMethod", enclosingMethodAttr),
                              ("Synthetic", syntheticAttr),
                              ("Signature", signatureAttr),
                              ("SourceFile", sourceFileAttr),
                              ("SourceDebugExtension", sourceDebugExtensionAttr),
                              ("LineNumberTable", lineNumberTableAttr),
                              ("LocalVariableTable", localVariableTableAttr),
                              ("LocalVariableTypeTable", localVariableTypeTableAttr),
                              ("Deprecated", deprecatedAttr),
                              ("RTVisibleAnns", rtVisibleAnnsAttr),
                              ("RTInvisibleAnns", rtInvisibleAnnsAttr),
                              ("RTVisibleParamAnns", rtVisibleParamAnnsAttr),
                              ("RTInvisibleParamAnns", rtInvisibleParamAnnsAttr),
                              ("AnnDefault", annDefaultAttr),
                              ("BootstrapMethods", bootstrapMethodsAttr)]
                
constantValueAttr len = ConstantValue <$> getWord

getExceptionTable = Exception <$> getWord <*> getWord <*> getWord <*> getWord
codeAttr pool len = CodeAttr <$> getWord <*> getWord
                         <*> several getByte
                         <*> several getExceptionTable
                         <*> several (getAttr pool)

-- -> StackMapTable
stackMapTableAttr len = StackMapTable <$> times getStackMapFrame len
getStackMapFrame = do
  tag <- getByte
  findWithDefault failingStackMapFrame tag stackMapFrameList $ tag
findWithDefault dft tag m =
  case take 1 . filter (elem tag . fst) $ m of
    [(_, f)] -> f
    _ -> dft
stackMapFrameList = [([0..63], sameFrameParser),
                     ([64..127], sameLocals1StackItemFrame),
                     ([247], sameLocals1StackItemFrameExtended),
                     ([248..250], chopFrame),
                     ([251], sameFrameExtended),
                     ([252..254], appendFrame),
                     ([255], fullFrame)]
failingStackMapFrame tag = fail "AAAAAAAA!!!!1111"

sameFrameParser tag = return $ SameFrame tag
sameLocals1StackItemFrame tag = SameLocals1StackItemFrame tag <$> parseVerifTypeInfo
sameLocals1StackItemFrameExtended tag =
  SameLocals1StackItemFrameExtended tag <$> getWord <*> parseVerifTypeInfo
chopFrame tag = ChopFrame tag <$> getWord
sameFrameExtended tag = SameFrameExtended tag <$> getWord  
appendFrame tag = AppendFrame tag
                   <$> getWord
                   <*> times parseVerifTypeInfo ((fromIntegral tag) - 251)
fullFrame tag = FullFrame tag <$> getWord
                <*> several parseVerifTypeInfo
                <*> several parseVerifTypeInfo
--   -> VerifTypeInfo
verifTypeInfo :: Map.Map Word8 (Get VerifTypeInfo)
verifTypeInfo = Map.fromList [(0, return TopVariableInfo), (1, return IntegerVariableInfo),
                                     (2, return FloatVariableInfo), (3, return LongVariableInfo),
                                     (4, return DoubleVariableInfo), (5, return NullVariableInfo),
                                     (6, return UninitializedThisVariableInfo),
                                     (7, objectVariableInfo),
                                     (8, uninitializedVariableInfo)]
objectVariableInfo = ObjectVariableInfo <$> getWord
uninitializedVariableInfo = UninitializedVariableInfo <$> getWord
failingVerifInfo = fail "Unknown verif info"
parseVerifTypeInfo = do
  tag <- getByte
  Map.findWithDefault failingVerifInfo tag verifTypeInfo
--   <-- VerifTypeInfo
-- <- StackMapTable

exceptionsAttr len = Exceptions <$> several getWord

innerClass = InnerClassInfo <$> getWord <*> getWord <*> getWord
              <*> (foldMask innerClassAccessFlagsMap <$> getWord)
innerClassesAttr len = InnerClasses <$> times innerClass len

enclosingMethodAttr len = EnclosingMethod <$> getWord <*> getWord

syntheticAttr len = return Synthetic

signatureAttr len = Signature <$> getWord

sourceFileAttr len = SourceFile <$> getWord

sourceDebugExtensionAttr len =
  SourceDebugExtension <$> (bytesToString <$> times getWord len)

lineNumberTableAttr len = LineNumberTable
                                    <$> several (LineNumber <$> getWord <*> getWord)

localVariableInfoParser= LocalVariableInfo <$> getWord <*> getWord <*> getWord <*> getWord <*> getWord
localVariableTableAttr len =
  LocalVariableTable <$> several localVariableInfoParser
localVariableTypeTableAttr len =
  LocalVariableTypeTable <$> several localVariableInfoParser

deprecatedAttr len = return Deprecated

-- --> Annotations
rtVisibleAnnsAttr len = RTVisibleAnns <$> several  parseAnnAttr
rtInvisibleAnnsAttr len = RTInvisibleAnns <$> several parseAnnAttr
rtVisibleParamAnnsAttr len = RTVisibleParamAnns <$> times (several parseAnnAttr) len
rtInvisibleParamAnnsAttr len = RTInvisibleParamAnns <$> times (several parseAnnAttr) len

parseAnnAttr = Ann <$> getWord <*> several elementValuePairParser
elementValuePairParser = ElementValuePair <$> getWord <*> elementValueParser
elementValueParser = do
  tag <- getByteString 1
  let tagChar = bytesToString tag !! 0
  case take 1 . filter (elem tagChar . fst) $ elementValueParsersList of
    [(_, parser)] -> parser tagChar
    _ -> fail "Aaaa"

elementValueParsersList = [("BCDFIJSZs", parseConstValue), ("e", parseEnumValue),
                           ("c", parseClassValue), ("@", parseAnnValue),
                           ("[", parseArrayValue)]
parseConstValue tag = ElementConstValue tag <$> getWord
parseEnumValue tag = ElementEnumConstValue tag <$> getWord <*> getWord
parseClassValue tag = ElementClassInfoIndex tag <$> getWord
parseAnnValue tag = ElementAnnValue tag <$> parseAnnAttr
parseArrayValue tag = ElementArrayValue tag <$> several elementValueParser
-- <-- Annotations

annDefaultAttr len = AnnDefault <$> (unpack <$> getByteString (fromIntegral len))

bootstrapMethodsAttr len = BootstrapMethods <$>
                                     several (BootstrapMethod <$> getWord <*> several getWord)
