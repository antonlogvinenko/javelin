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
parseAttr pool text len = case Map.lookup text attrsNamesMap of
  Just parser -> parser pool len
  Nothing -> UnknownAttr <$> getByteString (fromIntegral len)

attrsNamesMap = Map.fromList [("ConstantValue", constantValueAttr),
                                   ("CodeAttr", codeAttr),
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
                                   ("RTVisibleParameterAnns", rtVisibleParameterAnnsAttr),
                                   ("RTInvisibleParameterAnns", rtInvisibleParameterAnnsAttr),
                                   ("AnnDefault", annotationDefaultAttr),
                                   ("BootstrapMethods", bootstrapMethodsAttr)]

constantValueAttr pool len = ConstantValue <$> getWord

getExceptionTable = Exception <$> getWord <*> getWord <*> getWord <*> getWord
codeAttr pool len = CodeAttr <$> getWord <*> getWord
                         <*> severalTimes getByte
                         <*> severalTimes getExceptionTable
                         <*> severalTimes (getAttr pool)

-- -> StackMapTable
stackMapTableAttr pool len = StackMapTable <$> nTimes getStackMapFrame len
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
                   <*> nTimes parseVerifTypeInfo ((fromIntegral tag) - 251)
fullFrame tag = FullFrame tag <$> getWord
                <*> severalTimes parseVerifTypeInfo
                <*> severalTimes parseVerifTypeInfo
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

exceptionsAttr pool len = Exceptions <$> severalTimes getWord

innerClass = InnerClassInfo <$> getWord <*> getWord <*> getWord
              <*> (foldMask innerClassAccessFlagsMap <$> getWord)
innerClassesAttr pool len = InnerClasses <$> nTimes innerClass len

enclosingMethodAttr pool len = EnclosingMethod <$> getWord <*> getWord

syntheticAttr pool len = return Synthetic

signatureAttr pool len = Signature <$> getWord

sourceFileAttr pool len = SourceFile <$> getWord

sourceDebugExtensionAttr pool len =
  SourceDebugExtension <$> (bytesToString <$> nTimes getWord len)

lineNumberTableAttr pool len = LineNumberTable
                                    <$> severalTimes (LineNumber <$> getWord <*> getWord)

localVariableInfoParser= LocalVariableInfo <$> getWord <*> getWord <*> getWord <*> getWord <*> getWord
localVariableTableAttr pool len =
  LocalVariableTable <$> severalTimes localVariableInfoParser
localVariableTypeTableAttr pool len =
  LocalVariableTypeTable <$> severalTimes localVariableInfoParser

deprecatedAttr pool len = return Deprecated

-- --> Annotations
rtVisibleAnnsAttr pool len = RTVisibleAnns <$> severalTimes  parseAnnAttr
rtInvisibleAnnsAttr pool len = RTInvisibleAnns <$> severalTimes parseAnnAttr
rtVisibleParameterAnnsAttr pool len = RTVisibleParameterAnns <$> nTimes (severalTimes parseAnnAttr) len
rtInvisibleParameterAnnsAttr pool len = RTInvisibleParameterAnns <$> nTimes (severalTimes parseAnnAttr) len

parseAnnAttr = Ann <$> getWord <*> severalTimes elementValuePairParser
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
parseArrayValue tag = ElementArrayValue tag <$> severalTimes elementValueParser
-- <-- Annotations

annotationDefaultAttr pool len = AnnDefault <$> (unpack <$> getByteString (fromIntegral len))

bootstrapMethodsAttr pool len = BootstrapMethods <$>
                                     severalTimes (BootstrapMethod <$> getWord <*> severalTimes getWord)
