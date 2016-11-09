module Javelin.ByteCode.Attribute
where

import Data.ByteString (unpack)
import Data.Word (Word32, Word8)
import qualified Data.Map.Lazy as Map (findWithDefault, fromList, Map, lookup, (!))
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
  attrLength <- getDWord
  case getFromPool pool attrNameIndex of
    Just (Utf8Info text) -> parseAttr pool text attrLength
    Just x -> fail $ "Utf8Info expected for attribute, but found "
    Nothing -> fail "Utf8Info expected for attribute, found nothing. X_X"

parseAttr :: [Constant] -> String -> Word32 -> Get AttrInfo
parseAttr pool "Code" len = codeAttr pool len
parseAttr _ text len = case Map.lookup text attrsNamesMap of
  Just parser -> parser $ fromIntegral len
  Nothing -> do
    byteString <- getByteString $ fromIntegral len
    return $ UnknownAttr byteString

attrsNamesMap = Map.fromList [("ConstantValue", constantValueAttr),
                              ("StackMapTable", stackMapTableAttr),
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
                              ("RuntimeVisibleAnnotations", rtVisibleAnnsAttr),
                              ("RuntimeInvisibleAnnotations", rtInvisibleAnnsAttr),
                              ("RuntimeVisibleParameterAnnotations", rtVisibleParamAnnsAttr),
                              ("RuntimeInvisibleParameterAnnotations", rtInvisibleParamAnnsAttr),
                              ("AnnDefault", annDefaultAttr),
                              ("BootstrapMethods", bootstrapMethodsAttr),
                              ("MethodParameters", methodParametersAttr),
                              ("RuntimeVisibleTypeAnnotations", rtVisibleTypeAnnotationsAttr),
                              ("RuntimeInvisibleTypeAnnotations", rtInvisibleTypeAnnotationsAttr)]
                
constantValueAttr len = ConstantValue <$> getWord

getExceptionTable = Exception <$> getWord <*> getWord <*> getWord <*> getWord

codeAttr pool len = do
  maxStack <- getWord
  maxLocals <- getWord
  codeLength <- getDWord
  code <- isolate (fromIntegral codeLength) parseInstructions
  exceptionTable <- several getExceptionTable
  attributes <- several (getAttr pool)
  return $ CodeAttr maxStack maxLocals code exceptionTable attributes

parseInstructions :: Get [Instruction]
parseInstructions = do
  finish <- isEmpty
  if finish
    then return []
    else do
      code <- getWord8
      instruction <- findInstructionParser code
      instructions <- parseInstructions
      return $ instruction : instructions

-- Remove when all parsers are defined!
findInstructionParser :: Word8 -> Get Instruction
findInstructionParser idx = case Map.lookup idx instructionParsers of
  Just p -> p
  Nothing -> return Unknown

instructionParsers :: Map.Map Word8 (Get Instruction)
instructionParsers = Map.fromList [
  -- Const ops
  (0x00, return Nop),
  (0x01, return AconstNull),
  (0x02, return IconstM1),
  (0x03, return Iconst0),
  (0x04, return Iconst1),
  (0x05, return Iconst2),
  (0x06, return Iconst3),
  (0x07, return Iconst4),
  (0x08, return Iconst5),
  (0x09, return Lconst0),
  (0x0a, return Lconst1),
  (0x0b, return Fconst0),
  (0x0c, return Fconst1),
  (0x0d, return Fconst2),
  (0x0e, return Dconst0),
  (0x0f, return Dconst1),
  (0x10, Bipush <$> getWord8),
  (0x11, Sipush <$> getWord8),
  (0x12, (Ldc . CPIndex8) <$> getWord8),
  (0x13, (LdcW . CPIndex16) <$> getWord),
  (0x14, (Ldc2W . CPIndex16) <$> getWord),

  -- Load ops
  (0x15, (Iload . Local) <$> getWord8),
  (0x16, (Lload . Local) <$> getWord8),
  (0x17, (Fload . Local) <$> getWord8),
  (0x18, (Dload . Local) <$> getWord8),
  (0x19, (Aload . Local) <$> getWord8),
  (0x1a, return Iload0),
  (0x1b, return Iload1),
  (0x1c, return Iload2),
  (0x1d, return Iload3),
  (0x1e, return Lload0),
  (0x1f, return Lload1),
  (0x20, return Lload2),
  (0x21, return Lload3),
  (0x22, return Fload0),
  (0x23, return Fload1),
  (0x24, return Fload2),
  (0x25, return Fload3),
  (0x26, return Dload0),
  (0x27, return Dload1),
  (0x28, return Dload2),
  (0x29, return Dload3),
  (0x2a, return Aload0),
  (0x2b, return Aload1),
  (0x2c, return Aload2),
  (0x2d, return Aload3),
  (0x2e, return Iaload),
  (0x2f, return Laload),
  (0x30, return Faload),
  (0x31, return Daload),
  (0x32, return Aaload),
  (0x33, return Baload),
  (0x34, return Caload),
  (0x35, return Saload),

  -- Store ops
  
  (0xb0, return Areturn),
  (0xb4, Getfield <$> getWord)]


-- -> StackMapTable
stackMapTableAttr len = do
  frames <- several getStackMapFrame
  return $ StackMapTable frames

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
failingStackMapFrame tag = fail "Failed to identify StackMapFrame item based on a tag"

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
                              (7, ObjectVariableInfo <$> getWord),
                              (8, UninitializedVariableInfo <$> getWord)]
failingVerifInfo = fail "Unknown verif info"
parseVerifTypeInfo = do
  tag <- getByte
  Map.findWithDefault failingVerifInfo tag verifTypeInfo
--   <-- VerifTypeInfo
-- <- StackMapTable

exceptionsAttr _ = Exceptions <$> several getWord

innerClass = InnerClassInfo <$> getWord <*> getWord <*> getWord
              <*> (foldMask innerClassAccessFlagsMap <$> getWord)
innerClassesAttr _ = do
  innerClasses <- several innerClass
  return $ InnerClasses innerClasses

enclosingMethodAttr _ = EnclosingMethod <$> getWord <*> getWord

syntheticAttr _ = return Synthetic

signatureAttr _ = Signature <$> getWord

sourceFileAttr _ = SourceFile <$> getWord

sourceDebugExtensionAttr len = do
  byteString <- getByteString (fromIntegral len)
  let string = bytesToString byteString
  return $ SourceDebugExtension string

lineNumberTableAttr len = do
  attrs <- several lineNumberAttr
  return $ LineNumberTable attrs
lineNumberAttr = do
  pc <- getWord
  line <- getWord
  return $ LineNumber pc line

localVariableInfoParser= LocalVariableInfo <$> getWord <*> getWord <*> getWord <*> getWord <*> getWord
localVariableTableAttr len =
  LocalVariableTable <$> several localVariableInfoParser
localVariableTypeTableAttr len =
  LocalVariableTypeTable <$> several localVariableInfoParser

deprecatedAttr len = return Deprecated

-- --> Annotations
rtVisibleAnnsAttr len = RTVisibleAnns <$> several  parseAnnAttr
rtInvisibleAnnsAttr len = RTInvisibleAnns <$> several parseAnnAttr
rtVisibleParamAnnsAttr len = do
  numParameters <- getByte
  annAttrs <- times (several parseAnnAttr) (fromIntegral numParameters)
  return $ RTVisibleParamAnns annAttrs
rtInvisibleParamAnnsAttr len = do
  numParameters <- getByte
  annAttrs <- times (several parseAnnAttr) (fromIntegral numParameters)
  return $ RTInvisibleParamAnns annAttrs

parseAnnAttr = Ann <$> getWord <*> several elementValuePairParser
elementValuePairParser = ElementValuePair <$> getWord <*> elementValueParser
elementValueParser = do
  tag <- getByteString 1
  let tagChar = head $ bytesToString tag
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
addDefauktAttr len = do
  value <- getByteString len
  let bytes = unpack value
  return $ AnnDefault bytes

bootstrapMethodsAttr len = BootstrapMethods <$>
                           several (BootstrapMethod <$> getWord <*> several getWord)

methodParametersAttr len = MethodParameters <$> several methodParameterAttr
methodParametersAccessFlagsMap = Map.fromList [(0x0010, ACC_FINAL),
                                               (0x1000, ACC_SYNTHETIC),
                                               (0x8000, ACC_MANDATED)]
methodParameterAttr = do
  nameIndex <- getWord
  accessFlags <- getWord
  let accessFlagsList = foldMask methodParametersAccessFlagsMap accessFlags
  return $ MethodParameter nameIndex accessFlagsList
  
rtVisibleTypeAnnotationsAttr len = RTVisibleTypeAnns <$> several parseTypeAnnAttr
rtInvisibleTypeAnnotationsAttr len = RTInvisibleTypeAnns <$> several parseTypeAnnAttr
parseTypeAnnAttr = do
  targetType <- getTargetType
  targetInfo <- findWithDefault failingTypeTargetInfo targetType typeTargetInfo
  typePath <- getTypePath
  annotation <- parseAnnAttr
  return $ TypeAnn targetType targetInfo typePath annotation
getTargetType = do
  tag <- getByte
  case Map.lookup tag typeTargetType of
    Nothing -> fail "Illegal tag for type target info"
    Just x -> return x
getTypePath = do
  len <- getByte
  times getTypePathElem $ fromIntegral len
    where getTypePathElem = TypePathElem <$> getByte <*> getByte
typeTargetType :: Map.Map Word8 TypeTargetType
typeTargetType = Map.fromList [(0x00, GenericClassInterface), (0x01, MethodConstructor),
                               (0x10, ClassInterfaceExtendsImplements),
                               (0x11, ClassInterfaceBound), (0x12, MethodConstructorBound),
                               (0x13, Field), (0x14, ReturnConstructed), (0x15, MethodConstructorReceiver), (0x16, MethodConstructorLambda),
                               (0x17, Throws),
                               (0x40, LocalVar), (0x41, ResourceVar),
                               (0x42, ExceptionParameter),
                               (0x43, InstanceOf), (0x44, New), (0x45, NewReference), (0x46, MethodReference),
                               (0x47, CastExpr), (0x48, NewArgType), (0x49, MethodArgType), (0x4A, NewRefArg), (0x4b, MethodRefArg)]
typeTargetInfo :: [([TypeTargetType], Get TypeTargetInfo)]
typeTargetInfo = [([GenericClassInterface, MethodConstructor], typeParameterTargetInfo),
                  ([ClassInterfaceExtendsImplements], supertypeTargetInfo),
                  ([ClassInterfaceBound, MethodConstructorBound], typeParameterBoundTargetInfo),
                  ([Field, ReturnConstructed, MethodConstructorReceiver], emptyTargetInfo),
                  ([MethodConstructorLambda], formalParameterTargetInfo),
                  ([Throws], throwsTargetInfo),
                  ([LocalVar, ResourceVar], localvarTargetInfo),
                  ([ExceptionParameter], catchTargetInfo),
                  ([InstanceOf, New, NewReference, MethodReference], offsetTargetInfo),
                  ([CastExpr, NewArgType, MethodArgType, NewRefArg, MethodRefArg], typeArgumentTargetInfo)]
failingTypeTargetInfo = fail "Unknown type target tag"
typeParameterTargetInfo = TypeParameterTarget <$> getByte
supertypeTargetInfo = SupertypeTarget <$> getWord
typeParameterBoundTargetInfo = TypeParameterBoundTarget <$> getByte <*> getByte
emptyTargetInfo = return EmptyTarget
formalParameterTargetInfo = FormalParameterTarget <$> getByte
throwsTargetInfo = ThrowsTarget <$> getWord
localvarTargetInfo = LocalvarTarget <$> several localVarTargetElem
  where localVarTargetElem = LocalVarTargetElem <$> getWord <*> getWord <*> getWord
catchTargetInfo = CatchTarget <$> getWord
offsetTargetInfo = OffsetTarget <$> getWord
typeArgumentTargetInfo = TypeArgumentTarget <$> getWord <*> getByte
