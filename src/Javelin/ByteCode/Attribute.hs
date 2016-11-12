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
  -- Constants
  (0x00, return Nop),
  (0x01, return AConstNull),
  (0x02, return IConstM1),
  (0x03, return IConst0), (0x04, return IConst1), (0x05, return IConst2),
  (0x06, return IConst3), (0x07, return IConst4), (0x08, return IConst5),
  (0x09, return LConst0), (0x0a, return LConst1),
  (0x0b, return FConst0), (0x0c, return FConst1), (0x0d, return FConst2),
  (0x0e, return DConst0), (0x0f, return DConst1),
  (0x10, BiPush <$> getWord8),
  (0x11, SiPush <$> getWord8),
  (0x12, Ldc <$> getCPIndex8),
  (0x13, LdcW <$> getCPIndex16),
  (0x14, Ldc2W <$> getCPIndex16),

  -- Loads
  (0x15, ILoad <$> getLocal),
  (0x16, LLoad <$> getLocal),
  (0x17, FLoad <$> getLocal),
  (0x18, DLoad <$> getLocal),
  (0x19, ALoad <$> getLocal),
  (0x1a, return ILoad0), (0x1b, return ILoad1), (0x1c, return ILoad2), (0x1d, return ILoad3),
  (0x1e, return LLoad0), (0x1f, return LLoad1), (0x20, return LLoad2), (0x21, return LLoad3),
  (0x22, return FLoad0), (0x23, return FLoad1), (0x24, return FLoad2), (0x25, return FLoad3),
  (0x26, return DLoad0), (0x27, return DLoad1), (0x28, return DLoad2), (0x29, return DLoad3),
  (0x2a, return ALoad0), (0x2b, return ALoad1), (0x2c, return ALoad2), (0x2d, return ALoad3),
  (0x2e, return IaLoad), (0x2f, return LaLoad), (0x30, return FaLoad), (0x31, return DaLoad),
  (0x32, return AaLoad), (0x33, return BaLoad), (0x34, return CaLoad), (0x35, return SaLoad),

  -- Stores
  (0x36, IStore <$> getLocal),
  (0x37, LStore <$> getLocal),
  (0x38, FStore <$> getLocal),
  (0x39, DStore <$> getLocal),
  (0x3a, AStore <$> getLocal),
  (0x3b, return IStore0), (0x3c, return IStore1), (0x3d, return IStore2), (0x3e, return IStore3),
  (0x3f, return LStore0), (0x40, return LStore1), (0x41, return LStore2), (0x42, return LStore3),
  (0x43, return FStore0), (0x44, return FStore1), (0x45, return FStore2), (0x46, return FStore3),
  (0x47, return DStore0), (0x48, return DStore1), (0x49, return DStore2), (0x4a, return DStore3),
  (0x4b, return AStore0), (0x4c, return AStore1), (0x4d, return AStore2), (0x4e, return AStore3),
  (0x4f, return IaStore), (0x50, return LaStore), (0x51, return FaStore), (0x52, return DaStore),
  (0x53, return AaStore), (0x54, return BaStore), (0x55, return CaStore), (0x56, return SaStore),

  -- Stack
  (0x57, return Pop), (0x58, return Pop2),
  (0x59, return Dup), (0x5a, return DupX1), (0x5b, return DupX2),
  (0x5c, return Dup2), (0x5d, return Dup2X1), (0x5e, return Dup2X2),
  (0x5f, return Swap),

  -- Math
  (0x60, return IAdd), (0x61, return LAdd), (0x62, return FAdd), (0x63, return DAdd),
  (0x64, return ISub), (0x65, return LSub), (0x66, return FSub), (0x67, return DSub),
  (0x68, return IMul), (0x69, return LMul), (0x6a, return FMul), (0x6b, return DMul),
  (0x6c, return IDiv), (0x6d, return LDiv), (0x6e, return FDiv), (0x6f, return DDiv),
  (0x70, return IRem), (0x71, return LRem), (0x72, return FRem), (0x73, return DRem),
  (0x74, return INeg), (0x75, return LNeg), (0x76, return FNeg), (0x77, return DNeg),

  (0x78, return IShl), (0x79, return LShl),
  (0x7a, return IShr), (0x7b, return LShr),
  (0x7c, return IUshr), (0x7d, return LUshr),

  (0x7e, return IAnd), (0x7f, return LAnd),
  (0x80, return IOr), (0x81, return LOr),
  (0x82, return IXor), (0x83, return LXor),
  (0x84, return IInc),

  -- Conversions
  (0x85, return I2L), (0x86, return I2F), (0x87, return I2D),
  (0x88, return L2I), (0x89, return L2F), (0x8a, return L2D),
  (0x8b, return F2I), (0x8c, return F2L), (0x8d, return F2D),
  (0x8e, return D2I), (0x8f, return D2L), (0x90, return D2F),

  (0x91, return I2B), (0x92, return I2C), (0x93, return I2S),

  -- Comparisons
  (0x94, return LCmp),
  (0x95, return FCmpL), (0x96, return FCmpG),
  (0x97, return DCmpL), (0x98, return DCmpG),

  (0x99, IfEq <$> getWord), (0x9a, IfNe <$> getWord),
  (0x9b, IfLt <$> getWord), (0x9c, IfGe <$> getWord),
  (0x9d, IfGt <$> getWord), (0x9e, IfLe <$> getWord),
  (0x9f, IfICmpEq <$> getWord), (0xa0, IfICmpNe <$> getWord),
  (0xa1, IfICmpLt <$> getWord), (0xa2, IfICmpGe <$> getWord),
  (0xa3, IfICmpGt <$> getWord), (0xa4, IfICmpLe <$> getWord),
  (0xa5, IfACmpEq <$> getWord), (0xa6, IfACmpNe <$> getWord),

  -- Control
  (0xa7, Goto <$> getWord), (0xa8, Jsr <$> getWord), (0xa9, Ret <$> getWord8),
  (0xaa, return TableSwitch), (0xab, return LookupSwitch),
  
  (0xac, return IReturn), (0xad, return LReturn), (0xae, return FReturn), (0xaf, return DReturn),
  (0xb0, return AReturn), (0xb1, return Return),

  -- References
  (0xb2, GetStatic <$> getCPIndex16), (0xb3, PutStatic <$> getCPIndex16),
  (0xb4, GetField <$> getCPIndex16), (0xb5, PutField <$> getCPIndex16),

  (0xb6, InvokeVirtual <$> getCPIndex16),
  (0xb7, InvokeSpecial <$> getCPIndex16),
  (0xb8, InvokeStatic <$> getCPIndex16),
  (0xb9, InvokeInterface <$> getCPIndex16 <*> getWord8),
  (0xba, InvokeDynamic <$> getCPIndex16 <*> getWord8),

  (0xbb, New_ <$> getCPIndex16), (0xbc, NewArray <$> getWord8), (0xbd, ANewArray <$> getCPIndex16),
  (0xbe, return ArrayLength),

  (0xbf, return AThrow),
  (0xc0, CheckCast <$> getCPIndex16), (0xc1, InstanceOf_ <$> getCPIndex16),
  (0xc2, return MonitorEnter), (0xc3, return MonitorExit),

  -- Extended
  (0xc4, return Wide),
  (0xc5, MultiANewArray <$> getCPIndex16 <*> getWord8),
  (0xc6, IfNull <$> getCPIndex16),
  (0xc7, IfNotNull <$> getCPIndex16),
  (0xc8, GotoW <$> getDWord),
  (0xc9, JsrW <$> getDWord),

  -- Reserved
  (0xca, return BreakPoint),
  (0xfe, return ImDep1), (0xff, return ImDep2)
  ]


getCPIndex16 :: Get CPIndex16
getCPIndex16 = CPIndex16 <$> getWord

getCPIndex8 :: Get CPIndex8
getCPIndex8 = CPIndex8 <$> getWord8

getLocal :: Get Local
getLocal = getWord8

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
