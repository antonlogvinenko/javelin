module Javelin.ByteCode.Data
where
  
import Data.ByteString (ByteString)
import Data.Word (Word32, Word16, Word8)

data ByteCode = ByteCode {minVer :: Word16, majVer :: Word16, body :: ClassBody}
                deriving (Show, Eq)

data ClassBody = ClassBody { constPool :: [Constant],
                             classAccessFlags :: [ClassAccessFlags],
                             this :: Word16,
                             super :: Word16,
                             interfaces :: [Word16],
                             fields :: [FieldInfo],
                             methods :: [MethodInfo],
                             attrs :: [AttrInfo] }
                 deriving (Show, Eq)

data ClassAccessFlags = ClassPublic | ClassFinal
                      | ClassSuper | ClassInterface | ClassAbstract
                      | ClassSynthetic | ClassAnn | ClassEnum deriving (Show, Eq)

data Constant = Utf8Info { stringValue :: String }
              | IntegerInfo {bytes :: Word32 }
              | FloatInfo {bytes :: Word32 }
              | LongInfo {highBytes :: Word32, lowBytes :: Word32 }
              | DoubleInfo {highBytes :: Word32, lowBytes :: Word32 }
              | ClassInfo {nameIndex :: Word16 }
              | StringInfo {stringIndex :: Word16 }
              | Fieldref {classIndex :: Word16, nameAndTypeIndex :: Word16 }
              | Methodref {classIndex :: Word16, nameAndTypeIndex :: Word16 }
              | InterfaceMethodref {classIndex :: Word16, nameAndTypeIndex :: Word16 }
              | NameAndTypeInfo { nameIndex :: Word16, nameAndTypeDescriptorIndex :: Word16 }
              | MethodHandleInfo { referenceKind :: Word8, refereneIndex :: Word16 }
              | MethodTypeInfo { methodTypeDescriptorIndex :: Word16 }
              | InvokeDynamicInfo { bootstrapMethodAttrIndex :: Word16, nameAndTypeIndex :: Word16 } deriving (Show, Eq)

data FieldInfo = FieldInfo { fieldAccessFlags :: [FieldInfoAccessFlag],
                             fieldNameIndex :: Word16,
                             fieldDescriptorIndex :: Word16,
                             fieldAttrs :: [AttrInfo]
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
                               methodAttrs :: [AttrInfo]
                             } deriving (Show, Eq)


data AttrInfo = UnknownAttr { unknownBytes :: ByteString }
                   | ConstantValue { constantValueIndex :: Word16 }
                   | CodeAttr { maxStack :: Word16,
                                     maxLocals :: Word16,
                                     code :: [Word8],
                                     exceptionTable :: [Exception],
                                     codeAttrs :: [AttrInfo] }
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
                   | RTVisibleAnns { annotations :: [Ann] }
                   | RTInvisibleAnns { annotations :: [Ann] }
                   | RTVisibleParameterAnns { parameterAnns :: [[Ann]] }
                   | RTInvisibleParameterAnns { parameterAnns :: [[Ann]] }
                   | AnnDefault { defaultValue :: [Word8] }
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
                  | ElementAnnValue { tag :: Char,
                                             annotation :: Ann }
                  | ElementArrayValue { tag :: Char,
                                        elementValues :: [ElementValue] }
                  deriving (Show, Eq)

data ElementValuePair = ElementValuePair { elementNameIndex :: Word16,
                                           elementValue :: ElementValue
                                         } deriving (Show, Eq)

data Ann = Ann { typeIndex :: Word16,
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
                           | InnerClassAnn | InnerClassEnum
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
