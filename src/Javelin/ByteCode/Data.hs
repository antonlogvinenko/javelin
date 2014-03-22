module Javelin.ByteCode.Data
where
  
import Data.ByteString (ByteString)
import Data.Word (Word32, Word16, Word8)
import Data.Int (Int32, Int64)

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
                      | ClassSynthetic | ClassAnn | ClassEnum
                      deriving (Show, Eq)

data Constant = Utf8Info { stringValue :: String }
              | IntegerInfo { integer :: Int32 }
              | FloatInfo { float :: Float }
              | LongInfo { long :: Int64 }
              | DoubleInfo { double :: Double }
              | ClassInfo { nameIndex :: Word16 }
              | StringInfo { stringIndex :: Word16 }
              | NameAndTypeInfo { nameIndex :: Word16, nameAndTypeDescriptorIndex :: Word16 }
              | Fieldref { classIndex :: Word16, nameAndTypeIndex :: Word16 }
              | Methodref { classIndex :: Word16, nameAndTypeIndex :: Word16 }
              | InterfaceMethodref { classIndex :: Word16, nameAndTypeIndex :: Word16 }
              | MethodHandleInfo { referenceKind :: Word8, referenceIndex :: Word16 }
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

data MethodInfo = MethodInfo { methodAccessFlags :: [MethodInfoAccessFlag],
                               methodNameIndex :: Word16,
                               methodInfoDescriptorIndex :: Word16,
                               methodAttrs :: [AttrInfo]
                             } deriving (Show, Eq)

data MethodInfoAccessFlag = MethodPublic | MethodPrivate | MethodProtected
                          | MethodStatic | MethodFinal | MethodSynchronized
                          | MethodBridge | MethodVarargs | MethodNative
                          | MethodAbstract | MethodStrict | MethodSynthetic
                          deriving (Show, Eq)

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
              | RTVisibleParamAnns { paramAnns :: [[Ann]] }
              | RTInvisibleParamAnns { paramAnns :: [[Ann]] }
              | RTVisibleTypeAnns { typeAnns :: [TypeAnn] }
              | RTInvisibleTypeAnns { typeAnns :: [TypeAnn] }
              | AnnDefault { defaultValue :: [Word8] }
              | BootstrapMethods { bootstrapMethods :: [BootstrapMethod] }
              | MethodParameters { parameters :: [MethodParameter] }
              deriving (Show, Eq)

data MethodParameter = MethodParameter { parameterNameIndex :: Word16,
                                         parameterAccessFlags :: [ParameterAccessFlag] }
                       deriving (Show, Eq)

data ParameterAccessFlag = ACC_FINAL | ACC_SYNTHETIC | ACC_MANDATED
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

data TypeAnn = TypeAnn { targetType :: TypeTargetType,
                         targetInfo :: TypeTargetInfo,
                         targetPath :: [TypePathElem],
                         typeAnnotation :: Ann }
             deriving (Show, Eq)
data TypePathElem = TypePathElem { typePathKind :: Word8,
                                   typeArgumentIndex :: Word8 }
                  deriving (Show, Eq)
data TypeTargetType = GenericClassInterface | MethodConstructor
                    | ClassInterfaceExtendsImplements
                    | ClassInterfaceBound | MethodConstructorBound
                    | Field | ReturnConstructed | MethodConstructorReceiver | MethodConstructorLambda
                    | Throws
                    | LocalVar | ResourceVar
                    | ExceptionParameter
                    | InstanceOf | New | NewReference | MethodReference
                    | CastExpr | NewArgType | MethodArgType | NewRefArg | MethodRefArg
                    deriving (Show, Eq)
data TypeTargetInfo = TypeParameterTarget { typeParameterIndex :: Word8}
                    | SupertypeTarget { superTypeIndex :: Word16 }
                    | TypeParameterBoundTarget { typeParameterIndex :: Word8,
                                                 boundIndex :: Word8 }
                    | EmptyTarget {}
                    | FormalParameterTarget { formalParameterIndex :: Word8 }
                    | ThrowsTarget { throwsTypeIndex :: Word16 }
                    | LocalvarTarget { localvarTargetTable :: [LocalVarTargetElem] }
                    | CatchTarget { exceptionTableIndex :: Word16 }
                    | OffsetTarget { offsetTarget :: Word16 }
                    | TypeArgumentTarget { offsetTarget :: Word16,
                                           typeArgumentTargetIndex :: Word8 }
                    deriving (Show, Eq)
data LocalVarTargetElem = LocalVarTargetElem { varStartPc :: Word16,
                                               varLen :: Word16,
                                               varIndex :: Word16 }
                        deriving (Show, Eq)

data Exception = Exception { startPc :: Word16,
                             endPc :: Word16,
                             handlerPc :: Word16,
                             catchType :: Word16
                           } deriving (Show, Eq)

data VerifTypeInfo = TopVariableInfo
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
                                                 stackItem :: VerifTypeInfo }
                   | SameLocals1StackItemFrameExtended { frameType :: Word8,
                                                         offsetData :: Word16,
                                                         stackItem :: VerifTypeInfo }
                   | ChopFrame { frameType :: Word8,
                                 offsetDelta :: Word16 }
                   | SameFrameExtended {frameType :: Word8,
                                        offsetDelta :: Word16 }
                   | AppendFrame { frameType :: Word8,
                                   offsetDelta :: Word16,
                                   locals :: [VerifTypeInfo] }
                   | FullFrame { frameType :: Word8,
                                 offsetDelta :: Word16,
                                 locals :: [VerifTypeInfo],
                                 stack :: [VerifTypeInfo] }
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


-- Signatures
type QualifiedName = [String]
type UnqualifiedName = String
data FieldType = BaseType { baseType :: BaseType }
               | ObjectType { className :: QualifiedName }
               | ArrayType { componentType :: FieldType }
               deriving (Show, Eq)
data BaseType = ByteT | CharT | DoubleT | FloatT | IntT | LongT | ShortT | BooleanT
              deriving (Show, Eq)

-- FieldDescriptor
data FieldDescriptor = FieldDescriptor { fieldType :: FieldType } deriving (Show, Eq)

-- MethodDescriptor
data MethodDescriptor = MethodDescriptor { parameterDescrs :: [FieldType],
                                           returnDescr :: ReturnDescriptor }
                        deriving (Show, Eq)
data ReturnDescriptor = FieldTypeDescriptor { returnTypeDescriptor :: FieldType }
                      | VoidDescriptor deriving (Show, Eq)


-- JavaTypeSignature
data JavaTypeSignature = FromReferenceTypeSignature { fieldTypeSignature :: ReferenceTypeSignature }
                       | FromBaseTypeSignature { baseTypeSignature :: BaseType }
                       deriving (Show, Eq)

-- ReferenceTypeSignature
data ReferenceTypeSignature = FromClassTypeSignature { fieldClassType :: ClassTypeSignature }
                            | ArrayTypeSignature { signatures :: [JavaTypeSignature] }
                            | FromTypeVariableSignature { signature :: TypeVariableSignature }
                            deriving (Show, Eq)

-- ClassTypeSignature
data ClassTypeSignature = ClassTypeSignature { packageSpecifier :: QualifiedName,
                                               simpleSignature :: SimpleClassTypeSignature,
                                               suffix :: [SimpleClassTypeSignature] }
                        deriving (Show, Eq)
data SimpleClassTypeSignature = SimpleClassTypeSignature { sctId :: String,
                                                           typeArguments :: [TypeArgument] }
                              deriving (Show, Eq)
data TypeArgument = TypeArgumentWithIndicator { indicator :: WildcardIndicator,
                                                typeArgumentSignature :: ReferenceTypeSignature }
                  | TypeArgument { typeArgumentSignature :: ReferenceTypeSignature }
                  | Asterisk
                  deriving (Show, Eq)
data WildcardIndicator = Plus | Minus deriving (Show, Eq)
data TypeVariableSignature = TypeVariableSignature { tvId :: UnqualifiedName } deriving (Show, Eq)


-- ClassSignature
data ClassSignature = ClassSignature { classTypeParameters :: [FormalTypeParameter],
                                       superclassSignature :: ClassTypeSignature,
                                       superinterfaceSignature :: [ClassTypeSignature] }
                    deriving (Show, Eq)
data FormalTypeParameter = FormalTypeParameter { ftId :: UnqualifiedName,
                                                 classBound :: ReferenceTypeSignature,
                                                 interfaceBound :: [ReferenceTypeSignature] }
                           deriving (Show, Eq)


--MethodSignature
data MethodSignature = MethodSignature { methodTypeParameters :: [FormalTypeParameter],
                                         typeSignatures :: [JavaTypeSignature],
                                         methodReturnType :: ReturnType,
                                         throwsTypeSignature :: [ThrowsSignature]
                                       } deriving (Show, Eq)
data ReturnType = ReturnTypeSignature { returnTypeSignature :: JavaTypeSignature }
                | VoidTypeSignature
                deriving (Show, Eq)
data ThrowsSignature = ThrowsSignature { throwsClassType :: ClassTypeSignature,
                                         typeVariable :: TypeVariableSignature }
                       deriving (Show, Eq)


-- FieldSignature
data FieldSignature = FieldSignature { referenceType :: ReferenceTypeSignature }
