module Javelin.ByteCode.Data
where
  
import Data.ByteString (ByteString)
import Data.Word (Word16, Word8, Word32, Word64)
import Data.Int (Int32, Int64)
import Javelin.Util
import Data.List (intercalate)
import Text.Printf
import Javelin.ByteCode.Utils


data Pilcrow = P { heading :: String, text :: [Pilcrow] }

alignLines :: Pilcrow -> [String]
alignLines P { heading = h, text = t } = [h ++ "\n"] ++
                                         (map ("\t" ++) (t >>= alignLines))

display :: Pilcrow -> String
display = concat . alignLines

-- Outputtab :: Int -> String -> String
space n str = (take n $ repeat ' ') ++ str
tab n str = (take n $ repeat '\t') ++ str

out :: [String] -> String
out = intercalate "\n"  

-- ByteCode
instance Show ByteCode where
  show (ByteCode min maj body@(ClassBody {constPool = (ConstantPool p)})) = display $
                                 P "Bytecode" [P ("minor version: " ++ show min) [],
                                               P ("major version: " ++ show maj) [],
                                               P ("flags: " ++ intercalate ", " (map show (classAccessFlags body))) [],
                                               P (printf "This: %s" (showConstant p (at p (this body)))) [],
                                               P (printf "Superclass: %s" (showConstant p (at p (super body)))) [],
                                               P (printf "Interfaces:\n%s" $ out $ map (showConstant p . (at p)) (interfaces body)) [],
                                               P (show (constPool body)) [],
                                               P ("Fields") (map (printField p) (fields body)),
                                               P ("Methods:") (map (printMethod p) (methods body)),
                                               P (printf "Class attributes:\n%s" $ out $ map show (attrs body)) []]

printField :: [Constant] -> FieldInfo -> Pilcrow
printField p f@(FieldInfo {fieldAccessFlags = accessFlags,
                           fieldNameIndex = name,
                           fieldDescriptorIndex = descriptor,
                           fieldAttrs = attributes}) =
  P "FieldInfo" [P (printf "Name\t\t #%d %s" name (showConstant p (at p name))) [],
                 P (printf "Descriptor\t #%d %s"  descriptor (showConstant p (at p descriptor))) [],
                 P (printf "AccessFlags\t %s"  (show accessFlags)) [],
                 P (printf "Attributes\t %s" (show attributes)) []]  

printMethod :: [Constant] -> MethodInfo -> Pilcrow
printMethod p m@(MethodInfo {methodAccessFlags = accessFlags,
                             methodNameIndex = name,
                             methodInfoDescriptorIndex = descriptor,
                             methodAttrs = attributes}) =
  P "MethodInfo" [P (printf "Name: #%d %s" name (showConstant p (at p name))) [],
                  P (printf "Descriptor: #%d %s" descriptor (showConstant p (at p descriptor))) [],
                  P (printf "AccessFlags = %s" (show accessFlags)) [],
                  P (printf "Attributes: %s" (out $ map (printAttribute p) attributes)) []]

printAttribute :: [Constant] ->  AttrInfo -> String
printAttribute p ca@(CodeAttr {}) = out $ [tab 2 $ printf "Max stack: %d" (maxStack ca),
                                           tab 2 $ printf "Max locals: %d" (maxLocals ca),
                                           tab 2 $ printf "Code:"] ++
                                    (map (tab 3 . printCode p) (code ca))
printAttribute p ca = show ca




printCode :: [Constant] -> Instruction -> String
printCode p c = case cpIndex c of
  Nothing -> show c
  Just idx -> out $ [show c,
                     tab 4 (showConst 3 p (at p idx))]
br :: CPIndex -> Maybe Word16
br (CPIndex idx) = Just idx

cpIndex :: Instruction -> Maybe Word16
cpIndex (Ldc x) = br x
cpIndex (LdcW x) = br x
cpIndex (Ldc2W x) = br x
cpIndex (GetStatic x) = br x
cpIndex (PutStatic x) = br x
cpIndex (GetField x) = br x
cpIndex (PutField x) = br x
cpIndex (InvokeVirtual x) = br x
cpIndex (InvokeSpecial x) = br x
cpIndex (InvokeStatic x) = br x
cpIndex (InvokeInterface x _) = br x
cpIndex (InvokeDynamic x _) = br x
cpIndex (New_ x) = br x
cpIndex (ANewArray x) = br x
cpIndex (CheckCast x) = br x
cpIndex (InstanceOf_ x) = br x
cpIndex (WideIInc x _) = br x
cpIndex (WideILoad x) = br x
cpIndex (WideFLoad x) = br x
cpIndex (WideALoad x) = br x
cpIndex (WideLLoad x) = br x
cpIndex (WideDLoad x) = br x
cpIndex (WideIStore x) = br x
cpIndex (WideFStore x) = br x
cpIndex (WideAStore x) = br x
cpIndex (WideLStore x) = br x
cpIndex (WideDStore x) = br x
cpIndex (WideRet x) = br x
cpIndex (MultiANewArray x _) = br x
cpIndex (IfNull x) = br x
cpIndex (IfNotNull x) = br x
cpIndex _ = Nothing



-- Constant pool
instance Show ConstantPool where
  show (ConstantPool p) = out $ "Constant pool:" : (map showConstLine $ zip (iterate (1+) 0) (map (showConst 0 p) p))
showConstLine :: (Int, String) -> String
showConstLine (i, c) = space 2 $ printf "#%d\t%s" (i + 1) c

showConstant :: [Constant] -> Constant -> String
showConstant = showConst 0

showConst :: Int -> [Constant] -> Constant -> String
showConst pad _ (Utf8Info s) = printf "Utf8 { %s }" s
showConst pad _ (IntegerInfo i) = printf "Int { %d }" i
showConst pad _ (FloatInfo f) = printf "Float { %f }" f
showConst pad _ (LongInfo l) = printf "Long { %d }" l
showConst pad _ (DoubleInfo d) = printf "Double { %f }" d

showConst pad p (StringInfo s) = printf "String { string = #%d: %s }" s (showConstant p (at p s))
showConst pad p (ClassInfo i) = printf "Class { name = #%d: %s }" i (showConstant p (at p i))
showConst pad p (NameAndTypeInfo n t) = out [printf "NameAndType {\tname = #%d %s" n (showConstant p (at p n)),
                                             tab (pad + 3) $ printf "descriptor = #%d %s }" t (showConstant p (at p t))]
                                    
showConst pad p (Fieldref c nt) = out [printf "Fieldref {\tclass = #%d %s" c (showConstant p (at p c)),
                                       tab (pad + 3) $ printf "name_and_type = #%d %s }" nt (showConst (pad + 5) p (at p nt))]
showConst pad p (Methodref c nt) = out [printf "Methodref {\tclass = #%d %s" c (showConstant p (at p c)),
                                        tab (pad + 3) $ printf "name_and_type = #%d %s }" nt (showConst (pad + 5) p (at p nt))]
showConst pad p (InterfaceMethodref c nt) = out [printf "InterfaceMethodref {\tclass = #%d %s" c (showConstant p (at p c)),
                                                 tab (pad + 3) $ printf "name_and_type = #%d %s }" nt (showConst (pad + 5) p (at p nt))]
showConst pad p (MethodHandleInfo rk ri) = out [printf "MethodHandleInfo {\treference_kind = #%d" rk,
                                                tab (pad + 3) $ printf "reference = #%d %s }" ri (showConst 0 p (at p ri))]
showConst pad p (MethodTypeInfo i) = out [printf "MethodTypeInfo {\tdescriptor = #%d %s }" i (showConst 0 p (at p i))]
showConst pad p (InvokeDynamicInfo bi ti) = out [printf "InvokeDynamicInfo {\tbootstrap_method_attr = #%d %s }" bi (showConst 0 p (at p bi)),
                                                 tab (pad + 3) $ printf "name_and_type = #%d %s }" ti (showConst 0 p (at p ti))]


-- Definitions

data ByteCode = ByteCode {minVer :: Word16, majVer :: Word16, body :: ClassBody}
              deriving Eq


data ClassBody = ClassBody { constPool :: ConstantPool,
                             classAccessFlags :: [ClassAccessFlags],
                             this :: Word16,
                             super :: Word16, --0 for java.lang.Object
                             interfaces :: [Word16],
                             fields :: [FieldInfo],
                             methods :: [MethodInfo],
                             attrs :: [AttrInfo] }
                 deriving Eq

newtype ConstantPool = ConstantPool [Constant] deriving (Eq)

  
data ClassAccessFlags = AccPublic | AccFinal
                    | AccSuper | AccInterface | AccAbstract
                    | AccSynthetic | AccAnn | AccEnum
                    deriving (Show, Eq)

data Constant = Utf8Info { stringValue :: String }
              | IntegerInfo { integer :: Int32 }
              | FloatInfo { float :: Float }
              | LongInfo { long :: Int64 }
              | DoubleInfo { double :: Double }
              | StringInfo { stringIndex :: Word16 }
              | ClassInfo { nameIndex :: Word16 }
              | NameAndTypeInfo { nameIndex :: Word16, nameAndTypeDescriptorIndex :: Word16 }
              | Fieldref { classIndex :: Word16, nameAndTypeIndex :: Word16 }
              | Methodref { classIndex :: Word16, nameAndTypeIndex :: Word16 }
              | InterfaceMethodref { classIndex :: Word16, nameAndTypeIndex :: Word16 }

              | MethodHandleInfo { referenceKind :: Word8, referenceIndex :: Word16 }
              | MethodTypeInfo { methodTypeDescriptorIndex :: Word16 }
              | InvokeDynamicInfo { bootstrapMethodAttrIndex :: Word16, nameAndTypeIndex :: Word16 } deriving Eq

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

newtype CPIndex = CPIndex Word16
                deriving (Eq, Show)
type Local = Word8
type BranchOffset = Word16
  
data Instruction =
                   -- Constants
                   Nop |
                   AConstNull |
                   IConstM1 | IConst0 | IConst1 | IConst2 | IConst3 | IConst4 | IConst5 |
                   LConst0 | LConst1 |
                   FConst0 | FConst1 | FConst2 |
                   DConst0 | DConst1 |
                   BiPush Word8 | SiPush Word16 |
                   Ldc CPIndex | LdcW CPIndex | Ldc2W CPIndex |

                   -- Loads
                   ILoad Local | LLoad Local | FLoad Local | DLoad Local | ALoad Local |
                   ILoad0 | ILoad1 | ILoad2 | ILoad3 |
                   LLoad0 | LLoad1 | LLoad2 | LLoad3 |
                   FLoad0 | FLoad1 | FLoad2 | FLoad3 |
                   DLoad0 | DLoad1 | DLoad2 | DLoad3 |
                   ALoad0 | ALoad1 | ALoad2 | ALoad3 |
                   IaLoad | LaLoad | FaLoad | DaLoad | AaLoad | BaLoad | CaLoad | SaLoad |

                   -- Stores
                   IStore Local | LStore Local | FStore Local | DStore Local | AStore Local |
                   IStore0 | IStore1 | IStore2 | IStore3 |
                   LStore0 | LStore1 | LStore2 | LStore3 |
                   FStore0 | FStore1 | FStore2 | FStore3 |
                   DStore0 | DStore1 | DStore2 | DStore3 |
                   AStore0 | AStore1 | AStore2 | AStore3 |
                   IaStore | LaStore | FaStore | DaStore | AaStore | BaStore | CaStore | SaStore |

                   -- Stack
                   Pop | Pop2 | Dup | DupX1 | DupX2 | Dup2 | Dup2X1 | Dup2X2 | Swap |

                   -- Math
                   IAdd | LAdd | FAdd | DAdd | ISub | LSub | FSub | DSub |
                   IMul | LMul | FMul | DMul | IDiv | LDiv | FDiv | DDiv |
                   IRem | LRem | FRem | DRem | INeg | LNeg | FNeg | DNeg |

                   IShl | LShl | IShr | LShr | IUshr | LUshr |

                   IAnd | LAnd | IOr | LOr | IXor | LXor | IInc Local Word8 |

                   -- Conversions
                   I2L | I2F | I2D | L2I | L2F | L2D | F2I | F2L | F2D | D2I | D2L | D2F |

                   I2B | I2C | I2S |

                   -- Comparisons
                   LCmp | FCmpL | FCmpG | DCmpL | DCmpG |

                   IfEq BranchOffset | IfNe BranchOffset |
                   IfLt BranchOffset | IfGe BranchOffset| IfGt BranchOffset | IfLe BranchOffset |
                   IfICmpEq BranchOffset | IfICmpNe BranchOffset|
                   IfICmpLt BranchOffset | IfICmpGe BranchOffset|
                   IfICmpGt BranchOffset | IfICmpLe BranchOffset |
                   IfACmpEq BranchOffset | IfACmpNe BranchOffset |

                   -- Control
                   Goto BranchOffset | Jsr BranchOffset | Ret Local |
                   TableSwitch Word32 Word32 Word32 [Word32] | LookupSwitch Word32 [(Word32, Word32)] |

                   IReturn | LReturn | FReturn | DReturn | AReturn | Return |

                   -- References
                   GetStatic CPIndex | PutStatic CPIndex |
                   GetField CPIndex | PutField CPIndex |

                   InvokeVirtual CPIndex | InvokeSpecial CPIndex |
                   InvokeStatic CPIndex | InvokeInterface CPIndex Word8 |
                   InvokeDynamic CPIndex Word8 |

                   New_ CPIndex | NewArray Word8 | ANewArray CPIndex |
                   ArrayLength |

                   AThrow | CheckCast CPIndex | InstanceOf_ CPIndex |
                   MonitorEnter | MonitorExit |

                   -- Extended
                   WideIInc CPIndex Word16 |
                   WideILoad CPIndex | WideFLoad CPIndex |
                   WideALoad CPIndex | WideLLoad CPIndex | WideDLoad CPIndex |
                   WideIStore CPIndex | WideFStore CPIndex |
                   WideAStore CPIndex | WideLStore CPIndex | WideDStore CPIndex |
                   WideRet CPIndex |

                   MultiANewArray CPIndex Word8 |
                   IfNull CPIndex | IfNotNull CPIndex |
                   GotoW Word32 | JsrW Word32  |

                   -- Reserved
                   BreakPoint | ImDep1 | ImDep2
                 deriving (Show, Eq)

data AttrInfo = UnknownAttr { unknownBytes :: ByteString }
              | ConstantValue { constantValueIndex :: Word16 }
              | CodeAttr { maxStack :: Word16,
                           maxLocals :: Word16,
                           code :: [Instruction],
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
                              
data ElementValue =
  ElementConstValue { tag :: Char, value :: Word16 }
  | ElementEnumConstValue { tag :: Char, typeNameIndex :: Word16, constNameIndex :: Word16 }
  | ElementClassInfoIndex { tag :: Char, classInfoIndex :: Word16 }
  | ElementAnnValue { tag :: Char, annotation :: Ann }
  | ElementArrayValue { tag :: Char, elementValues :: [ElementValue] }
  deriving (Show, Eq)
           
data ElementValuePair =
  ElementValuePair { elementNameIndex :: Word16, elementValue :: ElementValue }
  deriving (Show, Eq)

data Ann =
  Ann { typeIndex :: Word16, elementValuePairs :: [ElementValuePair] }
  deriving (Show, Eq)

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
              deriving (Show, Eq, Ord)

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
