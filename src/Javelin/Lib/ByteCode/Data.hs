module Javelin.Lib.ByteCode.Data where

import qualified Data.ByteString as BS
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Word as Word

tab = "    "

data Paragraph
  = P
      { heading :: String
      , text :: [Paragraph]
      }
  | POpt
      { heading :: String
      , text :: [Paragraph]
      }
  | L
      { heading :: String
      }

alignLines :: Bool -> Paragraph -> [String]
alignLines _ L {heading = h} = [h ++ "\n"]
alignLines opt POpt {heading = h, text = t} =
  [h ++ "\n"] ++
  if opt
    then (map (tab ++) (t >>= (alignLines opt)))
    else []
alignLines opt P {heading = h, text = t} =
  [h ++ "\n"] ++ (map (tab ++) (t >>= (alignLines opt)))

showByteCode :: Bool -> ByteCode -> String
showByteCode opt (ByteCode min maj body@ClassBody {constPool = cp@(ConstantPool p)}) =
  concat . alignLines opt $
  P "Bytecode"
    [ L (nameAndValue "Minor version: " min)
    , L (nameAndValue "Major version: " maj)
    , L (nameAndValue "Flags: " $
         List.intercalate ", " (map show (classAccessFlags body)))
    , P "This" [showConst p (this body)]
    , let superIdx = super body
       in P "Superclass"
            [ if superIdx > 1
                then showConst p superIdx
                else P "" []
            ]
    , P "Interfaces" $ map (showConst p) (interfaces body)
    , P "Constant pool " [showPool cp]
    , P "Fields" (map (printField p) (fields body))
    , P "Methods" (map (printMethod p) (methods body))
    , P "Class attributes" $ map (printAttribute p) (attrs body)
    ]

nameAndId :: String -> Word.Word16 -> String
nameAndId name id = name ++ " #" ++ show id

nameAndValue :: (Show s) => String -> s -> String
nameAndValue name value = name ++ " " ++ show value

-- ByteCode
instance Show ByteCode where
  show = showByteCode False

printField :: [Constant] -> FieldInfo -> Paragraph
printField p f@(FieldInfo { fieldAccessFlags = accessFlags
                          , fieldNameIndex = name
                          , fieldDescriptorIndex = descriptor
                          , fieldAttrs = attributes
                          }) =
  P "FieldInfo"
    [ POpt (nameAndId "Name" name) [showConst p name]
    , POpt (nameAndId "Descriptor" descriptor) [showConst p descriptor]
    , L $ nameAndValue "AccessFlags" accessFlags
    , P "Attributes" (map (printAttribute p) attributes)
    ]

printMethod :: [Constant] -> MethodInfo -> Paragraph
printMethod p m@(MethodInfo { methodAccessFlags = accessFlags
                            , methodNameIndex = name
                            , methodInfoDescriptorIndex = descriptor
                            , methodAttrs = attributes
                            }) =
  P "MethodInfo"
    [ POpt (nameAndId "Name" name) [showConst p name]
    , POpt (nameAndId "Descriptor" descriptor) [showConst p descriptor]
    , L $ nameAndValue "AccessFlags" (show accessFlags)
    , P "Attributes" (map (printAttribute p) attributes)
    ]

printAttribute :: [Constant] -> AttrInfo -> Paragraph
printAttribute p ca@(CodeAttr {}) =
  P "Code attribute"
    [ L $ nameAndValue "Max stack:" $ maxStack ca
    , L $ nameAndValue "Max locals:" $ maxLocals ca
    , P "Code" (map (printCode p) (code ca))
    ]
printAttribute p ca@(SourceFile idx) = L $ constHeadingRef p "Source file" idx
printAttribute p ca = L (show ca)

printCode :: [Constant] -> Instruction -> Paragraph
printCode p c =
  case cpIndex c of
    Nothing -> L (show c)
    Just idx ->
      POpt (show c ++ " -> " ++ showConstRefShort p idx) [showConst p idx]

br :: CPIndex -> Maybe Word.Word16
br (CPIndex idx) = Just idx

cpIndex :: Instruction -> Maybe Word.Word16
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

at :: Integral b => [a] -> b -> a
at cc i = cc !! (fromIntegral i - 1)

-- Constant pool
showPool :: ConstantPool -> Paragraph
showPool (ConstantPool p) = P "Constant pool" $ map (showConstant p) p

showConst :: [Constant] -> Word.Word16 -> Paragraph
showConst p idx = showConstant p (at p idx)

showConstRefShort :: [Constant] -> Word.Word16 -> String
showConstRefShort p idx = showConstShort p (at p idx)

showConstShort :: [Constant] -> Constant -> String
showConstShort _ (Utf8Info s) = s
showConstShort _ (IntegerInfo i) = show i
showConstShort _ (FloatInfo f) = show f
showConstShort _ (LongInfo l) = show l
showConstShort _ (DoubleInfo d) = show d
showConstShort p (StringInfo s) = showConstRefShort p s
showConstShort p (ClassInfo i) = showConstRefShort p i
showConstShort p (NameAndTypeInfo n t) =
  (showConstRefShort p n) ++ " of type " ++ (showConstRefShort p t)
showConstShort p (Fieldref c nt) =
  (showConstRefShort p c) ++ "#" ++ (showConstRefShort p nt)
showConstShort p (Methodref c nt) =
  (showConstRefShort p c) ++ "#" ++ (showConstRefShort p nt)
showConstShort p (InterfaceMethodref c nt) =
  (showConstRefShort p c) ++ "#" ++ (showConstRefShort p nt)
showConstShort p (MethodHandleInfo rk ri) =
  "Kind " ++ (show rk) ++ " " ++ (showConstRefShort p ri)
showConstShort p (MethodTypeInfo i) = (showConstRefShort p i)
showConstShort p (InvokeDynamicInfo bi ti) =
  (showConstRefShort p bi) ++ ", " ++ (showConstRefShort p ti)

constHeading :: [Constant] -> String -> Constant -> String
constHeading p name c = name ++ " -> " ++ showConstShort p c

constHeadingRef :: [Constant] -> String -> Word.Word16 -> String
constHeadingRef p name idx = constHeading p name (at p idx)

showConstant :: [Constant] -> Constant -> Paragraph
showConstant _ (Utf8Info s) = L $ nameAndValue "Utf8 " s
showConstant _ (IntegerInfo i) = L $ nameAndValue "Int " i
showConstant _ (FloatInfo f) = L $ nameAndValue "Float " f
showConstant _ (LongInfo l) = L $ nameAndValue "Long " l
showConstant _ (DoubleInfo d) = L $ nameAndValue "Double " d
showConstant p c@(StringInfo s) =
  POpt (constHeading p "String" c) [P (nameAndId "string" s) [showConst p s]]
showConstant p c@(ClassInfo i) =
  POpt (constHeading p "Class" c) [P (nameAndId "class" i) [showConst p i]]
showConstant p c@(NameAndTypeInfo n t) =
  POpt
    (constHeading p "NameAndType" c)
    [ P (nameAndId "name" n) [showConst p n]
    , P (nameAndId "descriptor" t) [showConst p t]
    ]
showConstant p c@(Fieldref cl nt) =
  POpt
    (constHeading p "Fieldref" c)
    [ P (nameAndId "class" cl) [showConst p cl]
    , P (nameAndId "name_and_type" nt) [showConst p nt]
    ]
showConstant p c@(Methodref cl nt) =
  POpt
    (constHeading p "Methodref" c)
    [ P (nameAndId "class" cl) [showConst p cl]
    , P (nameAndId "name_and_type" nt) [showConst p nt]
    ]
showConstant p c@(InterfaceMethodref cl nt) =
  POpt
    (constHeading p "InterfaceMethodref" c)
    [ P (nameAndId "class" cl) [showConst p cl]
    , P (nameAndId "name_and_type" nt) [showConst p nt]
    ]
showConstant p c@(MethodHandleInfo rk ri) =
  POpt
    (constHeading p "MethodHandleInfo" c)
    [ L (nameAndValue "reference_kind" rk)
    , P (nameAndId "reference" ri) [showConst p ri]
    ]
showConstant p c@(MethodTypeInfo i) =
  POpt
    (constHeading p "MethodTypeInfo" c)
    [P (nameAndId "descriptor" i) [showConst p i]]
showConstant p c@(InvokeDynamicInfo bi ti) =
  POpt
    (constHeading p "InvokeDynamicInfo" c)
    [ P (nameAndId "bootstrap_method_attr" bi) [showConst p bi]
    , P (nameAndId "name_and_type" ti) [showConst p ti]
    ]

-- Definitions
data ByteCode =
  ByteCode
    { minVer :: Word.Word16
    , majVer :: Word.Word16
    , body :: ClassBody
    }
  deriving (Eq)

data ClassBody =
  ClassBody
    { constPool :: ConstantPool
    , classAccessFlags :: [ClassAccessFlags]
    , this :: Word.Word16
    , super :: Word.Word16 --0 for java.lang.Object
    , interfaces :: [Word.Word16]
    , fields :: [FieldInfo]
    , methods :: [MethodInfo]
    , attrs :: [AttrInfo]
    }
  deriving (Eq)

newtype ConstantPool =
  ConstantPool [Constant]
  deriving (Eq)

data ClassAccessFlags
  = AccPublic
  | AccFinal
  | AccSuper
  | AccInterface
  | AccAbstract
  | AccSynthetic
  | AccAnn
  | AccEnum
  deriving (Show, Eq)

data Constant
  = Utf8Info
      { stringValue :: String
      }
  | IntegerInfo
      { integer :: Int.Int32
      }
  | FloatInfo
      { float :: Float
      }
  | LongInfo
      { long :: Int.Int64
      }
  | DoubleInfo
      { double :: Double
      }
  | StringInfo
      { stringIndex :: Word.Word16
      }
  | ClassInfo
      { nameIndex :: Word.Word16
      }
  | NameAndTypeInfo
      { nameIndex :: Word.Word16
      , nameAndTypeDescriptorIndex :: Word.Word16
      }
  | Fieldref
      { classIndex :: Word.Word16
      , nameAndTypeIndex :: Word.Word16
      }
  | Methodref
      { classIndex :: Word.Word16
      , nameAndTypeIndex :: Word.Word16
      }
  | InterfaceMethodref
      { classIndex :: Word.Word16
      , nameAndTypeIndex :: Word.Word16
      }
  | MethodHandleInfo
      { referenceKind :: Word.Word8
      , referenceIndex :: Word.Word16
      }
  | MethodTypeInfo
      { methodTypeaDescriptorIndex :: Word.Word16
      }
  | InvokeDynamicInfo
      { bootstrapMethodAttrIndex :: Word.Word16
      , nameAndTypeIndex :: Word.Word16
      }
  deriving (Eq, Show)

data FieldInfo =
  FieldInfo
    { fieldAccessFlags :: [FieldInfoAccessFlag]
    , fieldNameIndex :: Word.Word16
    , fieldDescriptorIndex :: Word.Word16
    , fieldAttrs :: [AttrInfo]
    }
  deriving (Show, Eq)

data FieldInfoAccessFlag
  = FieldPublic
  | FieldPrivate
  | FieldProtected
  | FieldStatic
  | FieldFinal
  | FieldVolatile
  | FieldTransient
  | FieldSynthetic
  | FieldEnum
  deriving (Show, Eq)

data MethodInfo =
  MethodInfo
    { methodAccessFlags :: [MethodInfoAccessFlag]
    , methodNameIndex :: Word.Word16
    , methodInfoDescriptorIndex :: Word.Word16
    , methodAttrs :: [AttrInfo]
    }
  deriving (Show, Eq)

data MethodInfoAccessFlag
  = MethodPublic
  | MethodPrivate
  | MethodProtected
  | MethodStatic
  | MethodFinal
  | MethodSynchronized
  | MethodBridge
  | MethodVarargs
  | MethodNative
  | MethodAbstract
  | MethodStrict
  | MethodSynthetic
  deriving (Show, Eq)

newtype CPIndex =
  CPIndex Word.Word16
  deriving (Eq)

instance Show CPIndex where
  show (CPIndex x) = "#" ++ show x

type Local = Word.Word8

type BranchOffset = Word.Word16

data Instruction
                   -- Constants
  = Nop
  | AConstNull
  | IConstM1
  | IConst0
  | IConst1
  | IConst2
  | IConst3
  | IConst4
  | IConst5
  | LConst0
  | LConst1
  | FConst0
  | FConst1
  | FConst2
  | DConst0
  | DConst1
  | BiPush Word.Word8
  | SiPush Word.Word16
  | Ldc CPIndex
  | LdcW CPIndex
  | Ldc2W CPIndex
                   -- Loads
  | ILoad Local
  | LLoad Local
  | FLoad Local
  | DLoad Local
  | ALoad Local
  | ILoad0
  | ILoad1
  | ILoad2
  | ILoad3
  | LLoad0
  | LLoad1
  | LLoad2
  | LLoad3
  | FLoad0
  | FLoad1
  | FLoad2
  | FLoad3
  | DLoad0
  | DLoad1
  | DLoad2
  | DLoad3
  | ALoad0
  | ALoad1
  | ALoad2
  | ALoad3
  | IaLoad
  | LaLoad
  | FaLoad
  | DaLoad
  | AaLoad
  | BaLoad
  | CaLoad
  | SaLoad
                   -- Stores
  | IStore Local
  | LStore Local
  | FStore Local
  | DStore Local
  | AStore Local
  | IStore0
  | IStore1
  | IStore2
  | IStore3
  | LStore0
  | LStore1
  | LStore2
  | LStore3
  | FStore0
  | FStore1
  | FStore2
  | FStore3
  | DStore0
  | DStore1
  | DStore2
  | DStore3
  | AStore0
  | AStore1
  | AStore2
  | AStore3
  | IaStore
  | LaStore
  | FaStore
  | DaStore
  | AaStore
  | BaStore
  | CaStore
  | SaStore
                   -- Stack
  | Pop
  | Pop2
  | Dup
  | DupX1
  | DupX2
  | Dup2
  | Dup2X1
  | Dup2X2
  | Swap
                   -- Math
  | IAdd
  | LAdd
  | FAdd
  | DAdd
  | ISub
  | LSub
  | FSub
  | DSub
  | IMul
  | LMul
  | FMul
  | DMul
  | IDiv
  | LDiv
  | FDiv
  | DDiv
  | IRem
  | LRem
  | FRem
  | DRem
  | INeg
  | LNeg
  | FNeg
  | DNeg
  | IShl
  | LShl
  | IShr
  | LShr
  | IUshr
  | LUshr
  | IAnd
  | LAnd
  | IOr
  | LOr
  | IXor
  | LXor
  | IInc Local Word.Word8
                   -- Conversions
  | I2L
  | I2F
  | I2D
  | L2I
  | L2F
  | L2D
  | F2I
  | F2L
  | F2D
  | D2I
  | D2L
  | D2F
  | I2B
  | I2C
  | I2S
                   -- Comparisons
  | LCmp
  | FCmpL
  | FCmpG
  | DCmpL
  | DCmpG
  | IfEq BranchOffset
  | IfNe BranchOffset
  | IfLt BranchOffset
  | IfGe BranchOffset
  | IfGt BranchOffset
  | IfLe BranchOffset
  | IfICmpEq BranchOffset
  | IfICmpNe BranchOffset
  | IfICmpLt BranchOffset
  | IfICmpGe BranchOffset
  | IfICmpGt BranchOffset
  | IfICmpLe BranchOffset
  | IfACmpEq BranchOffset
  | IfACmpNe BranchOffset
                   -- Control
  | Goto BranchOffset
  | Jsr BranchOffset
  | Ret Local
  | TableSwitch Word.Word32 Word.Word32 Word.Word32 [Word.Word32]
  | LookupSwitch Word.Word32 [(Word.Word32, Word.Word32)]
  | IReturn
  | LReturn
  | FReturn
  | DReturn
  | AReturn
  | Return
                   -- References
  | GetStatic CPIndex
  | PutStatic CPIndex
  | GetField CPIndex
  | PutField CPIndex
  | InvokeVirtual CPIndex
  | InvokeSpecial CPIndex
  | InvokeStatic CPIndex
  | InvokeInterface CPIndex Word.Word8
  | InvokeDynamic CPIndex Word.Word8
  | New_ CPIndex
  | NewArray Word.Word8
  | ANewArray CPIndex
  | ArrayLength
  | AThrow
  | CheckCast CPIndex
  | InstanceOf_ CPIndex
  | MonitorEnter
  | MonitorExit
                   -- Extended
  | WideIInc CPIndex Word.Word16
  | WideILoad CPIndex
  | WideFLoad CPIndex
  | WideALoad CPIndex
  | WideLLoad CPIndex
  | WideDLoad CPIndex
  | WideIStore CPIndex
  | WideFStore CPIndex
  | WideAStore CPIndex
  | WideLStore CPIndex
  | WideDStore CPIndex
  | WideRet CPIndex
  | MultiANewArray CPIndex Word.Word8
  | IfNull CPIndex
  | IfNotNull CPIndex
  | GotoW Word.Word32
  | JsrW Word.Word32
                   -- Reserved
  | BreakPoint
  | ImDep1
  | ImDep2
  deriving (Show, Eq)

isCodeAttrInfo :: AttrInfo -> Bool
isCodeAttrInfo CodeAttr {} = True
isCodeAttrInfo _ = False

data AttrInfo
  = UnknownAttr
      { unknownBytes :: BS.ByteString
      }
  | ConstantValue
      { constantValueIndex :: Word.Word16
      }
  | CodeAttr
      { maxStack :: Word.Word16
      , maxLocals :: Word.Word16
      , code :: [Instruction]
      , exceptionTable :: [Exception]
      , codeAttrs :: [AttrInfo]
      }
  | StackMapTable
      { entries :: [StackMapFrame]
      }
  | Exceptions
      { exceptionIndexTable :: [Word.Word16]
      }
  | InnerClasses
      { classes :: [InnerClassInfo]
      }
  | EnclosingMethod
      { enclosingClassIndex :: Word.Word16
      , enclosingMethodIndex :: Word.Word16
      }
  | Synthetic
  | Signature
      { signatureIndex :: Word.Word16
      }
  | SourceFile
      { sourceFileIndex :: Word.Word16
      }
  | SourceDebugExtension
      { debugExtension :: String
      }
  | LineNumberTable
      { lineNumberTable :: [LineNumber]
      }
  | LocalVariableTable
      { localVariableTable :: [LocalVariableInfo]
      }
  | LocalVariableTypeTable
      { localVariableTypeTable :: [LocalVariableInfo]
      }
  | Deprecated
  | RTVisibleAnns
      { annotations :: [Ann]
      }
  | RTInvisibleAnns
      { annotations :: [Ann]
      }
  | RTVisibleParamAnns
      { paramAnns :: [[Ann]]
      }
  | RTInvisibleParamAnns
      { paramAnns :: [[Ann]]
      }
  | RTVisibleTypeAnns
      { typeAnns :: [TypeAnn]
      }
  | RTInvisibleTypeAnns
      { typeAnns :: [TypeAnn]
      }
  | AnnDefault
      { defaultValue :: [Word.Word8]
      }
  | BootstrapMethods
      { bootstrapMethods :: [BootstrapMethod]
      }
  | MethodParameters
      { parameters :: [MethodParameter]
      }
  deriving (Show, Eq)

data MethodParameter =
  MethodParameter
    { parameterNameIndex :: Word.Word16
    , parameterAccessFlags :: [ParameterAccessFlag]
    }
  deriving (Show, Eq)

data ParameterAccessFlag
  = ACC_FINAL
  | ACC_SYNTHETIC
  | ACC_MANDATED
  deriving (Show, Eq)

data BootstrapMethod =
  BootstrapMethod
    { methodRef :: Word.Word16
    , arguments :: [Word.Word16]
    }
  deriving (Show, Eq)

data ElementValue
  = ElementConstValue
      { tag :: Char
      , value :: Word.Word16
      }
  | ElementEnumConstValue
      { tag :: Char
      , typeNameIndex :: Word.Word16
      , constNameIndex :: Word.Word16
      }
  | ElementClassInfoIndex
      { tag :: Char
      , classInfoIndex :: Word.Word16
      }
  | ElementAnnValue
      { tag :: Char
      , annotation :: Ann
      }
  | ElementArrayValue
      { tag :: Char
      , elementValues :: [ElementValue]
      }
  deriving (Show, Eq)

data ElementValuePair =
  ElementValuePair
    { elementNameIndex :: Word.Word16
    , elementValue :: ElementValue
    }
  deriving (Show, Eq)

data Ann =
  Ann
    { typeIndex :: Word.Word16
    , elementValuePairs :: [ElementValuePair]
    }
  deriving (Show, Eq)

data TypeAnn =
  TypeAnn
    { targetType :: TypeTargetType
    , targetInfo :: TypeTargetInfo
    , targetPath :: [TypePathElem]
    , typeAnnotation :: Ann
    }
  deriving (Show, Eq)

data TypePathElem =
  TypePathElem
    { typePathKind :: Word.Word8
    , typeArgumentIndex :: Word.Word8
    }
  deriving (Show, Eq)

data TypeTargetType
  = GenericClassInterface
  | MethodConstructor
  | ClassInterfaceExtendsImplements
  | ClassInterfaceBound
  | MethodConstructorBound
  | FieldTarget
  | ReturnConstructed
  | MethodConstructorReceiver
  | MethodConstructorLambda
  | Throws
  | LocalVar
  | ResourceVar
  | ExceptionParameter
  | InstanceOf
  | New
  | NewReference
  | MethodReference
  | CastExpr
  | NewArgType
  | MethodArgType
  | NewRefArg
  | MethodRefArg
  deriving (Show, Eq)

data TypeTargetInfo
  = TypeParameterTarget
      { typeParameterIndex :: Word.Word8
      }
  | SupertypeTarget
      { superTypeIndex :: Word.Word16
      }
  | TypeParameterBoundTarget
      { typeParameterIndex :: Word.Word8
      , boundIndex :: Word.Word8
      }
  | EmptyTarget
      {
      }
  | FormalParameterTarget
      { formalParameterIndex :: Word.Word8
      }
  | ThrowsTarget
      { throwsTypeIndex :: Word.Word16
      }
  | LocalvarTarget
      { localvarTargetTable :: [LocalVarTargetElem]
      }
  | CatchTarget
      { exceptionTableIndex :: Word.Word16
      }
  | OffsetTarget
      { offsetTarget :: Word.Word16
      }
  | TypeArgumentTarget
      { offsetTarget :: Word.Word16
      , typeArgumentTargetIndex :: Word.Word8
      }
  deriving (Show, Eq)

data LocalVarTargetElem =
  LocalVarTargetElem
    { varStartPc :: Word.Word16
    , varLen :: Word.Word16
    , varIndex :: Word.Word16
    }
  deriving (Show, Eq)

data Exception =
  Exception
    { startPc :: Word.Word16
    , endPc :: Word.Word16
    , handlerPc :: Word.Word16
    , catchType :: Word.Word16
    }
  deriving (Show, Eq)

data VerifTypeInfo
  = TopVariableInfo
  | IntegerVariableInfo
  | FloatVariableInfo
  | LongVariableInfo
  | DoubleVariableInfo
  | NullVariableInfo
  | UninitializedThisVariableInfo
  | ObjectVariableInfo
      { cpoolIndex :: Word.Word16
      }
  | UninitializedVariableInfo
      { offset :: Word.Word16
      }
  deriving (Show, Eq)

data StackMapFrame
  = SameFrame
      { frameType :: Word.Word8
      }
  | SameLocals1StackItemFrame
      { frameType :: Word.Word8
      , stackItem :: VerifTypeInfo
      }
  | SameLocals1StackItemFrameExtended
      { frameType :: Word.Word8
      , offsetData :: Word.Word16
      , stackItem :: VerifTypeInfo
      }
  | ChopFrame
      { frameType :: Word.Word8
      , offsetDelta :: Word.Word16
      }
  | SameFrameExtended
      { frameType :: Word.Word8
      , offsetDelta :: Word.Word16
      }
  | AppendFrame
      { frameType :: Word.Word8
      , offsetDelta :: Word.Word16
      , locals :: [VerifTypeInfo]
      }
  | FullFrame
      { frameType :: Word.Word8
      , offsetDelta :: Word.Word16
      , locals :: [VerifTypeInfo]
      , stack :: [VerifTypeInfo]
      }
  deriving (Show, Eq)

data InnerClassInfo =
  InnerClassInfo
    { innerClassInfoIndex :: Word.Word16
    , outerClassInfoIndex :: Word.Word16
    , innerNameIndex :: Word.Word16
    , innerClassAccessFlags :: [InnerClassAccessFlags]
    }
  deriving (Show, Eq)

data InnerClassAccessFlags
  = InnerClassPublic
  | InnerClassPrivate
  | InnerClassProtected
  | InnerClassStatic
  | InnerClassFinal
  | InnerClassInterface
  | InnerClassAbstract
  | InnerClassSynthetic
  | InnerClassAnn
  | InnerClassEnum
  deriving (Show, Eq)

data LineNumber =
  LineNumber
    { lineStartPc :: Word.Word16
    , lineNumber :: Word.Word16
    }
  deriving (Show, Eq)

data LocalVariableInfo =
  LocalVariableInfo
    { localVariableStartPc :: Word.Word16
    , localVariableLength :: Word.Word16
    , localVariableNameIndex :: Word.Word16
    , variableSignatureIndex :: Word.Word16
    , localVariableIndex :: Word.Word16
    }
  deriving (Show, Eq)
