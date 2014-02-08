module Javelin.ByteCode.DescSign
where

-- Fundamental definitions
type FullName = [String]
type UnqualifiedName = String

data FieldType = BaseType { baseType :: BaseType }
               | ObjectType { className :: FullName }
               | ArrayType { componentType :: FieldType }
               deriving (Show, Eq)
data BaseType = ByteT | CharT | DoubleT | FloatT | IntT | LongT | ShortT | BooleanT
              deriving (Show, Eq)


-- FieldDescriptor
data FieldDescriptor = FieldDescriptor { fieldType :: FieldType }
                     deriving (Show, Eq)


-- MethodDescriptor
data MethodDescriptor = MethodDescriptor { parameterDescrs :: [FieldType],
                                           returnDescr :: ReturnDescriptor }
                        deriving (Show, Eq)
data ReturnDescriptor = FieldTypeDescriptor { returnTypeDescriptor :: FieldType }
                      | VoidDescriptor deriving (Show, Eq)


-- ClassSignature
data ClassSignature = ClassSignature { classTypeParameters :: [FormalTypeParameter],
                                       superclassSignature :: ClassTypeSignature,
                                       superinterfaceSignature :: ClassTypeSignature }
                    deriving (Show, Eq)
data FormalTypeParameter = FormalTypeParameter { ftId :: String,
                                                 classBound :: FieldTypeSignature,
                                                 interfaceBound :: [FieldTypeSignature] }
                           deriving (Show, Eq)
data FieldTypeSignature = ClassFieldType { classTypeSignature :: ClassTypeSignature }
                        | ArrayFieldType { signatures :: [TypeSignature] }
                        | TypeVariable { typeVariableSignature :: TypeVariableSignature }
                        deriving (Show, Eq)
data TypeVariableSignature = TypeVariableSignature { tvId :: String } deriving (Show, Eq)                                
data ClassTypeSignature = ClassTypeSignature { packageSpecifier :: [String],
                                               simpleSignature :: SimpleClassTypeSignature,
                                               suffix :: [SimpleClassTypeSignature] }
                        deriving (Show, Eq)
data SimpleClassTypeSignature = SimpleClassTypeSignature { sctId :: String,
                                                           typeArguments :: [TypeArgument] }
                              deriving (Show, Eq)
data TypeArgument = TypeArgument { indicator :: WildcardIndicator,
                                   typeArgumentSignature :: FieldTypeSignature }
                  | Asterisk
                  deriving (Show, Eq)
data WildcardIndicator = Plus | Minus deriving (Show, Eq)
data TypeSignature = FieldTypeTypeSignature { fieldTypeSignature :: FieldTypeSignature }
                   | BaseTypeTypeSignature { baseTypeSignature :: BaseType }
                   deriving (Show, Eq)


--MethodTypeSignature
data MethodTypeSignature = MethodTypeSignature { methodTypeParameters :: [FormalTypeParameter],
                                                 typeSignatures :: [TypeSignature],
                                                 returnType :: ReturnType,
                                                 throwsSignature :: [ThrowsSignature]
                                               } deriving (Show, Eq)
data ReturnType = ReturnTypeSignature { typeSignature :: TypeSignature }
                | VoidTypeSignature
                deriving (Show, Eq)
data ThrowsSignature = ThrowsSignature { classType :: ClassTypeSignature,
                                         typeVariable :: TypeVariableSignature }
                       deriving (Show, Eq)
