module Javelin.ByteCode.DescSign
where

import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Error
import Control.Applicative ((<$), (<$>), (<*), (<*>))
import Text.ParserCombinators.Parsec.Combinator (endBy, sepBy)


parseFieldType = parse fieldTypeParser ""
parseBaseType = parse baseTypeParser ""
parseFieldDescriptor = parse fieldDescriptorParser ""
parseMethodDescriptor = parse methodDescriptorParser ""


-- Fundamental definitions
type StringParser a = CharParser () a

type QualifiedName = [String]
type UnqualifiedName = String
qualifiedNameParser = endBy classPartsParser (char ';')
classPartsParser = sepBy anyChar (char '/')

data FieldType = BaseType { baseType :: BaseType }
               | ObjectType { className :: QualifiedName }
               | ArrayType { componentType :: FieldType }
               deriving (Show, Eq)
fieldTypeParser = baseFieldTypeParser
                  <|> objectFieldTypeParser
                  <|> arrayFieldTypeParser
                  <?> "FieldType"
baseFieldTypeParser = BaseType <$> baseTypeParser
objectFieldTypeParser = ObjectType <$> qualifiedNameParser <* (char 'L')
arrayFieldTypeParser = ArrayType <$> fieldTypeParser <* (char '[')

data BaseType = ByteT | CharT | DoubleT | FloatT | IntT | LongT | ShortT | BooleanT
              deriving (Show, Eq)

baseTypeParser :: StringParser BaseType
baseTypeParser = ByteT <$ (char 'B')
                 <|> CharT <$ (char 'C')
                 <|> DoubleT <$ (char 'D')
                 <|> FloatT <$ (char 'F')
                 <|> IntT <$ (char 'I')
                 <|> LongT <$ (char 'J')
                 <|> ShortT <$ (char 'S')
                 <|> BooleanT <$ (char 'Z')
                 <?> "BaseType"


-- FieldDescriptor
data FieldDescriptor = FieldDescriptor { fieldType :: FieldType }
                     deriving (Show, Eq)
fieldDescriptorParser = FieldDescriptor <$> fieldTypeParser


-- MethodDescriptor
data MethodDescriptor = MethodDescriptor { parameterDescrs :: [FieldType],
                                           returnDescr :: ReturnDescriptor }
                        deriving (Show, Eq)
data ReturnDescriptor = FieldTypeDescriptor { returnTypeDescriptor :: FieldType }
                      | VoidDescriptor deriving (Show, Eq)
methodDescriptorParser = MethodDescriptor <$> (many fieldTypeParser) <*> returnDescriptorParser
returnDescriptorParser = (FieldTypeDescriptor <$> fieldTypeParser)
                         <|> (VoidDescriptor <$ (char 'v'))
                         <?> "ReturnDescriptor"


-- ClassSignature
parseClassSignature :: String -> ClassSignature
parseClassSignature = undefined
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
parseMethoSdypeignature :: String -> MethodTypeSignature
parseMethoSdypeignature = undefined
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
