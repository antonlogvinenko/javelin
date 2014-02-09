module Javelin.ByteCode.DescSign
where

import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Error
import Control.Applicative ((<$), (<$>), (<*), (<*>), (*>))
import Text.ParserCombinators.Parsec.Combinator (endBy, sepBy)


parseFieldType = parse fieldTypeParser ""
parseBaseType = parse baseTypeParser ""
parseFieldDescriptor = parse fieldDescriptorParser ""
parseMethodDescriptor = parse methodDescriptorParser ""
parseMethodTypeSignature = parse methodTypeSignatureParser ""
parseClassSignature = parse classSignatureParser ""


-- Fundamental definitions
type StringParser a = CharParser () a

type QualifiedName = [String]
type UnqualifiedName = String
unqualifiedNameParser :: StringParser UnqualifiedName
unqualifiedNameParser = undefined
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
objectFieldTypeParser = ObjectType <$>
                        ((char 'L') *> qualifiedNameParser <* (char ';'))
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
                         <|> VoidDescriptor <$ voidDescriptorParser
                         <?> "ReturnDescriptor"
voidDescriptorParser = char 'v'


-- ClassSignature
classSignatureParser :: StringParser ClassSignature
classSignatureParser = undefined
formalTypeParameterParser = undefined
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
fieldTypeSignatureParser :: StringParser FieldTypeSignature
fieldTypeSignatureParser = undefined

data TypeVariableSignature = TypeVariableSignature { tvId :: UnqualifiedName } deriving (Show, Eq)
typeVariableSignatureParser :: StringParser TypeVariableSignature
typeVariableSignatureParser = TypeVariableSignature <$> unqualifiedNameParser

data ClassTypeSignature = ClassTypeSignature { packageSpecifier :: [UnqualifiedName],
                                               simpleSignature :: SimpleClassTypeSignature,
                                               suffix :: [SimpleClassTypeSignature] }
                        deriving (Show, Eq)
classTypeSignatureParser :: StringParser ClassTypeSignature
classTypeSignatureParser = ClassTypeSignature
                           <$> (many unqualifiedNameParser)
                           <*> simpleClassTypeSignatureParser
                           <*> (many simpleClassTypeSignatureParser)
data SimpleClassTypeSignature = SimpleClassTypeSignature { sctId :: String,
                                                           typeArguments :: [TypeArgument] }
                              deriving (Show, Eq)
simpleClassTypeSignatureParser = SimpleClassTypeSignature <$> unqualifiedNameParser <*> (many typeArgumentParser)
                                       
data TypeArgument = TypeArgument { indicator :: WildcardIndicator,
                                   typeArgumentSignature :: FieldTypeSignature }
                  | Asterisk
                  deriving (Show, Eq)
typeArgumentParser :: StringParser TypeArgument
typeArgumentParser = TypeArgument <$> wildCardIndicatorParser <*> fieldTypeSignatureParser
                     <|> Asterisk <$ (char '*')
                     <?> "TypeArgument"
                           
data WildcardIndicator = Plus | Minus deriving (Show, Eq)
wildCardIndicatorParser = Plus <$ (char '+')
                          <|> Minus <$ (char '-')
                          <?> "wildcard indicator"
data TypeSignature = FieldTypeTypeSignature { fieldTypeSignature :: FieldTypeSignature }
                   | BaseTypeTypeSignature { baseTypeSignature :: BaseType }
                   deriving (Show, Eq)
typeSignatureParser = FieldTypeTypeSignature <$> fieldTypeSignatureParser
                      <|> BaseTypeTypeSignature <$> baseTypeParser
                      <?> "TypeSignature"


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

methodTypeSignatureParser :: StringParser MethodTypeSignature
methodTypeSignatureParser = MethodTypeSignature
                            <$> (many formalTypeParameterParser)
                            <*> (many typeSignatureParser)
                            <*> returnTypeParser
                            <*> (many throwsSignatureParser)
returnTypeParser = ReturnTypeSignature <$> typeSignatureParser
                   <|> VoidTypeSignature <$ voidDescriptorParser
                   <?> "ReturnType"
throwsSignatureParser = ThrowsSignature <$> classTypeSignatureParser <*> typeVariableSignatureParser
