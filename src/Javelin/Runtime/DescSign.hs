module Javelin.Runtime.DescSign
  ( parseFieldDescriptor
  , FieldType(..)
  , BaseType(..)
  , FieldDescriptor(..)
  ) where

import           Data.Word                                (Word16)

import           Control.Applicative                      ((*>), (<$), (<$>),
                                                           (<*), (<*>))
  -- Signatures

import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Combinator
import           Text.ParserCombinators.Parsec.Error      (ParseError)
import           Text.ParserCombinators.Parsec.Prim

type QualifiedName = [String]

type UnqualifiedName = String

data FieldType
  = BaseType { unBaseType :: BaseType }
  | ObjectType { unObjectType :: QualifiedName }
  | ArrayType { unArrayType :: FieldType }
  deriving (Show, Eq)

data BaseType
  = ByteT
  | CharT
  | DoubleT
  | FloatT
  | IntT
  | LongT
  | ShortT
  | BooleanT
  deriving (Show, Eq, Ord)

-- FieldDescriptor
data FieldDescriptor = FieldDescriptor
  { unFieldType :: FieldType
  } deriving (Show, Eq)

-- MethodDescriptor
data MethodDescriptor = MethodDescriptor
  { parameterDescrs :: [FieldType]
  , returnDescr     :: ReturnDescriptor
  } deriving (Show, Eq)

data ReturnDescriptor
  = FieldTypeDescriptor { returnTypeDescriptor :: FieldType }
  | VoidDescriptor
  deriving (Show, Eq)

-- JavaTypeSignature
data JavaTypeSignature
  = FromReferenceTypeSignature { fieldTypeSignature :: ReferenceTypeSignature }
  | FromBaseTypeSignature { baseTypeSignature :: BaseType }
  deriving (Show, Eq)

-- ReferenceTypeSignature
data ReferenceTypeSignature
  = FromClassTypeSignature { fieldClassType :: ClassTypeSignature }
  | ArrayTypeSignature { signatures :: [JavaTypeSignature] }
  | FromTypeVariableSignature { signature :: TypeVariableSignature }
  deriving (Show, Eq)

-- ClassTypeSignature
data ClassTypeSignature = ClassTypeSignature
  { packageSpecifier :: QualifiedName
  , simpleSignature  :: SimpleClassTypeSignature
  , suffix           :: [SimpleClassTypeSignature]
  } deriving (Show, Eq)

data SimpleClassTypeSignature = SimpleClassTypeSignature
  { sctId         :: String
  , typeArguments :: [TypeArgument]
  } deriving (Show, Eq)

data TypeArgument
  = TypeArgumentWithIndicator { indicator             :: WildcardIndicator
                              , typeArgumentSignature :: ReferenceTypeSignature }
  | TypeArgument { typeArgumentSignature :: ReferenceTypeSignature }
  | Asterisk
  deriving (Show, Eq)

data WildcardIndicator
  = Plus
  | Minus
  deriving (Show, Eq)

data TypeVariableSignature = TypeVariableSignature
  { tvId :: UnqualifiedName
  } deriving (Show, Eq)

-- ClassSignature
data ClassSignature = ClassSignature
  { classTypeParameters     :: [FormalTypeParameter]
  , superclassSignature     :: ClassTypeSignature
  , superinterfaceSignature :: [ClassTypeSignature]
  } deriving (Show, Eq)

data FormalTypeParameter = FormalTypeParameter
  { ftId           :: UnqualifiedName
  , classBound     :: ReferenceTypeSignature
  , interfaceBound :: [ReferenceTypeSignature]
  } deriving (Show, Eq)

--MethodSignature
data MethodSignature = MethodSignature
  { methodTypeParameters :: [FormalTypeParameter]
  , typeSignatures       :: [JavaTypeSignature]
  , methodReturnType     :: ReturnType
  , throwsTypeSignature  :: [ThrowsSignature]
  } deriving (Show, Eq)

data ReturnType
  = ReturnTypeSignature { returnTypeSignature :: JavaTypeSignature }
  | VoidTypeSignature
  deriving (Show, Eq)

data ThrowsSignature = ThrowsSignature
  { throwsClassType :: ClassTypeSignature
  , typeVariable    :: TypeVariableSignature
  } deriving (Show, Eq)

-- FieldSignature
data FieldSignature = FieldSignature
  { referenceType :: ReferenceTypeSignature
  }

parseFieldDescriptor :: String -> Either ParseError FieldDescriptor
parseFieldDescriptor = parse fieldDescriptorP ""

parseMethodDescriptor = parse methodDescriptorP ""

parseMethodSignature = parse methodSignatureP ""

parseClassSignature = parse classSignatureP ""

parseFieldSignature = parse fieldSignatureP ""

-- Fundamentals
type StringParser a = CharParser () a

nameSymbol = noneOf ".;[/"

unqualifiedNameP :: StringParser UnqualifiedName
unqualifiedNameP = many nameSymbol

qualifiedNameP :: StringParser QualifiedName
qualifiedNameP = sepBy unqualifiedNameP (char '/')

fieldTypeP =
  BaseType <$> baseTypeP <|>
  ObjectType <$> (char 'L' *> qualifiedNameP <* char ';') <|>
  ArrayType <$> (char '[' *> fieldTypeP) <?> "FieldType"

baseTypeP :: StringParser BaseType
baseTypeP =
  ByteT <$ char 'B' <|> CharT <$ char 'C' <|> DoubleT <$ char 'D' <|>
  FloatT <$ char 'F' <|>
  IntT <$ char 'I' <|>
  LongT <$ char 'J' <|>
  ShortT <$ char 'S' <|>
  BooleanT <$ char 'Z' <?> "BaseType"

-- FieldDescriptor
fieldDescriptorP = FieldDescriptor <$> fieldTypeP

-- MethodDescriptor
methodDescriptorP =
  MethodDescriptor <$> (char '(' *> many fieldTypeP) <*>
  (char ')' *> returnDescriptorP)

returnDescriptorP =
  FieldTypeDescriptor <$> fieldTypeP <|>
  VoidDescriptor <$ voidDescriptorP <?> "ReturnDescriptor"

voidDescriptorP = char 'v'

-- JavaTypeSignature
identifierP = many $ noneOf ".;[/<>:"

javaTypeSignatureP =
  FromReferenceTypeSignature <$> referenceTypeSignatureP <|>
  FromBaseTypeSignature <$> baseTypeP <?> "TypeSignature"

-- ReferenceTypeSignature
referenceTypeSignatureP =
  FromClassTypeSignature <$> classTypeSignatureP <|>
  ArrayTypeSignature <$> (char '[' *> many javaTypeSignatureP) <|>
  FromTypeVariableSignature <$>
  typeVariableSignatureP <?> "Field Type Signature"

-- ClassTypeSignature
classTypeSignatureP =
  (char 'L') *>
  (ClassTypeSignature <$> packageSpecifierP <*> simpleClassTypeSignatureP <*>
   many (char '.' *> simpleClassTypeSignatureP)) <*
  (char ';')

packageP = identifierP <* char '/'

packageSpecifierP = manyTill packageP (try (notFollowedBy packageP))

simpleClassTypeSignatureP =
  SimpleClassTypeSignature <$> identifierP <*> (typeArgumentsP <|> return [])

typeArgumentsP = char '<' *> many1 typeArgumentP <* char '>'

typeArgumentP =
  TypeArgumentWithIndicator <$> wildCardIndicatorP <*> referenceTypeSignatureP <|>
  TypeArgument <$> referenceTypeSignatureP <|>
  Asterisk <$ char '*' <?> "TypeArgument"

wildCardIndicatorP =
  Plus <$ char '+' <|> Minus <$ char '-' <?> "wildcard indicator"

typeVariableSignatureP =
  TypeVariableSignature <$> (char 'T' *> identifierP <* char ';')

-- ClassSignature
classSignatureP :: StringParser ClassSignature
classSignatureP =
  ClassSignature <$> typeParametersP <*> classTypeSignatureP <*>
  many classTypeSignatureP

typeParametersP =
  char '<' *> many1 formalTypeParameterP <* char '>' <|> return []

formalTypeParameterP =
  FormalTypeParameter <$> identifierP <*> classBoundP <*> many interfaceBoundP

classBoundP = char ':' *> referenceTypeSignatureP

interfaceBoundP = char ':' *> referenceTypeSignatureP

-- MethodSignature
methodSignatureP :: StringParser MethodSignature
methodSignatureP =
  MethodSignature <$> typeParametersP <*>
  (char '(' *> many javaTypeSignatureP <* char ')') <*>
  resultP <*>
  many throwsSignatureP

resultP =
  ReturnTypeSignature <$> javaTypeSignatureP <|>
  VoidTypeSignature <$ voidDescriptorP <?> "ReturnType"

throwsSignatureP =
  ThrowsSignature <$> (char '^' *> classTypeSignatureP) <*>
  (char '^' *> typeVariableSignatureP)

-- FieldSignature
fieldSignatureP = referenceTypeSignatureP
