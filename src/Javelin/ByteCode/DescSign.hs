module Javelin.ByteCode.DescSign
where

import Javelin.ByteCode.Data
  
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Error
import Control.Applicative ((<$), (<$>), (<*), (<*>), (*>))
import Text.ParserCombinators.Parsec.Combinator (endBy, sepBy)

parseFieldType = parse fieldTypeP ""
parseBaseType = parse baseTypeP ""
parseFieldDescriptor = parse fieldDescriptorP ""
parseMethodDescriptor = parse methodDescriptorP ""
parseMethodTypeSignature = parse methodTypeSignatureP ""
parseClassSignature = parse classSignatureP ""

-- Fundamental definitions
type StringParser a = CharParser () a

unqualifiedNameP :: StringParser UnqualifiedName
unqualifiedNameP = undefined
qualifiedNameP = endBy classPartsP (char ';')
classPartsP = sepBy anyChar (char '/')

fieldTypeP = BaseType <$> baseTypeP
             <|> ObjectType <$> (char 'L' *> qualifiedNameP <* char ';')
             <|> ArrayType <$> fieldTypeP <* char '['
             <?> "FieldType"

baseTypeP :: StringParser BaseType
baseTypeP = ByteT <$ char 'B'
            <|> CharT <$ char 'C'
            <|> DoubleT <$ char 'D'
            <|> FloatT <$ char 'F'
            <|> IntT <$ char 'I'
            <|> LongT <$ char 'J'
            <|> ShortT <$ char 'S'
            <|> BooleanT <$ char 'Z'
            <?> "BaseType"
            
-- FieldDescriptor
fieldDescriptorP = FieldDescriptor <$> fieldTypeP

-- MethodDescriptor
methodDescriptorP = MethodDescriptor <$> many fieldTypeP <*> returnDescriptorP
returnDescriptorP = FieldTypeDescriptor <$> fieldTypeP
                    <|> VoidDescriptor <$ voidDescriptorP
                    <?> "ReturnDescriptor"
voidDescriptorP = char 'v'

-- ClassSignature
classSignatureP :: StringParser ClassSignature
classSignatureP = ClassSignature <$> many formalTypeParameterP
                  <*> classTypeSignatureP
                  <*> classTypeSignatureP
  
formalTypeParameterP = FormalTypeParameter <$> unqualifiedNameP
                       <*> fieldTypeSignatureP
                       <*> many fieldTypeSignatureP
                                    
fieldTypeSignatureP :: StringParser FieldTypeSignature
fieldTypeSignatureP = ClassFieldType <$> classTypeSignatureP
                      <|> ArrayFieldType <$> many typeSignatureP
                      <|> TypeVariable <$> typeVariableSignatureP
                      <?> "Field Type Signature"

typeVariableSignatureP :: StringParser TypeVariableSignature
typeVariableSignatureP = TypeVariableSignature <$> unqualifiedNameP

classTypeSignatureP :: StringParser ClassTypeSignature
classTypeSignatureP = ClassTypeSignature <$> many unqualifiedNameP
                      <*> simpleClassTypeSignatureP
                      <*> many simpleClassTypeSignatureP
simpleClassTypeSignatureP = SimpleClassTypeSignature <$> unqualifiedNameP <*> many typeArgumentP
                                       
typeArgumentP :: StringParser TypeArgument
typeArgumentP = TypeArgument <$> wildCardIndicatorP <*> fieldTypeSignatureP
                <|> Asterisk <$ char '*'
                <?> "TypeArgument"

wildCardIndicatorP = Plus <$ char '+'
                     <|> Minus <$ char '-'
                     <?> "wildcard indicator"
typeSignatureP = FieldTypeTypeSignature <$> fieldTypeSignatureP
                 <|> BaseTypeTypeSignature <$> baseTypeP
                 <?> "TypeSignature"

--MethodTypeSignature
methodTypeSignatureP :: StringParser MethodTypeSignature
methodTypeSignatureP = MethodTypeSignature <$> many formalTypeParameterP
                       <*> many typeSignatureP
                       <*> returnTypeP
                       <*> many throwsSignatureP
returnTypeP = ReturnTypeSignature <$> typeSignatureP
              <|> VoidTypeSignature <$ voidDescriptorP
              <?> "ReturnType"
throwsSignatureP = ThrowsSignature <$> classTypeSignatureP <*> typeVariableSignatureP
