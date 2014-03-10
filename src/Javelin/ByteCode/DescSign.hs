module Javelin.ByteCode.DescSign
where

import Javelin.ByteCode.Data
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec.Error
import Control.Applicative ((<$), (<$>), (<*), (<*>), (*>))
import Text.ParserCombinators.Parsec.Combinator (endBy, sepBy)

parseFieldDescriptor = parse fieldDescriptorP ""
parseMethodDescriptor = parse methodDescriptorP ""
parseMethodTypeSignature = parse methodTypeSignatureP ""
parseClassSignature = parse classSignatureP ""

-- Fundamental definitions
type StringParser a = CharParser () a

unqualifiedNameP :: StringParser UnqualifiedName
nameSymbol = noneOf ".;[/"
unqualifiedNameP = many nameSymbol
qualifiedNameP = sepBy unqualifiedNameP (char '/')

fieldTypeP = BaseType <$> baseTypeP
             <|> ObjectType <$> (char 'L' *> qualifiedNameP <* char ';')
             <|> ArrayType <$> (char '[' *> fieldTypeP)
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
methodDescriptorP = MethodDescriptor
                    <$> (char '(' *> many fieldTypeP)
                    <*> (char ')' *> returnDescriptorP)
returnDescriptorP = FieldTypeDescriptor <$> fieldTypeP
                    <|> VoidDescriptor <$ voidDescriptorP
                    <?> "ReturnDescriptor"
voidDescriptorP = char 'v'

-- ClassSignature
identifierP = many $ noneOf ".;[/<>:"

classSignatureP :: StringParser ClassSignature
classSignatureP = ClassSignature
                  <$> formalTypeParametersP
                  <*> classTypeSignatureP
                  <*> many classTypeSignatureP



formalTypeParametersP = char '<' *> many1 formalTypeParameterP <* char '>'
                        <|> return []
formalTypeParameterP = FormalTypeParameter
                       <$> identifierP
                       <*> classBoundP
                       <*> many interfaceBoundP

classBoundP = char ':' *> fieldTypeSignatureP
interfaceBoundP = char ':' *> fieldTypeSignatureP
fieldTypeSignatureP :: StringParser FieldTypeSignature
fieldTypeSignatureP = ClassFieldType <$> classTypeSignatureP
                      <|> ArrayFieldType <$> (char '[' *> many typeSignatureP)
                      <|> TypeVariable <$> typeVariableSignatureP
                      <?> "Field Type Signature"

typeVariableSignatureP :: StringParser TypeVariableSignature
typeVariableSignatureP = TypeVariableSignature <$> (char 'T' *> identifierP <* char ';')

classTypeSignatureP :: StringParser ClassTypeSignature
classTypeSignatureP = (char 'L') *> (ClassTypeSignature
                      <$> packageSpecifierP
                      <*> simpleClassTypeSignatureP
                      <*> many (char '.' *> simpleClassTypeSignatureP))
                      <* (char ';')
packageP = identifierP <* char '/'
packageSpecifierP = manyTill packageP (try (notFollowedBy packageP))

                      
simpleClassTypeSignatureP = SimpleClassTypeSignature
                            <$> identifierP
                            <*> (typeArgumentsP <|> return [])
typeArgumentsP = char '<' *> many1 typeArgumentP <* char '>'
                                       
typeArgumentP :: StringParser TypeArgument
typeArgumentP = TypeArgumentWithIndicator <$> wildCardIndicatorP <*> fieldTypeSignatureP
                <|> TypeArgument <$> fieldTypeSignatureP
                <|> Asterisk <$ char '*'
                <?> "TypeArgument"

wildCardIndicatorP = Plus <$ char '+'
                     <|> Minus <$ char '-'
                     <?> "wildcard indicator"
typeSignatureP = FieldTypeTypeSignature <$> fieldTypeSignatureP
                 <|> BaseTypeTypeSignature <$> baseTypeP
                 <?> "TypeSignature"


parseX = parse methodTypeSignatureP ""


--MethodTypeSignature
methodTypeSignatureP :: StringParser MethodTypeSignature
methodTypeSignatureP = MethodTypeSignature
                       <$> formalTypeParametersP
                       <*> (char '(' *> many typeSignatureP <* char ')')
                       <*> returnTypeP
                       <*> many throwsSignatureP
returnTypeP = ReturnTypeSignature <$> typeSignatureP
              <|> VoidTypeSignature <$ voidDescriptorP
              <?> "ReturnType"
throwsSignatureP = ThrowsSignature
                   <$> (char '^' *> classTypeSignatureP)
                   <*> (char '^' *> typeVariableSignatureP)

