module Javelin.ByteCode.DescSign
where

import Javelin.ByteCode.Data
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Control.Applicative ((<$), (<$>), (<*), (<*>), (*>))


parseFieldDescriptor = parse fieldDescriptorP ""
parseMethodDescriptor = parse methodDescriptorP ""
parseMethodSignature = parse methodSignatureP ""
parseClassSignature = parse classSignatureP ""
parseFieldSignature = parse fieldSignatureP ""

-- Fundamentals
type StringParser a = CharParser () a

nameSymbol = noneOf ".;[/"
unqualifiedNameP = many nameSymbol
qualifiedNameP = sepBy unqualifiedNameP (char '/')
unqualifiedNameP :: StringParser UnqualifiedName

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


-- JavaTypeSignature
identifierP = many $ noneOf ".;[/<>:"
javaTypeSignatureP = FromReferenceTypeSignature <$> referenceTypeSignatureP
                 <|> FromBaseTypeSignature <$> baseTypeP
                 <?> "TypeSignature"


-- ReferenceTypeSignature
referenceTypeSignatureP = FromClassTypeSignature <$> classTypeSignatureP
                      <|> ArrayTypeSignature <$> (char '[' *> many javaTypeSignatureP)
                      <|> FromTypeVariableSignature <$> typeVariableSignatureP
                      <?> "Field Type Signature"


-- ClassTypeSignature
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
typeArgumentP = TypeArgumentWithIndicator <$> wildCardIndicatorP <*> referenceTypeSignatureP
                <|> TypeArgument <$> referenceTypeSignatureP
                <|> Asterisk <$ char '*'
                <?> "TypeArgument"

wildCardIndicatorP = Plus <$ char '+'
                     <|> Minus <$ char '-'
                     <?> "wildcard indicator"

typeVariableSignatureP = TypeVariableSignature <$> (char 'T' *> identifierP <* char ';')


-- ClassSignature
classSignatureP :: StringParser ClassSignature
classSignatureP = ClassSignature
                  <$> typeParametersP
                  <*> classTypeSignatureP
                  <*> many classTypeSignatureP

typeParametersP = char '<' *> many1 formalTypeParameterP <* char '>'
                        <|> return []
formalTypeParameterP = FormalTypeParameter
                       <$> identifierP
                       <*> classBoundP
                       <*> many interfaceBoundP
classBoundP = char ':' *> referenceTypeSignatureP
interfaceBoundP = char ':' *> referenceTypeSignatureP


-- MethodSignature
methodSignatureP :: StringParser MethodSignature
methodSignatureP = MethodSignature
                   <$> typeParametersP
                   <*> (char '(' *> many javaTypeSignatureP <* char ')')
                   <*> resultP
                   <*> many throwsSignatureP
resultP = ReturnTypeSignature <$> javaTypeSignatureP
              <|> VoidTypeSignature <$ voidDescriptorP
              <?> "ReturnType"
throwsSignatureP = ThrowsSignature
                   <$> (char '^' *> classTypeSignatureP)
                   <*> (char '^' *> typeVariableSignatureP)


-- FieldSignature
fieldSignatureP = referenceTypeSignatureP
