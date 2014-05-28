module Javelin.Runtime.LLI.Loading
where

import Javelin.ByteCode.Data
import Data.Int (Int32, Int64)

type Derive a = ConstantPool -> Int -> Maybe a

(!?) :: (Integral i) => [a] -> i -> Maybe a
arr !? idx = undefined

deriveClassInterface :: Derive (String, FieldDescriptor, ClassTypeSignature)
deriveClassInterface = undefined

deriveField :: Derive (String, FieldDescriptor, ClassTypeSignature)
deriveField = undefined

deriveClassMethod :: Derive (String, MethodDescriptor, ClassTypeSignature)
deriveClassMethod = undefined

deriveInterfaceMethod :: Derive (String, String, String)
deriveInterfaceMethod p i = do
  interfaceMethodRef <- p !? i
  case interfaceMethodRef of
    InterfaceMethodref classIndex typeIndex -> do
      classInfo <- p !? classIndex
      nameAndTypeInfo <- p !? typeIndex
      case (classInfo, nameAndTypeInfo) of
        (ClassInfo classNameIdx, NameAndTypeInfo methodNameIdx descriptorIdx) -> do
          className <- p !? classNameIdx
          methodName <- p !? methodNameIdx
          descriptor <- p!? descriptorIdx
          case (className, methodName, descriptor) of
            (Utf8Info a, Utf8Info b, Utf8Info c) -> return (a, b, c)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
    
deriveMethodHandle = undefined

deriveMethodType :: Derive String
deriveMethodType p i = case p !! i of
  MethodTypeInfo idx -> case p !! fromIntegral idx of
    Utf8Info x -> Just x
    _ -> Nothing
  _ -> Nothing



deriveCallSiteSpecifier = undefined

deriveStringLiteral :: Derive String
deriveStringLiteral p i = case p !! i of
  Utf8Info x -> Just x
  _ -> Nothing

data NumericLiteral = DoubleLiteral { getDouble :: Double }
                    | FloatLiteral { getFloat :: Float }
                    | IntegerLiteral { getInteger :: Int32 }
                    | LongLiteral { getLong :: Int64 }

deriveNumericLiteral :: Derive NumericLiteral
deriveNumericLiteral p i = case p !! i of
  DoubleInfo x -> Just $ DoubleLiteral x
  FloatInfo x -> Just $ FloatLiteral x
  LongInfo x -> Just $ LongLiteral x
  IntegerInfo x -> Just $ IntegerLiteral x
  _ -> Nothing
