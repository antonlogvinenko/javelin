module Javelin.Runtime.LLI.Loading
where

import Javelin.ByteCode.Data
import Data.Int (Int32, Int64)


-- Some basics

type Derive a = ConstantPool -> Int -> Maybe a

(!?) :: (Integral i) => [a] -> i -> Maybe a
arr !? idx = undefined

deriveFromClass :: (Integral i) => i -> i -> ConstantPool -> Maybe (String, String, String)
deriveFromClass classIdx typeIdx p = do
  classInfo <- p !? classIdx
  nameAndTypeInfo <- p !? typeIdx
  case (classInfo, nameAndTypeInfo) of
    (ClassInfo classNameIdx, NameAndTypeInfo methodNameIdx descriptorIdx) -> do
      className <- p !? classNameIdx
      methodName <- p !? methodNameIdx
      descriptor <- p!? descriptorIdx
      case (className, methodName, descriptor) of
        (Utf8Info a, Utf8Info b, Utf8Info c) -> return (a, b, c)
        _ -> Nothing
    _ -> Nothing

deriveClassInterface :: Derive (String, String, String)
deriveClassInterface = undefined

deriveField :: Derive (String, String, String)
deriveField p i = do
  fieldref <- p !? i
  case fieldref of
    Fieldref classIdx typeIdx -> deriveFromClass classIdx typeIdx p
    _ -> Nothing

deriveClassMethod :: Derive (String, String, String)
deriveClassMethod p i = do
  classMethodref <- p !? i
  case classMethodref of
    Methodref classIdx typeIdx -> deriveFromClass classIdx typeIdx p
    _ -> Nothing

deriveInterfaceMethod :: Derive (String, String, String)
deriveInterfaceMethod p i = do
  interfaceMethodRef <- p !? i
  case interfaceMethodRef of
    InterfaceMethodref classIndex typeIndex -> deriveFromClass classIndex typeIndex p
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
