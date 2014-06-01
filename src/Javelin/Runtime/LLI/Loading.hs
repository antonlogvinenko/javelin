module Javelin.Runtime.LLI.Loading
where

import Javelin.ByteCode.Data
import Data.Int (Int32, Int64)
import Data.Word (Word16)
import Data.Map.Lazy (fromList, Map, insert)
import Control.Applicative ((<$>))



-- Derivation datatypes

type DerivedPool = Map Int SymbolicReference

data SymbolicReference = ClassOrInterface { classInterfaceName :: String }
                       | FieldReference { field :: PartReference }
                       | ClassMethodReference { classMethod :: PartReference }
                       | InterfaceMethodReference { interfaceMethod :: PartReference }
                       | MethodHandleReference
                       | MethodTypeReference { typeReference :: String }
                       | CallSiteSpecifierReference
                       | StringLiteral { string :: String }
                       | DoubleLiteral { double :: Double }
                       | FloatLiteral { float :: Float }
                       | IntegerLiteral { integer :: Int32 }
                       | LongLiteral { long :: Int64 }
                       deriving (Show, Eq)

data PartReference = PartReference { partName :: String,
                                     partDescriptor :: String,
                                     ownerName :: String }
                   deriving (Show, Eq)


-- Derivation

derivePool :: ConstantPool -> DerivedPool
derivePool p = deriveReduce p (length p - 1) $ fromList []

deriveReduce :: ConstantPool -> Int -> DerivedPool -> DerivedPool
deriveReduce _ (-1) d = d
deriveReduce p i d = deriveReduce p (i - 1) d2
  where item = p !! i
        d2 = case deriveReference p item of
          Just ref -> insert i ref d
          Nothing -> d

deriveReference :: ConstantPool -> Constant -> Maybe SymbolicReference
deriveReference p c = case c of
  ClassInfo idx ->
    ClassOrInterface <$> deriveUtf8 p idx
  Fieldref classIdx typeIdx ->
    FieldReference <$> deriveFromClass classIdx typeIdx p
  Methodref classIdx typeIdx ->
    ClassMethodReference <$> deriveFromClass classIdx typeIdx p
  InterfaceMethodref classIdx typeIdx ->
    InterfaceMethodReference <$> deriveFromClass classIdx typeIdx p
  MethodHandleInfo x y -> undefined
  MethodTypeInfo idx -> MethodTypeReference <$> deriveUtf8 p idx
  InvokeDynamicInfo x y -> undefined
  StringInfo idx -> StringLiteral <$> deriveUtf8 p idx
  DoubleInfo val -> Just $ DoubleLiteral val
  FloatInfo val -> Just $ FloatLiteral val
  LongInfo val -> Just $ LongLiteral val
  IntegerInfo val -> Just $ IntegerLiteral val
  _ -> Nothing



-- Derivation utility functions

(!?) :: (Integral i) => [a] -> i -> Maybe a
arr !? idx = undefined

deriveFromClass :: (Integral i) => i -> i -> ConstantPool -> Maybe PartReference
deriveFromClass classIdx typeIdx p = do
  classInfo <- p !? classIdx
  nameAndTypeInfo <- p !? typeIdx
  case (classInfo, nameAndTypeInfo) of
    (ClassInfo classNameIdx, NameAndTypeInfo methodNameIdx descriptorIdx) -> do
      className <- p !? classNameIdx
      methodName <- p !? methodNameIdx
      descriptor <- p!? descriptorIdx
      case (className, methodName, descriptor) of
        (Utf8Info a, Utf8Info b, Utf8Info c) -> return $ PartReference a b c
        _ -> Nothing
    _ -> Nothing

deriveUtf8 :: ConstantPool -> Word16 -> Maybe String
deriveUtf8 p idx = do
  name <- p !? idx
  case name of
    Utf8Info name -> Just name
    _ -> Nothing
    


