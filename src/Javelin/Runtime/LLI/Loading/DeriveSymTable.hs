module Javelin.Runtime.LLI.Loading.DeriveSymTable
where

import Javelin.ByteCode.Data
import Javelin.Runtime.Structures

import Data.Word (Word16)

-- 5.1 The Runtime Constant Pool

deriveSymTable :: ConstantPool -> SymTable
deriveSymTable p = map (deriveReference p) p

deriveReference :: ConstantPool -> Constant -> SymbolicReference

deriveReference p c@(MethodHandleInfo x y) = undefined
deriveReference p c@(InvokeDynamicInfo x y) = undefined

deriveReference p c@(ClassInfo idx) = ClassOrInterface $ deriveUtf8 p idx
deriveReference p c@(Fieldref classIdx typeIdx) =
  FieldReference $ deriveFromClass classIdx typeIdx p
deriveReference p c@(Methodref classIdx typeIdx) =
  ClassMethodReference $ deriveFromClass classIdx typeIdx p
deriveReference p c@(InterfaceMethodref classIdx typeIdx) =
  InterfaceMethodReference $ deriveFromClass classIdx typeIdx p
deriveReference p c@(MethodTypeInfo idx) = MethodTypeReference $ deriveUtf8 p idx

deriveReference p c@(StringInfo idx) = StringLiteral $ deriveUtf8 p idx
deriveReference p c@(DoubleInfo val) = DoubleLiteral val
deriveReference p c@(FloatInfo val) = FloatLiteral val
deriveReference p c@(LongInfo val) = LongLiteral val
deriveReference p c@(IntegerInfo val) = IntegerLiteral val

deriveFromClass :: (Integral i) => i -> i -> ConstantPool -> PartReference
deriveFromClass classIdx typeIdx p =
  let classInfo = p !! fromIntegral classIdx
      nameAndTypeInfo = p !! fromIntegral typeIdx
      className = stringValue (p !! fromIntegral (nameIndex classInfo))
      methodName = stringValue (p !! fromIntegral (nameIndex nameAndTypeInfo))
      descriptor = stringValue (p !! fromIntegral (nameAndTypeDescriptorIndex nameAndTypeInfo))
  in PartReference className methodName descriptor

deriveUtf8 :: ConstantPool -> Word16 -> String
deriveUtf8 p idx = stringValue $ p !! fromIntegral idx
