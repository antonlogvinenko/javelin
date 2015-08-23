module Javelin.Runtime.LLI.Loading.DeriveSymTable
where

import Javelin.ByteCode.Data
import Javelin.Runtime.Structures

import Data.Word (Word16)

-- 5.1 The Run-Time Constant Pool

deriveSymTable :: ConstantPool -> SymTable
deriveSymTable p = map (deriveRef p) p

deriveRef :: ConstantPool -> Constant -> SymbolicReference

deriveRef p c@(MethodHandleInfo x y) = undefined
deriveRef p c@(InvokeDynamicInfo x y) = undefined

deriveRef p c@(ClassInfo idx) =
  ClassOrInterface $ deriveUtf8 p idx
deriveRef p c@(Fieldref classIdx nameAndTypeIdx) =
  FieldReference $ deriveFromClass classIdx nameAndTypeIdx p
deriveRef p c@(Methodref classIdx typeIdx) =
  ClassMethodReference $ deriveFromClass classIdx typeIdx p
deriveRef p c@(InterfaceMethodref classIdx typeIdx) =
  InterfaceMethodReference $ deriveFromClass classIdx typeIdx p
deriveRef p c@(MethodTypeInfo idx) =
  MethodTypeReference $ deriveUtf8 p idx

deriveRef p c@(StringInfo idx) =
  StringLiteral $ deriveUtf8 p idx
deriveRef p c@(DoubleInfo val) =
  DoubleLiteral val
deriveRef p c@(FloatInfo val) =
  FloatLiteral val
deriveRef p c@(LongInfo val) =
  LongLiteral val
deriveRef p c@(IntegerInfo val) =
  IntegerLiteral val

deriveFromClass :: (Integral i) => i -> i -> ConstantPool -> PartReference
deriveFromClass classIdx nameAndTypeIdx p =
  let classInfo = p !! fromIntegral classIdx
      nameAndTypeInfo = p !! fromIntegral nameAndTypeIdx
      className = stringValue $ p !! fromIntegral (nameIndex classInfo)
      memberName = stringValue $ p !! fromIntegral (nameIndex nameAndTypeInfo)
      memberDescriptor = stringValue $ p !! fromIntegral (nameAndTypeDescriptorIndex nameAndTypeInfo)
  in PartReference className memberName memberDescriptor

deriveUtf8 :: ConstantPool -> Word16 -> String
deriveUtf8 p idx = stringValue $ p !! fromIntegral idx
