module Javelin.Runtime.Instructions
where

import Control.Monad.State.Lazy (State, state)
import Javelin.Runtime.Thread (Thread(..),
                               Frame(..), ConstantPool, ProgramCounter,
                               FrameStack, Memory, StackElement(..), Locals(..),
                               BytesContainer,
                               pool, operands, locals, stackElement, getBytes)
import qualified Data.Map.Lazy as Map (fromList, Map)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Array.IArray (Array, array, (!), (//))
import Data.Binary.IEEE754 (floatToWord, doubleToWord)
import Data.Bits (rotate)



-- Primitive types
data Representation = Narrow { narrow :: Word32 }
                    | Wide { wide :: Word64 }

type JByte  =   Int8
type JShort =   Int16
type JInt   =   Int32
type JLong  =   Int64
type JBoolean = JInt
type JChar =    Word16
type JDouble =  Double
type JFloat =   Float
type JRaw =     Word64

narrowInt :: (Integral a) => a -> Representation 
narrowInt = Narrow . fromIntegral

wideInt :: (Integral a) => a -> Representation
wideInt = Wide . fromIntegral

class JType a where
  represent :: a -> Representation
instance JType Int8 where
  represent = narrowInt
instance JType Int16 where
  represent = narrowInt
instance JType Int32 where
  represent = narrowInt
instance JType Int64 where
  represent = wideInt
instance JType Word16 where
  represent = narrowInt
instance JType Double where
  represent = Wide . doubleToWord
instance JType Float where
  represent = Narrow . floatToWord
instance JType Word64 where
  represent = Wide

fetchBytes :: (BytesContainer c, Num a) => Int -> Int -> c -> a
fetchBytes len idx c = fromIntegral $ getBytes c idx len

jraw :: (BytesContainer c) => Int -> c -> JRaw
jraw = fetchBytes 8
jbyte :: (BytesContainer c) => Int -> c -> JByte
jbyte = fetchBytes 1
jshort :: (BytesContainer c) => Int -> c -> JShort
jshort = fetchBytes 2
jint :: (BytesContainer c) => Int -> c -> JInt
jint = fetchBytes 4
jlong :: (BytesContainer c) => Int -> c -> JLong
jlong = fetchBytes 8
jchar :: (BytesContainer c) => Int -> c -> JChar
jchar = fetchBytes 2
jboolean :: (BytesContainer c) => Int -> c -> JBoolean
jboolean = fetchBytes 1
jfloat :: (BytesContainer c) => Int -> c -> JFloat
jfloat = fetchBytes 8
jdouble :: (BytesContainer c) => Int -> c -> JDouble
jdouble = fetchBytes 8



-- Instructions DSL

type ThreadOperation a = State Thread a
type Instruction = ThreadOperation ()

getFrame :: Thread -> Frame
getFrame = (!!0) . frames

updFrame :: Thread -> (Frame -> Frame) -> Thread
updFrame t@(Thread {frames = (tframe:tframes)}) f = t {frames = f tframe : tframes}

getStack :: Thread -> [StackElement]
getStack = operands . getFrame

updStack :: Thread -> ([StackElement] -> [StackElement]) -> Thread
updStack t@(Thread {frames = (tframe@(Frame {operands = toperands}):tframes)}) f =
  t {frames = (tframe {operands = f toperands}):tframes}

getLocals :: Thread -> Locals
getLocals = locals . getFrame

updLocals :: Thread -> (Array Int Word32 -> Array Int Word32) -> Thread
updLocals t@(Thread {frames = (tframe@(Frame {locals = Locals {vars = tvars}}) : tframes)}) f =
  t {frames = (tframe {locals = Locals $ f tvars}):tframes} 

remove :: ThreadOperation ()
remove = state $ \t -> ((), updStack t $ drop 1)

peek :: (JType j) => (Int -> StackElement -> j) -> ThreadOperation j
peek f = state $ \t -> (f 0 . (!!0) . getStack $ t, t)

push :: (JType j) => j -> ThreadOperation ()
push j = state $ \t -> let elem = jToStackElement j
                       in ((), updStack t (elem:))

jToStackElement :: (JType j) => j -> StackElement
jToStackElement j = StackElement $ case represent j of
  Narrow x -> fromIntegral x
  Wide x -> x

arg :: (JType j) => (Int -> Locals -> j) -> Int -> ThreadOperation j
arg f n = state $ \t -> (f n $ getLocals t, t)

pushn :: (JType j) => [j] -> ThreadOperation ()
pushn js = state $ \t -> let vals = map jToStackElement js
                         in ((), updStack t (vals ++))

pop :: (JType j) => (Int -> StackElement -> j) -> ThreadOperation j
pop f = state $ \t -> let nElems = getStack t !! 0
                      in (f 0 nElems, updStack t $ drop 1)

popn :: (JType j) => (Int -> StackElement -> j) -> Int -> ThreadOperation [j]
popn f n = state $ \t -> let nElems = take n $ getStack t
                         in (map (f 0) nElems, updStack t $ drop n)

store :: (JType j) => j -> Word16 -> ThreadOperation ()
store j idx = state $ \t -> let idx = fromIntegral idx
                                arr = vars $ getLocals t
                                r = represent j
                                newLocals = case r of
                                  Narrow x -> arr // [(idx, x)]
                                  Wide x -> let (a, b) = split x
                                            in arr // [(idx, a), (idx+1, b)]
                            in ((), updLocals t $ \_ -> arr)

split :: Word64 -> (Word32, Word32)
split x = let a = fromIntegral x
              b = fromIntegral $ rotate x 32
              in (a, b)

load :: (JType j) => (Int -> Locals -> j) -> Word16 -> ThreadOperation j
load f idx = state $ \t -> (f (fromIntegral idx) $ getLocals t, t)

signExtend :: (Integral a) => a -> JInt
signExtend = fromIntegral


-- Instruction listing

instructions :: Map.Map Word8 (Int, Instruction)
instructions = Map.fromList [

  -- Constants

  (0x00, (0, nop)),
  (0x01, (0, aconst_null)),
  (0x02, (0, iconst_m1)),
  (0x03, (0, iconst_0)),
  (0x04, (0, iconst_1)),
  (0x05, (0, iconst_2)),
  (0x06, (0, iconst_3)),
  (0x07, (0, iconst_4)),
  (0x08, (0, iconst_5)),
  (0x09, (0, lconst_0)),
  (0x0a, (0, lconst_1)),
  (0x0b, (0, fconst_0)),
  (0x0c, (0, fconst_1)),
  (0x0d, (0, fconst_2)),
  (0x0e, (0, dconst_0)),
  (0x0f, (0, dconst_1)),
  (0x10, (0, bipush)),
  (0x11, (0, sipush)),
  (0x12, (0, ldc)),
  (0x13, (0, ldc_w)),
  (0x14, (0, ldc2_w)),
  
  -- Loads

  -- Stores

  -- Stack

  (0x57, (0, pop1)),
  (0x58, (0, pop2)),
  (0x59, (0, dup)),
  (0x5a, (0, dup_x1)),
  (0x5b, (0, dup_x2)),
  (0x5c, (0, dup2)),
  (0x5d, (0, dup2_x1)),
  (0x5e, (0, dup2_x2)),
  (0x5f, (0, swap)),

  -- Math

  -- Conversions

  -- Comparisons

  -- References

  -- Control

  -- Extended

  -- Reserved
  
  (0x60, (0, iadd)),

  (0x19, (0, iload)), (0x2a, (0, iload_0)), (0x2b, (0, iload_1)),
  (0x2c, (0, iload_2)), (0x2d, (0, iload_3)),
  
  (0x3a, (0, istore)), (0x4b, (0, istore_0)), (0x4c, (0, istore_1)),
  (0x4d, (0, istore_2)), (0x4e, (0, istore_3))]



-- Instructions implementation

-- Constants

nop = state $ \t -> ((), t)
iconst x = push (x :: JInt)
lconst x = push (x :: JLong)
fconst x = push (x :: JFloat)
dconst x = push (x :: JDouble)
aconst_null = iconst 0
iconst_m1 = iconst (-1)
iconst_0 = iconst 0
iconst_1 = iconst 1
iconst_2 = iconst 2
iconst_3 = iconst 3
iconst_4 = iconst 4
iconst_5 = iconst 5
lconst_0 = lconst 0
lconst_1 = lconst 1
fconst_0 = fconst 0
fconst_1 = fconst 1
fconst_2 = fconst 2
dconst_0 = dconst 0
dconst_1 = dconst 1
bipush = do
  byte <- arg jbyte 0
  push $ signExtend byte
sipush = do
  short <- arg jshort 0
  push $ signExtend short
ldc = undefined
ldc_w = undefined
ldc2_w = undefined


-- Stack

pop1 = do
  remove
pop2 = do
  remove
  remove
dup = do
  op <- peek jraw
  push op
dup_x1 = do
  [op1, op2] <- popn jraw 2
  pushn [op1, op2, op1]
dup_x2 = do
  [op1, op2, op3] <- popn jraw 3
  pushn [op1, op3, op2, op1]
dup2 = do
  [op1, op2] <- popn jraw 2
  pushn [op2, op1, op2, op1]
dup2_x1 = do
  [op1, op2, op3] <- popn jraw 3
  pushn [op2, op1, op3, op2, op1]
dup2_x2 = do
  [op1, op2, op3, op4] <- popn jraw 4
  pushn [op2, op1, op4, op3, op2, op1]
swap = do
  [op1, op2] <- popn jraw 2
  pushn [op2, op1]


-- Math
math operandType operation = do
  op1 <- pop operandType
  op2 <- pop operandType
  push $ operation op1 op2
iadd = math jint (+)
ladd = math jlong (+)
fadd = math jfloat (+)
dadd = math jdouble (+)
isub = math jint (-)
lsub = math jlong (-)
fsub = math jfloat (-)
dsub = math jdouble (-)
imul = math jint (*)
lmul = math jlong (*)
fmul = math jfloat (*)
dmul = math jdouble (*)
idiv = undefined
ldiv = undefined
fdiv = math jfloat (/)
ddiv = math jdouble (/)

iloadFrom idx = do
  var <- load jint idx
  push var
iload = do
  idx <- arg jchar 0
  iloadFrom idx
iload_0 = iloadFrom 0
iload_1 = iloadFrom 1
iload_2 = iloadFrom 2
iload_3 = iloadFrom 3
istoreAt idx  = do
  op <- pop jint
  store op idx
istore = do
  idx <- arg jchar 0
  istoreAt idx
istore_0 = istoreAt 0
istore_1 = istoreAt 1
istore_2 = istoreAt 2
istore_3 = istoreAt 3
