module Javelin.Runtime.Instructions
where

import Control.Monad.State.Lazy (State, state)
import Javelin.Runtime.Thread (Thread(..),
                               Frame(..), Locals, Operands, ConstantPool, ProgramCounter,
                               FrameStack, Memory,
                               pool, operands, locals)
import qualified Data.Map.Lazy as Map (fromList, Map)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)



-- Primitive types
data Representation = Narrow { narrow :: Word32 }
                    | Wide { wide :: Word64 }

cake32 :: (Num a) => a -> Word32
cake32 x = 42

cake64 :: (Num a) => a -> Word64
cake64 x = 42

type JByte  =   Int8
type JShort =   Int16
type JInt   =   Int32
type JLong  =   Int64
type JBoolean = JInt
type JChar =    Word16
type JDouble =  Double
type JFloat =   Float
type JRaw =     Word64


class JType a where
  represent :: a -> Representation
instance JType Int8 where
  represent x = Narrow $ cake32 x
instance JType Int16 where
  represent x = Narrow $ cake32 x
instance JType Int32 where
  represent x = Narrow $ cake32 x
instance JType Int64 where
  represent x = Wide $ cake64 x
instance JType Word16 where
  represent x = Narrow $ cake32 x
instance JType Double where
  represent x = Narrow $ cake32 x
instance JType Float where
  represent x = Wide $ cake64 x
instance JType Word64 where
  represent x = Wide x

jraw :: Word64 -> JRaw
jraw = id
jbyte :: Word64 -> JByte
jbyte repr = undefined
jshort :: Word64 -> JShort
jshort repr = undefined
jint :: Word64 -> JInt
jint repr = undefined
jlong :: Word64 -> JLong
jlong repr = undefined
jchar :: Word64 -> JChar
jchar repr = undefined
jboolean :: Word64 -> JBoolean
jboolean repr = undefined
jfloat :: Word64 -> JFloat
jfloat repr = undefined
jdouble :: Word64 -> JDouble
jdouble repr = undefined



-- Instructions DSL

type ThreadOperation a = State Thread a
type Instruction = ThreadOperation ()


remove :: ThreadOperation ()
remove = state $ \t -> let frames1 = frames t
                           operands1 = operands $ frames1 !! 0
                       in ((), t)

peek :: (JType j) => (Word64 -> j) -> ThreadOperation j
peek f = state $ \t -> let frames1 = frames t
                           operands1 = operands $ frames1 !! 0
                       in (f $ operands1 !! 0, t)

push :: (JType j) => j -> ThreadOperation ()
push j = state $ \t -> let frames1 = frames t
                           operands1 = operands (frames1 !! 0)
                           val = case represent j of
                             Narrow x -> undefined
                             Wide x -> undefined
                       in ((), Thread 0 [Frame undefined (val:operands1) undefined])

arg :: Int -> ThreadOperation Word8
arg n = state $ \t -> (42, t)

pushn :: (JType j) => [j] -> ThreadOperation ()
pushn val = state $ \t -> let frames1 = frames t
                              operands1 = operands (frames1 !! 0)
                          in ((), Thread 0 [Frame undefined ((map (fff . represent) val) ++ operands1) undefined])

pop :: (JType j) => (Word64 -> j) -> ThreadOperation j
pop f = state $ \t -> let frames1 = frames t
                          operands1 = operands $ frames1 !! 0
                      in (f $ operands1 !! 0, t)

popn :: (JType j) => (Word64 -> j) -> Int -> ThreadOperation [j]
popn f n = state $ \t -> let frames1 = frames t
                             operands1 = operands $ frames1 !! 0
                         in (take n $ map f operands1, t)

store :: (JType j) => j -> Word8 -> ThreadOperation ()
store j idx = state $ \t -> ((), t)

load :: (JType j) => (Word64 -> j) -> Word8 -> ThreadOperation j
load f idx = state $ \t -> (undefined, t)

fff :: Representation -> Word64
fff = undefined



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

int64 :: (Integral a) => a -> Int64
int64 x = fromIntegral x

nop = state $ \t -> ((), t)
iconst x = push (x :: JInt)
lconst x = push (x :: JLong)
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
fconst_0 = push (0 :: JFloat)
fconst_1 = push (1 :: JFloat)
fconst_2 = push (2 :: JFloat)
dconst_0 = push (0 :: JDouble)
dconst_1 = push (0 :: JDouble)
bipush = undefined
sipush = undefined
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



iadd = do
  op1 <- pop jint
  op2 <- pop jint
  push $ op1 + op2
iloadFrom idx = do
  var <- load jint idx
  push var
iload = do
  idx <- arg 0
  iloadFrom idx
iload_0 = iloadFrom 0
iload_1 = iloadFrom 1
iload_2 = iloadFrom 2
iload_3 = iloadFrom 3
istoreAt idx  = do
  op <- pop jint
  store op idx
istore = do
  idx <- arg 0
  istoreAt idx
istore_0 = istoreAt 0
istore_1 = istoreAt 1
istore_2 = istoreAt 2
istore_3 = istoreAt 3
