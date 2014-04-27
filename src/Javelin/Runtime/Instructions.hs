module Javelin.Runtime.Instructions
where

import Control.Monad.State.Lazy (State, state)
import Javelin.Runtime.Thread (Thread(..),
                               Frame(..), Locals, Operands, ConstantPool, ProgramCounter,
                               FrameStack, Memory,
                               pool, operands, locals)
import qualified Data.Map.Lazy as Map (fromList, Map(..), union, map, (!))
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
type Instruction = [Word8] -> ThreadOperation ()


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

pushn :: (JType j) => [j] -> ThreadOperation ()
pushn val = state $ \t -> let frames1 = frames t
                              operands1 = operands (frames1 !! 0)
                          in ((), Thread 0 [Frame undefined ((map (fff . represent) val) ++ operands1) undefined])

pop :: (JType j) => (Word64 -> j) -> ThreadOperation j
pop f = state $ \t -> let frames1 = frames t
                          operands1 = operands $ frames1 !! 0
                      in (f $ operands1 !! 0, t)

popn :: (JType j) => (Word64 -> j) -> ThreadOperation [j]
popn f = state $ \t -> let frames1 = frames t
                           operands1 = operands $ frames1 !! 0
                       in (map f operands1, t)

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

nop args = state $ \t -> ((), t)
iconst x = push (x :: JInt)
lconst x = push (x :: JLong)
aconst_null args = iconst 0
iconst_m1 args = iconst (-1)
iconst_0 args = iconst 0
iconst_1 args = iconst 1
iconst_2 args = iconst 2
iconst_3 args = iconst 3
iconst_4 args = iconst 4
iconst_5 args = iconst 5
lconst_0 args = lconst 0
lconst_1 args = lconst 1
fconst_0 args = push (0 :: JFloat)
fconst_1 args = push (1 :: JFloat)
fconst_2 args = push (2 :: JFloat)
dconst_0 args = push (0 :: JDouble)
dconst_1 args = push (0 :: JDouble)
bipush args = undefined
sipush args = undefined
ldc args = undefined
ldc_w args = undefined
ldc2_w args = undefined

-- Stack

pop1 args = do
  remove
pop2 args = do
  remove
  remove
dup args = do
  op <- peek jraw
  push op
dup_x1 args = do
  (op1:op2:_) <- popn jraw
  pushn [op1, op2, op1]
dup_x2 args = do
  (op1:op2:op3:_) <- popn jraw
  pushn [op1, op3, op2, op1]
dup2 args = do
  (op1:op2:_) <- popn jraw
  pushn [op2, op1, op2, op1]
dup2_x1 args = do
  (op1:op2:op3:_) <- popn jraw
  pushn [op2, op1, op3, op2, op1]
dup2_x2 args = do
  (op1:op2:op3:op4:_) <- popn jraw
  pushn [op2, op1, op4, op3, op2, op1]
swap args = do
  (op1:op2:_) <- popn jraw
  pushn [op2, op1]



iadd args = do
  op1 <- pop jint
  op2 <- pop jint
  push $ op1 + op2

iload (idx:args) = do
  var <- load jint idx
  push var

iload_0 args = iload [0]
iload_1 args = iload [1]
iload_2 args = iload [2]
iload_3 args = iload [3]

istore [idx] = do
  op <- pop jint
  store op idx
istore_0 args = istore [0]
istore_1 args = istore [1]
istore_2 args = istore [2]
istore_3 args = istore [3]


bytes2Int :: Word64 -> Integer
bytes2Int = fromIntegral

bytes2Short :: Word64 -> Integer
bytes2Short = fromIntegral

localsElem :: Integer -> Word32
localsElem = fromIntegral

stackElem :: Integer -> Word64
stackElem = fromIntegral
