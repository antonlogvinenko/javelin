module Javelin.Runtime.Instructions
where

import Control.Monad.State.Lazy (State, state)
import Javelin.Runtime.Thread (Thread(..),
                               Frame(..), Locals, Operands, ConstantPool, ProgramCounter,
                               FrameStack, Memory,
                               pool, operands, locals)
import qualified Data.Map.Lazy as Map (fromList, Map(..), union, map, (!))
import Data.Word (Word8, Word32, Word64)



-- Instructions DSL

type ThreadOperation a = State Thread a
type Instruction = [Word8] -> ThreadOperation ()

peek :: ThreadOperation Word64
peek = state $ \t -> let frames1 = frames t
                         operands1 = operands $ frames1 !! 0
                     in (operands1 !! 0, t)

pop :: ThreadOperation Word64
pop = state $ \t -> let frames1 = frames t
                        operands1 = operands $ frames1 !! 0
                    in (operands1 !! 0, t)

popn :: ThreadOperation [Word64]
popn = state $ \t -> let frames1 = frames t
                         operands1 = operands $ frames1 !! 0
                     in (operands1, t)

remove :: ThreadOperation ()
remove = state $ \t -> let frames1 = frames t
                           operands1 = operands $ frames1 !! 0
                       in ((), t)

push :: Word64 -> ThreadOperation ()
push val = state $ \t -> let frames1 = frames t
                             operands1 = operands (frames1 !! 0)
                         in ((), Thread 0 [Frame undefined (val:operands1) undefined])

pushn :: [Word64] -> ThreadOperation ()
pushn val = state $ \t -> let frames1 = frames t
                              operands1 = operands (frames1 !! 0)
                          in ((), Thread 0 [Frame undefined (val ++ operands1) undefined])


load :: Word8 -> ThreadOperation Integer
load idx = state $ \t -> (42, t)

store :: Word8 -> ThreadOperation ()
store idx = state $ \t -> ((), t)



-- Instruction listing

instructions :: Map.Map Word8 (Int, Instruction)
instructions = Map.fromList [

  -- Constants
  (0x00, (0, nop)),
  (0x01, (0, aconst_null)),
  (0x02, (0, iconst_null)),
  (0x03, (0, iconst_null)),
  (0x04, (0, nop)),
  (0x05, (0, nop)),
  (0x06, (0, nop)),
  (0x07, (0, nop)),
  (0x08, (0, nop)),
  (0x09, (0, nop)),
  (0x0a, (0, nop)),
  (0x0b, (0, nop)),
  (0x0c, (0, nop)),
  (0x0d, (0, nop)),
  (0x0e, (0, nop)),
  (0x0f, (0, nop)),
  (0x10, (0, nop)),
  (0x11, (0, nop)),
  (0x12, (0, nop)),
  (0x13, (0, nop)),
  (0x14, (0, nop)),
  
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

nop args = undefined
aconst_null args = undefined
iconst_null args = undefined


-- Stack

pop1 args = do
  remove
pop2 args = do
  remove
  remove
dup args = do
  op <- peek
  push op
dup_x1 args = do
  (op1:op2:_) <- popn
  pushn [op1, op2, op1]
dup_x2 args = do
  (op1:op2:op3:_) <- popn
  pushn [op1, op3, op2, op1]
dup2 args = do
  (op1:op2:_) <- popn
  pushn [op2, op1, op2, op1]
dup2_x1 args = do
  (op1:op2:op3:_) <- popn
  pushn [op2, op1, op3, op2, op1]
dup2_x2 args = do
  (op1:op2:op3:op4:_) <- popn
  pushn [op2, op1, op4, op3, op2, op1]
swap args = do
  (op1:op2:_) <- popn
  pushn [op2, op1]



iadd args = do
  op1 <- pop
  op2 <- pop
  push $ stackElem $ (bytes2Int op1) + (bytes2Int op2)

iload (idx:args) = do
  var <- load idx
  push $ stackElem var

iload_0 args = iload [0]
iload_1 args = iload [1]
iload_2 args = iload [2]
iload_3 args = iload [3]

istore args = undefined
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
