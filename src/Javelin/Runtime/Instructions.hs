module Javelin.Runtime.Instructions
where

import Control.Monad.State.Lazy (State, state)
import Javelin.Runtime.Thread (Thread(..),
                               Frame(..), Locals, Operands, ConstantPool, ProgramCounter,
                               FrameStack, Memory,
                               pool, operands, locals)
import qualified Data.Map.Lazy as Map (fromList, Map(..), union, map, (!))
import Data.Word (Word8, Word32, Word64)



type ThreadOperation a = State Thread a
type Instruction = [Word8] -> ThreadOperation ()

pop :: ThreadOperation Word64
pop = state $ \t -> let frames1 = frames t
                        operands1 = operands $ frames1 !! 0
                    in (operands1 !! 0, t)

push :: Word64 -> ThreadOperation ()
push val = state $ \t -> let frames1 = frames t
                             operands1 = operands (frames1 !! 0)
                         in ((), Thread 0 [Frame undefined (val:operands1) undefined])

load :: Word8 -> ThreadOperation Integer
load idx = state $ \t -> (42, t)

store :: Word8 -> ThreadOperation ()
store idx = state $ \t -> ((), t)

same = state $ \t -> ((), t)

instructions :: Map.Map Word8 (Int, Instruction)
instructions = Map.fromList [

  -- Constants
  (0x00, (0, nop)), (0x01, (0, aconst_null)), (0x02, (0, iconst_null)), (0x03, (0, iconst_null)),
  (0x04, (0, nop)), (0x05, (0, nop)), (0x06, (0, nop)), (0x07, (0, nop)),
  (0x08, (0, nop)), (0x09, (0, nop)), (0x0a, (0, nop)), (0x0b, (0, nop)),
  (0x0c, (0, nop)), (0x0d, (0, nop)), (0x0e, (0, nop)), (0x0f, (0, nop)),
  (0x10, (0, nop)), (0x11, (0, nop)), (0x12, (0, nop)), (0x13, (0, nop)),
  (0x14, (0, nop)),
  
  -- Loads

  -- Stores

  -- Stack

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
               
-- Constants
nop args = undefined
aconst_null args = undefined
iconst_null args = undefined


popElem :: Instruction
popElem args = do
  a <- pop
  same

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
