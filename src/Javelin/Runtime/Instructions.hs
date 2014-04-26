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

instructions :: Map.Map Word8 Instruction
instructions = Map.fromList [(0x60, iadd),
                                  
                                  (0x19, iload), (0x2a, iload_0), (0x2b, iload_1),
                                  (0x2c, iload_2), (0x2d, iload_3),

                                  (0x3a, istore), (0x4b, istore_0), (0x4c, istore_1),
                                  (0x4d, istore_2), (0x4e, istore_3)]

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
