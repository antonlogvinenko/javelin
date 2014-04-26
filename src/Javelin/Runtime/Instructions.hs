module Javelin.Runtime.Instructions
where

import Control.Monad.State.Lazy (State, state)
import Javelin.Runtime.Thread (Thread(..),
                               Frame(..), Locals, Operands, ConstantPool, ProgramCounter,
                               FrameStack, Memory,
                               pool, operands, locals)
import qualified Data.Map.Lazy as Map (fromList, Map(..), union, map, (!))
import Data.Array.IArray (Array, (//), (!))
import Data.Word (Word8, Word32, Word64)



type ThreadInstruction = Memory -> ProgramCounter -> FrameStack -> (Memory, Thread)
type FrameInstruction = Memory -> Locals -> Operands -> ConstantPool -> (Memory, Frame)

threadLift :: FrameInstruction -> ThreadInstruction
threadLift instr mem pc frames = let frame1 = head frames
                                     (mem2, frame2) = instr mem (locals frame1) (operands frame1) (pool frame1)
                                 in (mem2, Thread pc $ frame2 : tail frames)

instructions :: Map.Map Word8 ThreadInstruction
instructions = Map.union threadInstructions $ Map.map threadLift frameInstructions

instruction :: Word8 -> ThreadInstruction
instruction opcode = instructions Map.! opcode

type Instruction a = State Thread a

popElem :: Instruction Word64
popElem = state $ \t -> let frames1 = frames t
                            operands1 = operands $ frames1 !! 0
                        in (operands1 !! 0, t)

pushElem :: Word64 -> Instruction ()
pushElem val = state $ \t -> let frames1 = frames t
                                 operands1 = operands (frames1 !! 0)
                             in ((), Thread 0 [Frame undefined (val:operands1) undefined])

addOp :: State Thread ()
addOp = do
  op1 <- popElem
  op2 <- popElem
  pushElem $ stackElem $ (bytes2Int op1) + (bytes2Int op2)
  


-- Frame Instructions
frameInstructions :: Map.Map Word8 FrameInstruction
frameInstructions = Map.fromList [(0x32, aaload), (0x53, aastore), (0x01, aconstNull),

                                  (0x19, iload), (0x2a, iload_0), (0x2b, iload_1),
                                  (0x2c, iload_2), (0x2d, iload_3),

                                  (0x3a, istore), (0x4b, istore_0), (0x4c, istore_1),
                                  (0x4d, istore_2), (0x4e, istore_3),

                                  (0x93, i2s), (0x60, iadd)]

aaload mem locals stack pool = undefined
aastore mem locals stack pool = undefined
aconstNull mem locals stack pool = undefined
iadd mem locals (op1:op2:stack) pool = let sum = stackElem $ (bytes2Int op1) + (bytes2Int op2)
                                       in (mem, Frame locals (sum:stack) pool)
i2s mem locals (op:stack) pool = let intOp = stackElem . bytes2Short $ op
                                 in (mem, Frame locals (intOp:stack) pool)

iload = undefined
iload_0 = undefined
iload_1 = undefined
iload_2 = undefined
iload_3 = undefined

istore = undefined
istore_0 mem locals (op:stack) pool = let locals2 = locals // [(0, localsElem $ bytes2Int $ op)]
                                      in (mem, Frame locals2 stack pool)
istore_1 = undefined
istore_2 = undefined
istore_3 = undefined


bytes2Int :: Word64 -> Integer
bytes2Int = fromIntegral

bytes2Short :: Word64 -> Integer
bytes2Short = fromIntegral

localsElem :: Integer -> Word32
localsElem = fromIntegral

stackElem :: Integer -> Word64
stackElem = fromIntegral

-- Thread Instrutions
threadInstructions :: Map.Map Word8 ThreadInstruction
threadInstructions = Map.fromList [(0xba, invokedynamic)]

invokedynamic :: ThreadInstruction
invokedynamic mem pc frames = undefined
