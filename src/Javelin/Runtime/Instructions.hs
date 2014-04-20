module Javelin.Runtime.Instructions
where

import Javelin.Runtime.Thread (Thread(..),
                               Frame, Locals, Operands, ConstantPool, ProgramCounter,
                               FrameStack, Memory,
                               pool, operands, locals)
import Data.Map.Lazy (fromList, Map)
import Data.Word (Word8)



type ThreadInstruction = Memory -> ProgramCounter -> FrameStack -> (Memory, Thread)
type FrameInstruction = Memory -> Locals -> Operands -> ConstantPool -> (Memory, Frame)

threadLift :: FrameInstruction -> ThreadInstruction
threadLift instr mem pc frames = let frame1 = head frames
                                     (mem2, frame2) = instr mem (locals frame1) (operands frame1) (pool frame1)
                                 in (mem2, Thread pc $ frame2 : tail frames)

instructions :: Word8 -> ThreadInstruction
instructions idx = undefined
-- TODO: find in any array, convert if required



-- Frame Instructions
frameInstructions :: Map Word8 FrameInstruction
frameInstructions = fromList [(0x32, aaload), (0x53, aastore), (0x01, aconstNull)]

aaload :: FrameInstruction
aaload mem locals stack pool = undefined

aastore :: FrameInstruction
aastore mem locals stack pool = undefined

aconstNull :: FrameInstruction
aconstNull mem locals stack pool = undefined



-- Thread Instrutions
threadInstructions :: Map Word8 ThreadInstruction
threadInstructions = fromList [(0xba, invokedynamic)]

invokedynamic :: ThreadInstruction
invokedynamic = undefined
