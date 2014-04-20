module Javelin.Runtime.Instructions
where

import Javelin.Runtime.Thread (Thread(..),
                               Frame, Locals, Operands, ConstantPool, ProgramCounter,
                               FrameStack, Memory,
                               pool, operands, locals)

type ThreadInstruction = Memory -> ProgramCounter -> FrameStack -> (Memory, Thread)
type FrameInstruction = Memory -> Locals -> Operands -> ConstantPool -> (Memory, Frame)

inThread :: FrameInstruction -> ThreadInstruction
inThread instr mem pc frames = let frame1 = head frames
                                   (mem2, frame2) = instr mem (locals frame1) (operands frame1) (pool frame1)
                               in (mem2, Thread pc $ frame2 : tail frames)

threadInstructionsMnemonics = [()]
frameInstructionsMnemonics = [("aaload", aaload)]

getInstruction :: String -> ThreadInstruction
getInstruction = undefined
-- find in any array, convert if required


-- Frame Instructions: locals -> stack -> pool -> newFrame

aaload :: FrameInstruction
aaload mem locals stack pool = undefined

aastore :: FrameInstruction
aastore mem locals stack pool = undefined

aconstNull :: FrameInstruction
aconstNull mem locals stack pool = undefined

-- Thread Instrutions
