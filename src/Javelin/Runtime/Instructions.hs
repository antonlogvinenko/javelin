module Javelin.Runtime.Instructions
where

import Javelin.Runtime.Thread (Thread(..), Frame, Locals, Operands, ConstantPool, pool, operands, locals)

type ThreadInstruction = Thread -> Thread
type FrameInstruction = Locals -> Operands -> ConstantPool -> Frame

inThread :: FrameInstruction -> ThreadInstruction
inThread frameInstr thread = let pc1 = pc thread
                                 frames1 = frames thread
                                 frame1 = head frames1
                                 frame2 = frameInstr (locals frame1) (operands frame1) (pool frame1)
                                 frames2 = frame2 : tail frames1
                             in Thread pc1 frames2

threadInstructionsMnemonics = [()]
frameInstructionsMnemonics = [("aaload", aaload)]

getInstruction :: String -> ThreadInstruction
getInstruction = undefined
-- find in any array, convert if required


-- Frame Instructions: locals -> stack -> pool -> newFrame

aaload :: FrameInstruction
aaload locals stack pool = undefined

aastore :: FrameInstruction
aastore locals stack pool = undefined

aconstNull :: FrameInstruction
aconstNull locals stack pool = undefined

-- Thread Instrutions
