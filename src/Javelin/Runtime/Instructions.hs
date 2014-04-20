module Javelin.Runtime.Instructions
where

import Javelin.Runtime.Thread (Thread(..), Frame)

type Instruction = Thread -> Thread
type FrameInstruction = Frame -> Frame

liftInstr :: FrameInstruction -> Instruction
liftInstr frameInstr thread = let pc1 = pc thread
                                  frames1 = frames thread
                                  frame2 = frameInstr $ head frames1
                                  frames2 = frame2 : tail frames1
                              in Thread pc1 frames2
