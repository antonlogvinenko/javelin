module Javelin.Runtime.Thread
where

import Data.Word (Word8)
import Javelin.ByteCode.Data (Constant)

data Memory = Memory
            deriving (Show, Eq)

type ProgramCounter = Integer
type FrameStack = [Frame]

data Thread = Thread { pc :: ProgramCounter,
                       frames :: FrameStack }
              deriving (Show, Eq)

type Locals = [Word8]
type Operands = [Word8]
type ConstantPool = [Constant]
data Frame = Frame { locals :: Locals,
                     operands :: Operands,
                     pool :: ConstantPool }
             deriving (Show, Eq)

