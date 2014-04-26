module Javelin.Runtime.Thread
where

import Data.Word (Word32, Word64)
import Data.Array.IArray (Array, array)
import Javelin.ByteCode.Data (Constant)

data Memory = Memory
            deriving (Show, Eq)

type ProgramCounter = Integer
type FrameStack = [Frame]

data Thread = Thread { pc :: ProgramCounter,
                       frames :: FrameStack }
              deriving (Show, Eq)


type Locals = Array Integer Word32
type Operands = [Word64]
type ConstantPool = [Constant]
data Frame = Frame { locals :: Locals,
                     operands :: Operands,
                     pool :: ConstantPool }
             deriving (Show, Eq)

