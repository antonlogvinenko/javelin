module Javelin.Runtime.Thread
where

import Data.Word (Word8)
import Javelin.ByteCode.Data (Constant)
  
data Thread = Thread { pc :: Integer,
                       frames :: [Frame] }
              deriving (Show, Eq)

type Locals = [Word8]
type Operands = [Word8]
type ConstantPool = [Constant]
data Frame = Frame { locals :: Locals,
                     operands :: Operands,
                     pool :: ConstantPool }
             deriving (Show, Eq)

