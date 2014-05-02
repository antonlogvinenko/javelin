module Javelin.Runtime.Thread
where

import Data.Word (Word8, Word32, Word64)
import Data.Array.IArray (Array, array, (!))
import Javelin.ByteCode.Data (Constant)
import Data.Binary.Put
import Data.Binary.Get (getWord64be, runGet)
import Data.ByteString.Lazy (pack, ByteString)


data Memory = Memory
            deriving (Show, Eq)

type ProgramCounter = Integer
type FrameStack = [Frame]

data Thread = Thread { pc :: ProgramCounter,
                       frames :: FrameStack }
              deriving (Show, Eq)


class BytesContainer c where
  getBytes :: c -> Int -> Int -> Word64



data Arguments = Arguments { argumentsArray :: [Word8] } deriving (Show, Eq)
instance BytesContainer Arguments where
  getBytes c idx len = argumentToWord64 $ take len $ drop idx $ argumentsArray c

argumentToWord64 :: [Word8] -> Word64
argumentToWord64 bs = let normalized = (take (8 - length bs) bs) ++ bs
                   in runGet getWord64be $ pack normalized


data Locals = Locals { vars :: Array Int Word32 } deriving (Show, Eq)
instance BytesContainer Locals where
  getBytes c idx len = let arr = vars c
                       in if len <= 4
                          then localToWord64 [0, arr ! 0]
                          else localToWord64 [arr ! idx, arr ! (idx + 1)]


localToWord64 :: [Word32] -> Word64
localToWord64 bs = runGet getWord64be $ runPut $ putWord32be (bs !! 0) >> putWord32be (bs !! 1)



data StackElement = StackElement { stackElement :: Word64 } deriving (Show, Eq)
instance BytesContainer StackElement where
  getBytes c idx len = stackElement c




type ConstantPool = [Constant]
data Frame = Frame { locals :: Locals,
                     operands :: [StackElement],
                     pool :: ConstantPool }
             deriving (Show, Eq)

