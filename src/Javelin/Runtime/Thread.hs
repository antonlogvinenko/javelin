module Javelin.Runtime.Thread
where

import Data.Word (Word8)
import Javelin.ByteCode.Data (Constant)
  
data Thread = Thread { pc :: Integer,
                       frames :: [Frame] }
              deriving (Show, Eq)

data Frame = Frame { locals :: [Word8],
                     operands :: [Word8],
                     pool :: [Constant] }
             deriving (Show, Eq)

