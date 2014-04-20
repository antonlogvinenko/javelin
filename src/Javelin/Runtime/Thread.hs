module Javelin.Runtime.Structure.Thread
where

import Data.Word (Word8)
  
data Thread = Thread { pc :: Integer,
                       stack :: [Frame] }
              deriving (Show, Eq)

data Frame = Frame { locals :: [Word8],
                     stack :: [Word8],
                     pool :: [Constant] }
             deriving (Show, Eq)

