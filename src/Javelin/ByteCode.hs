module Javelin.ByteCode (parse,
                         require, upd2bytes, stub,
                         magicNumber, minorVersion, majorVersion,
                         constantPoolCount, constantPool,
                         ClassDef(..))
where

import Control.Monad.Error (ErrorT(..), runErrorT)
import Control.Monad.State.Lazy (state, State, runState)
import Data.ByteString (ByteString, unpack)
import Data.Word (Word32, Word16, Word8)

type ParsedByteCode = Either String ClassDef
    
-- | Transforms Java class bytecode into either error message or a class definition.
parse :: [Word8] -> ParsedByteCode
parse = fst . (runState . runErrorT $ classFileFormat) where
    x --> f = x >>= ErrorT . state . f
    classFileFormat = return EmptyClassDef -->
                      magicNumber --> minorVersion --> majorVersion -->
                      constantPoolCount --> constantPool

data ConstantPoolInfo = ConstantPoolInfo deriving (Show, Eq)

-- | Represents Java class file structure
data ClassDef = EmptyClassDef |
                ClassDef {minVer :: Word16,
                          majVer :: Word16,
                          constPoolSize :: Word16,
                          constPool :: [ConstantPoolInfo]
                         } deriving (Show, Eq)

stub cd bs = (Right cd, bs)

require :: Int -> [Word8] -> (ParsedByteCode, [Word8]) -> (ParsedByteCode, [Word8])
require len bs f = if length bs < len
                   then (Left "Unexpected EOF", bs)
                   else f
upd2bytes bs cdUpd = require 2 bs $
                     let high = bs !! 0
                         low = bs !! 1
                         ver = fromIntegral $ high * 256 + low
                     in (Right $ cdUpd ver, drop 2 bs)

magicNumber cd bs = if take 4 bs == [0xCA, 0xFE, 0xBA, 0xBE]
                    then (Right cd, drop 4 bs)
                    else (Left "Not a Java class format", bs)
minorVersion cd bs = upd2bytes bs $ \v -> cd {minVer = v}
majorVersion cd bs = upd2bytes bs $ \v -> cd {majVer = v}
constantPoolCount cd bs = upd2bytes bs $ \v -> cd {constPoolSize = v}
constantPool = stub


               
