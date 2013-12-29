module Javelin.ByteCode (parse) where
  
import Control.Monad.Error (ErrorT(..), runErrorT)
import Control.Monad.State.Lazy (state, State, runState)
import Data.ByteString (ByteString, unpack)
import Data.Word (Word32, Word16, Word8)

parse :: [Word8] -> Either String ClassDef
parse = fst . (runState . runErrorT $ classFileFormat) where
        x --> f = x >>= ErrorT . state . f
        classFileFormat = return EmptyClassDef -->
                          magicNumber --> minorVersion --> majorVersion -->
                          constantPoolCount --> constantPool

data ConstantPoolInfo = ConstantPoolInfo deriving (Show)

data ClassDef = EmptyClassDef |
                ClassDef {minVer :: Word16,
                          majVer :: Word16,
                          constPoolSize :: Word16,
                          constPool :: [ConstantPoolInfo]
                         } deriving (Show)

stub cd bs = (Right cd, bs)
require len bs f = if length bs < len
                   then (Left "Unexpected EOF", bs)
                   else f
upd2bytes bs cdUpd = require 2 bs $
                 let high = bs !! 1
                     low = bs !! 2
                     ver = fromIntegral $ high * 8 + low
                 in (Right $ cdUpd ver, drop 2 bs)
                    
magicNumber cd bs = if take 4 bs == [0xCA, 0xFE, 0xBA, 0xBE]
                    then (Right cd, drop 4 bs)
                    else (Left "Not a Java class format", bs)
minorVersion cd bs = upd2bytes bs $ \v -> cd {minVer = v}
majorVersion cd bs = upd2bytes bs $ \v -> cd {majVer = v}
constantPoolCount cd bs = upd2bytes bs $ \v -> cd {constPoolSize = v}
constantPool = stub



-- use lenses for upd2bytes
-- test - unit, functional (read file)
-- infrastructure: inform where the error happened - EOF when parsing WHAT?

-- hlint
-- parser functions
-- read file

