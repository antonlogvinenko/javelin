import Control.Monad.Error (ErrorT(..), runErrorT)
import Control.Monad.State.Lazy (state, State, runState)
import Data.ByteString (ByteString, unpack)
import Data.Word (Word32, Word16, Word8)

parse :: [Word8] -> Either String ClassDef
parse = fst . (runState . runErrorT $ classFileFormat) where
        x --> f = x >>= ErrorT . state . f
        classFileFormat = return EmptyClassDef
                          --> magicNumber --> minorVersion

data ClassDef = EmptyClassDef |
                ClassDef {minVer :: Word16,
                          majVer :: Word16} deriving (Show)

require len bs f = if length bs < len
                   then (Left "Unexpected EOF", bs)
                   else f

magicNumber cd bs = if take 4 bs == [0xCA, 0xFE, 0xBA, 0xBE]
                    then (Right cd, drop 4 bs)
                    else (Left "Not a Java class format", bs)

version bs cdUpd = require 2 bs $
                 let high = bs !! 1
                     low = bs !! 2
                     ver = fromIntegral $ high * 8 + low
                 in (Right $ cdUpd ver, bs)
minorVersion cd bs = version bs $ \x -> cd {minVer = x}
majorVersion cd bs = version bs $ \x -> cd {majVer = x}
                      
main = do
  let c = parse [0xCA, 0xFE, 0xBA, 0xBE, 0x00, 0x00]
  case c of
    Right cd -> putStrLn $ "Parsed ok :)"
    Left msg -> putStrLn $ "Failed with error: " ++ msg
  putStrLn "Complete"


-- parser functions
-- read file
-- test
-- export what?
-- module name, system
