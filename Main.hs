import Control.Monad.Error (ErrorT(..), runErrorT)
import Control.Monad.State.Lazy (state, State, runState)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (take, drop, pack, unpack)
import Data.Word (Word32, Word8)

parse :: ByteString -> Either String ClassDef
parse = fst . (runState . runErrorT $ classFileFormat) where
        x --> f = x >>= ErrorT . state . f
        classFileFormat = return EmptyClassDef
                          --> increment --> magicNumber

data ClassDef = EmptyClassDef |
                ClassDef {minorVersion :: Word32,
                          majorVersion :: Word32}

increment cd bs = (Right $ cd, bs)
breaking cd bs = (Left "cake!", bs)

magicNumber cd bs = if BS.take 4 bs == BS.pack [0xCA, 0xFE, 0xBA, 0xBE]
                    then (Right cd, BS.drop 4 bs)
                    else (Left "Not a Java class format", bs)
                      
main = do
  let c = parse $ BS.pack [0xCA, 0xFE, 0xBA, 0xBE]
  case c of
    Right v -> putStrLn "cake"
    Left v -> putStrLn "Broken!"
  putStrLn "asd"


-- parser functions
-- read file
-- test
-- export what?
-- module name, system
