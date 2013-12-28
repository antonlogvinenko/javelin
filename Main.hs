import Control.Monad.Error (ErrorT(..), runErrorT)
import Control.Monad.State.Lazy (state, State, runState)
import Data.ByteString (pack, ByteString)
import Data.Word (Word32)

parse :: ByteString -> Either String ClassDef
parse = fst . (runState . runErrorT $ classFileFormat) where
        x --> f = x >>= ErrorT . state . f
        classFileFormat = return EmptyClassDef
                          --> increment --> increment --> breaking --> decrement

data ClassDef = EmptyClassDef |
                ClassDef {minorVersion :: Word32,
                          majorVersion :: Word32}

increment count bytes = (Right $ count, bytes)
decrement count bytes = (Right $ count, bytes)
breaking count bytes = (Left "cake!", bytes)

main = do
  let c = parse $ pack [1, 2]
  case c of
    Right v -> putStrLn "cake"
    Left v -> putStrLn "Broken!"
  putStrLn "asd"


-- parser functions
-- read file
-- test
-- export what?
