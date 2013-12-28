import Control.Monad.Error (ErrorT(..), runErrorT)
import Control.Monad.State.Lazy (state, State, runState)
import Data.ByteString (pack, ByteString)
import Data.Word (Word32)

type ParseFun = ClassDef -> ByteString -> ((Either String ClassDef), ByteString)
type Parser = ErrorT String (State ByteString) ClassDef

(-->) :: Parser -> ParseFun -> Parser
x --> f = x >>= ErrorT . state . f

countAll :: ParseFun
countAll init = runState $ runErrorT $ classFileFormat where
                classFileFormat = return init
                                  --> increment --> increment --> breaking --> decrement

data ClassDef = EmptyClassDef |
                ClassDef {minorVersion :: Word32,
                          majorVersion :: Word32}


-- the following are all ParseFun functions:
increment count bytes = (Right $ count, bytes)
decrement count bytes = (Right $ count, bytes)
breaking count bytes = (Left "cake!", bytes)


main = do
  let c = fst $ countAll EmptyClassDef $ pack [1, 2]
  case c of
    Right v -> putStrLn "cake"
    Left v -> putStrLn "Broken!"
  putStrLn "asd"


-- ClassDefinition type
-- parser functions
-- read file
-- test
-- export what?
