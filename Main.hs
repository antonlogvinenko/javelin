import Control.Monad.Error (ErrorT(..), runErrorT)
import Control.Monad.State.Lazy (state, State, runState)
import Data.ByteString (pack, ByteString)

type ParseFun = Integer -> ByteString -> ((Either String Integer), ByteString)
type Parser = ErrorT String (State ByteString) Integer

(-->) :: Parser -> ParseFun -> Parser
x --> f = x >>= ErrorT . state . f

countAll :: ParseFun
countAll init = runState $ runErrorT $ classFileFormat where
                classFileFormat = return init
                                  --> increment --> increment --> breaking --> decrement

-- the following are all ParseFun functions:
increment count bytes = (Right $ count + 1, bytes)
decrement count bytes = (Right $ count - 1, bytes)
breaking count bytes = (Left "cake!", bytes)

main = do
  let c = fst $ countAll 0 $ pack [1, 2]
  case c of
    Right v -> putStrLn "cake"
    Left v -> putStrLn "Broken!"
  putStrLn "asd"
