import Javelin.ByteCode.ClassFile (parse)
import Data.ByteString.Lazy (pack)
import Javelin.ByteCode.Utils
import Data.Binary.Get

bytecode = [0xCA, 0xFE, 0xBA, 0xBE, 0x90, 0x87, 0x90, 0x87, 0x90, 0x87
           ]

-- | JVM entry point
main = do
  let c = parse bytecode
  case c of
    Right cd -> putStrLn $ "Parsed ok :)" ++ (show cd)
    Left msg -> putStrLn $ "Failed with error: " ++ msg
  putStrLn "Complete"


--main = do
--  let input = pack bytecode
--  let trades = runGetOrFail getTrade input
--  case trades of
--    Left x -> print x
--    Right x -> print x
--  print trades

