import Javelin.ByteCode (parse)

bytecode = [0xCA, 0xFE, 0xBA, 0xBE,
            0x00, 0x00,
            0x00
           ]

main = do
  let c = parse bytecode
  case c of
    Right cd -> putStrLn $ "Parsed ok :)"
    Left msg -> putStrLn $ "Failed with error: " ++ msg
  putStrLn "Complete"
