module Javelin.Main
where

import Javelin.ByteCode.Utils
import Data.Binary.Get
import Javelin.ByteCode.ClassFile (parse)
import Text.Parsec.Error
import qualified Data.ByteString as BS (readFile, unpack)

main = do
  bytestring <- BS.readFile  "Main.class"
  let words = BS.unpack bytestring
  case parse words of
    Right (bs, off, v) -> putStrLn $ show v
    Left (bs, off, v) -> putStrLn $ concat [v, show off, "/", show (length words), "\n",
                                           -- show (take ((fromIntegral off) + 5) words),
                                            "\n",
                                           -- show (words !! ((fromIntegral off) - 1)),
                                            "\n"]
