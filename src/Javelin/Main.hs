module Javelin.Main
where

import Javelin.ByteCode.Utils
import Data.Binary.Get
import Javelin.ByteCode.ClassFile (parse)
import Text.Parsec.Error
import qualified Data.ByteString as BS (readFile, unpack, ByteString)
import System.Directory
import System.Environment
import System.IO
import Control.Monad

main0 = do
  bytestring <- BS.readFile  "Main.class"
  let words = BS.unpack bytestring
  case parse words of
    Right (bs, off, v) -> putStrLn $ show v
    Left (bs, off, v) -> putStrLn $ concat [v, show off, "/", show (length words), "\n",
                                           -- show (take ((fromIntegral off) + 5) words),
                                            "\n",
                                           -- show (words !! ((fromIntegral off) - 1)),
                                            "\n"]

main = do
  path <- getArgs
  let len = length path
  if len > 0
    then searchBugs $ path !! 0
    else putStrLn "Not enough arguments"

validate className = case className of
    Right (_, _, _) ->
      " ok"
    Left (bs, off, v) ->
      " failed"

searchBugs :: FilePath -> IO ()
searchBugs path = do
  files <- getDirectoryContents path
  let names = map (path ++) .
              filter (`notElem` [".", ".."]) $
              files
  parsed <- sequence .
            map (liftM parse . liftM BS.unpack . BS.readFile) $
            names
  sequence_ $ (map $ putStrLn . show) . zip names . map validate $ parsed
