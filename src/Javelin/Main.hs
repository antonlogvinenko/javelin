module Javelin.Main
where

import Javelin.ByteCode.Utils
import Javelin.ByteCode.DescSign
import Data.Binary.Get
import Javelin.ByteCode.ClassFile (parse)
import Text.Parsec.Error
import qualified Data.ByteString as BS (readFile, unpack, ByteString)
import System.Directory
import System.Environment
import System.IO
import Control.Monad

main0 = do
  path <- getArgs
  bytestring <- BS.readFile (concat path)
  let words = BS.unpack bytestring
  case parse words of
    Right (bs, off, v) -> putStrLn $ show v
    Left (bs, off, v) -> putStrLn $ concat [v, show off, "/", show (length words), "\n",
                                           -- show (take ((fromIntegral off) + 5) words),
                                            "\n",
                                           -- show (words !! ((fromIntegral off) - 1)),
                                            "\n"]

main1 = do
  path <- getArgs
  let len = length path
  if len > 0
    then searchBugs $ path !! 0
    else putStrLn "Not enough arguments"

-- (IDLjava/lang/Thread;)Ljava/lang/Object;
main = do
  a <- getArgs
  putStrLn $ show $ parseX $ a !! 0

validate className = case className of
    Right _ -> True
    Left _ -> False

searchBugs :: FilePath -> IO ()
searchBugs path = do
  files <- getDirectoryContents path
  let names = map (path ++) .
              filter (`notElem` [".", ".."]) $
              files
  parsed <- sequence .
            map (liftM validate . liftM parse . liftM BS.unpack . BS.readFile) $
            names
  sequence_ $ (map $ putStrLn . show) . zip names  $ parsed
  putStrLn $ ("All files passed: " ++) . show . foldl (&&) True $ parsed
