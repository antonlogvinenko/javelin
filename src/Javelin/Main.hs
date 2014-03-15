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

validate className = case className of
    Right _ -> True
    Left _ -> False

runClasses :: FilePath -> IO ()
runClasses path = do
  files <- getDirectoryContents path
  let names = map (path ++) .
              filter (`notElem` [".", ".."]) $
              files
  parsed <- sequence .
            map (liftM validate . liftM parse . liftM BS.unpack . BS.readFile) $
            names
  sequence_ $ (map $ putStrLn . show) . zip names  $ parsed
  putStrLn $ ("All files passed: " ++) . show . foldl (&&) True $ parsed

runClass path = do
  bytestring <- BS.readFile path
  let words = BS.unpack bytestring
  case parse words of
    Right (bs, off, v) -> putStrLn $ show v
    Left (bs, off, v) -> putStrLn $ concat [v, show off, "/", show (length words), "\n",
                                           -- show (take ((fromIntegral off) + 5) words),
                                            "\n",
                                           -- show (words !! ((fromIntegral off) - 1)),
                                            "\n"]

runFunction arg = do
  putStrLn $ show $ parseClassSignature arg

main = do
  args <- getArgs
  if length args < 2
    then putStrLn "Specify running mode: [f|c|cs] for function/class/classes and mode argument"
    else let arg0 = args !! 0
             arg1 = args !! 1
         in case arg0 of
           "f" -> runFunction arg1
           "c" -> runClass arg1
           "cs" -> runClasses arg1
