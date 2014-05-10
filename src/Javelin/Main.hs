module Javelin.Main
where

import Javelin.ByteCode.DescSign
import Javelin.ByteCode.ClassFile (parse)
import qualified Data.ByteString as BS (readFile, unpack)
import System.Directory
import System.Environment
import Control.Monad

validate className = case className of
    Right _ -> True
    Left _ -> False

testClasses :: FilePath -> IO ([String], Bool)
testClasses path = do
  files <- getDirectoryContents path
  let names = map (path ++) .
              filter (`notElem` [".", ".."]) $
              files
  parsed <- mapM (liftM (validate . parse . BS.unpack) . BS.readFile) names
  return (zipWith (curry show) names parsed, and parsed)

runClasses :: FilePath -> IO ()
runClasses path = do
  (io, result) <- testClasses path
  mapM_ print io
  putStrLn $ ("All files passed: " ++) . show $ result

runClass path = do
  bytestring <- BS.readFile path
  let words = BS.unpack bytestring
  case parse words of
    Right (_, _, v) -> print v
    Left (_, off, v) -> putStrLn $ concat [v, show off, "/", show (length words), "\n",
                                           -- show (take ((fromIntegral off) + 5) words),
                                            "\n",
                                           -- show (words !! ((fromIntegral off) - 1)),
                                            "\n"]

runFunction arg = do
  print $ parseClassSignature arg

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
