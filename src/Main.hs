module Main
where

import Javelin.ByteCode.DescSign
import Javelin.ByteCode.ClassFile (parse)
import qualified Data.ByteString as BS (readFile, unpack)
import System.Directory
import System.Environment
import Control.Monad
import Javelin.Runtime.Thread (runJVM)
import Javelin.ByteCode.Stats (stats)

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

disasmClass path = do
  bytestring <- BS.readFile path
  let words = BS.unpack bytestring
  case parse words of
    Right (_, _, v) -> print v
    Left (_, off, v) -> putStrLn $ concat [v, show off, "/", show (length words), "\n",
                                            "\n",
                                            "\n"]

runFunction arg = print $ parseClassSignature arg

printHelp = putStrLn "Specify mode: [disasm|stats|f|cs|jvm] for function/class/classes and mode argument"
  
main = do
  args <- getArgs
  if length args < 2
    then printHelp
    else let (arg0:arg1:restArgs) = args
         in case arg0 of
           "f" -> runFunction arg1
           "disasm" -> disasmClass arg1
           "cs" -> runClasses arg1
           "jvm" -> let (classPath:mainArgs) = restArgs
                    in do
                      traces <- runJVM classPath arg1 mainArgs
                      print traces
           "stats" -> let outputFile = if (length restArgs > 0)
                                       then Just $ restArgs !! 0
                                       else Nothing
                      in stats arg1 outputFile
           _ -> (putStrLn $ arg0 ++ "is an unknown command") >> printHelp
