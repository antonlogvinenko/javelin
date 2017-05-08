module Main
where

import Javelin.ByteCode.ClassFile (parseRaw)
import qualified Data.ByteString as BS (readFile, unpack)
import System.Directory
import System.Environment
import Control.Monad
import Javelin.Runtime.Thread (runJVM)
import Javelin.ByteCode.Stats (stats)
import Javelin.ByteCode.Data (showByteCode)
import Control.Monad.Trans.Maybe (runMaybeT)
import Javelin.Runtime.LLI.ClassPath
import Javelin.Runtime.Structures
import Javelin.Runtime.LLI.Loading (load)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

validate className = case className of
    Right _ -> True
    Left _ -> False

testClasses :: FilePath -> IO ([String], Bool)
testClasses path = do
  files <- getDirectoryContents path
  let names = map (path ++) .
              filter (`notElem` [".", ".."]) $
              files
  parsed <- mapM (liftM (validate . parseRaw. BS.unpack) . BS.readFile) names
  return (zipWith (curry show) names parsed, and parsed)

runClasses :: FilePath -> IO ()
runClasses path = do
  (io, result) <- testClasses path
  mapM_ print io
  putStrLn $ ("All files passed: " ++) . show $ result

disasmClass opt path = do
  bytestring <- BS.readFile path
  let words = BS.unpack bytestring
  case parseRaw words of
    Right (_, _, v) -> putStrLn $ showByteCode opt v
    Left (_, off, v) -> putStrLn $ "Failed to parse file " ++ path ++ ". Offset " ++ show off

disasmClass2 opt bs = 
  let words = BS.unpack bs
  in case parseRaw words of
    Right (_, _, v) -> putStrLn $ showByteCode opt v
    Left (_, off, v) -> putStrLn $ "Failed to parse file"

printHelp = putStrLn "Specify mode: [disasm|disasmFull|stats|classpath|loadClass|cs|jvm] for function/class/classes and mode argument"

cake :: FilePath -> String -> ExceptT VMError IO Runtime
cake classPath className = do
  layout <- getClassSourcesLayout classPath
  let rt = newRuntime layout
  classBytes <- getClassBytes className layout
  load (ClassId BootstrapClassLoader className) rt

main = do
  args <- getArgs
  if length args < 2
    then printHelp
    else let (arg0:arg1:restArgs) = args
         in case arg0 of
           "disasm" -> disasmClass False arg1
           "disasmFull" -> disasmClass True arg1
           "stats" -> let outputFile = if (length restArgs > 0)
                                       then Just $ restArgs !! 0
                                       else Nothing
                      in stats arg1 outputFile
           "classpath" -> let classPath = arg1
                              (className:_) = restArgs
                              bb = runExceptT $ do
                                layout <- getClassSourcesLayout classPath
                                classBytes <- getClassBytes className layout
                                lift $ disasmClass2 False classBytes
                          in do
                            b <- bb
                            print b
           "loadClass" -> let classPath = arg1
                              (className:_) = restArgs
                              bb = runExceptT $ cake classPath className
                          in do
                            b <- bb
                            print b
           "cs" -> runClasses arg1
           "jvm" -> let (classPath:mainArgs) = restArgs
                    in do
                      traces <- runJVM classPath arg1 mainArgs
                      print traces
           _ -> (putStrLn $ arg0 ++ "is an unknown command") >> printHelp
