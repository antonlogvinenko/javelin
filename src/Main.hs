module Main
where

import qualified Javelin.Main as M
import Javelin.Runtime.LLI.ClassPath


main :: IO ()
main = do
--  files <- getClassSourcesLayout "/Users/anton/dev/haskell/javelin/acceptance"
           --"/Library/Java/JavaVirtualMachines/jdk1.8.0.jdk/Contents/Home/jre/lib/"
--  print files
  putStrLn "Running from outside of the box"
  M.main
