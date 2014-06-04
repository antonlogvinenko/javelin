module Javelin.Runtime.LLI.ClassPath

where

import Control.Monad (forM)
import Control.Applicative ((<*>), (<$>))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))

getClassPathFiles :: FilePath -> IO [FilePath]
getClassPathFiles dir = do
  files <- getRealFiles dir
  -- doing [FilesPaths] -> (FilePath -> IO [FilePath]) -> IO [[FilePath]]
  allFiles <- forM files weNeedToGoDeeper
  return $ concat allFiles

getRealFiles :: FilePath -> IO [FilePath]
getRealFiles dir = do
  map (dir </>) <$> filter (`notElem` [".", ".."]) <$> getDirectoryContents dir

weNeedToGoDeeper :: FilePath -> IO [FilePath]
weNeedToGoDeeper path = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then getClassPathFiles path
    else return [path]
  
  
