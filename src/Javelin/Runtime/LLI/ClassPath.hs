module Javelin.Runtime.LLI.ClassPath (getClassSourcesLayout, getClassBytes)

where

import Data.ByteString (ByteString)
import Control.Monad (forM)
import Control.Applicative ((<*>), (<$>))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Data.Map.Lazy as Map (Map, fromList)
import Data.List

getClassPathFiles :: FilePath -> IO [FilePath]
getClassPathFiles dir = do
  files <- getRealFiles dir
  -- doing [FilesPaths] -> (FilePath -> IO [FilePath]) -> IO [[FilePath]]
  allFiles <- forM files weNeedToGoDeeper
  return $ concat allFiles

getRealFiles :: FilePath -> IO [FilePath]
getRealFiles dir = map (dir </>) <$> filter (`notElem` [".", ".."]) <$> getDirectoryContents dir

weNeedToGoDeeper :: FilePath -> IO [FilePath]
weNeedToGoDeeper path = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then getClassPathFiles path
    else return [path]

data ClassSource = JarFile { getPath :: FilePath }
                 | ClassFile { getPath :: FilePath }
                 deriving (Show, Eq)

type Layout = Map String ClassSource

getClassSourcesLayout :: FilePath -> IO Layout
getClassSourcesLayout dir = Map.fromList <$>
                            foldl folder [] <$>
                            map getType <$>
                            getClassPathFiles dir

folder :: [(String, ClassSource)] -> Maybe (String, ClassSource) -> [(String, ClassSource)]
folder list (Just x) = x:list
folder list Nothing = list

getType :: FilePath -> Maybe (String, ClassSource)
getType path
  | ".class" `isSuffixOf` path = Just (path, ClassFile path)
  | ".jar" `isSuffixOf` path = Just (path, JarFile path)
  | otherwise = Nothing


getClassBytes :: String -> IO Layout -> IO (Maybe ByteString)
getClassBytes = undefined
