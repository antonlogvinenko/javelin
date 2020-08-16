module Javelin.Interpreter.ClassPathLoading
  ( ClassPathLoading
  , getClassSourcesLayout
  , getClassBytes
  ) where

import qualified Data.ByteString.Lazy as BSL (readFile)
import qualified Data.List as List
import qualified Data.Map as Map (Map, empty, fromList, unions)
import qualified System.Directory as Dir (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

import qualified Codec.Archive.Zip as Zip
import Data.List.Split (splitOn)
import Data.String.Utils (strip)
import Data.Function

import Javelin.Lib.Structures

import Control.Monad.Trans
import Javelin.Capability.Classes
import Javelin.Interpreter.JVMApp

instance ClassPathLoading JVM where
  getClassSourcesLayout paths =
    let pathsList = strip <$> splitOn ":" paths
     in liftIO $ do
          layout <- getClassPathLayout pathsList
          return $ ClassPathLayout layout pathsList

getClassPathLayout :: [FilePath] -> IO (Map.Map ClassName ClassSource)
getClassPathLayout paths = Map.unions <$> mapM getClassPathElementLayout paths

getClassPathElementLayout :: FilePath -> IO (Map.Map ClassName ClassSource)
getClassPathElementLayout path
  | ".class" `List.isSuffixOf` path = extractFileClass path
  | isZip path = extractZipClasses path
  | otherwise = do
    isDirectory <- Dir.doesDirectoryExist path
    if not isDirectory
      then return Map.empty
      else getFilesInClassPath path >>= getClassPathLayout

getFilesInClassPath :: FilePath -> IO [FilePath]
getFilesInClassPath path = do
  isDirectory <- Dir.doesDirectoryExist path
  if not isDirectory
    then return [path]
    else do
      files <-
        map (path </>) <$> filter (`notElem` [".", ".."]) <$>
        Dir.getDirectoryContents path
      allFiles <- mapM getFilesInClassPath files :: IO [[FilePath]]
      return $ concat allFiles

zipContentFold :: [ClassSource] -> FilePath -> [ClassSource]
zipContentFold list path
  | List.isSuffixOf ".class" path = ClassFile path : list
  | isZip path = JarFile path : list
  | otherwise = list

extractZipClasses :: FilePath -> IO (Map.Map ClassName ClassSource)
extractZipClasses path = do
  raw <- BSL.readFile path
  let arc = Zip.toArchive raw
      allFiles = Zip.filesInArchive arc
      paths = (map getPath $ foldl zipContentFold [] allFiles) :: [FilePath]
      s = JarFile path
  return $ Map.fromList $ (\c -> (c, s)) <$> pathToClass <$> paths

extractFileClass :: FilePath -> IO (Map.Map ClassName ClassSource)
extractFileClass path =
  return
    (Map.fromList [(pathToClass (filePathToClassPath path), ClassFile path)])

--Convert "main/test/App.class" to expected class path "test/App.class"
filePathToClassPath :: String -> String
filePathToClassPath path = path & splitOn "/" & drop 1 & List.intercalate "/"

--Convert "com/util/java/List.class" to "com/util/java/List"
--Can be used to transalte class file pathes (inside jar files or on filesystem) to expected class name
pathToClass :: FilePath -> ClassName
pathToClass path = path & splitOn ".class" & head

isZip :: FilePath -> Bool
isZip path = any (\s -> List.isSuffixOf s path) [".jar", ".zip", ".war", ".ear"]
