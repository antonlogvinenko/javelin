module Javelin.Interpreter.ClassPathLoading
  ( ClassPathLoading
  , getClassSourcesLayout
  , getClassBytes
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import qualified Codec.Archive.Zip as Zip
import qualified Data.List.Split as Split
import qualified Data.String.Utils as String
import Control.Monad.IO.Class (liftIO)
import Javelin.Interpreter.Logging()

import Data.Function ((&))

import Javelin.Lib.Structures
import Javelin.Capability.Classes
import Javelin.Interpreter.JVMApp

instance ClassPathLoading JVM where
  getClassSourcesLayout paths =
    let pathsList = String.strip <$> Split.splitOn ":" paths
     in do
      layout <- getClassPathLayout pathsList
      return $ ClassPathLayout layout pathsList

getClassPathLayout :: [FilePath] -> JVM (Map.Map ClassName ClassSource)
getClassPathLayout paths = Map.unions <$> mapM getClassPathElementLayout paths

getClassPathElementLayout :: FilePath -> JVM (Map.Map ClassName ClassSource)
getClassPathElementLayout path
  | ".class" `List.isSuffixOf` path = extractFileClass path
  | any (\s -> List.isSuffixOf s path) [".jar", ".zip", ".war", ".ear"] = id $! extractZipClasses path
  | otherwise = do
    isDirectory <- liftIO $ Dir.doesDirectoryExist path
    if not isDirectory
      then return Map.empty
      else (getFilesInDirectory path) >>= getClassPathLayout

extractFileClass :: FilePath -> JVM (Map.Map ClassName ClassSource)
extractFileClass path =
  --Convert "main/test/App.class" to expected class path "test/App.class"
  let filePathToClassPath :: String -> String
      filePathToClassPath path = path & Split.splitOn "/" & drop 1 & List.intercalate "/"
  in
  return
    (Map.fromList [(pathToClass (filePathToClassPath path), ClassFile path)])

extractZipClasses :: FilePath -> JVM (Map.Map ClassName ClassSource)
extractZipClasses path = liftIO $ do
  raw <- BSL.readFile path
  let arc = Zip.toArchive raw
      allFiles = Zip.filesInArchive arc :: [FilePath]
      bytes = Map.fromList $ map (\f -> (f, pfft arc f)) allFiles :: Map.Map FilePath (Maybe BSS.ByteString)
      s = JarFile path ((Map.!) bytes)
  return $ Map.fromList $ (\c -> (c, s)) <$> pathToClass <$> allFiles

pfft :: Zip.Archive -> FilePath -> Maybe BSS.ByteString
pfft arc classPath = do
  entry <- Zip.findEntryByPath classPath arc
  return $ BSL.toStrict $ Zip.fromEntry entry

getFilesInDirectory :: FilePath -> JVM [FilePath]
getFilesInDirectory path = do
  isDirectory <- liftIO $ Dir.doesDirectoryExist path
  if not isDirectory
    then return [path]
    else do
      files <-
        map ((FilePath.</>) path) <$> filter (`notElem` [".", ".."]) <$>
        (liftIO $ Dir.getDirectoryContents path)
      allFiles <- mapM getFilesInDirectory files :: JVM [[FilePath]]
      return $ concat allFiles

--Convert "com/util/java/List.class" to "com/util/java/List"
--Can be used to transalte class file pathes (inside jar files or on filesystem) to expected class name
pathToClass :: FilePath -> ClassName
pathToClass path = path & Split.splitOn ".class" & head