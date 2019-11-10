module Javelin.Interpreter.ClassPathLoading
  ( ClassPathLoading
  , getClassSourcesLayout
  , getClassBytes
  ) where

import           Data.ByteString            as BSS (ByteString)
import           Data.ByteString.Lazy       as BSL (ByteString, readFile,
                                                    toStrict)
import           Data.List
import           Data.Map                   as Map (Map, empty, fromList,
                                                    lookup, unions)
import           System.Directory           (doesDirectoryExist,
                                             getDirectoryContents)
import           System.FilePath            ((</>))

import           Codec.Archive.Zip
import           Data.Either.Utils          (maybeToEither)
import           Data.List.Split            (splitOn)
import           Data.String.Utils          (strip)

import           Javelin.Lib.Structures

import           Control.Applicative        ((<$>))
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Flow
import           Javelin.Interpreter.JVMApp
import           Javelin.Capability.Classes


instance ClassPathLoading JVM where
  getClassSourcesLayout paths = 
    let pathsList = strip <$> splitOn ":" paths
    in liftIO $ do
      layout <- getClassPathLayout pathsList
      return $ ClassPathLayout layout pathsList


getClassPathLayout :: [FilePath] -> IO (Map ClassName ClassSource)
getClassPathLayout paths = unions <$> mapM getClassPathElementLayout paths

getClassPathElementLayout :: FilePath -> IO (Map ClassName ClassSource)
getClassPathElementLayout path
  | ".class" `isSuffixOf` path = extractFileClass path
  | isZip path = extractZipClasses path
  | otherwise = do
    isDirectory <- doesDirectoryExist path
    if not isDirectory
      then return Map.empty
      else getFilesInClassPath path >>= getClassPathLayout

getFilesInClassPath :: FilePath -> IO [FilePath]
getFilesInClassPath path = do
  isDirectory <- doesDirectoryExist path
  if not isDirectory
    then return [path]
    else do
      files <-
        map (path </>) <$> filter (`notElem` [".", ".."]) <$>
        getDirectoryContents path
      allFiles <- mapM getFilesInClassPath files :: IO [[FilePath]]
      return $ concat allFiles

zipContentFold :: [ClassSource] -> FilePath -> [ClassSource]
zipContentFold list path
  | isSuffixOf ".class" path = ClassFile path : list
  | isZip path = JarFile path : list
  | otherwise = list

extractZipClasses :: FilePath -> IO (Map ClassName ClassSource)
extractZipClasses path = do
  raw <- BSL.readFile path
  let arc = toArchive raw
      allFiles = filesInArchive arc
      paths = (map getPath $ foldl zipContentFold [] allFiles) :: [FilePath]
      s = JarFile path
  return $ Map.fromList $ (\c -> (c, s)) <$> pathToClass <$> paths

extractFileClass :: FilePath -> IO (Map ClassName ClassSource)
extractFileClass path = return (Map.fromList [(pathToClass (filePathToClassPath path), ClassFile path)])

--Convert "main/test/App.class" to expected class path "test/App.class"
filePathToClassPath :: String -> String
filePathToClassPath path = path |> splitOn "/" |> drop 1 |> intercalate "/"

--Convert "com/util/java/List.class" to "com/util/java/List"
--Can be used to transalte class file pathes (inside jar files or on filesystem) to expected class name
pathToClass :: FilePath -> ClassName
pathToClass path = path |> splitOn ".class" |> head

isZip :: FilePath -> Bool
isZip path = any (\s -> isSuffixOf s path) [".jar", ".zip", ".war", ".ear"]