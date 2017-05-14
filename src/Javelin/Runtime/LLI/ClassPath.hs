module Javelin.Runtime.LLI.ClassPath (getClassSourcesLayout, getClassBytes)

where

import Data.ByteString.Lazy as BSL (ByteString, toStrict, readFile)
import Data.ByteString as BSS (ByteString)
import Control.Monad (forM)
import Control.Arrow ((>>>))
import Control.Applicative ((<$>))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Data.Map.Lazy as Map (Map, fromList, lookup, unions, empty)
import Data.List

import Control.Monad.Trans

import Data.List.Split
import Codec.Archive.Zip
import Data.Either.Utils (maybeToEither)

import Javelin.Runtime.Structures
import Data.List.Split (splitOn)
import Javelin.Util

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class


-- Getting layout for classpath
getClassSourcesLayout :: String -> ExceptT VMError IO ClassPathLayout
getClassSourcesLayout paths =
  let pathsList = splitOn ";" paths
      layoutList = sequence $ map getClassPathElementLayout pathsList :: IO [Map ClassName ClassSource]
  in lift $ do
    layout <- unions <$> layoutList
    return $ ClassPathLayout layout pathsList

zipSuffixes = [".jar", ".zip", ".war", ".ear"]
isZip :: FilePath -> Bool
isZip path = any (\s -> isSuffixOf s path) zipSuffixes

getClassPathElementLayout :: FilePath -> IO (Map ClassName ClassSource)
getClassPathElementLayout path
  | ".class" `isSuffixOf` path = extractFileClass path
  | isZip path = extractZipClasses path
  | otherwise = do
    isDirectory <- doesDirectoryExist path
    if not isDirectory
      then return Map.empty
      else do
        pathes <- getClassPathFiles path :: IO [FilePath]
        let layouts = mapM getClassPathElementLayout pathes :: IO [Map ClassName ClassSource]
        unions <$> layouts
                
getClassPathFiles :: FilePath -> IO [FilePath]
getClassPathFiles path = do
  isDirectory <- doesDirectoryExist path
  if not isDirectory
    then return [path]
    else do
         files <- map (path </>) <$> filter (`notElem` [".", ".."]) <$> getDirectoryContents path
         allFiles <- mapM getClassPathFiles files :: IO [[FilePath]]
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
  let allFiles = filesInArchive arc
  let paths = map getPath $ foldl zipContentFold [] allFiles
  let s = JarFile path
  map pathToClass >>> map (\c -> (c, s)) >>> Map.fromList >>> return $ paths

extractFileClass :: FilePath -> IO (Map ClassName ClassSource)
extractFileClass path = return $ Map.fromList [(pathToClass path, ClassFile path)]

pathToClass :: FilePath -> ClassName
pathToClass path = head $ splitOn "." path

classToPath :: ClassName -> FilePath
classToPath name = name ++ ".class"



getClassBytes :: ClassName -> ClassPathLayout -> ExceptT VMError IO BSS.ByteString
getClassBytes name (ClassPathLayout classes _) = do
  source <- ExceptT $ return $ maybeToEither (ClassNotFoundException name) $ Map.lookup name classes
  getClassFromSource name source

getClassFromSource :: ClassName -> ClassSource -> ExceptT VMError IO BSS.ByteString
getClassFromSource name (ClassFile path) = do
  x <- ExceptT $ return $ maybeToEither (ClassNotFoundException name) $ classMatchesPath name path
  lift $ BSL.toStrict <$> BSL.readFile x
getClassFromSource name (JarFile path) = ExceptT $ do
  raw <- BSL.readFile path
  return $ maybeToEither (ClassNotFoundException name) $ do
    let arc = toArchive raw
        classPath = classToPath name
    entry <- findEntryByPath classPath arc
    return $ BSL.toStrict $ fromEntry entry

classMatchesPath :: String -> FilePath -> Maybe FilePath
classMatchesPath name path = if classToPath name /= path then Nothing else Just path
