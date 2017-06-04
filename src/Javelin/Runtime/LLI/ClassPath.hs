module Javelin.Runtime.LLI.ClassPath (getClassSourcesLayout, getClassBytes)
where

import Data.Map as Map (Map, fromList, lookup, unions, empty)
import Data.List
import Data.ByteString.Lazy as BSL (ByteString, toStrict, readFile)
import Data.ByteString as BSS (ByteString)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))

import Codec.Archive.Zip
import Data.Either.Utils (maybeToEither)
import Data.String.Utils (strip)
import Data.List.Split (splitOn)

import Javelin.Runtime.Structures

import Control.Applicative ((<$>))
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Flow


getClassSourcesLayout :: String -> ExceptT VMError IO ClassPathLayout
getClassSourcesLayout paths =
  let pathsList = strip <$> splitOn ";" paths
  in lift $ do
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
         files <- map (path </>) <$> filter (`notElem` [".", ".."]) <$> getDirectoryContents path
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
extractFileClass path = return $ Map.fromList [(pathToClass path, ClassFile path)]

pathToClass :: FilePath -> ClassName
pathToClass path = head $ splitOn "." path

isZip :: FilePath -> Bool
isZip path = any (\s -> isSuffixOf s path) [".jar", ".zip", ".war", ".ear"]


getClassBytes :: ClassName -> ClassPathLayout -> ExceptT VMError IO BSS.ByteString
getClassBytes name (ClassPathLayout classes _) = do
  source <- classes |> Map.lookup name |> maybeToEither (ClassNotFoundException name) |> return |> ExceptT
  getClassFromSource name source

getClassFromSource :: ClassName -> ClassSource -> ExceptT VMError IO BSS.ByteString
getClassFromSource name (ClassFile path) =
  if classToPath name == path
  then lift $ BSL.toStrict <$> BSL.readFile path
  else throwE $ ClassNotFoundException name
       
getClassFromSource name (JarFile path) = ExceptT $ do
  raw <- BSL.readFile path
  return $ maybeToEither (ClassNotFoundException name) $ do
    let arc = toArchive raw
        classPath = classToPath name
    entry <- findEntryByPath classPath arc
    return $ BSL.toStrict $ fromEntry entry

classToPath :: ClassName -> FilePath
classToPath name = name ++ ".class"
