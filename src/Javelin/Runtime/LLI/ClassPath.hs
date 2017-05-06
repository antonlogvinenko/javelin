module Javelin.Runtime.LLI.ClassPath (getClassSourcesLayout, getClassBytes)

where

import Data.ByteString.Lazy as BSL (ByteString, toStrict, readFile)
import Data.ByteString as BSS (ByteString)
import Control.Monad (forM)
import Control.Arrow ((>>>))
import Control.Applicative ((<$>))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Data.Map.Lazy as Map (fromList, lookup, unions)
import Data.List

import Control.Monad.Trans.Maybe
import Control.Monad.Trans

import Data.List.Split
import Codec.Archive.Zip

import Javelin.Runtime.Structures
import Data.List.Split (splitOn)
import Javelin.Util


-- Getting layout for classpath
getClassSourcesLayout :: String -> IO ClassPathLayout
getClassSourcesLayout paths =
  let pathsList = splitOn ";" paths
      layoutList = sequence $ map getClassSourceLayout pathsList :: IO [ClassPathLayout]
  in unions <$> layoutList :: IO ClassPathLayout

getClassSourceLayout :: FilePath -> IO ClassPathLayout
getClassSourceLayout dir = do
  files <- getClassPathFiles dir
  let sources = foldl folder [] files
  classes <- mapM extractClasses sources
  return $ Map.fromList $ concat classes

getClassPathFiles :: FilePath -> IO [FilePath]
getClassPathFiles dir = do
  files <- map (dir </>) <$> filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
  allFiles <- forM files weNeedToGoDeeper
  return $ concat allFiles
  
weNeedToGoDeeper :: FilePath -> IO [FilePath]
weNeedToGoDeeper path = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then getClassPathFiles path
    else return [path]

folder :: [ClassSource] -> FilePath -> [ClassSource]
folder list path
  | ".class" `isSuffixOf` path = ClassFile path : list
  | ".jar" `isSuffixOf` path = JarFile path : list
  | otherwise = list

extractClasses :: ClassSource -> IO [(ClassName, ClassSource)]
extractClasses s@(JarFile path) = do
  paths <- getJarClasses path
  map pathToClass >>> map (\c -> (c, s)) >>> return $ paths
extractClasses s@(ClassFile p) = return [(pathToClass p, s)]

getJarClasses :: FilePath -> IO [FilePath]
getJarClasses path = do
  raw <- BSL.readFile path
  let arc = toArchive raw
  let allFiles = filesInArchive arc
  return $ map getPath $ foldl folder [] allFiles
  
pathToClass :: FilePath -> ClassName
pathToClass path = replace '/' '.' $ head $ splitOn "." path

classToPath :: ClassName -> FilePath
classToPath name = (replace '.' '/' name) ++ ".class"



-- using MaybeT { IO (Maybe a) }
getClassBytes :: ClassName -> ClassPathLayout -> MaybeT IO BSS.ByteString
getClassBytes name layout = do
  source <- toMaybeT $ Map.lookup name layout
  getClassFromSource name source

getClassFromSource :: ClassName -> ClassSource -> MaybeT IO BSS.ByteString
getClassFromSource name (ClassFile path) = do
  x <- toMaybeT $ classMatchesPath name path
  lift $ BSL.toStrict <$> BSL.readFile x
getClassFromSource name (JarFile path) = MaybeT $ do
  raw <- BSL.readFile path
  return $ do
    let arc = toArchive raw
        classPath = classToPath name
    entry <- findEntryByPath classPath arc
    return $ BSL.toStrict $ fromEntry entry

classMatchesPath :: String -> FilePath -> Maybe FilePath
classMatchesPath name path = if classToPath name /= path then Nothing else Just path
