module Javelin.Runtime.LLI.ClassPath (getClassSourcesLayout, getClassBytes)

where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy as BS (readFile)
import Control.Monad (forM)
import Control.Applicative ( (<$>))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Data.Map.Lazy as Map (Map, fromList, (!), lookup)
import Data.List

import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.List.Split
import Codec.Archive.Zip

-- Getting all classpath files

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




-- Getting layout for classpath
  
type Layout = Map ClassName ClassSource
type ClassName = String
data ClassSource = JarFile { getPath :: FilePath }
                 | ClassFile { getPath :: FilePath }
                 deriving (Show, Eq)

getClassSourcesLayout :: FilePath -> IO Layout
getClassSourcesLayout dir = do
  files <- getClassPathFiles dir
  let sources = foldl folder [] $ map getType files
  classes <- mapM extractClasses sources
  return $ Map.fromList $ concat classes

getType :: FilePath -> Maybe ClassSource
getType path
  | ".class" `isSuffixOf` path = Just $ ClassFile path
  | ".jar" `isSuffixOf` path = Just $ JarFile path
  | otherwise = Nothing

folder :: [ClassSource] -> Maybe ClassSource -> [ClassSource]
folder list (Just x) = x:list
folder list Nothing = list

extractClasses :: ClassSource -> IO [(ClassName, ClassSource)]
extractClasses s@(JarFile path) = do
  paths <- getJarContents path
  return $ map (\c -> (c, s))  $ map stripClassName paths
extractClasses s@(ClassFile p) = return [(stripClassName p, s)]

stripClassName :: FilePath -> ClassName
stripClassName path = head $ splitOn "." $ last $ splitOn "/" path

getJarContents :: FilePath -> IO [FilePath]
getJarContents path = do
  raw <- BS.readFile path
  let arc = toArchive raw
  return $ filesInArchive arc


  

-- using MaybeT { IO (Maybe a) }
getClassBytes :: ClassName -> IO Layout -> IO (Maybe ByteString)
getClassBytes name layout = runMaybeT $ do
  source <- MaybeT $ Map.lookup name <$> layout
  lift $ getClassFromSource name source

getClassFromSource :: ClassName -> ClassSource -> IO ByteString
getClassFromSource _ (ClassFile path) = BS.readFile path
getClassFromSource name (JarFile path) = undefined






