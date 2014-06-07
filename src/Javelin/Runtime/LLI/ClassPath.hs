module Javelin.Runtime.LLI.ClassPath (getClassSourcesLayout, getClassBytes, Layout, ClassName)

where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy as BS (readFile)
import Control.Monad (forM)
import Control.Applicative ((<$>))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Data.Map.Lazy as Map (Map, fromList, lookup)
import Data.List

import Control.Monad.Trans.Maybe
import Control.Monad.Trans

import Data.List.Split
import Codec.Archive.Zip




-- Getting layout for classpath
  
type Layout = Map ClassName ClassSource
type ClassName = String
data ClassSource = JarFile { getPath :: FilePath }
                 | ClassFile { getPath :: FilePath }
                 deriving (Show, Eq)

getClassSourcesLayout :: FilePath -> IO Layout
getClassSourcesLayout dir = do
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
  return $ map (\c -> (c, s))  $ map pathToClass paths
extractClasses s@(ClassFile p) = return [(pathToClass p, s)]

getJarClasses :: FilePath -> IO [FilePath]
getJarClasses path = do
  raw <- BS.readFile path
  let arc = toArchive raw
  let allFiles = filesInArchive arc
  return $ map getPath $ foldl folder [] allFiles
  
pathToClass :: FilePath -> ClassName
pathToClass path = replace '/' '.' $ head $ splitOn "." path

classToPath :: ClassName -> FilePath
classToPath name = (replace '.' '/' name) ++ ".class"




-- using MaybeT { IO (Maybe a) }
getClassBytes :: ClassName -> IO Layout -> MaybeT IO ByteString
getClassBytes name layout = do
  source <- MaybeT $ Map.lookup name <$> layout
  getClassFromSource name source

getClassFromSource :: ClassName -> ClassSource -> MaybeT IO ByteString
getClassFromSource name (ClassFile path) = do
  x <- toMaybeT $ classMatchesPath name path
  lift $ BS.readFile x
getClassFromSource name (JarFile path) = MaybeT $ do
  raw <- BS.readFile path
  return $ do
    let arc = toArchive raw
        classPath = classToPath name
    entry <- findEntryByPath classPath arc
    return $ fromEntry entry

classMatchesPath :: String -> FilePath -> Maybe FilePath
classMatchesPath name path = if classToPath name /= path then Nothing else Just path




-- These are some missing Haskell functions, need to move somewhere else
toMaybeT :: (Monad m) => Maybe a -> MaybeT m a
toMaybeT = MaybeT . return

-- Srsly, Haskell, where is my 'replace' function?
replace :: Char -> Char -> String -> String
replace co cr = map (\c -> if c == co then cr else c)
