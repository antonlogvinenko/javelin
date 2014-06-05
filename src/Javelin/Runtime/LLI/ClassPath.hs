module Javelin.Runtime.LLI.ClassPath (getClassSourcesLayout, getClassBytes)

where

import Data.ByteString (ByteString)
import Control.Monad (forM)
import Control.Applicative ( (<$>))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Data.ByteString as BS (readFile)
import Data.Map.Lazy as Map (Map, fromList, (!), lookup)
import Data.List

import Control.Monad.Trans
import Control.Monad.Trans.Maybe


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
getClassSourcesLayout dir = Map.fromList <$>
                            concat <$>
                            map extractClasses <$>
                            foldl folder [] <$>
                            map getType <$>
                            getClassPathFiles dir

getType :: FilePath -> Maybe ClassSource
getType path
  | ".class" `isSuffixOf` path = Just $ ClassFile path
  | ".jar" `isSuffixOf` path = Just $ JarFile path
  | otherwise = Nothing

folder :: [ClassSource] -> Maybe ClassSource -> [ClassSource]
folder list (Just x) = x:list
folder list Nothing = list

extractClasses :: ClassSource -> [(ClassName, ClassSource)]
extractClasses (JarFile p) = undefined
extractClasses s@(ClassFile p) = [(stripClassName p, s)]

stripClassName :: FilePath -> ClassName
stripClassName p = undefined



-- Getting class bytecode
toMaybeT :: Maybe a -> MaybeT IO a
toMaybeT = MaybeT . return

-- using MaybeT { IO (Maybe a) }
getClassBytes :: ClassName -> Layout -> IO (Maybe ByteString)
getClassBytes name layout = runMaybeT $ do
  path <- toMaybeT $ getPath <$> Map.lookup name layout
  lift $ BS.readFile path
