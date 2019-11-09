module Javelin.Capability.Classes where

import           Data.ByteString            as BSS (ByteString)
import           Javelin.Lib.Structures

class Monad m => ClassPathLoading m where
    getClassSourcesLayout :: String -> m ClassPathLayout
    getClassBytes :: ClassName -> ClassPathLayout -> m (Either VMError BSS.ByteString)

class Monad m => ClassLoading m where
    loadClassX :: String -> m (Either VMError LoadedClass)
    initClassX :: ClassId -> Runtime -> m (Either VMError Runtime)

class Monad m => Logging m where
    dump :: Show a => String -> a -> m ()
    console :: Show a => String -> a -> m ()

class Monad m => Termination m where
    terminate :: Show a => a -> m ()