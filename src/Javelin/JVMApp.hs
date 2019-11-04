{-# language GeneralizedNewtypeDeriving #-}
module Javelin.JVMApp where

import           Control.Monad.Reader       (ReaderT(..), MonadReader, ask, asks)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Javelin.Runtime.Structures

data JVMConfig = JVMConfig

newtype JVM a = JVMX { unJVM :: ReaderT JVMConfig IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader JVMConfig)

class Monad m => ClassPathLayout m where
  getClassSourcesLayout2 :: String -> m (Either VMError String)

class Monad m => ClassLoading m where
  loadClass :: String -> m (Either VMError LoadedClass)
  initClass :: String -> m (Either VMError LoadedClass)

class Monad m => Logging m where
  logX :: Show a => a -> m ()
  consoleX :: Show a => a -> m ()


instance Logging JVM where
  logX = liftIO . putStrLn . show
  consoleX = liftIO . (writeFile "jvm.log") . show 
