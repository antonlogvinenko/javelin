{-# language GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds #-}

module Javelin.JVMApp where

import           Control.Monad.Reader       (ReaderT(..), MonadReader, ask, asks)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Javelin.Runtime.Structures (ClassPathLayout, VMError, LoadedClass, ClassId, Runtime)
import           System.Exit                (die)


-- todo move to appropriate module
class Monad m => ClassLoading m where
  loadClass :: String -> m (Either VMError LoadedClass)
  initClass :: ClassId -> Runtime -> m (Either VMError Runtime)
instance ClassLoading JVM where
  loadClass = undefined
  initClass = undefined
  

data JVMConfig = JVMConfig

newtype JVM a = JVMX { unJVM :: ReaderT JVMConfig IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader JVMConfig)

runJVMApp :: JVM a -> JVMConfig -> IO a
runJVMApp jvmApp jvmConfig = runReaderT (unJVM jvmApp) jvmConfig