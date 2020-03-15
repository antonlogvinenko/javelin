{-# language GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds #-}

module Javelin.Interpreter.JVMApp where

import           Control.Monad.Reader       (ReaderT(..), MonadReader, ask, asks)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Javelin.Lib.Structures     (ClassPathLayout, VMError, LoadedClass, ClassId, Runtime)
import           System.Exit                (die)

data JVMConfig = JVMConfig { silentMode :: Bool }

newtype JVM a = JVMX { unJVM :: ReaderT JVMConfig IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader JVMConfig)

runJVMApp :: JVM a -> JVMConfig -> IO a
runJVMApp jvmApp jvmConfig = runReaderT (unJVM jvmApp) jvmConfig