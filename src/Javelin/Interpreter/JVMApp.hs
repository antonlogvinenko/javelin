{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module Javelin.Interpreter.JVMApp where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), ask, asks)
import Javelin.Lib.Structures
  ( ClassId
  , ClassPathLayout
  , LoadedClass
  , Runtime
  , VMError
  )
import System.Exit (die)

data JVMConfig =
  JVMConfig
    { loggingMode :: Bool
    }

newtype JVM a =
  JVMX
    { unJVM :: ReaderT JVMConfig IO a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader JVMConfig)

runJVMApp :: JVM a -> JVMConfig -> IO a
runJVMApp jvmApp jvmConfig = runReaderT (unJVM jvmApp) jvmConfig
