{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module Javelin.Interpreter.JVMApp where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT(..))

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
