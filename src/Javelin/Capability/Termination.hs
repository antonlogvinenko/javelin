module Javelin.Capability.Termination where

import           Javelin.JVMApp
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           System.Exit                (die)

class Monad m => Termination m where
    terminate :: Show a => a -> m ()
  
instance Termination JVM where
    terminate = liftIO . die . show