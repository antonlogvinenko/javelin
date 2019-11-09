module Javelin.Interpreter.Termination where

import           Javelin.Capability.Classes
import           Javelin.Interpreter.JVMApp
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           System.Exit                (die)
  
instance Termination JVM where
    terminate = liftIO . die . show