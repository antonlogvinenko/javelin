module Javelin.Interpreter.Termination where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Javelin.Capability.Classes
import Javelin.Interpreter.JVMApp
import System.Exit (die)

instance Termination JVM where
  terminate = liftIO . die . show
