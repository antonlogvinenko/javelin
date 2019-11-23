module Javelin.Interpreter.StdIO where

import           Javelin.Capability.Classes
import           Javelin.Interpreter.JVMApp
import           Control.Monad.IO.Class     (MonadIO, liftIO)
    
instance StdIO JVM where
    runIO = liftIO