module Javelin.Interpreter.StdIO where

import Control.Monad.IO.Class (liftIO)
import Javelin.Capability.Classes
import Javelin.Interpreter.JVMApp

instance StdIO JVM where
  runIO = liftIO
