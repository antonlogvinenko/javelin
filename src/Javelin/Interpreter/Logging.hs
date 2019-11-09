module Javelin.Interpreter.Logging where

import           Javelin.Capability.Classes
import           Javelin.Interpreter.JVMApp
import           Control.Monad.IO.Class     (MonadIO, liftIO)
    
instance Logging JVM where
    dump x a = liftIO $ writeFile "jvmDump.log" (x ++ show a)
    console x a = liftIO $ putStrLn (x ++ (show a))