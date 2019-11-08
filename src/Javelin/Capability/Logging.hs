module Javelin.Capability.Logging where

import           Javelin.JVMApp
import           Control.Monad.IO.Class     (MonadIO, liftIO)

class Monad m => Logging m where
    dump :: Show a => String -> a -> m ()
    console :: Show a => String -> a -> m ()
    
instance Logging JVM where
    dump x a = liftIO $ writeFile "jvmDump.log" (x ++ show a)
    console x a = liftIO $ putStrLn (x ++ (show a))