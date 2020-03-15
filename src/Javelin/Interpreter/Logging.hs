module Javelin.Interpreter.Logging where

import           Javelin.Capability.Classes
import           Javelin.Interpreter.JVMApp
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader
    
doIO :: Bool -> IO () -> JVM ()
doIO True io = liftIO io
doIO False _ = liftIO $ return ()

instance Logging JVM where
    dump x a = do
        silent <- asks silentMode
        doIO silent $ writeFile "jvmDump.log" (x ++ " " ++ show a)
    console x a = do
        silent <- asks silentMode
        doIO silent $ putStrLn (x ++ " " ++ (show a))
    say x = do
        silent <- asks silentMode
        doIO silent $ putStrLn x