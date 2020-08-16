module Javelin.Interpreter.Logging where

import Control.Monad.Reader
import Javelin.Capability.Classes
import Javelin.Interpreter.JVMApp
import qualified Rainbow as R
import Data.Text as Text

doIO :: Bool -> IO () -> JVM ()
doIO True io = liftIO io
doIO False _ = liftIO $ return ()

instance Logging JVM where
  dump x a = do
    logging <- asks loggingMode
    doIO logging $ writeFile "jvmDump.log" (x ++ " " ++ show a)
  console x a = do
    logging <- asks loggingMode
    doIO logging (R.putChunkLn $ R.fore R.blue $ R.chunk $ Text.pack (x ++ " " ++ show a))
  say x = do
    logging <- asks loggingMode
    doIO logging (R.putChunkLn $ R.fore R.green $ R.chunk $ Text.pack x)
