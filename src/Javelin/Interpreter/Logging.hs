{-# LANGUAGE InstanceSigs #-}
module Javelin.Interpreter.Logging where

import Control.Monad.Reader
import Javelin.Capability.Classes
import Javelin.Interpreter.JVMApp
import qualified Rainbow as R
import Data.Text as Text
import Data.Time.Clock as Clock

doIO :: Bool -> IO () -> JVM ()
doIO True io = liftIO io
doIO False _ = liftIO $ return ()

instance Logging JVM where
  dump x a = do
    logging <- asks loggingMode
    doIO logging $ writeFile "jvmDump.log" (x ++ " " ++ show a)
  console x a = do
    logging <- asks loggingMode
    started <- asks start
    doIO logging $ do
      time <- Clock.getCurrentTime
      let diff = Clock.diffUTCTime time started
      R.putChunk $ R.fore R.green $ R.chunk $ Text.pack ("[" ++ (show diff) ++ "] ")
      R.putChunkLn $ R.fore R.magenta $ R.chunk $ Text.pack (x ++ " " ++ show a)
  emptyLine = do
    logging <- asks loggingMode
    doIO logging $ R.putChunkLn $ R.chunk $ Text.pack ""
  msg x = console "msg: " x