{-# language GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds #-}

module Javelin.JVMApp where

import           Control.Monad.Reader       (ReaderT(..), MonadReader, ask, asks)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Javelin.Runtime.Structures (ClassPathLayout, VMError, LoadedClass, ClassId, Runtime)
import           System.Exit                (die)



-- class (MonadClassPathLayout m, Logging m, Termination m, ClassLoading m)=> Global m
type Global m = (MonadClassPathLayout m, Logging m, Termination m, ClassLoading m )

class Monad m => MonadClassPathLayout m where
  getClassSourcesLayout2 :: String -> m (Either VMError ClassPathLayout)

class Monad m => ClassLoading m where
  loadClass :: String -> m (Either VMError LoadedClass)
  initClass :: ClassId -> Runtime -> m (Either VMError Runtime)

class Monad m => Termination m where
  terminate :: Show a => a -> m ()

class Monad m => Logging m where
  dump :: Show a => String -> a -> m ()
  console :: Show a => String -> a -> m ()



data JVMConfig = JVMConfig

newtype JVM a = JVMX { unJVM :: ReaderT JVMConfig IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader JVMConfig)
  
instance ClassLoading JVM where
  loadClass = undefined
  initClass = undefined

instance Logging JVM where
  dump x a = liftIO $ writeFile "jvm.dump" (x ++ show a)
  console x a = liftIO $ putStrLn (x ++ (show a))

instance Termination JVM where
  terminate = liftIO . die . show

instance MonadClassPathLayout JVM where
  getClassSourcesLayout2 = undefined

runJVMApp :: JVM a -> JVMConfig -> IO a
runJVMApp jvmApp jvmConfig = runReaderT (unJVM jvmApp) jvmConfig