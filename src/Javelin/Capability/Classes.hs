{-# LANGUAGE ConstraintKinds #-}

module Javelin.Capability.Classes where

import Data.ByteString as BSS (ByteString)
import Javelin.Lib.Structures

class Monad m =>
      ClassPathLoading m
  where
  getClassSourcesLayout :: String -> m ClassPathLayout

class Monad m =>
      ClassLoading m
  where
  getClassBytes ::
       ClassName -> ClassPathLayout -> m (Either VMError BSS.ByteString)
  initClass :: ClassId -> Runtime -> m (Either VMError Runtime)
  loadClass :: ClassId -> Runtime -> m (Either VMError Runtime)

class Monad m =>
      Logging m
  where
  dump :: Show a => String -> a -> m ()
  console :: Show a => String -> a -> m ()
  say :: String -> m ()

class Monad m =>
      Termination m
  where
  terminate :: Show a => a -> m ()

class Monad m =>
      StdIO m
  where
  runIO :: IO a -> m a

type Global m
   = ( ClassPathLoading m
     , Logging m
     , Termination m
     , ClassLoading m
     , Logging m
     , StdIO m)
