{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Provides compositions of the functions from Data.IORef
-- with Data.Has' getter. This greatly facilitates the ReaderT
-- pattern.
--
-- This module provides plain functions with an IO result type instead of relying on
-- MonadReader/MonadIO constraints to facilitate working with libraries
-- which are specialised to IO. The polymorphic variants can be recovered with
-- reader and liftIO.
module Data.Has.IORef (
  module Data.Has.IORef,
  -- | Reexported for convenience
  module Data.IORef,
  -- | Reexported for convenience.
  module Data.Has
) where

import           Data.IORef

import           Data.Has

import           Data.Has.Generic

type HasRef env a = Has env (IORef a)

readRef :: HasRef env a => env -> IO a
readRef = readIORef . getter

writeRef :: HasRef env a => env -> a -> IO ()
writeRef = writeIORef . getter

modifyRef :: HasRef env a => env -> (a -> a) -> IO ()
modifyRef = modifyIORef . getter

modifyRef' :: HasRef env a => env -> (a -> a) -> IO ()
modifyRef' = modifyIORef' . getter

atomicModifyRef :: HasRef env a => env -> (a -> (a, b)) -> IO b
atomicModifyRef = atomicModifyIORef . getter

atomicModifyRef' :: HasRef env a => env -> (a -> (a, b)) -> IO b
atomicModifyRef' = atomicModifyIORef' . getter
