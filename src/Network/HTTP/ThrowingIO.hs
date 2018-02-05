{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.HTTP.ThrowingIO (throwingHttp) where

import Control.Exception (throwIO)
import Control.Monad

import Control.Monad.IO.Class

import Network.HTTP.Req

newtype MonadThrowingHttp m a = MonadThrowingHttp { throwingHttp :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadHttp (MonadThrowingHttp m) where
  handleHttpException = MonadThrowingHttp . liftIO . throwIO
