{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.HTTP.ThrowingIO (throwingHttp) where

import Control.Exception (throwIO)
import Control.Monad

import Control.Monad.IO.Class

import Network.HTTP.Req

newtype MonadThrowingHttp a = MonadThrowingHttp { throwingHttp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadHttp MonadThrowingHttp where
  handleHttpException = MonadThrowingHttp . throwIO